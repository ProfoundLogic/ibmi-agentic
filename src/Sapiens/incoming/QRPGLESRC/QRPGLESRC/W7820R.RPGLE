     H Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7820R - COMMERCIAL ROLLOVER
     H*****************************************************************
     H**                                                              *
     H** PROGRAM DESCRIPTION:                                         *
     H**    This is a Commercial version of the Rollover process      *
     H**    which runs for a range of Policy Expiration dates.        *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**    W7820C                                                    *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**    SRDDIF8                                                   *
     H**                                                              *
     H** PROGRAMMER: C:57                                             *
     H** CREATED:    08/28/96         PROJECT: CM022                  *
     H**                                                              *
     H** NOTE:       BASED ON CLT 068 W7820R2                         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/01/25 C:300 Support: 119372 (CoveRisk)                     *
     H*                Auto Renewal flag from Increased limit file not*
     H*                working. WTLMP read fails due to 0 KKEFDT      *
     H*                                                               *
     H* 04/07/25 C:65  Support: 115957 (Sunz)                         *
     H*                Ignore PEO rule if leasing ID of '1'.          *
     H*                                                               *
     H* 06/24/24 C:300 Project: 146WKR (Prescient)                    *
     H*                Clear Collateral fields (ELCOLWP, ELCOLWA, and *
     H*                ELCOLRP) in WDELP in renewal year              *
     H*                                                               *
     H* 05/10/24 C:079 Support: 115133                                *
     H*                Recompile due to change in WPIENDORSE          *
     H*                                                               *
     H* 03/13/24 C:062 Project: 146WKQ                                *
     H*                Recompile                                      *
     H*                                                               *
     H* 03/04/24 C:079 Project: WC2951                                *
     H*                Recompile over WDELP changes                   *
     H*                                                               *
     H* 01/04/24 C:300 Support: 104074 (CoveRisk)                     *
     H*                Default Rate Deviation in Quote Renewal based  *
     H*                on set-up even if current year does not have   *
     H*                deviation. Effective date of deviation in renewal
     H*                year should be effective date of renewal year. *
     H*                                                               *
     H* 09/22/23 C:65  Support: 113990 (Sunz - 150WDE)                *
     H*                Begin counting reporting weeks on policy eff   *
     H*                date not billing start date.  Add in safety    *
     H*                coding to stop counting weeks at 99.           *
     H*                                                               *
     H* 04/25/23 C:291 Support: 100129                                *
     H*                Do not copy all the cancellation detail/history*
     H*                records from SRD to New added division when div*
     H*                is added to cancelled policy. Only create CX   *
     H*                Detail/History for current active CX record.   *
     H*                                                               *
     H* 03/14/23 C:333 Support: 103849                                *
     H*                "DNQ" Mod Rollover at Renewal.                 *
     H*                                                               *
     H* 10/20/22 C:291 Project 148WDV                                 *
     H*                Do not roll the payrolls if new option 'F'     *
     H*                is selected.                                   *
     H*                                                               *
     H* 07/18/22 C:33  Support: 94693                                 *
     H*                Use standardized waiver routine.               *
     H*                                                               *
     H* 03/14/22 C:65  Support: 101377 (WC2278)                       *
     H*                Do not renew contacts into quote - no more     *
     H*                quote file.                                    *
     H*                                                               *
     H* 01/25/22 C:136 Support: 98291                                 *
     H*               -Don't renew all divisions when called from     *
     H*                midterm add of divisions. Added division parm. *
     H*               -Don't overwrite cx issue date on cx detail and *
     H*                history records.                               *
     H*               -Clear cx date when writing quote WMEMP         *
     H*                                                               *
     H* 02/24/21 C:65  Support: 94246 (Amwins)                        *
     H*                When renewing in a non-commerical group, check *
     H*                group year ending date against policy exp date.*
     H*                                                               *
     H* 11/18/20 C:180 Project WC2757                                 *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 11/05/20 C:65  Support: 92687 (CoveRisk)                      *
     H*                If deviations are being defaulted to zero then *
     H*                clear the effective/expiration date fields.    *
     H*                                                               *
     H* 01/23/20 C:65  Support: 86879 (LUBA)                          *
     H*                If 3C verbiage is not checked to be overridden *
     H*                then clear the fields in employer detail.      *
     H*                                                               *
     H* 12/17/19 C:180 Project: 150WEH                                *
     H*                Clear new field ELAUDSR Audited with SR        *
     H*                                                               *
     H* 10/09/19 C:298 Project: WC2685                                *
     H*                Mid-Term Add of Division on Cancelled Policy.  *
     H*                                                               *
     H* 10/15/19 C:65  Support: 84006 (LUBA)                          *
     H*                Remove any "Q" status records from WAQTP prior *
     H*                to rolling agent info and possibly creating    *
     H*                new WAQTP records.  A division renewing should *
     H*                not have a "quote" status record any longer as *
     H*                it is a live division.                         *
     H*                                                               *
     H* 06/14/19 C:136 Support: 79276 (133WQP)                        *
     H*                Add WXFMP1 to files that roll with waivers     *
     H*                to quote.                                      *
     H*                                                               *
     H* 05/15/19 C:300 Support: 79413 (SUNZ)                          *
     H*                Unable to Renew Master Policy                  *
     H*                Increaseing WKD array size                     *
     H*                                                               *
     H* 05/13/19 C:180 Project: C150WDY                               *
     H*                Add renewal option E.                          *
     H*                                                               *
     H* 04/16/19 C:291 Project: WC2206                                *
     H*                Clear Exp Mod 1 Not Rated Flag.                *
     H*                                                               *
     H* 04/15/19 C:291 Support: 75198                                 *
     H*                Copy only one wmmup record for specific waiver *
     H*                to Renewal year under seq 0.                   *
     H*                                                               *
     H* 01/09/19 C:267 Support: 74870                                 *
     H*                Removed 12/27/18 changes for this support below.
     H*                Revise logic for Vendors whereby ALL Vendors   *
     H*                for all divisions are rolling over at renewal. *
     H*                                                               *
     H* 01/08/19 C:33  Project: WC2610                                *
     H*                Clear score values.
     H*                                                               *
     H* 12/27/18 C:267 Support: 74870                                 *
     H*                Vendors for ALL divisions are not rolling over *
     H*                at renewal.  Added new logical without division.
     H*                                                               *
     H*                                                               *
     H* 12/14/18 C:291 Project: 133WQP                                *
     H*                Copy only one wmmup record per form to Quote.  *
     H*                                                               *
     H* 10/29/18 C:180 Project: 150WDE                                *
     H*                Process weekly reports option b.               *
     H*                                                               *
     H* 11/12/18 C:291 Project: 133WQP                                *
     H*                Copy Waiver details when renew to Quote.       *
     H*                                                               *
     H* 08/20/18 C:291 Project: 133WQP                                *
     H*                Renew Waiver Endorsements to Quote.            *
     H*                                                               *
     H* 07/12/18 C:291 Project: 148WAV                                *
     H*                Default deviations on Renewal.                 *
     H*                                                               *
     H* 05/11/18 C:291 Project: 150WCR                                *
     H*                Do not send policy to error report if policy   *
     H*                is overridden for Master/MCP policy assignment.*
     H*                                                               *
     H* 05/01/18 C:33  Support: 69695                                 *
     H*                Ensure large deductibles renew                 *
     H*                                                               *
     H* 02/21/18 C:137 Project: WC2588                                *
     H*                Clear quote transfer date field                *
     H*                                                               *
     H* 02/01/18 C:136 Project: 148WAH                                *
     H*                Clear policy fee fields in WDELP               *
     H*                                                               *
     H* 12/06/17  C:33  Support: 51426                                *
     H*                 Add renewal year                              *
     H*                                                               *
     H* 09/08/17 C:65  Project: 150WAR                                *
     H*                Default % of manual premium from Group Year    *
     H*                when rolling policies for client 150.          *
     H*                                                               *
     H* 08/08/17 C:062 Project 150WAP                                 *
     H*                Add Combined Peo processing                    *
     H*                                                               *
     H* 04/07/17 C:062 Project 136WHF                                 *
     H*                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 03/31/17 C:136 Support: 46414                                 *
     H*                Prevent renewal to live if renewal to quote    *
     H*                exists.
     H*                                                               *
     H* 08/17/16 C:298 PROJECT: WC2070                                *
     H*                Clear ANC charge field                         *
     H*                                                               *
     H* 01/12/16 C:136 Support: PHW39248                              *
     H*                Only allow divisions with policy to date       *
     H*                within range to renew.                         *
     H*                                                               *
     H* 12/14/15 C:65  Project: Phw#38862 - LUBA (Ref WC2254)         *
     H*                Do not print error of future year exists.      *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 06/23/15 C:136 Project: WC2182                                *
     H*               -Single Employer Renewal                        *
     H*               -Remove 82398 per GW
     H*                                                               *
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 07/16/14 C:136 Project: WC2295                                *
     H*                Do not annualize OK Rate Split payrolls        *
     H*                for 02/01/2014                                 *
     H*                                                               *
     H* 07/10/14 C:290 Support: WA114 86103                           *
     H*                If a group is claims only then allowed the     *
     H*                employer to rollover to next year though the   *
     H*                rate set code is inactive                      *
     H*                                                               *
     H* 06/16/14 C:136 Project: WC2283                                *
     H*                Do not annualize TN Rate Split payrolls        *
     H*                                                               *
     H* 06/06/14 C:298 PROJECT 129WSB : Redmine # 4237                *
     H*                Project 129WAI: Moved back to base.            *
     H*                                                               *
     H* 05/20/14 C:302 Support: WA136 85492                           *
     H*                Clear net rate fields on renewal.              *
     H*                                                               *
     H* 03/16/14 C:302 Support: WA133 79797                           *
     H*                Use the Policy year effective date to get      *
     H*                the payroll class when the current one has     *
     H*                expired and has a transfer code.               *
     H*                                                               *
     H* 12/06/13 C:283 Support: WA093 84124                           *
     H*                Use the latest Policy year effective date      *
     H*                when checking if the Class Code is expired.    *
     H*                                                               *
     H* 11/19/13 C:290 SUPPORT: WA102 82398                           *
     H*                Do not rollover if the Rate Set Code is no     *
     H*                longer active.                                 *
     H*                                                               *
     H* 11/15/13 C:290 SUPPORT: WA114 83707                           *
     H*                Intialize the Renewal Tracking dates prior to  *
     H*                Rollover process to populate them correctly.   *
     H*                                                               *
     H* 09/18/13 C:180 Support WA146 82907                            *
     H*                New renewal quote statuses.                    *
     H*                                                               *
     H* 07/03/13 C:136 Project: 147WAI                                *
     H*                Roll dividend plan info in WDELP if flagged    *
     H*                to do so in Dividend Plan Table                *
     H*                                                               *
     H* 07/02/13 C:282 Project: 147WAZ  Priss #17                     *
     H*                Condition roll of Bill On Eff Day Flag         *
     H*                group detail default value for SCS only.       *
     H*                                                               *
     H* 04/02/13 C:062 SUPPORT: WA102 81895                           *
     H*                Populate date.                                 *
     H*                                                               *
     H* 03/15/13 C:190 Project: 147WAZ                                *
     H*                Clear the policy-level 'bill installments on   *
     H*                the effective date flag' on the first renewal  *
     H*                for OptaComp policies.                         *
     H*                                                               *
     H* 11/15/12 C:136 SUPPORT: WA129 81020                           *
     H*                DO NOT ROLL AGENT HISTORY RECORD IF EFF        *
     H*                DATE IS TOO FAR IN THE FUTURE                  *
     H*                                                               *
     H* 05/07/12 C:283 SUPPORT: WA090 80094                           *
     H*                Correct Billing Start Date Zero Issue.         *
    H*                                                               *
     H* 10/13/11 C:191 PROJECT WC2079                                 *
     H*                Rename WMRTP fields as they conflict with WDRTP*
     H*                                                               *
    H* 10/04/11 C:136 Support: WA099 75891                           *
    H*                Correct logic for payroll renewal              *
    H*                                                               *
     H* 09/23/11 C:191 Project: WC2037 Priss #27                      *
     H*                Clear out ELUPSA for rollover                  *
     H*                                                               *
     H* 03/04/11 C:33  Project: WC1851   Priss #111                   *
     H*               -Implement WDEFP.                               *
     H*                                                               *
     H* 02/21/11 C:246 SUPPORT: 75466 CLT099                          *
     H*                REF    : 139WPL PRISS 107                      *
     H*                PRINT THE POLICY IN THE EXPECTION REPORT FOR   *
     H*                THE  POLICY EXPIRED WITHIN SAME GROUP POLICY   *
     H*                YEAR AS IT WAS EFFECTIVE.                      *
     H*                                                               *
     H* 02/14/11 C:136 SUPPORT: WA102 75849                           *
     H*                CORRECT PAYROLL CLASS TRANSFER LOGIC           *
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP FOR POLICY PAGE, ITEM 3C.       *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Include HCN Program data.                      *
     H*                                                               *
     H* 10/18/10 C:191 Project: WC1939                                *
     H*                Converted to RPGLE                             *
     H*                Pulled in W7820R3                              *
     H*                If the payroll effective date is the ARD or the*
     H*                rate change date and a rate change is in effect*
     H*                then do not annualize the payroll amount.      *
     H*                                                               *
     H* 10/12/10 C:136 SUPPORT: WA133 74437                           *
     H*                PREVENT STATE REPORTING DIVISION FROM          *
     H*                INCORRECTLY RENEWING IF OTHER DIVISION HAS     *
     H*                DIFFERENT EXPIRATION DATE                      *
     H*                                                               *
     H* 09/07/10 C:255 PROJECT: WC1929                                *
     H*                Add changes in program to check flag 'Disable  *
     H*                DE Rate flag' based on this, changes for proje-*
     H*                ct WC1768 are disabled.                        *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                WHEN DEVIATIONS ARE SPLIT.                     *
     H*                                                               *
     H* 04/06/10 C:062 PROJECT: 25WRB PRISS 19                        *
     H*               -ADD NEW YEAR TO ENTRY PARMS                    *
     H*                                                               *
     H* 03/22/10 C:136 PROJECT: 25WRB PRISS 17                        *
     H*               -ADD NEW YEAR TO ENTRY PARMS                    *
     H*               -REMOVE PROCESSING FOR SECOND RENEWAL FLAG      *
     H*                                                               *
     H* 03/12/10 C:62  PROJECT: 25WQK                                 *
     H*                Add write to WMSHLQ                            *
     H*                                                               *
     H* 02/05/10 C:65  PROJECT: 25WRB                                 *
     H*                CALL W7820R3 WITH MODE 'L' TO COPY LARGE DEDUC *
     H*                TO NEW YEAR IF FLAGGED TO AUTO RENEW.          *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/22/10 C:259 PROJECT: 139WNQ                                *
     H*                ADD TERTIARY AGENT ON RENEWAL PROCESS          *
     H*                                                               *
     H* 01/14/10 C:190 PROJECT: 25WRC                                 *
     H*                BILLING ALOGRITHM RETROFIT                     *
     H*                - For CLT025,  clear ELSTFC, ELD2EF, ELD2EX    *
     H*                  ELD2EX, and ELMULT.                          *
     H*                                                               *
     H* 12/29/09 C:136 PROJECT: WC1861                                *
     H*                ADD AZ RATE SPLIT - 01/01/2010                 *
     H*                                                               *
     H* 12/18/09 C:33  Project: WC1017    WA046/70983                 *
     H*               -Retain new policy year dates for loading to    *
     H*                 Quote Tracking.                               *
     H*               -Only change the agent effective date on an     *
     H*                 agent of record change.                       *
     H*                                                               *
     H* 11/23/09 C:136 PROJECT: 139WNR                                *
     H*                FL RATE SPLIT FOR 07/01/09 - CLIENT SPECIFIC   *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT 93WIB                                  *
     H*                Clear the deductible discount detail fields    *
     H*                when the policy rolls to a new year.           *
     H*                Update the deductible fields with the details  *
     H*                from the deductible discount table file if the *
     H*                codes are populated                            *
     H*                                                               *
     H* 05/27/09 C:062 PROJECT 133WGW                                 *
     H*                Add agent rollover logic.                      *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use the new logical(s) to get the description  *
     H*                by extension.                                  *
     H*                                                               *
     H* 04/06/09 C:190 SUPPORT: 67727                                 *
     H*                This is a revision to current processing in    *
     H*                Renewal program which is clearing all the      *
     H*                Waiver values on the new Employer Detail record*
     H*                that is written.                               *
     H*                                                               *
     H* 01/20/09 C:136 SUPPORT: WA139 65954                           *
     H*                WRITE A MASS BILLING RECORD IF RENEWING TO     *
     H*                QUOTE PER GW                                   *
     H*                                                               *
     H* 12/16/08 C:136 PROJECT: WC1768                                *
     H*                DE RATE SPLIT FOR 10/01/08                     *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 09/03/08 C:062 PROJECT: WC1611                                *
     H*                UPDATE ELNWRW                                  *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 02/08/08 C:65  Project: 146WBC                                *
     H*                CONDITION ROLL OF BILL ON EFF DAY FLAG ON      *
     H*                GROUP DETAIL DEFAULT VALUE.                    *
     H*                                                               *
     H* 12/31/07 C:136 WA132 60309                                    *
     H*                USE SYSTEM DATE OR POLICY EFF DATE FOR AGENT   *
     H*                HISTORY EFF DT, WHICHEVER IS LESS              *
     H*                                                               *
     H* 12/26/07 C:136 SUPPORT: WA045 61616                           *
     H*                - USE POLICY EFF DT INSTEAD OF INCEPTION DATE  *
     H*                WHEN WRITING A 'RENEWAL TO QUOTE' QUOTE        *
     H*                TRACKING RECORD                                *
     H*                                                               *
     H* 12/14/07 C:136 SUPPORT: WA114 61245                           *
     H*                WRITE RENEWAL TRACKING RECORD WHEN RENEWING    *
     H*                TO QUOTE ALSO                                  *
     H*                                                               *
     H* 12/11/07 C:190 PROJECT: WC1500                               *
     H*              - Add new parm for "State Reporting Division     *
     H*                Only Renew"                                    *
     H*              - If parm is "Y" check to see what State I/E     *
     H*                is and include/exclude states that match based *
     H*                on the state reporting divisions.              *
     H*                                                               *
     H* 11/09/07 C:136 SUPPORT: WA071 60878                           *
     H*                DO NOT CLEAR STOCK DISCOUNT FIELDS IF THERE    *
     H*                IS NO SET UP AT GROUP LEVEL                    *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 55504                            *
     H*                NOT NEEDED.                                    *
     H*                                                               *
     H* 09/19/07 C:65  Project: SUPP# 59900                           *
     H*                CLEAR THE PRIOR AND CURRENT YEAR PAYROLL ARRAYS*
     H*                                                               *
     H* 07/23/07 C:33  Project: WC1196  Priss #26                     *
     H*                Clear the Mass Audit Request flag (ELMRFL) when*
     H*                 renewing.                                     *
     H*                                                               *
     H* 07/05/07 C:65  Project: WC1189  Revisions                     *
     H*                ADD CODING TO CONDITION THE CLEARING OF STOCK  *
     H*                DISCOUNT FIELDS.  ALSO ADD ADDITIONAL CODING   *
     H*                TO CLEAR THE SAFETY FEE FIELDS IF THERE IS NO  *
     H*                APPLIED TO SETTING AT STAT LEVEL. KEY STAT     *
     H*                FILE READ BASED ON PRIOR POL EFF DATE FOR      *
     H*                DO NOT RENEW FLAGS.                            *
     H*                                                               *
     H* 06/26/07 C:65  Project: WC1189  Revisions                     *
     H*                FURTHER CHANGE TO ENSURE INCR LIMITS AUTO      *
     H*                RENEW FLAGS ARE PROPERLY POPULATED.            *
     H*                                                               *
     H* 06/14/07 C:33  Project: WC1189  Revisions                     *
     H*                Changes needed for testing.                    *
     H*                                                               *
     H* 06/05/07 C:132 Project: WC1189  Revisions                     *
     H*              - Add processing to check WDRRP stat rpt rating  *
     H*                rollover flags. First check new policy year    *
     H*                for stat codes/heading, if found and has no    *
     H*                code/heading, then clear assoicated in WDELP.  *
     H*              - Check 2nd Increased Limit Auto renew flags.    *
     H*                                                               *
     H* 05/24/07 C:136 SUPPORT: WA139 51899                           *
     H*                CORRECT LOGIC FOR 'DO NOT RENEW' FLAG FOR      *
     H*                MULTI-DIV POLICIES. ALSO ADD LOGIC THAT        *
     H*                WILL EXCLUDE ALL DIVISIONS IF THE 'DO NOT      *
     H*                RENEW' FLAG IS SET TO 'Y' ON THE STATE REP     *
     H*                DIVISION.                                      *
     H*                                                               *
     H* 04/06/07 C:65  PROJECT: WC1514                                *
     H*                CLEAR NEW PREMIUM FIELD FOR SHORT RATE.        *
     H*                                                               *
     H* 02/05/07 C:132 Project: WC1189                                *
     H*                Add processing to clear all credit/surcharge   *
     H*                fields in Employer Detail (WDELP) prior to     *
     H*                writing new record to WDELP.                   *
     H*                                                               *
     H* 12/12/06 C:33  Project: WC1299                                *
     H*                Do not re-default the agent rate unless the    *
     H*                 request has been made to do so.               *
     H*                                                               *
     H* 12/06/06 C:61  PROJECT WC1444                                 *
     H*                Replace code that creates new "Carrier #" with *
     H*                call to API WA200R.                            *
     H*
     H* 09/20/06 C:136 PROJECT: WC1463                                *
     H*                RENEW BY GROUP FUND YEAR VALUE INSTEAD OF      *
     H*                ADDING 1 TO RENEWING YEAR                      *
     H*                                                               *
     H* 08/23/06 C:180 PROJECT: 114WMF                                *
     H*
     H* 07/06/06 C:168 PROJECT 132WEW                                 *
     H*                cleared the aircraft reinsurance fields if expir
     H*
     H* 06/30/06 C:33  Seq#: WA139/53018                              *
     H*                Verify the employer has not already been       *
     H*                 renewed to quote when renewing trying to renew*
     H*                 to live.                                      *
     H*                                                               *
     H* 04/18/06 C:168 PROJECT 102WDO                                 *
     H*                Added Include/Exclude states                   *
     H*                                                               *
     H*  03/23/06 C:132 PROJECT: 71WGC - WC1377                       *
     H*               - Add code for 4th rollover option: Use greater *
     H*                 of audited payrolls or estimated payrolls.    *
     H*               - Update new field WMEPP/EPRLOP with rollover   *
     H*                 option code passed into program.              *
     H*                                                               *
     H* 03/22/06 C:33  Seq#: WA093/51473                              *
     H*                Roll Employee count same as Payroll.           *
     H*                                                               *
     H* 03/07/06 C:136 SUPPORT: WA045 51164                           *
     H*                If rolling audited payroll, do not roll        *
     H*                prior year payrolls if they don't exist        *
     H*                in current year.  Include current year         *
     H*                payrolls in renewal year if they don't exist   *
     H*                in prior year.                                 *
     H*                                                               *
     H* 02/07/06 C:33  Project: WC1017    WA139/51292                 *
     H*                Load correct agent rate.                       *
     H*                                                               *
     H* 01/27/06 C:15  Support: WA139/50730                           *
     H*                Change Carrier policy # build.                 *
     H*                                                               *
     H* 01/24/06 C:65  PROJECT 114WHO                                 *
     H*                CLEAR RETRO SETTLEMENT FIELDS.                 *
     H*                                                               *
     H* 10/14/05 C:65  PROJECT 114WGH                                 *
     H*                CHECK TO SEE IF ANY DEFAULT EXCLUDED STATES    *
     H*                NEED TO BE COPIED IN FOR THE NEW WDELP RECORD. *
     H*                A DEFAULT RECORD FOR A GROUP IS DESIGNATED     *
     H*                WITH A ZERO EMPLOYER# AND A ZERO FYR.          *
     H*                                                               *
     H* 08/13/05 C:61  SUPPORT WA111 48515                            *
     H*                Correct so that Employer Payroll Employee      *
     H*                count WMEPP/EPECNT is updated to new year      *
     H*                correctly.                                     *
     H*                                                               *
     H* 08/12/05 C:132 SUPPORT: WA135 48330                           *
     H*                Change CARRIER POLICY # format to append       *
     H*                Current Policy Year from ELFYR and the         *
     H*                carrier policy format year is "%%%" instead    *
     H*                of adding 1 to the policy year before update   *
     H*                to WMRPL/RPCAP#.                               *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109BC}                                        *
     H*                Account for Quarterly and Semi-Annual plans    *
     H*                 when rolling S/R payroll.                     *
     H*                                                               *
     H* 06/24/05 C:09  SUPPORT: WA135 XXXXX                           *
     H*                CHANGE CARRIER POLICY # FORMAT TO APPEND       *
     H*                '-%%%' IF CLIENT #135 PROCESSING AND THE       *
     H*                POLICY YEAR = 104 AND THERE IS NO SUFFIX       *
     H*                CURRENTLY.                                     *
     H*                                                               *
     H* 05/16/05 C:136 SUPPORT: WA135 47102                           *
     H*                CORRECT ERROR MESSAGES ON RENEWAL REPORT       *
     H*                TO REPORT THE CORRECT PAYROLL CLASS.           *
     H*                                                               *
     H* 04/07/05 C:156 PROJECT: 126WDK                                *
     H*                MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.        *
     H*                                                               *
     H* 01/07/05 C:136 PROJECT: WC1250                                *
     H*                Clear domestic terrorism fields.               *
     H*                                                               *
     H* 09/20/04 C:33  Project: WC1017    WA072/43916                 *
     H*                Allow accounts with same info in quote to renew*
     H*                 if NOT using 'Renew to Quote'.                *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                      *
     H*                Clear 4th and 5th assessment fields.           *
     H*                                                               *
     H* 06/22/04 C:61  PROJECT 114WHL                                 *
     H*                Create "Renewal Tracking Record".              *
     H*                                                               *
     H* 01/13/04 C:136 Project: WC1116                                *
     H*                Roll direct debit history into quote.          *
     H*                                                               *
     H* 01/09/04 C:136 Project: WC1115                                *
     H*                Roll vendor history information.               *
     H*                                                               *
     H* 12/10/03 C:65  Project: WC1017/SUPP#40169                     *
     H*                DO NOT TRANSFER A CLASS TO THE NEW CLASS CODE  *
     H*                IF THE NEW YEAR IS ANNIV RATED AND THE OLD     *
     H*                CLASS IS NOT EXPIRED AS OF THE ANNIV DATE.     *
     H*                                                               *
     H* 12/04/03 C:61  SUPPORT WA081 40211                            *
     H*                Write Initial User with write of new record.   *
     H*                                                               *
     H* 12/03/03 C:33  Seq#: WA102/40190                              *
     H*                Load new policy from date to billing record.   *
     H*                                                               *
     H* 10/10/03 C:33  Project: WC1017                                *
     H*                Renew to quote if requested.                   *
     H*                                                               *
     H* 10/07/03 C:65  PROJECT: WC1149                                *
     H*                DO NOT ANNUALIZE PAYROLLS THAT WERE SPLIT FOR  *
     H*                THE 10/1/3 FL RATE CHANGE.                     *
     H*                                                               *
     H* 08/19/03 C:65  PROJECT: SY048                                 *
     H*                CLEAR NEW MIN PREM AMOUNT FIELDS.              *
     H*                                                               *
     H* 06/06/03 C:136 SUPPORT: 37452  (PROJECT: 12WAK)               *
     H*                Calculate S/R payroll based on new field       *
     H*                SRTODT if it is  used instead of SRRPMY.       *
     H*                                                               *
     H* 04/30/03 C:65  PROJECT: WC1123                                *
     H*                DO NOT ANNUALIZE PAYROLLS THAT WERE SPLIT FOR  *
     H*                THE 4/1/3 FL RATE CHANGE.                      *
     H*                                                               *
     H* 01/27/03 C:136 PROJECT: WC744                                 *
     H*                Clear the new Massachusetts ARAP premium       *
     H*                surcharge fields in WDELP on rollover.         *
     H*                                                               *
     H* 01/17/03 C:65  PROJECT: WC1120                                *
     H*                CLEAR THE NEW FIELDS FOR TERRORISM RISK ACT.   *
     H*                                                               *
     H* 09/04/02 C:136 Support: WA099 33450                           *
     H*                Out of bounds error occurs if formatting field *
     H*                in WW Groups Option 40 doesn't have policy year*
     H*                formatting characters. Put in a check to       *
     H*                prevent carrier renewal processing if format-  *
     H*                ting field doesn't contain such characters.    *
     H*                                                               *
     H* 08/09/02 C:61  SUPPORT WA093  33743                           *
     H*                Determine appropriate anniversary date in      *
     H*                SRANNL after determining which payroll will be *
     H*                used.                                          *
     H*                                                               *
     H* 04/25/02 C:61  SUPPORT WA081 32221                            *
     H*                Make accumulation of S/R payroll specific to   *
     H*                special designation (SRSPEC, subcontractor).   *
     H*                                                               *
     H* 10/19/01  C:90   PROJECT: WC1054                              *
     H*                  Check "Update carrier #" flag to determine   *
     H*                  if a new carrier number should be created    *
     H*                  for the new year.                            *
     H*                                                               *
     H* 04/16/01  C:132  PROJECT: WC577                               *
     H*                  Check for Fields not being Zeroed out in New *
     H*                  Year Added WDELP Records.                    *
     H*                                                               *
     H* 03/12/01  C:35   PROJECT: SUPPORT WA112 #26681                *
     H*                  CHAIN TO EXISTING YEAR INSTEAD OF SETGT IN   *
     H*                  CASE OTHER DIVISION IS MISSING THAT YEAR.    *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: WC1024                                *
     H*                CLEAR OUT NEW FIELDS FOR 3RD ASSESSMENT.       *
     H*                                                               *
     H* 11/30/00 C:65  PROJECT: WC577                                 *
     H*                CLEAR OUT NEW FIELDS FOR ROLLED ACCOUNTS.      *
     H*                                                               *
     H* 11/30/00 C:33  SEQ#: WA107/24956                              *
     H*                IF A SHORT TERM POLICY ENDS PRIOR TO THE GROUP *
     H*                EXPIRATION DATE, DO NOT ALLOW THE POLICY TO BE *
     H*                ROLLED.  PRINT RECORD ON EXCEPTION REPORT.     *
     H*                                                               *
     H* 11/01/00 C:33  PROJECT: WC1008                                *
     H*                WHEN TRANSFERRING, IF LAST STATUS IS 'C' OR 'R'*
     H*                LOAD TO QTXFER                                 *
     H*                                                               *
     H* 10/11/00  C:35   PROJECT: WC843/SUPPORT WA102 #24619          *
     H*                  ROLL PAYROLL EXTENSION INTO NEW YEAR.        *
     H*                                                               *
     H* 09/15/00 C:65  PROJECT: SUPPORT#18745                         *
     H*                MOVE THE BILLING REQUEST CODE AFTER THE AGENT  *
     H*                TRANSFER.                                      *
     H*                                                               *
     H* 08/31/00  C:35   PROJECT: SUPPORT WA095 #24163                *
     H*                  Y2K ISSUE: ON S/R ROLL TYPE 'B', IF ACCOUNT  *
     H*                  HAD SPLIT PAYROLL ON ANNIVERSARY DATE,       *
     H*                  ROLLED PAYROLL WAS UNDERSTATED, DUE TO CODE  *
     H*                  COUNTING REPORTS ON SPLIT PAYROLLS AS A      *
     H*                  MONTH EACH.  TOTAL REPORTED MONTHS THEN      *
     H*                  EXCEEDED 12, RESULTING IN A NEGATIVE         *
     H*                  ADJUSTMENT.                                  *
     H*                                                               *
     H* 07/12/00 C:90  PROJECT: WA048/23511                           *
     H*                CLEAR NEW WAIVER PREMIUM RELATED FIELDS IN     *
     H*                WDELP.                                         *
     H*                                                               *
     H* 06/30/00 C:90  PROJECT: WA107/23349                           *
     H*                REMOVED INDICATORS CONTROLLING THE PRINT OF    *
     H*                DET1.                                          *
     H*                                                               *
     H* 05/30/00 C:65  PROJECT: WC577                                 *
     H*                ANNUALIZE PAYROLLS THAT ARE MIDTERM AND NOT    *
     H*                ARD.                                           *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                                                               *
     H* 12/15/99 C:90  PROJECT: WC577                                 *
     H*                Y2K FIX LOAD INITIAL DATE, TIME, AND PROGRAM   *
     H*                FIELDS FOR WDELP, WMAHP, AND WMEPP.            *
     H*                                                               *
     H* 10/29/99 C:87  PROJECT: SUPP #20117                           *
     H*                Revision made on 10/12/99 caused some emps not *
     H*                to roll.  Chgd code to remedy.                 *
     H*                                                               *
     H* 10/25/99 C:61  PROJECT WC949                                  *
     H*              - If deductible discount renewal flag WTDDP/DDRNW*
     H*                is 'N', then set bill deductible if cancelled  *
     H*                flag (WDELP/EXCXBL) to 'N' on renewal.         *
     H*              - Correct read of increased limits file.         *
     H*                                                               *
     H* 10/21/99 C:33  PROJECT: WC948                                 *
     H*                IMPLEMENT SECONDARY AGENT OF RECORD CHANGE AND *
     H*                QUOTE TRACKING TRANSFER.                       *
     H*                                                               *
     H* 10/12/99 C:87  PROJECT: SUPP WA093 #19996                     *
     H*                Did not pick subsequent divs if main div was   *
     H*                cancelled (due to chg made on 2/18/99).        *
     H*                                                               *
     H* 09/22/99 C:61  PROJECT WC949                                  *
     H*                Check auto renewal flags for rating factors.   *
     H*                If 'N', clear rating factor value when writing *
     H*                new policy year.                               *
     H*                                                               *
     H* 09/20/99 C:61  PROJECT WC948                                  *
     H*                If 'agent of record' is not zero, then write   *
     H*                agent history record for new policy year with  *
     H*                an effective date equal to the new policy year *
     H*                effective date.                                *
     H*                                                               *
     H*  08/12/99 C:118 PROJECT: WC577
     H*                 Add 'NEW EFF DATE' and 'NEW EXP DATE' to
     H*                 report.
     H*
     H*  06/01/99 C:118 SUPPORT: WA091/18241                          *
     H*                 Print 'No records meet selection criteria' if *
     H*                 this is the case. Modified routing page.      *
     H*                 Added chain loop to division cancellation     *
     H*                 check.                                        *
     H*                                                               *
     H*  05/12/99 C:113 PROJECT: CM045                                *
     H*                 Waiver of Subrogation rewrite                 *
     H*                 Added code to clear new waiver premium        *
     H*                 related fields in WDELP.                      *
     H*                                                               *
     H* 02/18/99  C:87    PROJECT: SUPP WA072 #16473                  *
     H*                   Process was picking up divisions that were  *
     H*                   cancelled when it should not have.          *
     H*                   Changed FUND to GROUP in message line.      *
     H*                                                               *
     H* 02/05/99  C:87    PROJECT: WC878                              *
     H*                   Add code to roll Entity/Location detail     *
     H*                   records (WDLDP) into the new policy year,   *
     H*                   if associated Ent/Loc records (WMALP) do    *
     H*                   not have a delete date prior to the policy  *
     H*                   from date of the new policy year.           *
     H*                                                               *
     H* 07/28/98  C:108   PROJECT: WC816                              *
     H*           C:35    CHANGE ROLLOVER DETAIL REPORT TO SHOW       *
     H*                   POLICY EFFECTIVE AND EXPIRATION DATES.      *
     H*                                                               *
     H*  07/14/98 C:65 PROJECT: WC845                                 *
     H*                CLEAR IREP DISCOUNT FIELD.                     *
     H*                                                               *
     H*  04/06/98 C:65 PROJECT: WC816                                 *
     H*                FYR IS NO LONGER BEGIN PASSED IN, INSTEAD EACH *
     H*                EMPLOYER ROLLS INTO ITS NEXT SEQUENTIAL FYR.   *
     H*                IF AN EMPLOYER TRIES TO ROLL INTO A YEAR WITH  *
     H*                NO FUND DETAIL, ADD TO THE ERROR LISTING RPT.  *
     H*                                                               *
     H*  03/17/98 C:65 PROJECT: WC705                                 *
     H*                SET SCHDULE DB/CR EFFECTIVE DATE.              *
     H*                SET DEVIATION #2  EFFECTIVE DATE.              *
     H*                SET DEDUC DISC    EFFECTIVE DATE.              *
     H*                SET 2ND INCR LMT  EFFECTIVE DATE.              *
     H*                                                               *
     H*  02/17/98 C:80 PROJECT: WC800                                 *
     H*                CHANGED PARAMETER QQPCT FROM 3 TO 5 CHARACTERS *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMEPP*
     H*                                                               *
     H* 11/21/96 C:65 PROJECT: CM023                                  *
     H*               BEFORE WRITING OR UPDATING BILLING REQUEST FILE,*
     H*               INTIALIZE THE NEW FIELDS FOR POLICY DATE,       *
     H*               AGENT#, AND BUSINESS FLAG.                      *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 08 - GENERAL                                                  *
     H* 09 - GENERAL                                                  *
     H*                                                               *
     H* 11 - GENERAL                                                  *
     H*                                                               *
     H* 40 - GENERAL CHAIN                                            *
     H* 41 - CANCELLED ACCOUNT, RENEWAL FLAG, DATE ISSUES             *
     H*                                                               *
     H* 50 - GENERAL CHAIN                                            *
     H* 51 - READP INDICATOR FOR WDELP                                *
     H*                                                               *
     H* 60 - GENERAL CHAIN                                            *
     H* 61 - GENERAL CHAIN                                            *
     H*                                                               *
     H*****************************************************************
     H**                                                              *
     H** FILE USAGE:                                                  *
     H**                                                              *
     H** DISPLAY FILES:                                               *
     H**    (NONE)                                                    *
     H**                                                              *
     H** DATA FILES:                                                  *
     H**    WDELL  (PEL01): CO#/FND/EMP#/DIV/FYR                      *
     H**    WDELL4 (PEL02): CO#/FND/PTDT                              *
     H**    WMEML  (PEM01): CO#/FND/EMP#                              *
     H**    WMEPL1 (PEP01): CO#/FND/FYR/EMP#/DIV                      *
     H**    WDF1L  (PF101): CO#/FND/FYR                               *
     H**    WMUPL  (PUP01): USER                                      *
     H**    WMFNL  (PFN01): CO#/FND                                   *
     H**    SMCOL  (PCO01): CO#                                       *
     H**    SMORL  (POR01): PGM/PRT                                   *
     H**    WMAHL  (PAH01): CO#/FND/EMP#/DIV                          *
     H**    WABLL  (PBL01): CO#/FND/FYR/EMP#/DIV                      *
     H**    WTABL  (PBS01): CO#/KEY                                   *
     H**    WASRL2 (PSR01): CO#/FND/FYR/EMP#/DIV                      *
     H**    WMAHL1 (PAH02): CO#/FND/FYR/AGT#/EMP#/DIV/EFDT            *
     H**                                                              *
     H*****************************************************************
     F*
     FWDELL     IF A E           K DISK
     FWDELL4    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F*** begin add *** 09/08/17 ********************************
     FWDF1L2    IF   E           K DISK    rename(PF101:PF102) prefix(L2)
     F*** end add ***** 09/08/17 ********************************
     FWMEML     IF   E           K DISK
     FWMEPL1E   UF A E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX00)
     FWDF1L1    IF   E           K DISK
     F                                     RENAME(PF101:PF10L1)
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWXRXL     UF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWABLL     UF A E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PRT01)
      *
      *  Renewal Tracking
     FWDRTL     UF A E           K DISK
      *
      *  System Control
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
     FWASRL2    IF   E           K DISK
     FWMAHL1    IF A E           K DISK
     F                                     RENAME(PAH01:PAH02)
     FWMALL     IF   E           K DISK
     FWDLDL     IF A E           K DISK
      * Rating State
     FWMRSL     IF   E           K DISK
      *
      * Statistical Reporting/Rating
     FWDRRL     IF   E           K DISK
      *
      * Deductible Discount
     FWTDDL2    IF   E           K DISK
      *
      * Increased Limits
     FWTLML1    IF   E           K DISK
     F***  BEGIN CHG  ***  10/15/19  *********************************
     F*****WAQTL7    IF A E           K DISK
     FWAQTL7    UF A E           K DISK
     F***  END CHG  *****  10/15/19  *********************************
     FWMAAL1    IF   E           K DISK
     FWMARL     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWMA3L     IF   E           K DISK
     FWMOOL     IF   E           K DISK
     F*** begin del ***  03/14/22  **************************************
     F*****WMECL     IF   E           K DISK
     F*** end del *****  03/14/22  **************************************
     FWMEMLQ    IF A E           K DISK
     F                                     RENAME(PEM01:PEM01Q)
     FWMSHLQ    O    E             DISK
     F                                     RENAME(PSH01:PSH01Q)
     FWMA3LQ    O  A E             DISK
     F                                     RENAME(PA301:PA301Q)
     FWMOOLQ    O  A E             DISK
     F                                     RENAME(POO01:POO01Q)
     F*** begin del ***  03/14/22  **************************************
     F*****WMECLQ    O  A E             DISK
     F*****                                     RENAME(PEC01:PEC01Q)
     F*** end del *****  03/14/22  **************************************
     FWDELLQ    IF A E           K DISK
     F                                     RENAME(PEL01:PEL01Q)
     FWMEPLQE   UF A E           K DISK
     F                                     RENAME(PEP01:PEP01Q)
     FWMAHLQ    IF A E           K DISK
     F                                     RENAME(PAH01:PAH01Q)
     FWMALLQ    O  A E             DISK
     F                                     RENAME(PAL01:PAL01Q)
     FWDLDLQ    O  A E             DISK
     F                                     RENAME(PLD01:PLD01Q)
     FWDVHL1    IF A E           K DISK
     FWDVHLQ    O  A E             DISK
     F                                     RENAME(PVH01:PVH01Q)
     FWDPLL     IF A E           K DISK
     FWMDBL     IF   E           K DISK
     FWMDBLQ    O  A E             DISK
     F                                     RENAME(PDB01:PDB01Q)
     FWDXSL     IF A E           K DISK
     FWABLLQ    O  A E             DISK
     F                                     RENAME(PBL01:PBL01Q)
     FWMRTL     IF   E           K DISK
     F                                     RENAME(PRT01:PRT0X)
     F                                     prefix(rx:2)
     FWMLDL1    IF A E           K DISK    USROPN
     F                                     RENAME(PLD01:PLD02)
     FWMLDL1Q   O  A E             DISK    USROPN
     F                                     RENAME(PLD01:PLD02Q)
     FWTFIL     if   e           k disk    usropn
     F                                     rename(pfi01:pfi00)
     FWDEFL1    if a e           k disk    usropn
     FWDEFL1Q   if a e           k disk    usropn
     F                                     rename(pef01:pef01Q)
     FWDHCL3    IF A E           K DISK    RENAME(PHC01:PHC03)
     FWDHCLQ    O    E             DISK    RENAME(PHC01:PHC00Q)
     FWMHCL     IF   E           K DISK    RENAME(PHC01:MHC00)
     fwtdvl11   if   e           k disk    rename(pdv01 : pdv0l11)
      *
      *** Beg Add ***** 08/20/18 *************************************
     fwmmul6    if   e           k disk    rename(pmu01:pmu06)
     fwmmulQ    o    e             disk    rename(pmu01:pmu01Q)
     fwtenl     if   e           k disk    rename(pen01:pen00)
      *** End Add ***** 08/20/18 *************************************
      *** Beg Add ***** 11/12/18 *************************************
     fwmwvl     if   e           k disk    rename(pwv01:pwv00)
     fwmwvl1Q   o    e             disk    rename(pwv01:pwv01Q)
      *** End Add ***** 11/12/18 *************************************
      *** Beg Add ***** 06/14/19 *****************************
     fwxfmp1    if   e           k disk    rename(pfm01:pfm01)
     fwxfml11   o    e           k disk    rename(pfm01:pfm11)
      *** End Add ***** 06/14/19 *****************************
      ***** Beg Add ***** 10/09/19 ***************************
     fwdcxl     if a e           k disk    rename(pcx01:pcx00)
     fwdxhl     if a e           k disk    rename(pxh01:pxh00)
      ***** End Add ***** 10/09/19 ***************************
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
      *
      * ============================================================ **
     D*
     d flgDivPlnRnw    s              1
     D msg             s             68    dim(8) ctData perRcd(1)
     DisRtStCd         s              1n
     D INFO            S             80    DIM(9) CTDATA PERRCD(1)
     D CHG             S             10    DIM(2) CTDATA PERRCD(1)
     D MSG1            S             80    DIM(2) CTDATA PERRCD(1)
     D INF             S              1    DIM(80)
     D PYC             S             11    DIM(9999)
     D CYC             S             11    DIM(9999)
      *****************************************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
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
      *
     D                 DS
     D  XXPFDT                 1      7  0
     D  XXPFYR                 1      3  0
      *
     D                 DS
     D  XXPTDT                 1      7  0
     D  XXPTYR                 1      3  0
      *
     D                 DS
     D  XXBSDT                 1      7  0
     D  XXBSYR                 1      3  0
      *
     D                 DS
     D  AHAGT#                 1      5  0
     D  XXCAGT                 1      5
      *
     D                 DS
     D  XXSTIN                 1     21
     D  QQST1                  1      2
     D  QQST2                  3      4
     D  QQST3                  5      6
     D  QQST4                  7      8
     D  QQST5                  9     10
     D  QQST6                 11     12
     D  QQST7                 13     14
     D  QQST8                 15     16
     D  QQST9                 17     18
     D  QQST10                19     20
     D  QQINEX                21     21
     D*
     D TITLE           C                   CONST('Rollover Exceptions -
     D                                     for Manual Set-Up')
     D TITLE1          C                   CONST('RENEWAL: Detail List-
     D                                     ing of Accounts -
     D                                     Renewed')
     D CHGNUM          C                   CONST('WA003R')
     d isCancelled     s               n
     d isRenewable     s               n
     d errorMsg        s            200
     d futDtlExists    s               n
     d goodPolDates    s               n
     d qteEmpExists    s               n
     d rnwSingEmpFlg   s               n
     d x               s              3  0
     d xxMsg1          s             65
     d***  Begin Add  ***  12/06/17  *************************************
     d strFyr          s              3    inz
     d decFyr          s              3  0 inz
     d***  End   Add  ***  12/06/17  *************************************
      ***** Beg Add ***** 10/09/19 ***************************************
     d intDiv          s              5  0 inz
      ***** End Add ***** 10/09/19 ***************************************
      *** beg add ***** 01/25/22 ***************************
     d isAddingDiv     s               n
     d intRnwDiv       s              5  0 inz
      *** end add ***** 01/25/22 ***************************
      *** Beg add ***** 04/25/23 ***************************
     d svReq#          s                   like(CXreq#) Inz(0)
      *** End add ***** 04/25/23 ***************************
     d***  Begin Add  ***  07/18/22  ****************
     d/copy wPrEndorse
     d***  Begin Add  ***  07/18/22  ****************
     d/copy WprEmplr
     D                 DS
     D  XXTODT                 1      7  0
     D  XXTOYR                 1      3  0
      *
      * POLICY FROM DATE
     D                 DS
     D  FRMDTE                 1      8  0
     D  FRDDM                  1      4  0
     D  FRMCY                  5      8  0
      * POLICY TO DATE
     D                 DS
     D  TODATE                 1      8  0
     D  DTEDM                  1      4  0
     D  DTECY                  5      8  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *****************************************************************
     D##datein         S              8  0
     D##fromfmt        S              8
     D##tofmt          S              8
     D@@dateout        S             10
     D@@err            S              1
     d savEliPgm       s                   like(ELipgm)
     d rsEmp#          s                   like(qqEmp#)

     DSRCVTDAT         PR                  Extpgm('SRCVTDAT')
     D##datein                        8  0
     D##fromfmt                       8
     D##tofmt                         8
     D@@dateout                      10
     D@@err                           1
      *** Begin Add 05/13/19 *****************************************
     DSRANNS           PR             5  0
     D                                7  0
     D                                7  0

     D  Edays          s              5  0 INZ
      *** End   Add 05/13/19 *****************************************

      *** Begin Add 10/29/18 *****************************************
     DSRDAY8           PR                  Extpgm('SRDAY8')
     D                                8  0
     D                                4
     D                                9
     D                                1

     D SRDTE           S              8  0
     D SRSDAY          S              4
     D SRLDAY          S              9
     D SRerr           S              1

     D DAY             S              2    DIM(13) CTDATA PERRCD(13)
     D NxtBillDate     S              7P 0 INZ
     D xxDay           S                   LIKE(BSDOWK)
     D x1              S              3P 0 INZ
     D x2              S              3P 0 INZ
     D x3Cnt           S              5P 0 INZ
      *** Beg Chg 05/15/19 *****************************************
     D***** WKD             S              7  0 DIM(53) ASCEND INZ(*HIVAL)
     D*** begin chg ***  09/22/23  ***********************************
     D***** WKD             S              7  0 DIM(55) ASCEND INZ(*HIVAL)
     D WKD             S              7  0 DIM(99) ASCEND INZ(*HIVAL)
     D*** end chg *****  09/22/23  ***********************************
      *** End Chg 05/15/19 *****************************************
     D T               S              3P 0 INZ
     D xxRPMY          S                   LIKE(SRRPMY)
     D SVbsdt          S                   LIKE(ELbsdt)
     D SVptdt          S                   LIKE(ELptdt)
      *** End Add 10/29/18 *****************************************
      *** Beg Add ***** 12/14/18 *************************************
     D  intWav#        s              5  0
     D  index          s              5  0
      *** Beg Chg ***** 04/15/19 *************************************
      ***  arrFormDiv     s             20    Dim(50)  Inz
     D  arrForm        s             15    Dim(50)  Inz
     D  Strform        s             15
      *** End Chg ***** 04/15/19 *************************************

      *** Beg Del ***** 04/15/19 *************************************
     D*** StrFormDiv      DS
     D***  Strform                1     15
     D***  StrDiv                16     20
      *** End Del ***** 04/15/19 *************************************
      *** End Add ***** 12/14/18 *************************************
      *================================================================
      *  KLIST
      *================================================================
      *
      *  Renewal Tracking
      *
     C     RTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
      *
      ***** END ADD ******* 06/22/04 **********************************
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     EMKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     EMKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
      *
     C     EMKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXKYR
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
     C                   KFLD                    XXFTDT
      *
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      ***** BEG ADD ***** 05/24/07 ************************************
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SVFYR
      ***** END ADD ***** 05/24/07 ************************************
      *
      ***** BEG ADD ***** 10/12/10 ************************************
     C     ELKY4A        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
      ***** END ADD ***** 10/12/10 ************************************
      ***** BEG ADD ***** 12/11/07 **************************************
      *
     C     ELKY5         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELPTDT
     C                   KFLD                    ELEMP#
      *
     C     ELKY6         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELPTDT
     C                   KFLD                    ELEMP#
     C                   KFLD                    KKDIV
      *
     C     ELKY7         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELPTDT
     C                   KFLD                    ELEMP#
     C                   KFLD                    SVDIV
      *
     C     ELKY8         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C     RSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    @@SKST
      *
     C     ELKY9         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      ***** END ADD ***** 12/11/07 **************************************
      *
      *  EMPLOYER PAYROLL FILE
      *
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXKYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** BEGIN ADD ***  12/10/03  ************************************
      *
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     EPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    X1CLAS            5 0
     C                   KFLD                    X1SPEC            1
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   KFLD                    X1EXT             5
     C*** END   ADD ***  04/20/09  ************************************
     C                   KFLD                    X1EFDT            7 0
     C*
     C     EPKY1Q        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKY2Q        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    X1EFDT            7 0
     C                   KFLD                    X1CLAS            5 0
     C                   KFLD                    X1SPEC            1
     C*** END ADD *****  12/10/03  ************************************
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   KFLD                    X1EXT             5
     C*** END   ADD ***  04/20/09  ************************************
      *
      ***** BEG ADD ***** 03/23/06 ***********************************
      *  KEY FOR PRIOR AUDITED PAYROLL
     C     EPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XX2YR                          PRIOR YR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SVCLAS
      *
      *  KEY FOR CURRENT PAYROLL RECORD
     C     EPKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXXYR                          CURRENT YEAR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    S2CLAS
     C                   KFLD                    S2SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   KFLD                    S2EXT
     C*** END   ADD ***  04/20/09  ************************************
     C                   KFLD                    S2EFDT
      ***** END ADD ***** 03/23/06 ***********************************
     C***** BEG ADD ***** 04/01/08 ***********************************
     C     EPKYL         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR                          PRIOR YR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    X2CLAS
     C***** END ADD ***** 04/01/08 ***********************************
      *
      *  SELF REPORTING PAYROLL FILE
      *
     C     SRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXKYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *  TABLE FILE
      *
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *  FUND DETAIL
      *
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
     C     F1KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXKYR
      *
      *** BEG ADD ***** 09/20/06 *************************************
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELPTDT
      *** END ADD ***** 09/20/06 *************************************
     C***  Begin Add  ***  12/12/06  **************************************
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C***  End   Add  ***  12/12/06  **************************************
      *
      *  AGENT CROSSOVER KEY
      *
     C     RXKY          KLIST
     C                   KFLD                    XXTIME
     C                   KFLD                    XXRATE
      *
     C     RXKY2         KLIST
     C                   KFLD                    XXTIME
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE AGENT HISTORY FILE
      *
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     AHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXAGT#
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT            7 0
     C*** BEGIN ADD ***  11/21/96  ***********************************
     C     AHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*** END ADD *****  11/21/96  ***********************************
     C*
     C***  Begin Add  ***  10/10/03  *********************************
     C     AHKYQ         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXAGT#
     C***  End   Add  ***  10/10/03  *********************************
      *
      *  BILL TRANSACTION FILE
      *
     C     BLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C*  Entity/Location file
     C     ALKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  Entity/Location detail file
     C     LDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C                   KFLD                    XXXYR
     C***  END   ADD  ***  02/05/99  **********************************
     C***  BEG   ADD  ***  06/06/14  **********************************
     C*  Location Payroll File
     C     PLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXXYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C***  END   ADD  ***  06/06/14  **********************************
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
      ***** BEG ADD ******* 09/22/99 **********************************
      ***** BEG ADD ******* 04/18/06 **********************************
     C     WMRSLK        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      ***** END ADD ******* 04/18/06 **********************************
      *
      * Rating State
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      *   Statistical Reporting / Rating
      *
     C     RRKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C                   KFLD                    KKEFDT
      *
     C     RRKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C*** BEGIN ADD ***  04/06/09  ************************************
      *
     C     RRKY3         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C                   KFLD                    XXPFDT
     C*** END   ADD ***  04/06/09  ************************************
      *
      *   Deductible Discount
      *
     C     DDKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C                   KFLD                    KKCODE
     C                   KFLD                    KKEFDT
      *
     C     DDKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C                   KFLD                    KKCODE
      *
      *   Increased Limits
      *
     C     LMKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
     C                   KFLD                    KKEFDT
      *
     C     LMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C                   KFLD                    KKKEY
      ***** END ADD ******* 09/22/99 **********************************
     C***  BEGIN ADD  ***  10/21/99  *********************************
     C     QTKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHEMP#
     C                   KFLD                    AHDIV
     C                   KFLD                    XXSTAT
     C***  END   ADD  ***  10/21/99  *********************************
     C***  BEGIN ADD  ***  10/15/19  *********************************
     C     QTKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXSTAT
     C***  END   ADD  ***  10/15/19  *********************************
      **** BEGIN ADD **** 10/19/01 ************************************
      ***** BEG DEL ******* 12/06/06 **********************************
     C*********
     C****CARRIER STATE/REPORTING FILE
     C*********
     C*********  RPKY      KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
     C*********            KFLD           XXEMP#
     C*********            KFLD           XXDIV
     C*********            KFLD           XXKYR
     C*********
     C****GROUP CARRIER INFORMATION
     C*********
     C*********  GCKY      KLIST
     C*********            KFLD           XXCO#
     C*********            KFLD           XXFND
      ***** END DEL ******* 12/06/06 **********************************
      **** END   ADD **** 10/19/01 ************************************
     C*
     C***  Begin Add  ***  10/10/03  *********************************
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    X2CLAS
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    X2CLAS
     C**** BEG CHG ****** 04/01/08 ************************************
     C**********           KFLD           XXSVPF
     C                   KFLD                    X2EFDT            7 0
     C**** END CHG ****** 04/01/08 ************************************
     C*
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRATE
     C*
     C     ARKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXRATE
     C                   KFLD                    XXPFDT
     C*
     C     OOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C***  End   Add  ***  10/10/03  *********************************
      *
     C*** BEG ADD *******  01/09/04  *********************************
     C     VHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXXYR
      *
     C     VHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXXYR
     C                   KFLD                    XXDATE
     C*** END ADD *******  01/09/04  *********************************
     C*** BEGIN ADD ***  10/14/05  ************************************
      *
      *   EXCLUDED STATES FILE
      *
     C     XSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    X1EMP#            9 0
     C                   KFLD                    X1FYR             3 0
     C*
     C     XSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
     C                   KFLD                    XSST
     C*** END ADD *****  10/14/05  ************************************
     C*** BEGIN ADD ****** 10/18/10 ************************************
     C* Brought in from W7820R3
      *  KEY LIST
      *
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *** BEG CHG ***** 03/22/10 **************************************
     C***********          KFLD           QQKYR
     C                   KFLD                    XXXYR
      *** END CHG ***** 03/22/10 **************************************
     C*** END   ADD ****** 10/18/10 ************************************
     C*
     C*   FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AHRT#         XXRATE
     C     *LIKE         DEFINE    EPARAT        SVARAT
     C     *LIKE         DEFINE    ELPFDT        XXSVPF
     C     *LIKE         DEFINE    ELPTDT        XXSVPT
     C     *LIKE         DEFINE    ELPTDT        XXDATE
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        XXPEMP
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXKYR
     C     *LIKE         DEFINE    F1FYR         XXFYR
     C     *LIKE         DEFINE    F1FYR         XXXYR
     C     *LIKE         DEFINE    NCKEY         XXKEY
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    EPCLAS        S2CLAS
      **** BEGIN ADD **** 10/11/00 *********************************************
     C     *LIKE         DEFINE    EPEXT         S2EXT
      **** END   ADD **** 10/11/00 *********************************************
      ***** BEG ADD ******* 08/13/05 **********************************
     C     *LIKE         DEFINE    EPECNT        S2ECNT
     C     *LIKE         DEFINE    EPACNT        S2ACNT
     C     *LIKE         DEFINE    EPECNT        SVECNT
     C     *LIKE         DEFINE    EPACNT        SVACNT
      ***** END ADD ******* 08/13/05 **********************************
     C     *LIKE         DEFINE    EPSPEC        S2SPEC
     C     *LIKE         DEFINE    EPSC          S2SC
     C     *LIKE         DEFINE    EPSAFL        S2SAFL
     C     *LIKE         DEFINE    EPAAPR        S2AAPR
     C     *LIKE         DEFINE    EPSAPR        S2SAPR
     C     *LIKE         DEFINE    EPEAPR        S2EAPR
     C     *LIKE         DEFINE    EPARAT        S2ARAT
     C     *LIKE         DEFINE    EPEFDT        S2EFDT
     C     *LIKE         DEFINE    EPEFDT        XXPRDT
     C     *LIKE         DEFINE    EPAAPR        XXAMT
      ***** BEG ADD ***** 03/23/06 ************************************
     C     *LIKE         DEFINE    EPAAPR        XAPAMT
      ***** END ADD ***** 03/23/06 ************************************
     C     *LIKE         DEFINE    ELPTDT        XXFTDT
     C     *LIKE         DEFINE    ELPTDT        XXTTDT
     C     *LIKE         DEFINE    ELPTDT        X1FROM
     C     *LIKE         DEFINE    ELPTDT        X1TO
     C     *LIKE         DEFINE    ELPTDT        XXLPDT
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C     *LIKE         DEFINE    ALELID        XXELID
     C***  END   ADD  ***  02/05/99  **********************************
     C***** BEGIN ADD **** 08/12/99 ***********************************
     C     *LIKE         DEFINE    ELPFDT        XXNFDT
     C     *LIKE         DEFINE    ELPTDT        XXNTDT
     C***** END   ADD **** 08/12/99 ***********************************
      ***** BEG ADD ******* 09/22/99 **********************************
     C     *LIKE         DEFINE    RRCO#         KKCO#
     C     *LIKE         DEFINE    RRFND         KKFND
     C     *LIKE         DEFINE    RRST          KKST
     C     *LIKE         DEFINE    RREFDT        KKEFDT
     C     *LIKE         DEFINE    DDCODE        KKCODE
     C     *LIKE         DEFINE    LMKEY         KKKEY
      ***** END ADD ******* 09/22/99 **********************************
     C***  BEGIN ADD  ***  10/21/99  *********************************
     C     *LIKE         DEFINE    QTSTAT        XXSTAT
     C***  END   ADD  ***  10/21/99  *********************************
     C***  Begin Add  ***  10/10/03  **************************************
     C     *LIKE         DEFINE    EPCLAS        X2CLAS
     C     *LIKE         DEFINE    PRXFER        SVXFER
     C     *LIKE         DEFINE    QQRTOQ        XXRTOQ
     C     *LIKE         DEFINE    *IN60         DET1PR
     C     *LIKE         DEFINE    *IN60         INFOPR
     C     *LIKE         DEFINE    AHRT#         SVRT#
      *** BEG ADD ***** 05/24/07 **************************************
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELFYR         SVFYR
      *** END ADD ***** 05/24/07 **************************************
      *** BEG ADD ***** 12/11/07 **************************************
     C     *LIKE         DEFINE    EMDIV         KKDIV
      *** END ADD ***** 12/11/07 **************************************
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C***  End   Add  ***  10/10/03  **************************************
      *
     C*+* BEGIN ADD *+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C     *LIKE         DEFINE    ELPFDT        X2PFDT
     C     *LIKE         DEFINE    ELPTDT        X2PTDT
     C     *LIKE         DEFINE    ELPROR        X2PROR
     C     *LIKE         DEFINE    ELFFL9        X2FFL9
     C*+* END   ADD *+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQTYPE            1
     C*** BEGIN CHG *** 02/17/98 **************************************
     C******************** PARM           QQPCT   3
     C                   PARM                    QQPCT             5
     C*** END   CHG *** 02/17/98 **************************************
     C                   PARM                    QQOPT             1
     C                   PARM                    QQIDFL            1
     C                   PARM                    QQFTDT            8
     C                   PARM                    QQTTDT            8
     C                   PARM                    QQTIME            6
     C***  Begin Add  ***  10/10/03  **************************************
     C                   PARM                    QQRTOQ            1
     C***  End   Add  ***  10/10/03  **************************************
     C***  Begin Add  ***  04/18/06  **************************************
     C                   PARM                    QQSTIN           21
      ***** BEG ADD ***** 12/11/07 **************************************
     C                   PARM                    QQSRDF            1
      ***** END ADD ***** 12/11/07 **************************************
      *** beg add ***** 06/23/15 **************************************
     C                   PARM                    QQEMP#            9
      *** end add ***** 06/23/15 **************************************
      *** beg add ***** 01/25/22 **************************************
     C                   PARM                    QQDIV             5
      *** end add ***** 01/25/22 **************************************
      *
      *** BEG ADD ***** 11/23/09 ****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *** END ADD ***** 11/23/09 ****************************************
      *
     C                   MOVEL     QQSTIN        XXSTIN
      *
B01  C     QQINEX        IFEQ      'I'
     C                   MOVE      *ON           *IN21
E01  C                   ENDIF
      *
B01  C     QQINEX        IFEQ      'E'
     C                   MOVE      *ON           *IN22
E01  C                   ENDIF
     C***  End   Add  ***  04/18/06  **************************************
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
     C     PRMDIF        PLIST
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
      *
      *  INITIALIZE WORK FIELDS
      *
      **** BEGIN CHG **** 08/31/00 *********************************************
     C******************** Z-ADD0         SVYYMM  40
     C                   Z-ADD     0             SVYYMM            5 0
      **** END   CHG **** 08/31/00 *********************************************
     C                   Z-ADD     0             XXAMTM           11 2
     C                   Z-ADD     0             XXAMTP           11 2
     C                   Z-ADD     0             XXAVG            11 2
     C*** BEGIN CHG *** 02/17/98 **************************************
     C******************** Z-ADD0         XXPCT1  31
     C******************** Z-ADD0         XXPCT   43
     C                   Z-ADD     0             XXPCT1            5 2
     C                   Z-ADD     0             XXPCT             5 4
     C*** END   CHG *** 02/17/98 **************************************
     C                   Z-ADD     0             FDAYS             5 0
     C                   Z-ADD     0             MNTHS             2 0
     C                   Z-ADD     0             XXLPDT
     C                   Z-ADD     0             XQFROM            8 0
     C                   Z-ADD     0             XQTO              8 0
     C                   Z-ADD     0             PDAYS             5 0
     C                   Z-ADD     0             RDAYS             5 0
     C                   Z-ADD     0             XXFACT           15 9
     C                   Z-ADD(H)  0             XXWORK           11 0
     C                   Z-ADD     0             XXWK92            9 2
     C*** BEGIN DEL ***  04/06/98  ************************************
     C*****                Z-ADD0         RSFYR   30
     C*** END DEL *****  04/06/98  ************************************
     C                   Z-ADD     0             XXTOT             5 0
     C***** BEGIN DEL **** 06/01/99 ***********************************
     C**********           Z-ADD0         XXTOT2  50
     C***** END DEL ****** 06/01/99 ***********************************
      *
     C                   MOVE      *BLANKS       XXOPT             1
     C                   MOVE      *BLANKS       XXIDFL            1
     C                   MOVE      *BLANKS       SVCLAS            5 0
     C**** BEG ADD ****** 04/01/08 *********************************************
     C                   MOVE      *BLANKS       SVEFDT            7 0
     C**** END ADD ****** 04/01/08 *********************************************
      **** BEGIN ADD **** 10/11/00 *********************************************
     C                   MOVE      *BLANKS       SVEXT             5
      **** END   ADD **** 10/11/00 *********************************************
      ***** BEG ADD ******* 08/13/05 **********************************
     C                   Z-ADD     0             SVECNT
     C                   Z-ADD     0             SVACNT
      ***** END ADD ******* 08/13/05 **********************************
     C                   MOVE      *BLANKS       SVSAFL            1
     C                   MOVE      *BLANKS       SVSC              1
     C                   MOVE      *BLANKS       SVSPEC            1
     C                   MOVE      *BLANKS       XXFLG0            1
     C                   MOVE      *BLANKS       RSCHG            10
      *
      *  INITIALIZE FIELDS BASED ON *ENTRY PARMS
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQTIME        XXTIME            6 0
     C                   MOVE      QQFND         XXFND
     C***  Begin Add  ***  12/06/17  *************************************
     C                   MOVE      QQFYR         strFyr
     C                   MOVE      QQFYR         decFyr
     C***  End   Add  ***  12/06/17  *************************************
     C                   MOVE      QQPCT         XXPCT1
     C                   MOVE      QQOPT         XXOPT
     C                   MOVE      QQIDFL        XXIDFL
     C                   MOVE      QQFTDT        X1FTDT            8 0
     C                   MOVE      QQTYPE        X1TYPE            1
     C*
     C                   CLEAR                   XXRTOQ
B01  C     Q1PRMS        IFGE      11
     C                   MOVE      QQRTOQ        XXRTOQ
E01  C                   ENDIF
     C*
     C                   Z-ADD     X1FTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFTDT
     C*
     C                   MOVE      QQTTDT        X1TTDT            8 0
     C*
     C                   Z-ADD     X1TTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTTDT
      *
     C     XXPCT1        DIV       100           XXPCT
     C                   EXSR      SR1ST
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
      /free
       rsEmp# = '*ALL';
       If Q1prms >= 14 and qqEmp# <> *zeros and qqEmp# <> *blanks;
          xxEmp# = %dec(qqEmp#:9:0);
          rsEmp# = %editc(xxEmp#:'Z');
          rnwSingEmpFlg = *on;
          xxdiv = GetStRptDv(xxCo#:xxFnd:xxEmp#);
          Setgt elky1 pel01;
          Readpe elky1 pel01;
        //***  Begin Add  ***  12/06/17  **********************************
          Dow not %eof and (strFyr <> *blanks and strFyr <> *zeros)
           and ELfyr <> decFyr;
             Readpe elky1 pel01;
          Enddo;
        //***  End   Add  ***  12/06/17  **********************************
          xxftdt = ELptdt;
          xxttdt = ELptdt;
       Endif;
       //***** Beg Add ***** 10/09/19 **********************
       //***** Beg Chg ***** 01/25/22 **********************
       // If QQtype = 'Z';
       //    isAddingDiv = *On;
       //    QQtype = 'C';
       // Endif;
       If QQtype = 'Z';
          isAddingDiv = *On;
          Monitor;
             intRnwDiv = %Int(QQdiv);
          On-Error;
             xxMsg = 'Division not passed in for midterm add.';
             Exsr ChekOf;
             *Inlr = *On;
             Return;
          Endmon;
          QQtype = 'C';
       Endif;
       //***** End Chg ***** 01/25/22 **********************
       //***** End Add ***** 10/09/19 **********************
      /end-free
     C                   CLEAR                   EXRAFL
     C     EXKY          CHAIN     PEX00                              60
     C*
     C* INITIALIZATION OF *IN36 AND XXTOT1
     C* *IN36 INDICATES IF ANY EMPLOYERS SUCCESSFULLY ROLLED OVER
     C* XXTOT1 IS THE SUCCESSFUL EMPLOYER ROLLOVER COUNTER
     C*
     C                   MOVE      *OFF          *IN36
     C                   Z-ADD     0             XXTOT1            5 0
      *
      * READ EMPLOYER DETAIL RECORDS IN EXPIRATION DATE ORDER
      *
     C                   Z-ADD     0             XXPEMP
      *
      *** beg chg ***** 06/23/15 **************************************
     C*****ELKY2*        SETLL     PEL02
     C*****ELKY3*        READE     PEL02                                  40
B01  C******IN40*        DOWEQ     *OFF
     C*****ELPTDT        ANDLE     XXTTDT
      /free
       Select;
       //*** beg add ***** 01/25/22 *********************
       When isAddingDiv;
          Setll (xxCo#:xxFnd:xxFtDt:xxEmp#:intRnwDiv) pel02;
          Reade (xxCo#:xxFnd:xxFtDt:xxEmp#:intRnwDiv) pel02;
       //*** end add ***** 01/25/22 *********************
       When rnwSingEmpFlg = *on;
          Setll (xxCo#:xxFnd:xxFtDt:xxEmp#)pel02;
          Reade (xxCo#:xxFnd:xxFtDt:xxEmp#)pel02;
       Other;
          Setll (xxCo#:xxFnd:xxFtDt)pel02;
          Reade elky3 pel02;
       Endsl;
       Dow not(%eof) and ELptdt <= xxTtDt;
      /end-free
      *** end chg ***** 06/23/15 **************************************
      *
      ***** BEG ADD ******* 04/18/06 **********************************
      *
      * ONLY PROCESS RECORD IF IND 25 IS ON. IND 25 IS JUST USED FOR
      * CHECKING STATES IF A STATE INCLUDE OR EXCLUDE IS CHOSEN
      *
     C                   EXSR      RISKSR
     C                   MOVE      *ON           *IN25
      *
B02  C     QQINEX        IFEQ      'E'
B03  C     QQSRDF        IFEQ      'Y'
      *
B04  C     RISKST        IFEQ      QQST1
     C     RISKST        OREQ      QQST2
     C     RISKST        OREQ      QQST3
     C     RISKST        OREQ      QQST4
     C     RISKST        OREQ      QQST5
     C     RISKST        OREQ      QQST6
     C     RISKST        OREQ      QQST7
     C     RISKST        OREQ      QQST8
     C     RISKST        OREQ      QQST9
     C     RISKST        OREQ      QQST10
     C                   MOVE      *OFF          *IN25
      ***** BEG ADD ***** 12/11/07 **************************************
     C                   MOVE      *HIVAL        KKDIV
     C     ELKY6         SETGT     PEL02
E04  C                   ENDIF
X03  C                   ELSE
     C                   Z-ADD     ELDIV         SVDIV
     C     ELKY5         SETLL     PEL02
     C     ELKY5         READE     PEL02                                  60
B04  C     *IN60         DOWEQ     *OFF
     C                   EXSR      RISKSR
      *
B05  C     RISKST        IFEQ      QQST1
     C     RISKST        OREQ      QQST2
     C     RISKST        OREQ      QQST3
     C     RISKST        OREQ      QQST4
     C     RISKST        OREQ      QQST5
     C     RISKST        OREQ      QQST6
     C     RISKST        OREQ      QQST7
     C     RISKST        OREQ      QQST8
     C     RISKST        OREQ      QQST9
     C     RISKST        OREQ      QQST10
     C                   MOVE      *OFF          *IN25
     C                   MOVE      *HIVAL        KKDIV
     C     ELKY6         SETGT     PEL02
     C                   LEAVE
E05  C                   ENDIF
      *
     C     ELKY5         READE     PEL02                                  60
E04  C                   ENDDO
      *
B04  C     *IN25         IFEQ      *ON
     C     ELKY7         SETLL     PEL02
     C     ELKY7         READE     PEL02                                  60
E04  C                   ENDIF
E03  C                   ENDIF
      *
      ***** END ADD ***** 12/11/07 **************************************
E02  C                   ENDIF
      *
      *
B02  C     QQINEX        IFEQ      'I'
     C     RISKST        ANDNE     QQST1
     C     RISKST        ANDNE     QQST2
     C     RISKST        ANDNE     QQST3
     C     RISKST        ANDNE     QQST4
     C     RISKST        ANDNE     QQST5
     C     RISKST        ANDNE     QQST6
     C     RISKST        ANDNE     QQST7
     C     RISKST        ANDNE     QQST8
     C     RISKST        ANDNE     QQST9
     C     RISKST        ANDNE     QQST10
     C                   MOVE      *OFF          *IN25
E02  C                   ENDIF
      *
B02  C     *IN25         IFEQ      *ON
      /free
       xxFnd  = ELfnd;
       xxEmp# = ELemp#;
       xxDiv  = ELdiv;
       xxXyr  = ELfyr;
       *in33  = *off;
       *in34  = *off;
       //*** Begin Add *** 10/29/18 ***********************************//
       //*** begin chg ***  09/22/23  **********************************
       //*****SVbsdt = ELbsdt;
       SVbsdt = ELPfdt;
       //*** end chg *****  09/22/23  **********************************
       SVptdt = ELptdt;
       //*** End   Add *** 10/29/18 ***********************************//
      /end-free
     C*
B04  C     ELEMP#        IFNE      XXPEMP
     C                   MOVE      ELEMP#        XXPEMP
      *
      *  READ THE EMPLOYER RECORDS
      *
      /free
       //*** beg chg ***** 01/25/22 ***********
       //Setll (xxCo#:xxFnd:xxEmp#)pem01;
       //Reade (xxCo#:xxFnd:xxEmp#)pem01;
       Select;
       When isAddingDiv;
          Setll (xxCo#:xxFnd:xxEmp#:intRnwDiv) pem01;
          Reade (xxCo#:xxFnd:xxEmp#:intRnwDiv) pem01;
       Other;
          Setll (xxCo#:xxFnd:xxEmp#) pem01;
          Reade (xxCo#:xxFnd:xxEmp#) pem01;
       EndSl;
       //*** end chg ***** 01/25/22 ***********
       Dow not(%eof);
          *in41  = *off;
          det1pr = *off;
          xxDiv  = EMdiv;
          xxKyr  = xxXyr;
       // Check if year exists for employer and it expires in range
       Chain elKy pel01;
       Select;
          When %found and ELptdt >= xxFtDt and ELptdt <= xxTtDt;
       Other;
          Reade (xxCo#:xxFnd:xxEmp#)pem01;
          Iter;
       Endsl;
       // Get future year from Group Detail
       Setgt  f1Ky1 pf10l1;
       Readpe fnKy  pf10l1;
       xxFyr = F1fyr;
       errorMsg = 'Renewal';
       isCancelled = ChkCxStatus(xxCo#:ELfnd:ELemp#:ELdiv:errorMsg); //msg(3)
       //***** Beg Chg ***** 10/09/19 *****************************
       //*****If isCancelled = *on;
       If isCancelled = *on and not(isAddingDiv);
       //***** End Chg ***** 10/09/19 *****************************
          *in41 = *on;
          xxMsg = errorMsg;
          Exsr chekOf;
       Endif;

       errorMsg = 'Renewal';
       futDtlExists = ChkFutDtl(xxCo#:ELfnd:ELemp#:EMdiv:ELfyr:errorMsg); //no msg
       If futDtlExists;
          *in41 = *on;
       Endif;

       errorMsg = 'Renewal';
       isRenewable = ChkRnwStatus(xxCo#:ELfnd:ELemp#:ELdiv:ELfyr:errorMsg); //msg(6)
       If not(isRenewable);
          *in41 = *on;
          xxMsg = errorMsg;
          exsr chekOf;
       Endif;

       errorMsg = 'Renewal';
       qteEmpExists = ChkQteEmp(xxCo#:ELfnd:ELemp#:EMdiv:errorMsg);   //no msg
       //***** Beg Chg ***** 10/09/19 *****************************
       //*****If qteEmpExists;
       If qteEmpExists and not(isAddingDiv);
       //***** End Chg ***** 10/09/19 *****************************
          *in41 = *on;
       Endif;

       errorMsg = 'Renewal';
       //msg1 or msg(4) or msg(5)
       goodPolDates = ChkPolDates(xxCo#:ELfnd:ELemp#:ELdiv:ELfyr:errorMsg);
       If not(goodPolDates);
          *in41 = *on;
          xxMsg = errorMsg;
          Exsr chekOf;
       Endif;

       //*** Begin Add *** 08/08/17 **************************************
       // Check for errors if combinded peo
       //*** Begin Chg *** 05/11/18 **************************************
       //If FNpeoF = 'C';
       //*** begin chg ***  04/07/25  ****************************************
       //******If FNpeoF = 'C' and EMovrMP <> 'Y';  //Not overriden for Master/MCP Policy assignment
       //Not overriden for Master/MCP Policy assignment
       If FNpeoF = 'C' and EMovrMP <> 'Y' and EMLsid <> '1';
       //*** end chg *****  04/07/25  ****************************************
       //*** End   Chg *** 05/11/18 **************************************
          // Get the rating State
          Chain (ELco#:ELrtCd) prs01;
          // Check for PEO errors
          errorMsg = EdtPeoPolType(ELco#:ELfnd:ELemp#:ELdiv:RSjrSt:ELpfDt);
          // If error is found then write to report and don't renew
          If errorMsg <>  ' ';
             *in41 = *On;
             xxMsg = errorMsg;
             Exsr chekOf;
          EndIf;
       EndIf;
       //*** End   Add *** 08/08/17 **************************************

      /end-free
      *
      *  Get group detail for the existing year.
      *
B06  C     *IN41         IFEQ      *OFF
     C                   MOVE      XXXYR         XXKYR
      *
     C     F1KY2         CHAIN     PF101                              60
     C                   Z-ADD     F1STPO        XXSTPO            7 0
     C                   Z-ADD     F1ENPO        XXENPO            7 0
      *
      *  Get group detail for the renewal year.
      *
     C     F1KY          CHAIN     PF101                              60
B07  C     *IN60         IFEQ      *OFF
      *
      *  GET FROM YEAR PRORATA FACTOR
      *
     C                   Z-ADD     ELPROR        XXPROR            5 4
     C*
     C                   MOVE      *OFF          BADDTS            1
      *
      *  LOAD NEW EMPLOYER DETAIL RECORD
      *
     C                   MOVE      XXXYR         XXKYR
     C     ELKY          CHAIN     PEL01                              51
B08  C     *IN51         IFEQ      *OFF
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2FROM            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2TO              8 0
     C*
     C                   MOVE      X2FROM        QQFROM            8 0
     C                   MOVE      X2TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     X2FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     X2TO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
B09  C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
E09  C                   END
     C*
B09  C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
E09  C                   END
     C*
B09  C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2FROM
     C*
     C                   Z-ADD     X2FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X2FROM
     C*
B10  C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
E10  C                   ENDIF
     C*
E09  C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        P2DAYS            5 0
     C                   Z-ADD     ELPFDT        X2PFDT
     C                   Z-ADD     ELPTDT        X2PTDT
     C                   Z-ADD     ELPROR        X2PROR
     C                   MOVEL     ELFFL9        X2FFL9
     C*
      *
     C                   MOVE      ELPFDT        SVPFDT            7 0
     C                   Z-ADD     XXFYR         ELFYR
      *
      *
     C                   Z-ADD     ELPTDT        XXPFDT
     C                   Z-ADD     ELPTDT        XXPTDT
     C                   ADD       1             XXPTYR
     C                   Z-ADD     XXPFDT        ELPFDT
     C                   Z-ADD     XXPTDT        ELPTDT
     C*** begin add ***  02/24/21  ********************************************
     C                   If        FNCGFL <> 'Y' and ELPTDT > F1ENPO
     C                   Z-add     F1ENPO        ELPTDT
     C                   Z-add     F1ENPO        XXPTDT
     C                   Endif
     C*** end add *****  02/24/21  ********************************************
     C                   Z-ADD     *ZEROS        XXPRDT
     C                   IF        ELPFDT > XXPRDT
     C                   EVAL      XXPRDT = ELPFDT
     C                   ENDIF
     C     *LIKE         DEFINE    ELPFDT        XNWPFD
     C     *LIKE         DEFINE    ELPTDT        XNWPTD
     C                   Z-ADD     XXPFDT        XNWPFD
     C                   Z-ADD     XXPTDT        XNWPTD
     C*
B09  C     XXRTOQ        IFEQ      'Y'
     C*
     C*  If Renewing to Quote write the master record
     C*
     C                   MOVE      'Y'           EMRQFL
     C*
     C                   MOVEL     Q1PGM         EMIPGM
     C                   MOVEL     Q1PGM         EMPGM
     C                   MOVEL     Q1USER        EMIUSR
     C                   MOVEL     Q1USER        EMUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     C                   MOVEL     SRDTO         EMIDT
     C*
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     DSTIME        EMITIM
      *** beg add ***** 02/21/18 ****************************
     C                   Clear                   EMqtfr
      *** end add ***** 02/21/18 ****************************
      *** beg add ***** 01/25/22 ****************************
     C                   Clear                   EMcad1
      *** end add ***** 01/25/22 ****************************
     C*
     C                   WRITE     PEM01Q
     C*
     C*  If Renewing to Quote write the billing address record
     C*
     C     A3KY          CHAIN     PA301                              60
     C*
B10  C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     Q1PGM         A3IPGM
     C                   MOVEL     Q1PGM         A3PGM
     C                   MOVEL     Q1USER        A3IUSR
     C                   MOVEL     Q1USER        A3USER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         A3DT
     C                   MOVEL     SRDTO         A3IDT
     C*
     C                   Z-ADD     DSTIME        A3TIME
     C                   Z-ADD     DSTIME        A3ITM
     C*
     C                   WRITE     PA301Q
     C*
E10  C                   ENDIF
     C*
     C*  If Renewing to Quote write the owner/officer records
     C*
     C     OOKY          SETLL     POO01
B10  C     *IN60         DOUEQ     *ON
     C     OOKY          READE     POO01                                  60
     C*
B11  C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     Q1PGM         OOIPGM
     C                   MOVEL     Q1PGM         OOPGM
     C                   MOVEL     Q1USER        OOIUSR
     C                   MOVEL     Q1USER        OOUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         OODT
     C                   MOVEL     SRDTO         OOIDT
     C*
     C                   Z-ADD     DSTIME        OOTIME
     C                   Z-ADD     DSTIME        OOITIM
     C*
     C                   WRITE     POO01Q
     C*
E11  C                   ENDIF
     C*
E10  C                   ENDDO
     C*
     C*  If Renewing to Quote write the employer contact records
     C*
     C*** begin del ***  03/14/22  **************************************
     C*****ECKY          SETLL     PEC01
B10  C******IN60         DOUEQ     *ON
     C*****ECKY          READE     PEC01                                  60
     C*****
B11  C******IN60         IFEQ      *OFF
     C*****
     C*****              MOVEL     Q1PGM         ECIPGM
     C*****              MOVEL     Q1PGM         ECPGM
     C*****              MOVEL     Q1USER        ECIUSR
     C*****              MOVEL     Q1USER        ECUSER
     C*****
     C*****              TIME                    DSDTTM
     C*****
     C*****              Z-ADD     DSDATE        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         ECDT
     C*****              MOVEL     SRDTO         ECIDT
     C*****
     C*****              Z-ADD     DSTIME        ECTIME
     C*****              Z-ADD     DSTIME        ECITIM
     C*****
     C*****              WRITE     PEC01Q
     C*****
E11  C*****              ENDIF
     C*****
E10  C*****              ENDDO
     C*** end del *****  03/14/22  **************************************
     C*
      *** BEG ADD ***** 01/13/04 **********************************************
     C*
     C*  If Renewing to Quote write the direct debit history record
     C*
     C     ECKY          SETLL     PDB01
B10  C     *IN60         DOUEQ     *ON
     C     ECKY          READE     PDB01                                  60
     C*
B11  C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     Q1PGM         DBIPGM
     C                   MOVEL     Q1PGM         DBPGM
     C                   MOVEL     Q1USER        DBIUSR
     C                   MOVEL     Q1USER        DBUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DBDT
     C                   MOVEL     SRDTO         DBIDT
     C*
     C                   Z-ADD     DSTIME        DBTIME
     C                   Z-ADD     DSTIME        DBITM
     C*
     C                   WRITE     PDB01Q
     C*
E11  C                   ENDIF
     C*
E10  C                   ENDDO
     C*
E09  C                   ENDIF
     C*
     C                   Z-ADD     XXPFDT        XXNFDT
     C                   Z-ADD     XXPTDT        XXNTDT
      /Free
       Eval ##datein = ELBSDT ;
       Eval ##fromfmt = '*CYMD' ;
       Eval ##tofmt = *Blanks;
       Eval @@dateout = *Blanks ;
       Eval @@err = *Blanks ;

       SRCVTDAT(##datein : ##fromfmt : ##tofmt : @@dateout : @@err) ;

       If @@err = '1' ;
          Eval ELBSDT = *Zeros ;
          Eval XXBSDT = ELBSDT ;
       Else ;
          Eval XXBSDT = ELBSDT ;
          Eval XXBSYR += 1 ;
       Endif ;

       Select ;
       When XXBSDT = 0 Or
             XXBSDT > ELPFDT ;
          Eval ELBSDT = ELPFDT ;
       When XXBSDT <= ELPFDT ;
          Eval ELBSDT = XXBSDT ;
       Other ;

       Endsl ;

      /End-Free
     C                   Z-ADD     ELPFDT        XXSTPO            7 0
     C                   Z-ADD     ELPTDT        XXENPO            7 0
     C                   Z-ADD     ELPFDT        XXSVPF
     C                   Z-ADD     ELPTDT        XXSVPT
     C                   Z-ADD     ELANDT        XXANDT
     C                   ADD       1             XXANYR
      *
B09  C     XXANDT        IFLE      XXSVPF
     C     XXANDT        ORGE      XXSVPT
     C                   Z-ADD     0             ELANDT
E09  C                   ENDIF                                                  XXANDT IFLE
     C                   Z-ADD     ELANDT        XXSVAN            7 0
     C*
     C* EXPERIENCE MOD TRANSFER
      *** Beg Add ***** 03/14/23 ******************************

       //* move third modifier date with no modifier to 2nd if 2nd is no longer in use.
       If elexm3 = 0 and elemd3 <> 0 and elemd2 <= elpfdt;
          elemd2 = elemd3;
          elexm2 = elexm3;
          elarp2 = elarp3;
          clear elemd3;
          clear elexm3;
          clear elarp3;
          clear elexmd3nf;
       Endif;

       //* move second modifier date with no modifier to 1st if 1st is no longer in use.
       If elexm2 = 0 and elemd2 <> 0 and elemd1 <= elpfdt;
          elemd1 = elemd2;
          elexm1 = elexm2;
          elarp1 = elarp2;
          clear elemd2;
          clear elexm2;
          clear elarp2;
          clear elexmd2nf;
       Endif;

       //* Clear first modifier info if there is a date but no mod.
       If elexm1 = 0 and elemd1 <> 0;
          clear elemd1;
          clear elnoexm1;
          clear elarp1;
          clear elexmd1nf;
       Endif;
      *** End Add ***** 03/14/23 ******************************
     C*
B09  C     ELEXM2        IFNE      *ZEROS
B10  C     ELEXM3        IFNE      *ZEROS
B11  C     ELEMD2        IFLE      ELPFDT
B12  C     ELEMD3        IFGT      ELPFDT
     C                   MOVE      ELEXM2        ELEXM1
     C                   MOVE      ELARP2        ELARP1
     C                   MOVE      ELEMD2        ELEMD1
     C                   MOVE      ELEXM3        ELEXM2
     C                   MOVE      ELARP3        ELARP2
     C                   MOVE      ELEMD3        ELEMD2
     C                   Z-ADD     0             ELEXM3
     C                   Z-ADD     0             ELEMD3
     C                   Z-ADD     0             ELARP3
      *** Beg Add ***** 04/16/19 ******************************
     C                   Clear                   ELNOEXM1
      *** End Add ***** 04/16/19 ******************************
      *** Beg Add ***** 03/14/23 ******************************
       elexmd1nf = elexmd2nf;
       elexmd2nf = elexmd3nf;
       clear elexmd3nf;
      *** End Add ***** 03/14/23 ******************************
X12  C                   ELSE
     C                   MOVE      ELEXM3        ELEXM1
     C                   MOVE      ELARP3        ELARP1
     C                   MOVE      ELEMD3        ELEMD1
     C                   Z-ADD     0             ELEXM2
     C                   Z-ADD     0             ELEMD2
     C                   Z-ADD     0             ELEXM3
     C                   Z-ADD     0             ELEMD3
     C                   Z-ADD     0             ELARP2
     C                   Z-ADD     0             ELARP3
      *** Beg Add ***** 04/16/19 ******************************
     C                   Clear                   ELNOEXM1
      *** End Add ***** 04/16/19 ******************************
      *** Beg Add ***** 03/14/23 ******************************
       elexmd1nf = elexmd3nf;
       clear elexmd3nf;
       clear elexmd2nf;
      *** End Add ***** 03/14/23 ******************************
E12  C                   ENDIF                                                  ELEMD3 IFGT
      *
E11  C                   ENDIF                                                  ELEMD2 IFLE
      *
X10  C                   ELSE                                                   ELEXM3
      *
B11  C     ELEMD2        IFLE      ELPFDT
     C                   MOVE      ELEXM2        ELEXM1
     C                   MOVE      ELARP2        ELARP1
     C                   MOVE      ELEMD2        ELEMD1
     C                   Z-ADD     0             ELEXM2
     C                   Z-ADD     0             ELEMD2
     C                   Z-ADD     0             ELEXM3
     C                   Z-ADD     0             ELEMD3
     C                   Z-ADD     0             ELARP2
     C                   Z-ADD     0             ELARP3
      *** Beg Add ***** 04/16/19 ******************************
     C                   Clear                   ELNOEXM1
      *** End Add ***** 04/16/19 ******************************
      *** Beg Add ***** 03/14/23 ******************************
       elexmd1nf = elexmd2nf;
       clear elexmd2nf;
       clear elexmd3nf;
      *** End Add ***** 03/14/23 ******************************
E11  C                   ENDIF                                                  ELEMD2 IFLE
      *
E10  C                   ENDIF                                                  ELEXM3 IFNE
      *
E09  C                   ENDIF                                                  ELEXM2 IFNE
     C*
      *
      *  CHANGE TO DEFAULT TO WHAT THE VALUE OF ELASP2 WAS FOR THE
      *  PREVIOUS YEAR.  NO "MOVE" STATEMENT IS NEEDED SINCE ELASP2
      *  WILL CONTAIN THE DEVIATION VALUE FROM EMPLOYER DETAIL FOR THE
      *  PREVIOUS FUND YEAR AT THIS POINT IN THE PROGRAM
      *
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C                   Z-ADD     XXDATE        ELIDT
     C                   Z-ADD     DSTIME        ELITM
      /free
       savEliPgm = ELipgm;
      /end-free
     C                   MOVEL(P)  Q1PGM         ELIPGM
     C                   MOVE      *BLANKS       ELFAF
      *** Begin Add *** 12/17/19 ******************************************************************
     C                   MOVE      *BLANKS       ELAUDSR
      *** End   Add *** 12/17/19 ******************************************************************
     C                   MOVE      *BLANKS       ELPROR
     C                   MOVE      *BLANKS       ELEXM2
     C                   MOVE      *BLANKS       ELEMD2
     C                   MOVE      *BLANKS       ELEXM3
     C                   MOVE      *BLANKS       ELEMD3
     C                   MOVE      *BLANKS       ELPRTD
     C                   MOVE      *BLANKS       ELOUTS
     C                   MOVE      *BLANKS       ELTCLP
     C                   MOVE      *BLANKS       ELT#CL
     C                   MOVE      *BLANKS       ELTCLC
     C                   MOVE      *BLANKS       ELANPR
     C                   MOVE      *BLANKS       ELAAPR
     C                   Z-ADD     0             ELEMNA
     C                   Z-ADD     0             ELAMNA
     C                   MOVE      *ZEROS        ELMAPR
     C                   MOVE      *ZEROS        ELSTPR
     C                   MOVE      *ZEROS        ELEDC1
     C                   MOVE      *ZEROS        ELEDC2
     C                   MOVE      *ZEROS        ELEDC3
     C                   MOVE      *ZEROS        ELADC1
     C                   MOVE      *ZEROS        ELADC2
     C                   MOVE      *ZEROS        ELADC3
     C                   MOVE      *ZEROS        ELEARC
     C                   MOVE      *ZEROS        ELAARC
     C                   MOVE      *ZEROS        ELAMPR
     C                   MOVE      *ZEROS        ELASPR
     C                   MOVE      *ZEROS        ELEXP1
     C                   MOVE      *ZEROS        ELEXP2
     C                   MOVE      *ZEROS        ELEXP3
     C                   MOVE      *ZEROS        ELAXP1
     C                   MOVE      *ZEROS        ELAXP2
     C                   MOVE      *ZEROS        ELAXP3
     C                   MOVE      *BLANKS       ELAMNP
     C                   MOVE      *BLANKS       ELEMNP
     C                   MOVE      *ZEROS        ELILEF
     C                   MOVE      *ZEROS        ELENMP
     C                   MOVE      *ZEROS        ELANMP
     C                   MOVE      *ZEROS        ELEIN1
     C                   MOVE      *ZEROS        ELAIN1
     C                   MOVE      *ZEROS        ELEDCT
     C                   MOVE      *ZEROS        ELADCT
     C                   MOVE      *ZEROS        ELEDD2
     C                   MOVE      *ZEROS        ELADD2
     C                   MOVE      *ZEROS        ELTXDU
     C                   MOVE      *ZEROS        ELEDCM
     C                   MOVE      *ZEROS        ELADCM
     C                   MOVE      *ZEROS        ELEDCS
     C                   MOVE      *ZEROS        ELADCS
     C                   MOVE      *ZEROS        ELDISP
     C                   MOVE      *ZEROS        ELEWAV
     C                   MOVE      *ZEROS        ELAWAV
     C                   Z-ADD     0             ELESBJ
     C                   Z-ADD     0             ELASBJ
     C                   Z-ADD     0             ELEMPM
     C                   Z-ADD     0             ELAMPM
     C                   Z-ADD     0             ELINMP
     C                   Z-ADD     0             ELTXPR
     C                   Z-ADD     0             ELEDCR
     C                   Z-ADD     0             ELADCR
     C                   Z-ADD     0             ELEFRS
     C                   Z-ADD     0             ELAFRS
     C                   Z-ADD     0             ELEDC
     C                   Z-ADD     0             ELADC
     C                   Z-ADD     0             ELEMIN
     C                   Z-ADD     0             ELAMIN
     C                   Z-ADD     0             ELEMAX
     C                   Z-ADD     0             ELAMAX
     C                   Z-ADD     0             ELEIN2
     C                   Z-ADD     0             ELAIN2
     C                   Z-ADD     0             ELEDV2
     C                   Z-ADD     0             ELADV2
     C                   Z-ADD     0             EL1APR
     C                   Z-ADD     0             EL2APR
     C                   Z-ADD     0             EL3APR
     C                   Z-ADD     0             EL3FPR
     C                   Z-ADD     0             EL3AAM
     C                   Z-ADD     0             EL4APR
     C                   Z-ADD     0             EL4FPR
     C                   Z-ADD     0             EL4AAM
     C                   Z-ADD     0             EL5APR
     C                   Z-ADD     0             EL5FPR
     C                   Z-ADD     0             EL5AAM
     C                   Z-ADD     0             EL6APR
     C                   Z-ADD     0             EL6FPR
     C                   Z-ADD     0             EL6AAM
     C                   Z-ADD     0             EL7APR
     C                   Z-ADD     0             EL7FPR
     C                   Z-ADD     0             EL7AAM
     C                   Z-ADD     0             ELETCA
     C                   Z-ADD     0             ELATCA
     C                   Z-ADD     0             EL1FPR
     C                   Z-ADD     0             EL2FPR
     C                   Z-ADD     0             EL1AAM
     C                   Z-ADD     0             EL2AAM
     C                   Z-ADD     0             ELMFAM
     C                   Z-ADD     0             ELSFAM
     C                   Z-ADD     0             ELETER
     C                   Z-ADD     0             ELATER
     C                   Z-ADD     0             ELEDTR
     C                   Z-ADD     0             ELADTR
     C                   Z-ADD     0             ELRPE1
     C                   Z-ADD     0             ELRPE2
     C                   Z-ADD     0             ELRPE3
     C                   Z-ADD     0             ELRPA1
     C                   Z-ADD     0             ELRPA2
     C                   Z-ADD     0             ELRPA3
     C                   MOVE      *BLANK        ELFSET
     C                   Z-ADD     0             ELSTAM
     C                   Z-ADD     0             ELLSDT
     C                   Z-ADD     0             ELSRAM
     C                   Z-ADD     0             ELUPSA
      /Free
       ELanca = 0;
       ELancm = 0;
       //*** begin add ***  01/23/20  ******************************
       If ELFFLA <> 'Y';
          Clear ELPP3C;
          Clear ELPPC3;
       Endif;
       //*** end add *****  01/23/20  ******************************
       //*** begin add ***  09/08/17  *****************************************
       //*** begin add ***  09/08/17  *****************************************
       If cltID = '150';

          Chain(n) (ELCo#:ELFnd:ELFyr) pf102;
          If %found;
             ELManP = L2F1ManP;
          Endif;

       Endif;
       //*** end add *****  09/08/17  *****************************************
       //*** end add *****  09/08/17  *****************************************
      /End-free
     C                   CLEAR                   BSSRFQ
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
B09  C     *IN60         IFEQ      *OFF
     C                   MOVEL     BSRLCD        XXWK3A            3
      *
B10  C     XXWK3A        IFNE      *BLANKS
     C     XXWK3A        ANDNE     *ZEROS
     C                   MOVEL     BSRLCD        ELBSCD
E10  C                   ENDIF                                                  XXWK3A IFNE
E09  C                   ENDIF                                                  *IN60 IFEQ
     C                   CLEAR                   ELDEDP
     C                   CLEAR                   ELAMPO
     C                   CLEAR                   ELAMAG
     C                   CLEAR                   EL2DDP
     C                   CLEAR                   EL2APO
     C                   CLEAR                   EL2AAG
      *
      *  Get auto renewal flags
      *
     C                   EXSR      SRRNW
      *
      *  This is a revision to current processing in the Renewal program
      *  which is clearing all the Waiver values on the new Employer Detail
      *  record that is written.
      *
     C     RRKY3         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
B09  C     *IN60         IFEQ      *ON
     C     RRWVAC        OREQ      *ZEROS
     C                   MOVE      *BLANKS       ELWVTP                         WV TYPE
     C                   MOVE      *ZEROS        ELWVPC                         WV %
     C                   MOVE      *ZEROS        ELFCHG                         WV FLAT CHARG
     C                   MOVE      *ZEROS        ELWMIN                         MIN WAV SURCHARG
     C                   Z-ADD     *ZEROS        ELWMAX                         WV MAX CHARG
     C                   MOVE      *BLANKS       ELWVFL                         WV SURCHARG PRM LVL
E09  C                   ENDIF
      *
      *  IF THE DEVIATION EXISTS AND DID NOT EXPIRE, UPDATE
      *  THE EFFECTIVE DATE WITH THE NEW POLICY START DATE.
      *
B09  C     ELASP2        IFNE      *ZEROS
     C     ELDVEX        ANDEQ     *ZEROS
     C     ELASP2        ORNE      *ZEROS
     C     ELDVEX        ANDNE     *ZEROS
     C     ELDVEX        ANDGT     ELPFDT
     C                   Z-ADD     XXSVPF        ELDVEF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELDVEF
     C                   Z-ADD     *ZEROS        ELDVEX
     C                   Z-ADD     *ZEROS        ELASP2
E09  C                   ENDIF                                                  ELAPS2 IFNE
      *
B09  C  N71RRRWD1        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELDVEF
     C                   Z-ADD     *ZEROS        ELDVEX
     C                   Z-ADD     *ZEROS        ELASP2
E09  C                   ENDIF                                                  RRRWD1
      *
      *  PROCESSING FOR DEV#1 2ND PERIOD
B09  C     ELD1S2        IFNE      *ZEROS
     C     ELD1X2        ANDEQ     *ZEROS
     C     ELD1S2        ORNE      *ZEROS
     C     ELD1X2        ANDNE     *ZEROS
     C     ELD1X2        ANDGT     ELPFDT
     C                   Z-ADD     XXSVPF        ELD1D2
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELD1D2
     C                   Z-ADD     *ZEROS        ELD1X2
     C                   Z-ADD     *ZEROS        ELD1S2
E09  C                   ENDIF                                                  ELD1S2 IFNE
     C*
B09  C  N71RRRWD1        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELD1D2
     C                   Z-ADD     *ZEROS        ELD1X2
     C                   Z-ADD     *ZEROS        ELD1S2
E09  C                   ENDIF                                                  RRRWD1
      *
      * Renew Year Deviation Flags - deviation # 1 & # 2
B09  C     ZZRWD1        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELDVEF
     C                   Z-ADD     *ZEROS        ELDVEX
     C                   Z-ADD     *ZEROS        ELASP2
E09  C                   ENDIF                                                  ZZRWD1
B09  C     ZZRWD2        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELDEV2
B10  C     CLTID         IFEQ      '025'
     C                   Z-ADD     *ZEROS        ELSTFC
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
     C*** Begin Add *** 04/07/17 *********************************
B10  C     CLTID         IFEQ      '136'
     C                   Z-ADD     *ZEROS        ELDEV2F
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
     C                   Z-ADD     *ZEROS        ELDEV2BP
     C                   Z-ADD     *ZEROS        ELDEV2AP
     C*** End   Add *** 04/07/17 *********************************
E09  C                   ENDIF                                                  ZZRWD2
      *
      *  IF THE MANAGED CARE CREDIT EXISTS AND DID NOT EXPIRE IN THE
      *  PREVIOUS FYR, UPDATE THE EFFECTIVE DATE WITH THE NEW POLICY
      *  START DATE.
      *
B09  C     ELASPM        IFNE      *ZEROS
     C     ELMCEX        ANDEQ     *ZEROS
     C     ELASPM        ORNE      *ZEROS
     C     ELMCEX        ANDNE     *ZEROS
     C     ELMCEX        ANDGT     ELPFDT
     C                   Z-ADD     XXSVPF        ELMCEF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELMCEF
     C                   Z-ADD     *ZEROS        ELMCEX
     C                   Z-ADD     *ZEROS        ELASPM
E09  C                   ENDIF                                                  ELASPM IFNE
      *
B09  C  N71RRRW1A        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELMCEF
     C                   Z-ADD     *ZEROS        ELMCEX
     C                   Z-ADD     *ZEROS        ELASPM
     C                   Z-ADD     *ZEROS        ELEDCM
     C                   Z-ADD     *ZEROS        ELADCM
E09  C                   ENDIF                                                  RRRW1A
      *
      *
      * Renew Year Subject Premium Credit Flags
B09  C     ZZR1AC        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELASPM
     C                   Z-ADD     *ZEROS        ELMCEF
     C                   Z-ADD     *ZEROS        ELMCEX
     C                   Z-ADD     *ZEROS        ELEDCM
     C                   Z-ADD     *ZEROS        ELADCM
E09  C                   ENDIF                                                  ZZR1AC
B09  C     ZZR1BC        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELASPS
     C                   Z-ADD     *ZEROS        ELSCEF
     C                   Z-ADD     *ZEROS        ELSCEX
     C                   Z-ADD     *ZEROS        ELEDCS
     C                   Z-ADD     *ZEROS        ELADCS
E09  C                   ENDIF                                                  ZZR2BC
      *
      *  IF THE INCREASED LIMITS EXPIRATION DATE IS NOT ZERO, CLEAR THE
      *  INCREASED LIMIT AND DATES.
      *
B09  C     ELILEX        IFNE      *ZEROS
     C     ELILEX        ANDLE     ELPFDT
     C                   Z-ADD     *ZEROS        ELILEF
     C                   Z-ADD     *ZEROS        ELILEX
     C                   MOVE      *BLANKS       ELILCD
     C                   ENDIF
      *
      *  IF THE 2ND INCR LIMITS EXPIRATION DATE IS NOT ZERO, CLEAR THE
      *  INCREASED LIMIT AND DATES.
      *  IF EITHER OF THE INCR LIMITS CODES HAVE A NO RENEW FLAG SET,
      *  CLEAR THE INCREASED LIMIT AND DATES BEFORE CHECKING LIMITS DATES
      *
B09  C     X1LRNW        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELILEF
     C                   Z-ADD     *ZEROS        ELILEX
     C                   MOVE      *BLANKS       ELILCD
     C                   MOVE      *ZEROS        ELEIN1
     C                   MOVE      *ZEROS        ELAIN1
E09  C                   ENDIF                                                  X1MRNW
      *
B09  C     X2LRNW        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELI2EF
     C                   Z-ADD     *ZEROS        ELI2EX
     C                   MOVE      *BLANKS       ELINL2
     C                   MOVE      *ZEROS        ELEIN2
     C                   MOVE      *ZEROS        ELAIN2
E09  C                   ENDIF                                                  X1MRNW
      *
      *
B09  C     ELI2EX        IFNE      *ZEROS
     C     ELI2EX        ANDLE     ELPFDT
     C                   Z-ADD     *ZEROS        ELI2EF
     C                   Z-ADD     *ZEROS        ELI2EX
     C                   MOVE      *BLANKS       ELINL2
X09  C                   ELSE
B10  C     ELINL2        IFNE      *BLANKS
     C                   Z-ADD     XXSVPF        ELILEF
     C                   MOVE(P)   ELINL2        ELILCD
     C                   Z-ADD     0             ELI2EF
     C                   MOVE      *BLANKS       ELINL2
      *
E10  C                   ENDIF                                                  ELI2CD IFNE
E09  C                   ENDIF                                                  ELILEX IFNE
      *
      *
      *  IF THE CONTRACTOR CREDIT EXISTS AND DID NOT EXPIRE IN THE
      *  PREVIOUS FYR, UPDATE THE EFFECTIVE DATE WITH THE NEW POLICY
      *  START DATE.
      *
B09  C     ELASP1        IFNE      *ZEROS
     C     ELCREX        ANDEQ     *ZEROS
     C     ELASP1        ORNE      *ZEROS
     C     ELCREX        ANDNE     *ZEROS
     C     ELCREX        ANDGT     ELPFDT
      *
     C                   Z-ADD     XXSVPF        ELCREF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELCREF
     C                   Z-ADD     *ZEROS        ELCREX
     C                   Z-ADD     *ZEROS        ELASP1
E09  C                   ENDIF                                                  ELASP1 IFNE
      *
B09  C  N71RRRWCC        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELCREF
     C                   Z-ADD     *ZEROS        ELCREX
     C                   Z-ADD     *ZEROS        ELASP1
E09  C                   ENDIF                                                  RRRWCC
      *
      * Renew Year Deviation Flag - Contractor credit
B09  C     ZZRWCC        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELCREF
     C                   Z-ADD     *ZEROS        ELCREX
     C                   Z-ADD     *ZEROS        ELASP1
E09  C                   ENDIF                                                  ZZRWCC
     C*
     C*  If the 1st Deductible is expired and the 2nd is not, move the 2nd
     C*   information to the first and clear the 2nd.
     C*  If the 1st and 2nd Deductible are expired, clear both.
     C*  In order for there to be a 2nd deductible, the first must expire.
     C*
B09  C                   SELECT
S01  C     ELDCEX        WHENNE    *ZEROS
     C     ELDCEX        ANDLE     ELPFDT
     C     EL2DEX        ANDEQ     *ZEROS
     C                   Z-ADD     EL2DEF        ELDCEF
     C                   Z-ADD     EL2DEX        ELDCEX
     C                   Z-ADD     ELDD2P        ELDDCT
     C                   MOVE      ELDCD2        ELDCDE
     C                   MOVE      XX2RNW        XX1RNW
     C                   Z-ADD     *ZEROS        EL2DEF
     C                   Z-ADD     *ZEROS        EL2DEX
     C                   Z-ADD     *ZEROS        ELDD2P
     C                   MOVE      *BLANKS       ELDCD2
     C                   MOVE      *BLANKS       XX2RNW
S02  C     ELDCEX        WHENNE    *ZEROS
     C     ELDCEX        ANDLE     ELPFDT
     C     EL2DEX        ANDNE     *ZEROS
     C                   Z-ADD     *ZEROS        ELDCEF
     C                   Z-ADD     *ZEROS        ELDCEX
     C                   Z-ADD     *ZEROS        ELDDCT
     C                   MOVE      *BLANKS       ELDCDE
B10  C     EL2DEX        IFNE      *ZEROS
     C     EL2DEX        ANDGT     ELPFDT
     C                   Z-ADD     EL2DEF        ELDCEF
     C                   Z-ADD     EL2DEX        ELDCEX
     C                   Z-ADD     ELDD2P        ELDDCT
     C                   MOVE      ELDCD2        ELDCDE
     C                   MOVE      XX2RNW        XX1RNW
     C                   Z-ADD     *ZEROS        EL2DEF
     C                   Z-ADD     *ZEROS        EL2DEX
     C                   Z-ADD     *ZEROS        ELDD2P
     C                   MOVE      *BLANKS       ELDCD2
     C                   MOVE      *BLANKS       XX2RNW
E10  C                   ENDIF                                                  EL2DEX>ELPFDT
B10  C     EL2DEX        IFNE      *ZEROS
     C     EL2DEX        ANDLE     ELPFDT
     C                   Z-ADD     *ZEROS        EL2DEF
     C                   Z-ADD     *ZEROS        EL2DEX
     C                   Z-ADD     *ZEROS        ELDD2P
     C                   MOVE      *BLANKS       ELDCD2
E10  C                   ENDIF                                                  EL2CEX<=ELPFDT
E09  C                   ENDSL
     C*
     C*  If a deductible code still exists, the prior years deductible did
     C*   NOT expire, therefor the effective date of the deductible for the
     C*   current year will be the policy from date.
     C*
B09  C     ELDCDE        IFNE      *BLANKS
     C                   Z-ADD     XXSVPF        ELDCEF
E09  C                   ENDIF
     C***  END   CHG  ***  04/07/05 *************************************
      *
     C***  BEGIN CHG  ***  04/07/05 *************************************
     C**********
     C**N72***** DDRNW     IFEQ 'N'
B09  C     XX1RNW        IFEQ      'N'
     C***  END   CHG  ***  04/07/05 *************************************
     C                   Z-ADD     *ZEROS        ELDCEF
     C                   Z-ADD     *ZEROS        ELDCEX
     C                   Z-ADD     *ZEROS        ELDDCT
     C                   Z-ADD     *ZEROS        ELEDCT
     C                   Z-ADD     *ZEROS        ELADCT
     C                   MOVE      *BLANKS       ELDCDE
      ***** BEG ADD ******* 10/25/99 **********************************
     C                   MOVE      *BLANK        ELCXBL
      ***** END ADD ******* 10/25/99 **********************************
E09  C                   ENDIF                                                  DDRNW
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
B09  C     XX2RNW        IFEQ      'N'
     C                   Z-ADD     *ZEROS        EL2DEF
     C                   Z-ADD     *ZEROS        EL2DEX
     C                   Z-ADD     *ZEROS        ELDD2P
     C                   Z-ADD     *ZEROS        ELEDD2
     C                   Z-ADD     *ZEROS        ELADD2
     C                   MOVE      *BLANKS       ELDCD2
E09  C                   ENDIF
     C***  END   ADD  ***  04/07/05 *************************************
      *
      ***** END ADD ******* 09/22/99 **********************************
      *
      *  IF THE SAFETY CREDIT EXISTS AND DID NOT EXPIRE IN THE
      *  PREVIOUS FYR, UPDATE THE EFFECTIVE DATE WITH THE NEW POLICY
      *  START DATE.
      *
B09  C     ELASPS        IFNE      *ZEROS
     C     ELSCEX        ANDEQ     *ZEROS
      ***** BEG ADD ***** 02/05/07 ************************************
     C     ELASPS        ORNE      *ZEROS
     C     ELSCEX        ANDNE     *ZEROS
     C     ELSCEX        ANDGT     ELPFDT
      ***** END ADD ***** 02/05/07 ************************************
     C                   Z-ADD     XXSVPF        ELSCEF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELSCEF
     C                   Z-ADD     *ZEROS        ELSCEX
     C                   Z-ADD     *ZEROS        ELASPS
E09  C                   ENDIF                                                  ELASPS IFNE
     C*** BEGIN ADD ***  03/17/98  ************************************
      ***** BEG ADD ******* 09/22/99 **********************************
      *
B09  C  N71RRRW1B        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELSCEF
     C                   Z-ADD     *ZEROS        ELSCEX
     C                   Z-ADD     *ZEROS        ELASPS
     C                   Z-ADD     *ZEROS        ELEDCS
     C                   Z-ADD     *ZEROS        ELADCS
E09  C                   ENDIF                                                  RRRW1B
      *
      ***** END ADD ******* 09/22/99 **********************************
      *
      *  IF THE SCHEDULE DB/CR EXISTS AND DID NOT EXPIRE IN THE
      *  PREVIOUS FYR, UPDATE THE EFFECTIVE DATE WITH THE NEW POLICY
      *  START DATE.
      *
B09  C     ELDBCR        IFNE      *ZEROS
     C     ELSDEX        ANDEQ     *ZEROS
      ***** BEG ADD ***** 02/05/07 ************************************
     C     ELDBCR        ORNE      *ZEROS
     C     ELSDEX        ANDNE     *ZEROS
     C     ELSDEX        ANDGT     ELPFDT
      ***** END ADD ***** 02/05/07 ************************************
     C                   Z-ADD     XXSVPF        ELSDEF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELSDEF
     C                   Z-ADD     *ZEROS        ELSDEX
     C                   Z-ADD     *ZEROS        ELDBCR
E09  C                   ENDIF                                                  ELDBCR IFNE
      ***** BEG ADD ******* 09/22/99 **********************************
      *
      ***** BEG ADD ***** 06/05/07 ************************************
      * Renew Year Deviation Flags - Schedule DB/CR factor
B09  C     ZZDBCD        IFEQ      'N'
     C***  Begin Chg  ***  06/14/07  **************************************
     C********** ELDBCD    ANDGT1.0000
     C     ELDBCR        ANDGT     1.0000
     C***  End   Chg  ***  06/14/07  **************************************
     C                   Z-ADD     *ZEROS        ELSDEF
     C                   Z-ADD     *ZEROS        ELSDEX
     C                   Z-ADD     *ZEROS        ELDBCR
E09  C                   ENDIF                                                  ZZDBCD
B09  C     ZZCRCD        IFEQ      'N'
     C***  Begin Chg  ***  06/14/07  **************************************
     C********** ELDBCD    ANDLT1.0000
     C     ELDBCR        ANDLT     1.0000
     C***  End   Chg  ***  06/14/07  **************************************
     C                   Z-ADD     *ZEROS        ELSDEF
     C                   Z-ADD     *ZEROS        ELSDEX
     C                   Z-ADD     *ZEROS        ELDBCR
E09  C                   ENDIF                                                  ZZCRCD
B09  C     ZZCRCD        IFEQ      'N'
     C     ZZDBCD        ANDEQ     'N'
     C                   Z-ADD     *ZEROS        ELSDEF
     C                   Z-ADD     *ZEROS        ELSDEX
     C                   Z-ADD     *ZEROS        ELDBCR
E09  C                   ENDIF                                                  ZZCRCD-ZZDBCD
      ***** END ADD ***** 06/05/07 ************************************
      *
B09  C  N71RRRWDC        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELSDEF
     C                   Z-ADD     *ZEROS        ELSDEX
     C                   Z-ADD     *ZEROS        ELDBCR
     C                   Z-ADD     *ZEROS        ELEDC
     C                   Z-ADD     *ZEROS        ELADC
E09  C                   ENDIF                                                  RRRWDC
      *
      ***** END ADD ******* 09/22/99 **********************************
      *
      *  IF THE 2ND DEVIATION EXISTS AND DID NOT EXPIRE IN THE
      *  PREVIOUS FYR, UPDATE THE EFFECTIVE DATE WITH THE NEW POLICY
      *  START DATE.
      *
B09  C     ELDEV2        IFNE      *ZEROS
     C     ELD2EX        ANDEQ     *ZEROS
      ***** BEG ADD ***** 02/05/07 ************************************
     C     ELDEV2        ORNE      *ZEROS
     C     ELD2EX        ANDNE     *ZEROS
     C     ELD2EX        ANDGT     ELPFDT
      ***** END ADD ***** 02/05/07 ************************************
     C*** Begin Add *** 04/07/17 *********************************
B09  C     ELDEV2F       ORNE      *ZEROS
     C     ELD2EX        ANDEQ     *ZEROS
     C     ELDEV2F       ORNE      *ZEROS
     C     ELD2EX        ANDNE     *ZEROS
     C     ELD2EX        ANDGT     ELPFDT
     C*** End   Add *** 04/07/17 *********************************
     C                   Z-ADD     XXSVPF        ELD2EF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELDEV2
      ***** BEG ADD ***** 01/14/10 ************************************
B10  C     CLTID         IFEQ      '025'
     C                   Z-ADD     *ZEROS        ELSTFC
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
      ***** END ADD ***** 01/14/10 ************************************
     C*** Begin Add *** 04/07/17 *********************************
B10  C     CLTID         IFEQ      '136'
     C                   Z-ADD     *ZEROS        ELDEV2F
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
     C                   Z-ADD     *ZEROS        ELDEV2BP
     C                   Z-ADD     *ZEROS        ELDEV2AP
     C*** End   Add *** 04/07/17 *********************************
E09  C                   ENDIF                                                  ELDEV2 IFNE
     C***** BEG ADD ******* 06/21/10 **********************************
     C*  PROCESSING FOR DEV#2 2ND PERIOD
B09  C     ELD2S2        IFNE      *ZEROS
     C     ELD2X2        ANDEQ     *ZEROS
     C     ELD2S2        ORNE      *ZEROS
     C     ELD2X2        ANDNE     *ZEROS
     C     ELD2X2        ANDGT     ELPFDT
     C                   Z-ADD     XXSVPF        ELD2D2
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELD2D2
     C                   Z-ADD     *ZEROS        ELD2X2
     C                   Z-ADD     *ZEROS        ELD2S2
     C*
B10  C     CLTID         IFEQ      '025'
     C                   Z-ADD     *ZEROS        ELSTFC
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
     C*
E09  C                   ENDIF                                                  ELDEV2 IFNE
      *
B09  C  N71ZZDISC        IFNE      *BLANK
     C     ELASP3        ANDEQ     *ZERO
     C                   MOVE      ZZDISC        ELDISC
E09  C                   ENDIF
B09  C  N71ZZDSPC        IFNE      *ZERO
     C     ELDISC        ANDEQ     *BLANK
     C                   Z-ADD     ZZDSPC        ELASP3
E09  C                   ENDIF
      *** END CHG ***** 11/09/07 **************************************
     C*
     C* CLEAR SAFETY FEE IF NO APPLIED TO SETTING IN RENEWAL YEAR.
     C*
B09  C     ZZSFAP        IFEQ      *BLANK
     C                   Z-ADD     0             ELSFPC
     C                   Z-ADD     0             ELSFMN
E09  C                   ENDIF
     C*** END ADD *****  07/05/07  **************************************
      *
     C*** BEGIN DLT ***  04/06/09  ************************************
     C*************        Z-ADD*ZEROS    ELWMAX
     C*** END   DLT ***  04/06/09  ************************************
     C                   Z-ADD     *ZEROS        ELHXMN
      ***** END ADD ***** 02/05/07 ************************************
      *
B09  C  N71RRRWD2        IFEQ      'N'
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELDEV2
     C                   Z-ADD     *ZEROS        ELEDV2
     C                   Z-ADD     *ZEROS        ELADV2
     C***** BEG ADD ******* 06/21/10 **********************************
     C                   Z-ADD     *ZEROS        ELD2D2
     C                   Z-ADD     *ZEROS        ELD2X2
     C                   Z-ADD     *ZEROS        ELD2S2
     C                   Z-ADD     *ZEROS        ELD2E2
     C                   Z-ADD     *ZEROS        ELD2A2
     C***** END ADD ******* 06/21/10 **********************************
      ***** BEG ADD ***** 01/14/10 ************************************
B10  C     CLTID         IFEQ      '025'
     C                   Z-ADD     *ZEROS        ELSTFC
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
      ***** END ADD ***** 01/14/10 ************************************
     C*** Begin Add *** 04/07/17 *********************************
B10  C     CLTID         IFEQ      '136'
     C                   Z-ADD     *ZEROS        ELDEV2F
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
     C                   Z-ADD     *ZEROS        ELDEV2BP
     C                   Z-ADD     *ZEROS        ELDEV2AP
     C*** End   Add *** 04/07/17 *********************************
E09  C                   ENDIF                                                  RRRWD2
      *
      ***** END ADD ******* 09/22/99 **********************************
     C*
      *  IF THE DEDUCTIBLE DISC EXISTS AND DID NOT EXPIRE IN THE
      *  PREVIOUS FYR, UPDATE THE EFFECTIVE DATE WITH THE NEW POLICY
      *  START DATE.
      *
B09  C     ELDEV2        IFNE      *ZEROS
     C     ELD2EX        ANDEQ     *ZEROS
     C*** Begin Add *** 04/07/17 *********************************
B09  C     ELDEV2F       ORNE      *ZEROS
     C     ELD2EX        ANDEQ     *ZEROS
     C*** End   Add *** 04/07/17 *********************************
     C                   Z-ADD     XXSVPF        ELD2EF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELDEV2
      ***** BEG ADD ***** 01/14/10 ************************************
B10  C     CLTID         IFEQ      '025'
     C                   Z-ADD     *ZEROS        ELSTFC
     C                   Z-ADD     *ZEROS        ELD2EF
     C                   Z-ADD     *ZEROS        ELD2EX
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
      ***** END ADD ***** 01/14/10 ************************************
     C*** Begin Add *** 04/07/17 *********************************
B10  C     CLTID         IFEQ      '136'
     C                   Z-ADD     *ZEROS        ELDEV2F
     C                   Z-ADD     *ZEROS        ELMULT
E10  C                   ENDIF                                                  CLTID='025'
     C                   Z-ADD     *ZEROS        ELDEV2BP
     C                   Z-ADD     *ZEROS        ELDEV2AP
     C*** End   Add *** 04/07/17 *********************************
E09  C                   ENDIF                                                  ELDEV2 IFNE
     C*** END ADD *****  03/17/98  ************************************
      ***** BEG ADD ******* 07/06/06 **********************************
      *
      * If the aircraft reinsurance has expired, then clear the fields
      *
B09  C     ELARCP        IFNE      *ZEROS
     C     ELAREX        ANDEQ     *ZEROS
     C     ELARCA        ORNE      *ZEROS
     C     ELAREX        ANDEQ     *ZEROS
     C                   Z-ADD     XXSVPF        ELAREF
X09  C                   ELSE
     C                   Z-ADD     *ZEROS        ELARCA
     C                   Z-ADD     *ZEROS        ELARCP
     C                   Z-ADD     *ZEROS        ELAREF
     C                   Z-ADD     *ZEROS        ELAREX
E09  C                   ENDIF
      ***** END ADD ******* 07/06/06 **********************************
     C*** BEGIN ADD *** 09/03/08 **************************************
     C*
     C* CALL API TO DETERMINE WHAT THE NEW/RENEWAL FLAG SHOULD BE
     C*
     C                   CALL      'WA069R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELFYR         #1FYR             3 0
     C     ELNWRW        PARM      *BLANKS       #1NWRW            1
     C*** END   ADD *** 09/03/08 **************************************
      *
      *  GET BILLING SCHEDULE FILE
      *
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
B09  C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       BSSEFL
E09  C                   ENDIF                                                  *IN60 = *ON
     C*** BEGIN ADD ***  02/08/08  ***********************************
     C*** BEG ADD ***  07/02/13  ***********************************
     C     CLTID         IFEQ      '146'
     C*** End ADD ***  07/02/13  ***********************************
     C                   MOVE      F1BLFL        ELBLFL
     C*** BEG ADD ***  07/02/13  ***********************************
     C                   ENDIF
     C*** End ADD ***  07/02/13  ***********************************
     C*** END ADD *****  02/08/08  ***********************************
      *
      * WRITE EMPLOYER DETAIL TO NEW FYR
      *
     C***  Begin Add  ***  10/10/03  *********************************
     C                   CLEAR                   ELRQFL
     C                   CLEAR                   ELCOFL
      *
     C*+* BEGIN ADD *+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   MOVE      *BLANK        ELFFL9
     C*+* END ADD *+*+*  08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      ***** BEG ADD ******* 12/04/03 **********************************
     C                   MOVE      Q1USER        ELIUSR
      ***** END ADD ******* 12/04/03 **********************************
     C***  Begin Add  ***  07/23/07  **************************************
     C                   CLEAR                   ELMRFL
     C***  End   Add  ***  07/23/07  **************************************
     C***  Begin Add  ***  01/08/19  **************************************
      /free
       Clear ELscore;
       Clear ELscoreDt;
       Clear ELscoreBP;
      /end-free
     C***  End   Add  ***  01/08/19  **************************************
     C***  Begin Add  ***  03/15/13  **************************************
      /free
       If FNgrpAsc = 'OPTACOMP' and savEliPgm <> 'W7820R';
          Clear ELblFl;
       Endif;
      /end-free
     C***  End   Add  ***  03/15/13  **************************************
     C*** Begin Add *** 04/02/13 *******************************************
     C                   If        ELilEf=0 and ELilCd<>' '
     C                   Eval      ELilEf=xxSvPf
     C                   EndIf
     C*** End   Add *** 04/02/13 *******************************************
      *** beg add ***** 07/03/13 *******************************************
      /free
       Clear flgDivPlnRnw;
       Setgt (ELco# : ELdvcd : ELpfdt) pdv0l11;
       Readpe (ELco# : ELdvcd) pdv0l11;
       Dow not(%eof);
          If DVefdt > ELpfdt or (DVexdt <> 0 and DVexdt <= ELpfdt);
             Readpe (ELco# : ELdvcd) pdv0l11;
             Iter;
          Endif;
          flgDivPlnRnw = DVauto;
          Leave;
       Readpe (ELco# : ELdvcd) pdv0l11;
       Enddo;

       Clear ELdvpr;
       If flgDivPlnRnw <> 'Y';
          Clear ELdvcd;
          Clear ELdvpc;
          Clear ELdvdf;
       Endif;

       //*** beg add ***** 02/01/18 **************
       Clear ELepf;
       Clear ELapf;
       //*** end add ***** 02/01/18 **************
       //*** beg add ***** 02/21/18 **************
       Clear ELqtfr;
       //*** end add ***** 02/21/18 **************
       //*** beg add ***** 07/12/18 **************
       //*** beg chg ***** 01/04/24 **************
       //*** If RRDV1DFRNW = 'Y' and ELDVEF <> 0;
       If RRDV1DFRNW = 'Y';
       //*** end chg ***** 01/04/24 **************
          ELASP2 = RRDV1DFT;
       //*** begin add ***  11/05/20  **************************************
          If ELASP2 = 0;
             ELDVEF = 0;
             ELDVEX = 0;
          Endif;
       //*** end add *****  11/05/20  **************************************
       //*** beg add ***** 01/04/24 **************
          If Elasp2 <> 0;
             ELdvEf = ELpfdt;
             ELdvEx = 0;
          Endif;
       //*** end chg ***** 01/04/24 **************
       Endif;
       //*** beg chg ***** 01/04/24 **************
       //*** If RRDV2DFRNW = 'Y' and ELD2EF <> 0;
       If RRDV2DFRNW = 'Y';
       //*** end chg ***** 01/04/24 **************
          ELDEV2 = RRDV2DFT;
       //*** begin add ***  11/05/20  **************************************
          If ELDEV2 = 0;
             ELD2EF = 0;
             ELD2EX = 0;
          Endif;
       //*** end add *****  11/05/20  **************************************
       //*** beg add ***** 01/04/24 **************
          If Eldev2 <> 0;
             ELd2ef = ELpfdt;
             ELd2ex = 0;
          Endif;
       //*** end chg ***** 01/04/24 **************
       Endif;
       //*** end add ***** 07/12/18 **************
       //*** beg add ***** 06/24/24 **************
       ELcolwp = 0;
       ELcolwa = 0;
       ELcolrp = 0;
       //*** end add ***** 06/24/24 **************

      /end-free
      *** end add ***** 07/03/13 *******************************************
      *
B09  C     XXRTOQ        IFEQ      'Y'
     C                   MOVE      'Y'           ELRQFL
     C                   WRITE     PEL01Q
     C*** BEGIN ADD *** 03/12/10 **************************************
B10  C     CLTID         IFEQ      '025'
     C                   EXSR      WRTSHQ
E10  C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
X09  C                   ELSE
     C***  End   Add  ***  10/10/03  *********************************
     C                   WRITE     PEL01
     C*** BEGIN ADD ***  10/14/05  ************************************
     C*
     C                   Z-ADD     0             X1EMP#
     C                   Z-ADD     0             X1FYR
     C*
     C     XSKY          CHAIN     PXS01                              60
B10  C     *IN60         DOWEQ     *OFF
     C*
     C     XSKY1         CHAIN     PXS01                              60
B11  C     *IN60         IFEQ      *ON
     C                   Z-ADD     ELEMP#        XSEMP#
     C                   Z-ADD     ELFYR         XSFYR
     C                   MOVEL     Q1USER        XSIUSR
     C                   MOVEL     Q1USER        XSUSER
     C                   MOVEL     Q1PGM         XSIPGM
     C                   MOVEL     Q1PGM         XSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XSIDT
     C                   MOVEL     SRDTO         XSDT
     C                   MOVEL     DSTIME        XSITIM
     C                   MOVEL     DSTIME        XSTIME
     C                   WRITE     PXS01
E11  C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XSKY          READE     PXS01                                  60
E10  C                   ENDDO                                                  *IN60 DOWEQ
      *
E09  C                   ENDIF
      *
      *  ROLL LARGE DEDUCTIBLE - (W7820R3 CAN BE PULLED BACK INTO W7820R
      *  WHEN W7820R IS CONVERTED TO RPGIV. W7820R RAN OUT OF FILE ROOM.
      *
     C* Logic from W7820R3 implemented in routine SRRollLgDed
     C***  Begin Chg  ***  05/01/18  **************************************
     C**********         MOVE      '1'           QQMODE            1
     C                   MOVE      'L'           QQMODE            1
     C***  End   Chg  ***  05/01/18  **************************************
     C                   EXSR      SRRollLgDed
     C*
     C* Process Employer Fee Information
     C                   exsr      SrEmpFee
     C*
     C* Process HCN Program information
     C                   exsr      SrHCNPgm
      *
     C                   EXSR      SRRNTK
      *
      //*** Beg Add ***** 08/20/18 ******************
      /free
         //*** Beg Chg ***** 11/12/18 ******************
         //***If XXRTOQ = 'Y';
         //***  Begin Add  ***  07/18/22  **********************
         //If XXRTOQ = 'Y' and ELdiv = GetStRptDv(xxCo#:xxFnd:xxEmp#);
         If XXRTOQ = 'Y';
         //***  End   Add  ***  07/18/22  **********************
         //*** End Chg ***** 11/12/18 ******************
            Exsr RollWaivers;
         Endif;
      /End-free
      //*** End Add ***** 08/20/18 ******************

      *  Create new Carrier #, if not renew to QUOTE
      *
B09  C     XXRTOQ        IFNE      'Y'
      *
     C                   CALL      'WA200R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM      Q1PGM         #1PGM            10
      *
E09  C                   ENDIF
      *
      *  IF SELF REPORTING, GET NUMBER OF FUND POLICY DAYS
      *
B09  C     BSSEFL        IFEQ      'S'
     C*
     C                   Z-ADD     XXPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZEROS        QQDAYS            5 0
     C                   MOVE      *BLANKS       QQERR             1
     C                   MOVE      '1'           QQTERM            1
      *
     C                   CALL      'SRDDIF8'     PRMDIF
      *
B10  C     QQDAYS        IFEQ      366
     C                   Z-ADD     365           FDAYS
X10  C                   ELSE
     C                   Z-ADD     QQDAYS        FDAYS
E10  C                   ENDIF                                                  QQDAYS IFEQ
      *
      *** Begin Change *** 10/29/18 ******************************************
     C***     FDAYS         DIV       30            MNTHS
      /Free
       Select;
       When BSfreq = 'W';
           Exsr GetPlanWeeks;
           mnths = fDays/7;
       Other;
           mnths = fDays/30;
       EndSl;
      /End-Free
      *** End   Change *** 10/29/18 ******************************************
     C*
     C*  If frequency is 'Quarterly' or 'Semi-annual' set MNTHS for
     C*  expected number of reports
B10  C                   SELECT
S01  C     BSSRFQ        WHENEQ    'Q'
     C                   Z-ADD     4             MNTHS
S02  C     BSSRFQ        WHENEQ    'S'
     C                   Z-ADD     2             MNTHS
E10  C                   ENDSL
     C*
E09  C                   ENDIF                                                  BSSEFL IFEQ
     C*
     C*  Roll Entity/Location Detail records - go through each entity/
     C*  location record and process the corresponding detail record
     C     ALKY          CHAIN     PAL01                              60
B09  C     *IN60         DOWEQ     *OFF
     C*
     C*    Don't roll if Entity/Location rec delete date < pol from date
B10  C     ALDLDT        IFEQ      *ZERO
     C     ALDLDT        ORGT      ELPFDT
     C*
     C                   MOVE      ALELID        XXELID
     C*
     C*      If record exists, roll into next year
     C     LDKY          CHAIN     PLD01                              61
B11  C     *IN61         IFEQ      *OFF
     C*
     C                   Z-ADD     XXFYR         LDFYR
     C                   MOVE      Q1USER        LDUSER
     C                   MOVE      Q1PGM         LDPGM
     C                   MOVE      DSTIME        LDTIME
     C                   MOVE      XXDATE        LDDT
     C                   MOVE      LDUSER        LDIUSR
     C                   MOVE      LDPGM         LDIPGM
     C                   MOVE      LDTIME        LDITM
     C                   MOVE      LDDT          LDIDT
     C*
B12  C     XXRTOQ        IFEQ      'Y'
     C                   WRITE     PLD01Q
X12  C                   ELSE
     C                   WRITE     PLD01
     C*
     C*  If location payroll record exists, roll into next year
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C                   MOVE      *ZEROS        SVCLAS
     C                   MOVE      *BLANKS       SVSPEC
     C                   MOVE      *ZEROS        SVEAPR           11 2
     C*
     C     PLKY          SETLL     PPL01
     C     *IN61         DOUEQ     *ON
     C*
     C     PLKY          READE     PPL01                                  61
     C     *IN61         IFEQ      *OFF
     C*
     C     PLCLAS        IFNE      SVCLAS
     C     PLSPEC        ORNE      SVSPEC
      *
     C     SVCLAS        IFNE      0
      *
     C     SVCADT        IFEQ      0
     C     SVSTAT        ANDEQ     'A'
     C     SVCADT        ORGT      ELPFDT
     C     SVSTAT        ANDEQ     'A'
      *
     C                   Z-ADD     PLCLAS        S2CLAS
     C                   Z-ADD     PLEFDT        S2EFDT
     C                   Z-ADD     PLEAPR        S2EAPR
     C                   MOVE      PLSPEC        S2SPEC
     C                   Z-ADD     PLECNT        S2ECNT            7 0
     C                   Z-ADD     PLCADT        S2CADT            7 0
     C                   MOVE      PLSTAT        S2STAT            1
      *
     C                   EXSR      WRTPLO
      *
     C                   Z-ADD     S2CLAS        PLCLAS
     C                   Z-ADD     S2EFDT        PLEFDT
     C                   Z-ADD     S2EAPR        PLEAPR
     C                   MOVE      S2SPEC        PLSPEC
     C                   Z-ADD     S2ECNT        PLECNT
     C                   Z-ADD     S2CADT        PLCADT
     C                   MOVE      S2STAT        PLSTAT
     C*
     C                   ENDIF                                                  PLCADT = 0
     C*
     C                   ENDIF                                                  SVCLAS <> 0
     C*
     C                   Z-ADD     PLCLAS        SVCLAS
     C                   MOVE      PLSPEC        SVSPEC
     C                   Z-ADD     PLEAPR        SVEAPR           11 2
     C                   Z-ADD     PLECNT        SVECNT            7 0
     C                   MOVE      PLSTAT        SVSTAT            1
     C                   Z-ADD     PLCADT        SVCADT            7 0
     C*
     C                   ELSE
     C*
     C                   ADD       PLEAPR        SVEAPR
     C                   Z-ADD     PLCADT        SVCADT
     C                   MOVE      PLSTAT        SVSTAT
     C                   Z-ADD     PLECNT        SVECNT
     C*
     C                   ENDIF                                                  SVCLAS <> PLCLAS
     C*
     C                   ELSE
     C*
     C     SVCLAS        IFNE      0
      *
     C     SVCADT        IFEQ      0
     C     SVSTAT        ANDEQ     'A'
     C     SVCADT        ORGT      ELPFDT
     C     SVSTAT        ANDEQ     'A'
     C                   EXSR      WRTPLO
     C                   ENDIF                                                  SVCADT = 0
      *
     C                   ENDIF                                                  SVCLAS <> 0
      *
     C                   ENDIF                                                  *IN61=*OFF
      *
     C                   ENDDO                                                  *IN61=*OFF
     C*
     C                   ENDIF                                                  *IN61=*OFF
E12  C                   ENDIF
     C*
E11  C                   ENDIF                                                  *IN61=*OFF
     C*
E10  C                   ENDIF                                                  ALDLDT=*ZERO
     C*
B10  C     XXRTOQ        IFEQ      'Y'
     C*
     C                   MOVEL     Q1PGM         ALIPGM
     C                   MOVEL     Q1PGM         ALPGM
     C                   MOVEL     Q1USER        ALIUSR
     C                   MOVEL     Q1USER        ALUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ALDT
     C                   MOVEL     SRDTO         ALIDT
     C*
     C                   Z-ADD     DSTIME        ALTIME
     C                   Z-ADD     DSTIME        ALITM
     C*
     C                   WRITE     PAL01Q
     C*
E10  C                   ENDIF
     C*
     C     ALKY          READE     PAL01                                  60
E09  C                   ENDDO                                                  *IN60=*OFF
      *
      *  ROLL ACTIVE VENDOR HISTORY RECORD
      *
     C     VHKY1         SETGT     PVH01
     C     VHKY          READPE    PVH01                                  60
      *
     C*** BEG CHG *****  01/09/19  ************************************
B09  C***  *IN60         IFEQ      *OFF
     C***  VHEXDT        ANDEQ     *ZERO
B09  C     *IN60         DOWEQ     *OFF
     C     VHEXDT        IFEQ      *ZERO
     C*** END CHG *****  01/09/19  ************************************
      *
     C                   Z-ADD     ELPFDT        VHEFDT
     C                   Z-ADD     XXFYR         VHFYR
     C                   MOVE(P)   Q1USER        VHUSER
     C                   MOVE(P)   Q1PGM         VHPGM
     C                   MOVE(P)   DSTIME        VHTIME
     C                   MOVE(P)   XXDATE        VHDT
     C                   MOVE(P)   VHUSER        VHIUSR
     C                   MOVE(P)   VHPGM         VHIPGM
     C                   MOVE(P)   VHTIME        VHITIM
     C                   MOVE(P)   VHDT          VHIDT
      *
B10  C     XXRTOQ        IFEQ      'Y'
     C                   WRITE     PVH01Q
X10  C                   ELSE
     C                   WRITE     PVH01
E10  C                   ENDIF
      *
E09  C                   ENDIF
     C*** BEG ADD *****  01/09/19  ************************************
     C     VHKY          READPE    PVH01                                  60
E09  C                   ENDDO
     C*** END ADD *****  01/09/19  ************************************
      /Free
       //*** Beg Chg ***** 04/25/23 ***********************************
       //***** Beg Add ***** 10/09/19 *********************************
         //WC2685 - Add CX Detail and CX History record
         //***If isAddingDiv = *On;
            //***intDiv = GetStRptDv(xxCo#:xxFnd:xxEmp#);
            //***Setll (xxCo#:xxFnd:xxFyr:xxEmp#:intDiv) pcx00;
            //***Dou %eof(wdcxl);
               //***Reade (xxCo#:xxFnd:xxFyr:xxEmp#:intDiv) pcx00;
               //***If %eof(wdcxl);
                  //***Leave;
               //***Endif;
              //*** beg chg ***** 01/25/22 *****************
              //*** CXco# = CXco#;
              //*** CXfnd = CXfnd;
              //*** CXfyr = CXfyr;
              //*** CXemp# = CXemp#;
              //*** CXdiv  = ELdiv;
              //*** CXcdat = xxDate;
              //*** CXcxdt = XHcxdt;
              //*** CXuser = Q1user;
              //*** CXpgm  = Q1pgm;
              //*** CXtime = DStime;
              //*** CXdt   = xxDate;
              //*** CXiusr = CXuser;
              //*** CXipgm = CXpgm;
              //*** CXitm  = CXtime;
              //*** CXidt  = CXdt;
               //***CXdiv  = ELdiv;
               //***CXuser = Q1user;
               //***CXpgm  = Q1pgm;
               //***CXtime = DStime;
               //***CXdt   = xxDate;
              //*** end chg ***** 01/25/22 *****************
               //***Write pcx00;
            //***Enddo;
            //***Setll (xxCo#:xxFnd:decFyr:xxEmp#:intDiv) pxh00;
            //***Dou %eof(wdxhl);
               //***Reade (xxCo#:xxFnd:decFyr:xxEmp#:intDiv) pxh00;
               //***If %eof(wdxhl);
                  //***Leave;
               //***Endif;
              //*** beg chg ***** 01/25/22 *****************
              //*** XHco# = XHco#;
              //*** XHfnd = XHfnd;
              //*** XHfyr = XHfyr;
              //*** XHemp# = XHemp#;
              //*** XHdiv  = ELdiv;
              //*** XHcdat = xxDate;
              //*** XHcxdt = XHcxdt;
               //***XHdiv  = ELdiv;
              //*** end chg ***** 01/25/22 *****************
               //***XHuser = Q1user;
               //***XHpgm  = Q1pgm;
               //***XHtime = DStime;
               //***XHdt   = xxDate;
               //***Write pxh00;
            //***Enddo;
         //***Endif;
       //***** End Add ***** 10/09/19 *********************************
         If isAddingDiv = *On;
            //Copy Cncellation detail records of the State reporting division to
            //new added Division.
            intDiv = GetStRptDv(xxCo#:xxFnd:xxEmp#);
            Setgt  (xxCo#:xxFnd:xxFyr:xxEmp#:intDiv:*Hival) pcx00;
            Readpe (xxCo#:xxFnd:xxFyr:xxEmp#:intDiv) pcx00;
            If Not %Eof(wdcxl) and CXrein <> 'Y';
               svReq# = CXreq#;
               CXreq# = 1;
               CXdiv  = ELdiv;
               CXuser = Q1user;
               CXpgm  = Q1pgm;
               CXtime = DStime;
               CXdt   = xxDate;
               CXidt = CXdt;
               CXitm = CXtime;
               CXiusr = CXuser;
               CXipgm = CXpgm;
               Write pcx00;

               //Copy Cncellation history records of the State reporting
               //division to new added Division.
               Setll (xxCo#:xxFnd:xxFyr:xxEmp#:intDiv:svReq#) pxh00;
               Reade (xxCo#:xxFnd:xxFyr:xxEmp#:intDiv:svReq#) pxh00;
               Dow Not %Eof(wdxhl);
                  XHdiv  = ELdiv;
                  XHreq# = CXreq#;
                  XHuser = Q1user;
                  XHpgm  = Q1pgm;
                  XHtime = DStime;
                  XHdt   = xxDate;
                  Write pxh00;
                  Reade (xxCo#:xxFnd:xxFyr:xxEmp#:intDiv:svReq#) pxh00;
               Enddo;
            Endif;
         Endif;
       //*** End Chg ***** 04/25/23 ***********************************
      /End-free
      *
      *  READ EMPLOYER PAYROLL RECORDS
      *
     C                   MOVE      *OFF          *IN51
     C                   MOVE      *BLANKS       SVCLAS            5 0
     C                   MOVE      *BLANKS       SVEFDT            7 0
     C                   MOVE      *BLANKS       SVEXT             5
     C                   Z-ADD     0             SVECNT
     C                   Z-ADD     0             SVACNT
     C                   MOVE      *BLANKS       SVSAFL            1
     C                   MOVE      *BLANKS       SVSC              1
     C                   MOVE      *BLANKS       SVSPEC            1
     C                   MOVE      *ZEROS        SVARAT
     C                   Z-ADD     *ZEROS        XXAMT
     C*** BEGIN ADD ***  09/19/07  *************************************
     C                   CLEAR                   PYC
     C                   CLEAR                   CYC
     C*** END ADD *****  09/19/07  *************************************
      *
      *  TYPE A:  1. USE AUDITED FOR EVERYONE EFFECTIVE PRIOR TO
      *              EXISTING FYR.  IF NOT AUDITED, USE CURRENT YR
      *              ESTIMATED & PRINT ON REPORT.
      *           2. NEW ACCTS USE ESTIMATED FOR CURRENT YR.
      *
      *  TYPE B:  1. IF SELF REPORTING ACCT, ANNUALIZE REPORTED
      *              PAYROLLS FOR CURRENT YR. IF NO REPORTED PAYROLLS
      *              FOUND, USE CURRENT YR ESTIMATED & PRINT ON REPORT.
      *           2. IF ESTIMATING ACCT AND BECAME EFFECTIVE DURING
      *              THE CURRENT YR, USE ESTIMATED PAYROLLS CURRENT YR.
      *           3. IF ESTIMATING ACCT & AUDITED IN PREVIOUS YR,
      *              USE PREVIOUS YR AUDITED PAYROLLS(ANNUALIZED).
      *           4. IF EST ACCT & NOT AUDITED IN PREVIOUS YR, USE
      *              CURRENT YR ESTIMATED PAYROLLS.
      *
      *  TYPE C:     USE CURRENT YEAR ESTIMATED PAYROLL FOR EVERYONE.
      *
      ***** BEG ADD ***** 03/23/06 ***********************************
      *
      *  TYPE D:    USE GREATER OF AUDITED AND ESTIMATED PAYROLL.
      *
      *             IF AUDITED, JUST SET XDTYPE TO "D" AND SAVE PRORATA,
      *             THEN SET XXTYPE TO "E" SO PROCESSES ESTIMATED FIRST.
      *             WHEN GET READY TO WRITE NEW PAYROLL RECORD, IF XDTYPE
      *             IS = "D", THEN GET AUDITED PAYROLL (ADUSTED W/PRORATA)
      *             AND COMPARE IT TO THE ESTIMATED PAYROLL, THEN USE THE
      *             AMOUNT IS THE GREATER OF THE TWO FOR THE NEW RECORD.
      *
     C                   MOVE      *BLANKS       XDTYPE            1
     C                   Z-ADD     *ZEROS        XDPROR            5 4
      /Free
      //*** Beg Add ***** 10/20/22 ******************************
       If QQType <> 'F';
      //*** End Add ***** 10/20/22 ******************************
      /End-Free
B09  C     QQTYPE        IFEQ      'D'
     C     XXXYR         SUB       1             XX2YR             3 0
     C                   Z-ADD     XX2YR         XXKYR
      *
     C     ELKY          CHAIN     PEL01                              60
B10  C     *IN60         IFEQ      *OFF
B11  C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELPROR        XDPROR
     C                   MOVE      'D'           XDTYPE
     C                   MOVE      'E'           XXTYPE
E11  C                   ENDIF                                                  ELFAF = "A"
E10  C                   ENDIF                                                  *IN60 IFEQ
      *
E09  C                   ENDIF                                                  QQTYPE IFEQ
      ***** END ADD ***** 03/23/06 ***********************************
      *
B09  C     QQTYPE        IFEQ      'A'
     C     XXXYR         SUB       1             XX2YR             3 0
     C                   Z-ADD     XX2YR         XXKYR
      *
     C     ELKY          CHAIN     PEL01                              60
B10  C     *IN60         IFEQ      *OFF
      *
B11  C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELPROR        XXPROR            5 4
     C                   MOVE      'A'           XXTYPE            1
X11  C                   ELSE
     C                   MOVE      XXXYR         XXKYR
     C                   MOVE      'E'           XXTYPE
     C                   MOVE      *ON           *IN33
     C                   MOVEL(P)  MSG(2)        XXMSG            65
     C*
     C                   Z-ADD     EMEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEDAT            6 0
     C*
     C                   EXSR      CHEKOF
E11  C                   ENDIF                                                  ELFAF IFEQ
      *
X10  C                   ELSE
      *
     C                   MOVE      XXXYR         XXKYR
     C                   MOVE      'E'           XXTYPE
E10  C                   ENDIF                                                  *IN60 IFEQ
E09  C                   ENDIF                                                  QQTYPE IFEQ
      *
B09  C     QQTYPE        IFEQ      'B'
      *** Begin Add *** 05/13/19 ***********************************************
     C     QQTYPE        OREQ      'E'
      *** End   Add *** 05/13/19 ***********************************************
      *
B10  C     BSSEFL        IFEQ      'S'
     C                   MOVE      XXXYR         XXKYR
     C                   MOVE      'S'           XXTYPE
X10  C                   ELSE
      *
B11  C     SVPFDT        IFGT      XXSTPO
     C                   MOVE      XXXYR         XXKYR
     C                   MOVE      'E'           XXTYPE
X11  C                   ELSE
      *
     C     XXXYR         SUB       1             XX2YR
     C                   Z-ADD     XX2YR         XXKYR
      *
     C     ELKY          CHAIN     PEL01                              60
B12  C     *IN60         IFEQ      *OFF
     C     ELFAF         ANDEQ     'A'
     C                   Z-ADD     ELPROR        XXPROR            5 4
     C                   MOVE      'A'           XXTYPE
      *
X12  C                   ELSE
     C                   MOVE      XXXYR         XXKYR
     C                   MOVE      'E'           XXTYPE
E12  C                   ENDIF                                                  *IN60 IFEQ
E11  C                   ENDIF                                                  SVPFDT IFGT
E10  C                   ENDIF                                                  BSSEFL IFEQ
E09  C                   ENDIF                                                  QQTYPE IFEQ
      *
B09  C     QQTYPE        IFEQ      'C'
     C     QQTYPE        OREQ      'D'
     C                   MOVE      XXXYR         XXKYR
     C                   MOVE      'E'           XXTYPE
E09  C                   ENDIF                                                  QQTYPE IFEQ
B09  C     XXTYPE        IFEQ      'A'
      *
      *   POPULATE PRIOR YEAR CLASS ARRAY
      *
     C                   Z-ADD     1             V                 5 0
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  51
B10  C     *IN51         DOWEQ     *OFF
     C                   CLEAR                   XXPKEY
     C                   MOVEL     EPCLAS        XXPKEY           11
     C     XXPKEY        CAT       EPSPEC:0      XXPKEY
     C     XXPKEY        CAT       EPEXT:0       XXPKEY
     C                   Z-ADD     1             W                 5 0
     C     XXPKEY        LOOKUP    PYC(W)                                 60
B11  C     *IN60         IFEQ      *OFF
     C     EPSTAT        ANDEQ     'A'
     C     EPCADT        ANDEQ     *ZERO
     C     EPSAFL        ANDEQ     'A'
     C                   MOVEL     EPCLAS        PYC(V)
     C     PYC(V)        CAT       EPSPEC:0      PYC(V)
     C     PYC(V)        CAT       EPEXT:0       PYC(V)
     C                   ADD       1             V
E11  C                   ENDIF
     C     EPKY          READE     PEP01                                  51
E10  C                   ENDDO
      *
      *   POPULATE CURRENT YEAR CLASS ARRAY
      *
     C                   Z-ADD     XXXYR         XXKYR
     C                   Z-ADD     1             V
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  51
B10  C     *IN51         DOWEQ     *OFF
     C                   CLEAR                   XXPKEY
     C                   MOVEL     EPCLAS        XXPKEY           11
     C     XXPKEY        CAT       EPSPEC:0      XXPKEY
     C     XXPKEY        CAT       EPEXT:0       XXPKEY
     C                   Z-ADD     1             W                 5 0
     C     XXPKEY        LOOKUP    CYC(W)                                 60
B11  C     *IN60         IFEQ      *OFF
     C     EPSTAT        ANDEQ     'A'
     C     EPCADT        ANDEQ     *ZERO
     C                   MOVEL     EPCLAS        CYC(V)
     C     CYC(V)        CAT       EPSPEC:0      CYC(V)
     C     CYC(V)        CAT       EPEXT:0       CYC(V)
     C                   ADD       1             V
E11  C                   ENDIF
     C     EPKY          READE     PEP01                                  51
E10  C                   ENDDO
     C                   Z-ADD     XX2YR         XXKYR
E09  C                   ENDIF                                                  XXTYPE = 'A'
      *
      *  STORE THE POLICY START AND END DATES FOR THE FUND YEAR
      *  SELECTED FOR USE.
      *
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     ELPFDT        XXPFDT
     C                   Z-ADD     ELPTDT        XXPTDT
     C                   Z-ADD     *ZEROS        PDAYS
      *
     C     EPKY          SETLL     PEP01
      *
B09  C     *IN51         DOUEQ     *ON
      *
     C     OVERP         TAG
     C                   Z-ADD     0             XXAMT1
      *
     C     EPKY          READE     PEP01                                  51
B10  C     *IN51         IFEQ      *OFF
      *
B11  C     EPCADT        IFGE      XXPFDT
     C     EPCADT        ANDLE     XXPTDT
     C                   EXSR      LDINF1
     C                   Z-ADD     1             INFO#             3 0
     C                   EXSR      SPLIT
     C                   ITER
E11  C                   ENDIF
     C*
B11  C     EPSTAT        IFNE      'A'
     C     EPCADT        ORNE      *ZEROS
     C                   GOTO      OVERP
E11  C                   ENDIF                                                  EPSTAT IFEQ
      *
B11  C     XXTYPE        IFEQ      'A'
     C     EPSAFL        ANDNE     'A'
     C                   GOTO      OVERP
E11  C                   ENDIF                                                  XXTYPE IFEQ
      *
      * DO NOT ROLL IF PRIOR YEAR CLASS DOESN'T EXIST IN NEW YEAR
      *
B11  C     XXTYPE        IFEQ      'A'
     C                   CLEAR                   XXPKEY
     C                   MOVEL     EPCLAS        XXPKEY           11
     C     XXPKEY        CAT       EPSPEC:0      XXPKEY
     C     XXPKEY        CAT       EPEXT:0       XXPKEY
     C                   Z-ADD     1             V
     C     XXPKEY        LOOKUP    CYC(V)                                 60
B12  C     *IN60         IFEQ      *OFF
     C                   GOTO      OVERP
E12  C                   ENDIF
E11  C                   ENDIF                                                  XXTYPE IFEQ
      *
      *  ON CHANGE IN CLASS, WRITE PREVIOUS CLASS TO NEW YEAR
      *
B11  C     EPCLAS        IFNE      SVCLAS
     C     EPSPEC        ORNE      SVSPEC
      **** BEGIN ADD **** 10/11/00 *********************************************
     C     EPEXT         ORNE      SVEXT
      **** END   ADD **** 10/11/00 *********************************************
      *
B12  C     SVCLAS        IFNE      *ZEROS
     C                   MOVE      EPCLAS        S2CLAS
     C                   MOVE      EPEXT         S2EXT
     C                   Z-ADD     EPECNT        S2ECNT
     C                   Z-ADD     EPACNT        S2ACNT
     C                   MOVE      EPSPEC        S2SPEC
     C                   MOVE      EPSC          S2SC
     C                   MOVE      EPSAFL        S2SAFL
     C                   MOVE      EPAAPR        S2AAPR
     C                   MOVE      EPSAPR        S2SAPR
     C                   MOVE      EPEAPR        S2EAPR
     C                   Z-ADD     EPARAT        S2ARAT
     C                   Z-ADD     EPEFDT        S2EFDT
      *
      *   If Type is "D", Get AUDITED Payroll by reading through the
      *   Prior year's payroll records for these class code
B13  C     XDTYPE        IFEQ      'D'
     C                   Z-ADD     *ZEROS        XAPAMT
     C     EPKY3         SETLL     PEP01
     C     EPKY3         READE     PEP01                                  60
      *
B14  C     *IN60         DOWEQ     *OFF
B15  C     XDPROR        IFGT      *ZEROS
     C     EPAAPR        ANDGT     *ZEROS
     C     EPAAPR        DIV       XDPROR        XAPAMT
     C                   MOVE      *ON           *IN60
E15  C                   ENDIF                                                  *IN60 = *ON
     C     EPKY3         READE     PEP01                                  60
E14  C                   ENDDO                                                  *IN60 = *ON
      *
      * NOW CHAIN BACK TO ESTIMATED RECORD
     C     EPKY4         CHAIN     PEP01                              60
E13  C                   ENDIF                                                  XDTYPE = D
      *
     C                   EXSR      SRWRIT
      *
     C                   MOVE      S2CLAS        EPCLAS
     C                   MOVE      S2EXT         EPEXT
     C                   Z-ADD     S2ECNT        EPECNT
     C                   Z-ADD     S2ACNT        EPACNT
     C                   MOVE      S2SPEC        EPSPEC
     C                   MOVE      S2SC          EPSC
     C                   MOVE      S2SAFL        EPSAFL
     C                   Z-ADD     S2AAPR        EPAAPR
     C                   Z-ADD     S2SAPR        EPSAPR
     C                   Z-ADD     S2EAPR        EPEAPR
     C                   Z-ADD     S2ARAT        EPARAT
     C                   MOVE      *ZEROS        XXAMT
     C                   Z-ADD     S2EFDT        EPEFDT
E12  C                   ENDIF                                                  SVCLAS IFNE
      *
     C                   MOVE      EPCLAS        SVCLAS
     C                   MOVE      EPEFDT        SVEFDT            7 0
     C                   MOVE      EPEXT         SVEXT
     C                   Z-ADD     EPECNT        SVECNT
     C                   Z-ADD     EPACNT        SVACNT
     C                   MOVE      EPSAFL        SVSAFL
     C                   MOVE      EPSC          SVSC
     C                   MOVE      EPSPEC        SVSPEC
     C                   Z-ADD     EPARAT        SVARAT
      *
      *  SET THE PAYROLL CLASS EFFECTIVE DATE TO ZERO.
      *
E11  C                   ENDIF                                                  EPCLAS IFNE
      *
      *  LOAD THE PAYROLL CLASS EFFECTIVE DATE.
      *  SAVE PAYROLL AMOUNTS FOR NEW WRITE
      *
B11  C     XXTYPE        IFEQ      'A'
     C                   Z-ADD     EPAAPR        XXAMT1           11 2
     C                   Z-ADD     EPACNT        XXCNT
X11  C                   ELSE
B12  C     XXTYPE        IFEQ      'S'
     C                   Z-ADD     0             XXAMTP
     C                   Z-ADD     0             XXAVG
     C                   Z-ADD     0             X
     C                   MOVE      *ON           *IN11
     C                   MOVE      'N'           XXFLG0
     C                   Z-ADD     0             SVYYMM
     C                   Z-ADD     0             XXAMTM
      *
      *   INCLUDE THE SELF REPORTING RECORD AS REPORTED MONTH IF THE
      *   AMOUNT IS 0, OR IF THE TOTAL AMT EXCEEDS 0.
      *
      /Free
       //*** Begin Add    ***  10/29/18 ***************************************
        Sorta WKD;
       //*** End   Add    ***  10/29/18 ***************************************
      /End-Free
     C     SRKY          CHAIN     PSR01                              60
B13  C     *IN60         DOWEQ     *OFF
      *
B14  C     SRCLAS        IFEQ      EPCLAS
     C     SRSPEC        ANDEQ     EPSPEC
      *
      *   IF VENDOR PROCESSING IS ENABLED, SRRPMY WILL NOT BE USED; HOWEVER
      *   SRTODT CAN BE MOVED INTO SRRPMY AND THE SAME ALGORITHM CAN BE USED.
      *   FOR THE CALCULATION OF ESTIMATED PAYROLL.
      *
      /Free
       //*** Begin Change ***  10/29/18 ***************************************
B15    //C     SRTODT        IFNE      0
       //C                   MOVEL     SRTODT        SRRPMY
E15    //C                   ENDIF
       //*
B15    //C     *IN11         IFEQ      *ON
       //C                   Z-ADD     SRRPMY        SVYYMM
       //C                   MOVE      *OFF          *IN11
E15    //C                   ENDIF
       //*
       //C     SRRPMY        IFNE      SVYYMM
       //C                   ADD       XXAMTM        XXAMTP
       //*
       //C     XXAMTM        IFNE      0
       //C     XXFLG0        OREQ      'Y'
       //C                   ADD       1             X
       //C                   ENDIF                                                  XXAMTM IFNE
       //*
       //C                   Z-ADD     0             XXAMTM
       //C                   MOVE      'N'           XXFLG0
       //C                   Z-ADD     SRRPMY        SVYYMM
       //C                   ENDIF                                                  SRRPMY IFNE

         Select;                                                                //Determine Period
         When BSfreq = 'W';
            xxRPMY = %LookupGE(SRTODT:WKD);

         When srTODT <> *Zeros;
            xxRPMY = %Dec(%Subst(%Editc(srTODT:'X'):1:5):5:0);

         Other;
            xxRPMY = srRPMY;
         Endsl;

         If *in11 = *On;
            svYYMM = xxRPMY;
            *in11 = *Off;
         Endif;

         If xxRPMY <> svYYMM;
            xxAmtP = xxAmtP + xxAmtM;

            If xxAmtM <> *Zero or xxFlg0 = 'Y';
               x = x + 1;
            Endif;

            xxAmtM = *Zeros;
            xxFlg0 = 'N';
            svYYMM = xxRPMY;
         Endif;
       //*** End   Change ***  10/29/18 ***************************************
      /End-Free
      *
B15  C     SRAMTP        IFEQ      0
     C                   MOVE      'Y'           XXFLG0
E15  C                   ENDIF                                                  SRAMTP IFEQ
      *
     C                   ADD       SRAMTP        XXAMTM
E14  C                   ENDIF                                                  SRCLAS IFEQ
      *
     C     SRKY          READE     PSR01                                  60
E13  C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ADD       XXAMTM        XXAMTP
      *
B13  C     XXAMTM        IFNE      0
     C     XXFLG0        OREQ      'Y'
     C                   ADD       1             X
E13  C                   ENDIF                                                  XXAMTM IFNE
      *
      *  1. CALCULATE AVERAGE MONTHLY PAYROLL
      *  2. CALCULATE # OF UNREPORTED MONTHS
      *  3. CALCULATE TOTAL PAYROLL FOR UNREPORTED MONTHS
      *  4. CALCULATE TOTAL PAYROLL
      *
B13  C     X             IFNE      0
     C     XXAMTP        DIV(H)    X             XXAVG
     C     MNTHS         SUB       X             Y                 2 0
      *
      /Free
        //*** Begin Add 05/13/19 *********************************************
        If QQtype = 'E';
           Y = *Zeros;
        Endif;
        //*** End   Add 05/13/19 *********************************************
      /End-Free
      *
     C     Y             MULT      XXAVG         XXDIFF           11 2
     C     XXDIFF        ADD       XXAMTP        XXAMT
      /Free
        //*** Begin Add 05/13/19 *********************************************
        If QQtype = 'E';
          Edays = SRANNS(EPefdt:ELptdt);
          If Edays > *zero;
             XXAMT = %DECH((XXAMT/Edays) * 365:11:0);
          Endif;
        Endif;
        //*** End   Add 05/13/19 *********************************************
      /End-Free
      *
X13  C                   ELSE
     C*
     C* TURNS ON IF THERE AREN'T ANY SELF-REPORTING PAYROLLS
     C* FOR THAT EMPLOYER.
     C*
     C                   MOVE      *ON           *IN34
     C                   Z-ADD     EMEFD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEDAT
     C                   MOVEL(P)  MSG(1)        XXMSG
     C                   MOVEL     EPCLAS        XXCLAS            6
     C                   MOVE      EPSPEC        XXCLAS
     C                   MOVE      XXCLAS        XXMSG
     C                   EXSR      CHEKOF
     C                   Z-ADD     EPEAPR        XXAMT
     C     *LIKE         DEFINE    EPECNT        XXCNT
     C                   Z-ADD     EPECNT        XXCNT
E13  C                   ENDIF                                                  X IFNE
X12  C                   ELSE
     C                   Z-ADD     EPEAPR        XXAMT1
     C                   Z-ADD     EPECNT        XXCNT
E12  C                   ENDIF                                                  XXTYPE IFEQ
E11  C                   ENDIF                                                  XXTYPE IFEQ
B11  C     PRSCCD        IFNE      'Y'
     C***  End   Add  ***  10/10/03  **************************************
     C                   EXSR      SRANNL
     C***  Begin Add  ***  10/10/03  **************************************
E11  C                   ENDIF
     C***  End   Add  ***  10/10/03  **************************************
     C                   ADD       XXAMT1        XXAMT
     C*** END ADD *****  05/30/00  ************************************
      *
      *   WRITE FINAL PAYROLL CLASS RECORD (*IN51-END OF EMPLOYER PR)
      *
X10  C                   ELSE
B11  C     SVCLAS        IFNE      *ZEROS
      *
      ***** BEG ADD ***** 03/23/06 ***********************************
      *   IF TYPE IS "D", GET AUDITED TO COMPARE HERE
      *
B12  C     XDTYPE        IFEQ      'D'
     C                   Z-ADD     *ZEROS        XAPAMT
     C     EPKY3         SETLL     PEP01
     C     EPKY3         READE     PEP01                                  60
      *
B13  C     *IN60         DOWEQ     *OFF
B14  C     XDPROR        IFGT      *ZEROS
     C     EPAAPR        ANDGT     *ZEROS
     C     EPAAPR        DIV       XDPROR        XAPAMT
     C                   MOVE      *ON           *IN60
E14  C                   ENDIF                                                  *IN60 = *ON
     C     EPKY3         READE     PEP01                                  60
E13  C                   ENDDO                                                  *IN60 = *ON
      *
      * NOW CHAIN BACK TO ESTIMATED RECORD
     C     EPKY4         CHAIN     PEP01                              60
E12  C                   ENDIF                                                  XDTYPE = D
      ***** END ADD ***** 03/23/06 ***********************************
      *
     C                   EXSR      SRWRIT
E11  C                   ENDIF                                                  SVCLAS IFNE
E10  C                   ENDIF                                                  *IN51 IFEQ
E09  C                   ENDDO                                                  *IN51 DOUEQ
     C*** BEG ADD *****  03/07/06 *************************************
B09  C     XXTYPE        IFEQ      'A'
      *
      *   ROLL CURRENT YEAR PAYROLLS THAT AREN'T IN PRIOR YEAR
      *
     C                   Z-ADD     0             XXAMT
     C                   Z-ADD     0             SVCLAS
     C*** BEG ADD *****  04/01/08 *************************************
     C                   Z-ADD     0             SVEFDT
     C*** END ADD *****  04/01/08 *************************************
     C                   Z-ADD     XXXYR         XXKYR
     C                   MOVE      'E'           XXTYPE
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     ELPFDT        XXPFDT
     C                   Z-ADD     ELPTDT        XXPTDT
     C                   Z-ADD     *ZEROS        PDAYS
     C                   Z-ADD     ELPROR        XXPROR
      *
     C     EPKY          SETLL     PEP01
B10  C     *IN51         DOUEQ     *ON
      *
     C                   Z-ADD     0             XXAMT1
     C     EPKY          READE     PEP01                                  51
B11  C     *IN51         IFEQ      *OFF
      *
B12  C     EPSTAT        IFNE      'A'
     C     EPCADT        ORNE      *ZEROS
     C                   ITER
E12  C                   ENDIF                                                  EPSTAT IFEQ
      *
      * DO NOT ROLL IF CURRENT YEAR CLASS EXISTS IN PRIOR YEAR
      *
     C                   CLEAR                   XXPKEY
     C                   MOVEL     EPCLAS        XXPKEY           11
     C     XXPKEY        CAT       EPSPEC:0      XXPKEY
     C     XXPKEY        CAT       EPEXT:0       XXPKEY
     C                   Z-ADD     1             V
     C     XXPKEY        LOOKUP    PYC(V)                                 60
B12  C     *IN60         IFEQ      *ON
     C                   ITER
E12  C                   ENDIF
      *
      *  ON CHANGE IN CLASS, WRITE PREVIOUS CLASS TO NEW YEAR
      *
B12  C     EPCLAS        IFNE      SVCLAS
     C     EPSPEC        ORNE      SVSPEC
     C     EPEXT         ORNE      SVEXT
      *
B13  C     SVCLAS        IFNE      *ZEROS
     C                   MOVE      EPCLAS        S2CLAS
     C                   MOVE      EPEXT         S2EXT
     C                   Z-ADD     EPECNT        S2ECNT
     C                   Z-ADD     EPACNT        S2ACNT
     C                   MOVE      EPSPEC        S2SPEC
     C                   MOVE      EPSC          S2SC
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     EPECNT        S2ECNT
     C                   Z-ADD     EPACNT        S2ACNT
     C***  End   Add  ***  03/22/06  **************************************
     C                   MOVE      EPSAFL        S2SAFL
     C                   MOVE      EPAAPR        S2AAPR
     C                   MOVE      EPSAPR        S2SAPR
     C                   MOVE      EPEAPR        S2EAPR
     C                   Z-ADD     EPARAT        S2ARAT
     C                   Z-ADD     EPEFDT        S2EFDT
      *
     C                   EXSR      SRWRIT
      *
     C                   MOVE      S2CLAS        EPCLAS
     C                   MOVE      S2EXT         EPEXT
     C                   Z-ADD     S2ECNT        EPECNT
     C                   Z-ADD     S2ACNT        EPACNT
     C                   MOVE      S2SPEC        EPSPEC
     C                   MOVE      S2SC          EPSC
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     S2ECNT        EPECNT
     C                   Z-ADD     S2ACNT        EPACNT
     C***  End   Add  ***  03/22/06  **************************************
     C                   MOVE      S2SAFL        EPSAFL
     C                   Z-ADD     S2AAPR        EPAAPR
     C                   Z-ADD     S2SAPR        EPSAPR
     C                   Z-ADD     S2EAPR        EPEAPR
     C                   Z-ADD     S2ARAT        EPARAT
     C                   MOVE      *ZEROS        XXAMT
     C                   Z-ADD     S2EFDT        EPEFDT
E13  C                   ENDIF                                                  SVCLAS IFNE
      *
     C                   MOVE      EPCLAS        SVCLAS
     C***  BEG ADD ******  04/01/08  **************************************
     C                   MOVE      EPEFDT        SVEFDT
     C***  END ADD ******  04/01/08  **************************************
     C                   MOVE      EPEXT         SVEXT
     C                   Z-ADD     EPECNT        SVECNT
     C                   Z-ADD     EPACNT        SVACNT
     C                   MOVE      EPSAFL        SVSAFL
     C                   MOVE      EPSC          SVSC
     C                   MOVE      EPSPEC        SVSPEC
     C                   Z-ADD     EPARAT        SVARAT
      *
      *  SET THE PAYROLL CLASS EFFECTIVE DATE TO ZERO.
      *
E12  C                   ENDIF                                                  EPCLAS IFNE
      *
      *  ACCUMULATE  AMOUNTS FOR NEW WRITE
      *
     C                   Z-ADD     EPEAPR        XXAMT1
     C                   EXSR      SRANNL
     C                   ADD       XXAMT1        XXAMT
      *
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     EPECNT        XXCNT
     C***  End   Add  ***  03/22/06  **************************************
      *
      *   WRITE FINAL PAYROLL CLASS RECORD (*IN51-END OF EMPLOYER PR)
      *
X11  C                   ELSE
B12  C     SVCLAS        IFNE      *ZEROS
     C                   EXSR      SRWRIT
E12  C                   ENDIF                                                  SVCLAS IFNE
E11  C                   ENDIF                                                  *IN51 IFEQ
E10  C                   ENDDO                                                  *IN51 DOUEQ
      *
E09  C                   ENDIF                                                  XXTYPE = 'A'
     C*** END ADD *****  03/07/06 *************************************
     C*** BEGIN ADD ***  12/10/03  ************************************
     C                   Z-ADD     0             X1CLAS
     C                   Z-ADD     0             X1EFDT
     C                   MOVE      *BLANK        X1SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   MOVE      *BLANK        X1EXT
     C*** END   ADD ***  04/20/09  ************************************
     C*
B09  C     XXRTOQ        IFEQ      'Y'
     C*
     C     EPKY1Q        CHAIN     PEP01Q                             60
B10  C     *IN60         DOWEQ     *OFF
     C*
B11  C     EPCLAS        IFNE      X1CLAS
     C     EPSPEC        ORNE      X1SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C     EPEXT         ORNE      X1EXT
     C*** END   ADD ***  04/20/09  ************************************
     C     EPEFDT        ORNE      X1EFDT
     C                   Z-ADD     EPCLAS        X1CLAS
     C                   MOVE      EPSPEC        X1SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   MOVEL     EPEXT         X1EXT
     C*** END   ADD ***  04/20/09  ************************************
     C                   Z-ADD     EPEFDT        X1EFDT
     C                   Z-ADD     EPEAPR        SVEAPR           11 2
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     EPECNT        SVECNT
     C***  End   Add  ***  03/22/06  **************************************
     C*
X11  C                   ELSE
     C*
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     SVECNT        EPECNT
     C***  End   Add  ***  03/22/06  **************************************
     C                   ADD       SVEAPR        EPEAPR
     C                   UPDATE    PEP01Q
     C     EPKY2Q        CHAIN     PEP01Q                             60
     C  N60              DELETE    PEP01Q
     C                   Z-ADD     0             X1CLAS
     C                   Z-ADD     0             X1EFDT
     C                   MOVE      *BLANK        X1SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   MOVE      *BLANK        X1EXT
     C*** END   ADD ***  04/20/09  ************************************
E11  C                   ENDIF                                                  EPCLAS IFNE
     C*
     C     EPKY1Q        READE     PEP01Q                                 60
E10  C                   ENDDO                                                  *IN60 DOWEQ
     C*
X09  C                   ELSE                                                   XXWTOQ IFEQ
     C*
     C     EPKY1         CHAIN     PEP01                              60
B10  C     *IN60         DOWEQ     *OFF
     C*
B11  C     EPCLAS        IFNE      X1CLAS
     C     EPSPEC        ORNE      X1SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C     EPEXT         ORNE      X1EXT
     C*** END   ADD ***  04/20/09  ************************************
     C     EPEFDT        ORNE      X1EFDT
     C                   Z-ADD     EPCLAS        X1CLAS
     C                   MOVE      EPSPEC        X1SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   MOVE      EPEXT         X1EXT
     C*** END   ADD ***  04/20/09  ************************************
     C                   Z-ADD     EPEFDT        X1EFDT
     C                   Z-ADD     EPEAPR        SVEAPR           11 2
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     EPECNT        SVECNT
     C***  End   Add  ***  03/22/06  **************************************
     C*
X11  C                   ELSE
     C*
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     SVECNT        EPECNT
     C***  End   Add  ***  03/22/06  **************************************
     C                   ADD       SVEAPR        EPEAPR
     C                   UPDATE    PEP01
     C     EPKY2         CHAIN     PEP01                              60
     C  N60              DELETE    PEP01
     C                   Z-ADD     0             X1CLAS
     C                   Z-ADD     0             X1EFDT
     C                   MOVE      *BLANK        X1SPEC
     C*** BEGIN ADD ***  04/20/09  ************************************
     C                   MOVE      *BLANK        X1EXT
     C*** END   ADD ***  04/20/09  ************************************
E11  C                   ENDIF                                                  EPCLAS IFNE
     C*
     C     EPKY1         READE     PEP01                                  60
E10  C                   ENDDO                                                  *IN60 DOWEQ
     C*
E09  C                   ENDIF                                                  XXWTOQ IFEQ
     C*** END ADD *****  12/10/03  ************************************
     C***  BEGIN ADD  ***  10/15/19  *********************************
      /Free
      //*** Beg Add ***** 10/20/22 ******************************
       Endif;
      //*** End Add ***** 10/20/22 ******************************
      /End-Free
      *
      *   Clean up any "Q" status records before rolling agent info
      *
      /free

          xxStat = 'Q';
          Setll qtky2 pqt01;
          Reade qtky2 pqt01;
          Dow not %eof;
             Delete pqt01;
             Reade qtky2 pqt01;
          Enddo;

      /end-free
     C***  END ADD  *****  10/15/19  *********************************
      *
      *   CHECK FOR ACTIVE AGENT OF RECORD
      *
     C                   MOVE      *OFF          *IN51
     C     AHKY          SETGT     PAH01
B09  C     *IN51         DOUEQ     *ON
     C     AHKY          READPE    PAH01                                  51

      *** beg add ***** 11/15/12 **************************************
      * Do not roll records with effective date beyond the renewal
      * year's expiration date
B10  c                   dow       *in51 = *off
B11  c                   if        AHefdt < xxSvpt
     c                   leave
E11  c                   endif
     c     ahky          readpe    pah01                                  51
E10  c                   enddo
      *** end add ***** 11/15/12 **************************************

B10  C     *IN51         IFEQ      *OFF
     C     AHSTAT        ANDNE     'I'
     C                   MOVE      XXCO#         AHCO#
     C                   MOVE      XXFND         AHFND
     C                   MOVE      XXEMP#        AHEMP#
     C                   MOVE      XXDIV         AHDIV
     C                   Z-ADD     XXFYR         AHFYR
     C                   MOVE      Q1USER        AHUSER
     C                   MOVE      Q1PGM         AHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        AHTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AHDT
      **** BEGIN ADD **** 12/15/99 ************************************
     C                   Z-ADD     XXDATE        AHIDT
     C                   Z-ADD     DSTIME        AHITM
     C                   MOVEL(P)  Q1PGM         AHIPGM
      **** END   ADD **** 12/15/99 ************************************
     C***  Begin Add  ***  10/10/03  **************************************
     C                   Z-ADD     AHRT#         SVRT#
     C***  End   Add  ***  10/10/03  **************************************
     C*
B11  C     AHAGRC        IFNE      0
     C*
     C***  BEGIN ADD  ***  10/21/99  *********************************
B12  C     AHAGT#        IFNE      AHAGRC
     C                   EXSR      QUOTE
E12  C                   ENDIF                                                  AHAGT#<>AHAGRC
     C***  END   ADD  ***  10/21/99  *********************************
     C*
     C                   Z-ADD     AHAGRC        AHAGT#
     C*** BEGIN ADD *** 05/27/09 **************************************
     C                   MOVEL     AHCRF1        AHCAF1
     C                   Z-ADD     XXDATE        AHEFDT
     C                   Z-ADD     AHPRRC        AHPROD
     C                   Z-ADD     AHRTRC        AHRT#
     C***  END   ADD  ***  10/21/99  *********************************
     C*
     C*** END CHG *****  11/20/97  ************************************
     C***  Begin Add  ***  12/18/09  **************************************
B12  C                   SELECT
S01  C     XNWPFD        WHENLT    XXDATE
     C                   Z-ADD     XNWPFD        AHEFDT
S02  C                   OTHER
     C                   Z-ADD     XXDATE        AHEFDT
E12  C                   ENDSL
     C***  End   Add  ***  12/18/09  **************************************
     C*
E11  C                   ENDIF                                                  AHAGRC IFNE
     C*
     C*
     C                   Z-ADD     0             AHAGRC
     C*** BEGIN ADD *** 05/27/09 **************************************
     C                   MOVEL     *BLANKS       AHCRF1
     C*** END   ADD *** 05/27/09 **************************************
     C***  BEGIN ADD  ***  10/21/99  *********************************
     C                   CLEAR                   AHPRRC
     C                   CLEAR                   AHRTRC
     C*
B11  C     AH2ARC        IFNE      *ZERO
     C                   Z-ADD     AH2ARC        AHSAGT
     C*** BEGIN ADD *** 05/27/09 **************************************
     C                   MOVEL     AHCRF2        AHCAF2
     C*** END   ADD *** 05/27/09 **************************************
     C                   Z-ADD     AH2PRC        AHSPRD
     C                   Z-ADD     AH2RRC        AHSRT#
E11  C                   ENDIF                                                  AH2ARC<>*ZERO
     C*
     C                   CLEAR                   AH2ARC
     C*** BEGIN ADD *** 05/27/09 **************************************
     C                   CLEAR                   AHCRF2
     C*** END   ADD *** 05/27/09 **************************************
     C                   CLEAR                   AH2PRC
     C                   CLEAR                   AH2RRC
     C***  END   ADD  ***  10/21/99  *********************************
     C***  BEG   ADD  ***  01/22/10  *********************************
B11  C     AH3ARC        IFNE      *ZERO
     C                   Z-ADD     AH3ARC        AH3AGT
     C                   Z-ADD     AH3PRC        AH3PRD
     C                   Z-ADD     AH3RRC        AH3RT#
E11  C                   ENDIF                                                  AH3ARC<>*ZERO
     C*
     C                   CLEAR                   AH3ARC
     C                   CLEAR                   AH3PRC
     C                   CLEAR                   AH3RRC
     C***  END   ADD  ***  01/22/10  *********************************
     C*
     C                   Z-ADD     AHRT#         XXRATE
     C     RXKY          CHAIN     PRX01                              60
B11  C     *IN60         IFEQ      *OFF
     C                   Z-ADD     RXNEWR        AHRT#
E11  C                   ENDIF                                                  *IN60 IFEQ
     C*
     C***  Begin Add  ***  10/10/03  *********************************
B11  C     XXRTOQ        IFEQ      'Y'
     C*
     C                   MOVE      'Q'           XXSTAT
     C*
     C***  BEGIN CHG  ***  10/15/19  *********************************
     C*****QTKY          CHAIN     PQT01                              60
     C     QTKY          CHAIN(N)  PQT01                              60
     C***  END CHG  *****  10/15/19  *********************************
     C*
B12  C     *IN60         IFEQ      *ON
     C                   EXSR      LOADQT
     C                   EXSR      WRITEQ
E12  C                   ENDIF                                                  *IN60=*ON
     C*
E11  C                   ENDIF
     C*
     C                   CLEAR                   AADRT#
     C                   Z-ADD     AHAGT#        XXAGT#
     C*
     C     AAKY          CHAIN     PAA01                              60
     C*
     C                   Z-ADD     AHRT#         XXRATE
     C*
     C     ARKY1         SETGT     PAR01
B11  C     *IN60         DOUEQ     *ON
     C     ARSTAT        OREQ      'A'
     C     ARKY          READPE    PAR01                                  60
E11  C                   ENDDO
     C*
B11  C     *IN60         IFEQ      *ON
     C                   EXSR      LDINF6
     C                   EXSR      LDINF7
     C                   Z-ADD     AADRT#        AHRT#
     C                   Z-ADD     6             INFO#
     C                   EXSR      SPLIT
     C                   Z-ADD     7             INFO#
     C                   EXSR      SPLIT
     C                   Z-ADD     AADRT#        AHRT#
E11  C                   ENDIF
      *
B11  C     AHRT#         IFNE      AADRT#
     C     EXRAFL        ANDEQ     'Y'
     C     AADRT#        ANDNE     *ZERO
     C                   EXSR      LDINF8
     C                   EXSR      LDINF9
     C                   Z-ADD     8             INFO#
     C                   EXSR      SPLIT
     C                   Z-ADD     9             INFO#
     C                   EXSR      SPLIT
     C                   Z-ADD     AADRT#        AHRT#
E11  C                   ENDIF
      *
      *  MAKE SURE NEW RECORD WAS NOT YET WRITTEN
      *
     C                   MOVE      AHEFDT        XXEFDT
     C                   MOVE      AHAGT#        XXAGT#
      *
B11  C     XXRTOQ        IFEQ      'Y'
     C*
     C     AHKYQ         CHAIN     PAH01Q                             60
     C*
B12  C     *IN60         IFEQ      *ON
     C                   WRITE     PAH01Q
E12  C                   ENDIF                                                   *IN60 IFEQ
     C*
X11  C                   ELSE
     C*
     C     AHKY1         CHAIN     PAH02                              60
      *
B12  C     *IN60         IFEQ      *ON
     C                   WRITE     PAH02
E12  C                   ENDIF                                                   *IN60 IFEQ
     C*
E11  C                   ENDIF
      *
      *  LOOP AGAIN IF A VALID CURRENT RECORD IS NOT ROLLED
      *
B11  C     AHEFDT        IFLE      XXSVPF
     C                   MOVE      *ON           *IN51
E11  C                   ENDIF                                                  AHEFDT IFLE
E10  C                   ENDIF                                                  *IN51 IFEQ
E09  C                   ENDDO                                                  *IN51 DOUEQ
      *
      *  WRITE TO OR UPDATE BILL TRANSACTION FILE
      *
     C     BLKY          CHAIN     PBL01                              60
     C                   MOVE      XXNFDT        BLPFDT
     C                   MOVE      'R'           BLRFLG
     C                   Z-ADD     *ZERO         BLAGT#
     C*
     C     AHKY2         SETGT     PAH01
     C                   READP     PAH01                                  50
B09  C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
E09  C                   ENDDO                                                  *IN50 == 0
     C*
B09  C     *IN50         IFEQ      '0'
     C     XXCO#         ANDEQ     AHCO#
     C     XXFND         ANDEQ     AHFND
     C     XXEMP#        ANDEQ     AHEMP#
     C     XXDIV         ANDEQ     AHDIV
     C     XXFYR         ANDEQ     AHFYR
     C                   MOVEL     AHAGT#        BLAGT#
E09  C                   ENDIF                                                  *IN50 == 0
B09  C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           BLFLG1
     C                   MOVE      'Y'           BLFLG3
     C*
B10  C     XXRTOQ        IFNE      'Y'
     C                   UPDATE    PBL01
E10  C                   ENDIF
      *
X09  C                   ELSE
      *
     C                   MOVE      XXCO#         BLCO#
     C                   MOVE      XXFND         BLFND
     C                   MOVE      XXFYR         BLFYR
     C                   MOVE      XXEMP#        BLEMP#
     C                   MOVE      XXDIV         BLDIV
     C                   MOVE      'Y'           BLFLG1
     C                   MOVE      *BLANKS       BLFLG2
     C                   MOVE      'Y'           BLFLG3
     C                   MOVE      *BLANKS       BLSTAT
     C                   MOVE      Q1USER        BLUSER
     C                   MOVE      Q1PGM         BLPGM
     C                   MOVE      DSTIME        BLTIME
     C                   MOVE      XXDATE        BLDT
      *
B10  C     XXRTOQ        IFEQ      'Y'
     C                   MOVE      *BLANK        BLFLG1
     C                   MOVE      *BLANK        BLFLG3
     C                   WRITE     PBL01Q
X10  C                   ELSE
     C                   WRITE     PBL01
E10  C                   ENDIF
      *** END CHG ***** 01/20/09 ******************************************
     C*
E09  C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  09/15/00  ************************************
     C*
     C* IF NOT PRESENT IN THE EMPLOYER DETAIL
     C*
B09  C     *IN41         IFEQ      *OFF
     C*
     C* IF HEAD1 NOT ALREADY PRINTED, PRINT HEAD.
     C*
B10  C     *IN36         IFEQ      *OFF
     C*
     C*
     C  N33
     CANN34              MOVE      *ON           *IN36
E10  C                   ENDIF
     C*
     C                   EXSR      CHEKOA
E09  C                   ENDIF
E07  C                   ENDIF                                                  *in51 = *off
     C*
E07  C                   ENDIF                                                  FUND DETAIL
E06  C                   ENDIF                                                  *IN41 IFEQ
      *
      /free
       //*** beg chg ***** 01/25/22 ***********
       //Reade (xxCo#:xxFnd:xxEmp#)pem01;
          Select;
          When isAddingDiv;
             Reade (xxCo#:xxFnd:xxEmp#:intRnwDiv) pem01;
          Other;
             Reade (xxCo#:xxFnd:xxEmp#) pem01;
          EndSl;
       //*** end chg ***** 01/25/22 ***********
       Enddo;
      /end-free
E03  C                   ENDIF                                                  ELEMP# IFNE
E02  C                   ENDIF                                                  *IN25 IFEQ
      /free
       Select;
       //*** beg add ***** 01/25/22 *********************
       When isAddingDiv;
          Reade (xxCo#:xxFnd:xxFtDt:xxEmp#:intRnwDiv) pel02;
       //*** end add ***** 01/25/22 *********************
       When rnwSingEmpFlg = *on;
          Reade (xxCo#:xxFnd:xxFtDt:xxEmp#) pel02;
       Other;
          Reade elky3 pel02;
       Endsl;
      /end-free
E01  C                   ENDDO                                                  *IN40 DOWEQ
      *
     C     ENDIT         TAG
     C*
     C     RXKY2         CHAIN     PRX01                              60
B01  C     *IN60         DOWEQ     *OFF
     C                   DELETE    PRX01
     C     RXKY2         READE     PRX01                                  60
E01  C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* IF EMPLOYERS SUCCESSFULLY ROLLED OVER THEN PRINT TOTAL NUMBER
     C* OF SUCCESSFULLY ROLLED OVER EMPLOYERS.
     C*
B01  C     XXTOT1        IFEQ      *ZERO
     C                   EXCEPT    NOREC1
X01  C                   ELSE
     C*
B02  C     *IN36         IFEQ      *ON
     C                   EXCEPT    TOT1
E02  C                   ENDIF
     C*
E01  C                   ENDIF
     C*
B01  C     XXTOT         IFEQ      *ZERO
     C                   EXCEPT    NOREC
X01  C                   ELSE
     C*
     C                   EXCEPT    TOT
     C*
E01  C                   ENDIF
     C*
      /Free
       xxprt = '0';
       Chain orky POR01;
       If %found;
          Except Route;
       Endif;
       xxprt = '1';
       Chain orky POR01;
       If %found;
          Except Route1;
       Endif;
      /End-free
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* CHEKOA - Check QSYSPRT1 for overflow
     C*================================================================
     C     CHEKOA        BEGSR
     C*
B01  C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOA
     C                   MOVE      *OFF          DET1PR
E01  C                   ENDIF
     C*
B01  C     DET1PR        IFEQ      *OFF
     C*
     C                   MOVEL     EMNAME        XXNAME           25
     C*
     C                   EXSR      GETDTS
     C*
B02  C     INFOPR        IFEQ      *OFF
     C                   EXCEPT    EXTRA1
E02  C                   ENDIF
     C*
     C                   EXCEPT    DET1
     C*
     C                   ADD       1             XXTOT1
     C                   MOVE      *ON           DET1PR
     C                   MOVE      *OFF          INFOPR
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR                                                  CHEKOA
     C*
     C*================================================================
     C* CHEKOF - Check QSYSPRT for overflow
     C*================================================================
     C     CHEKOF        BEGSR
     C*
B01  C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
E01  C                   ENDIF
     C*
     C                   MOVEL     EMNAME        XXNAME           25
     C*
     C                   EXSR      GETDTS
     C*
     C                   EXCEPT    DET
     C***** BEG ADD 02/21/11 ******************************************
B01  C     *IN75         IFEQ      *ON
     C                   EXCEPT    DETMSG
     C                   MOVE      *OFF          *IN75
E01  C                   ENDIF
     C***** END ADD 02/21/11 ******************************************
     C*
     C                   ADD       1             XXTOT
     C*
     C                   ENDSR                                                  CHEKOF
     C*
     C*================================================================
     C* GETDTS - Get the dates needed for printing
     C*================================================================
     C*
     C     GETDTS        BEGSR
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFFD            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEXPD            8 0
     C*
     C                   Z-ADD     XXNFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNEFD            8 0
     C*
     C                   Z-ADD     XXNTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNEXD            8 0
     C*
     C                   ENDSR                                                  GETDTS
     C*
     C*================================================================
     C* LDINF1 - Load the information to INFO,1
     C*================================================================
     C*
     C     LDINF1        BEGSR
     C*
     C                   MOVE(P)   '0'           X3CLAS            5
     C                   MOVEL     EPCLAS        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3CLAS
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(1)       INF
     C                   MOVEA     X3CLAS        INF(12)
     C                   MOVEA     INF           INFO(1)
     C*
     C                   ENDSR                                                  LDINF1
     C*
     C*================================================================
     C* LDINF2 - Load the information to INFO,2
     C*================================================================
     C*
     C     LDINF2        BEGSR
     C*
     C                   MOVE(P)   '0'           X3CLAS            5
     C                   MOVEL     SVCLAS        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3CLAS
E01  C                   ENDIF
     C*
     C                   MOVE(P)   '0'           X3XFER            5
     C                   MOVEL     SVXFER        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3XFER
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(2)       INF
     C                   MOVEA     X3CLAS        INF(12)
     C                   MOVEA     X3XFER        INF(47)
     C                   MOVEA     INF           INFO(2)
     C*
     C                   ENDSR                                                  LDINF2
     C*
     C*================================================================
     C* LDINF3 - Load the information to INFO,3
     C*================================================================
     C*
     C     LDINF3        BEGSR
     C*
     C                   MOVE(P)   '0'           X3CLAS            5
      *** BEG CHG ***** 05/16/05 **************************************
     C*****************    MOVELEPCLAS    XXNUM
     C                   MOVEL     SVCLAS        XXNUM
      *** END CHG ***** 05/16/05 **************************************
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3CLAS
E01  C                   ENDIF
     C*
     C                   MOVE(P)   '0'           X3XFER            5
      ***** BEG CHG ******* 12/06/13 **********************************
     C*************      MOVEL     PRXFER        XXNUM
     C                   MOVEL     PRCLAS        XXNUM
      ***** END CHG ******* 12/06/13 **********************************
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3XFER
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(3)       INF
     C                   MOVEA     X3CLAS        INF(12)
     C                   MOVEA     X3XFER        INF(50)
     C                   MOVEA     INF           INFO(3)
     C*
     C                   ENDSR                                                  LDINF3
     C*
     C*================================================================
     C* LDINF4 - Load the information to INFO,4
     C*================================================================
     C*
     C     LDINF4        BEGSR
     C*
     C                   ENDSR                                                  LDINF4
     C*
     C*================================================================
     C* LDINF5 - Load the information to INFO,5
     C*================================================================
     C*
     C     LDINF5        BEGSR
     C*
     C                   MOVE(P)   '0'           X3CLAS            5
      *** BEG CHG ***** 05/16/05 **************************************
     C*****************    MOVELEPCLAS    XXNUM
     C                   MOVEL     SVCLAS        XXNUM
      *** END CHG ***** 05/16/05 **************************************
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3CLAS
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(5)       INF
     C                   MOVEA     X3CLAS        INF(12)
     C                   MOVEA     INF           INFO(5)
     C*
     C                   ENDSR                                                  LDINF5
     C*
     C*================================================================
     C* LDINF6 - Load the information to INFO,6
     C*================================================================
     C*
     C     LDINF6        BEGSR
     C*
     C                   MOVE(P)   '0'           X3RATE            3
     C                   MOVEL     AHRT#         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3RATE
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(6)       INF
     C                   MOVEA     X3RATE        INF(12)
     C                   MOVEA     INF           INFO(6)
     C*
     C                   ENDSR                                                  LDINF6
     C*
     C*================================================================
     C* LDINF7 - Load the information to INFO,7
     C*================================================================
     C*
     C     LDINF7        BEGSR
     C*
     C                   MOVE(P)   '0'           X3RATE            3
     C                   MOVEL     AADRT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3RATE
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(7)       INF
     C                   MOVEA     X3RATE        INF(2)
     C                   MOVEA     INF           INFO(7)
     C*
     C                   ENDSR                                                  LDINF7
     C*
     C*================================================================
     C* LDINF8 - Load the information to INFO,8
     C*================================================================
     C*
     C     LDINF8        BEGSR
     C*
     C                   MOVE(P)   '0'           X3RATE            3
     C                   MOVEL     AHRT#         XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3RATE
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(8)       INF
     C                   MOVEA     X3RATE        INF(24)
     C                   MOVEA     INF           INFO(8)
     C*
     C                   MOVE(P)   '0'           X3RATE            3
     C                   MOVEL     AADRT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3RATE
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(8)       INF
     C                   MOVEA     X3RATE        INF(60)
     C                   MOVEA     INF           INFO(8)
     C*
     C                   ENDSR                                                  LDINF8
     C*
     C*================================================================
     C* LDINF9 - Load the information to INFO,9
     C*================================================================
     C*
     C     LDINF9        BEGSR
     C*
     C                   MOVE(P)   '0'           X3AGT#            5
     C                   MOVEL     XXAGT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3AGT#
E01  C                   ENDIF
     C*
     C                   MOVEA     INFO(9)       INF
     C                   MOVEA     X3AGT#        INF(12)
     C                   MOVEA     INF           INFO(9)
     C*
     C                   ENDSR                                                  LDINF9
     C*
     C*================================================================
     C* LDMSG3 - Load the information to MSG,3
     C*================================================================
     C*
     C     LDMSG3        BEGSR
     C*
     C                   MOVEL(P)  '  /  /  '    X3DATE           10
     C*
     C                   Z-ADD     CX(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C*
B01  C     XXNUM         IFNE      *BLANK
     C                   MOVEL     XXNUM         X3DATE
E01  C                   ENDIF
     C*
     C                   MOVEA     MSG(3)        INF
     C                   MOVEA     X3DATE        INF(56)
     C                   MOVEA     INF           MSG(3)
     C*
     C                   ENDSR                                                  LDMSG3
     C***  End   Add  ***  10/10/03  *********************************
     C*
     C***  BEGIN ADD  ***  10/21/99  *********************************
     C*===============================================================
     C* LOADQT - Load Quote Tracking fields and write initial record
     C*===============================================================
     C*
     C     LOADQT        BEGSR
     C*
     C                   CLEAR                   PQT01
     C*
     C                   Z-ADD     AHCO#         QTCO#
     C                   Z-ADD     AHFND         QTFND
     C                   Z-ADD     AHEMP#        QTEMP#
     C                   Z-ADD     AHDIV         QTDIV
     C                   Z-ADD     AHAGT#        QTAGT#
     C                   Z-ADD     EMIDT         QTORDT
     C                   Z-ADD     EMCHAP        QTCHAP
     C                   MOVEL     EMNAME        QTNAME
     C*
B01  C                   SELECT
S01  C     XXRTOQ        WHENEQ    'Y'
     C                   MOVE      'p'           QTTYPE
     C                   Z-ADD     XNWPFD        QTEFDT
S02  C                   OTHER
     C                   MOVE      'W'           QTTYPE
     C                   Z-ADD     EMEFD1        QTEFDT
E01  C                   ENDSL
     C*
     C                   MOVEL     EMIDT         QTNBDT
     C                   Z-ADD     *ZEROS        QTPREM
     C                   MOVE      *BLANKS       QTCARR
     C                   MOVE      *BLANKS       QTCOMM
     C*
B01  C                   SELECT
S01  C     XXRTOQ        WHENEQ    'Y'
     C                   MOVE      'Q'           QTSTAT
S02  C                   OTHER
     C                   MOVE      'W'           QTSTAT
E01  C                   ENDSL
     C*
     C                   ENDSR                                                  LOADQT
     C*
     C*===============================================================
     C* NEXTEL - Get the next WDELP record
     C*===============================================================
     C     NEXTEL        BEGSR
     C     ELKY3         READE     PEL02                                  40
     C                   ENDSR                                                  NEXTEL
     C*===============================================================
     C* QUOTE  - Create transfer records for the Quote Tracking file
     C*===============================================================
     C     QUOTE         BEGSR
     C*
     C                   MOVE      'W'           XXSTAT
     C*
     C***  BEGIN CHG  ***  10/15/19  *********************************
     C*****QTKY          CHAIN     PQT01                              60
     C     QTKY          CHAIN(N)  PQT01                              60
     C***  END CHG  *****  10/15/19  *********************************
     C*
B01  C     *IN60         IFEQ      *ON
     C                   EXSR      LOADQT
     C                   EXSR      WRITEQ
E01  C                   ENDIF                                                  *IN60=*ON
     C*
B01  C                   SELECT
S01  C     QTTYPE        WHENEQ    'C'
     C     QTTYPE        OREQ      'R'
     C                   MOVE      QTTYPE        QTXFER
S02  C                   OTHER
     C                   MOVE      *BLANKS       QTXFER
E01  C                   ENDSL
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   MOVE      'X'           QTTYPE
     C*
     C                   EXSR      WRITEQ
     C*
     C                   Z-ADD     AHAGRC        QTAGT#
     C                   MOVE      'T'           QTTYPE
     C*
     C                   EXSR      WRITEQ
     C*
     C***  BEGIN ADD  ***  11/01/00  *********************************
     C                   CLEAR                   QTXFER
     C***  END   ADD  ***  11/01/00  *********************************
     C*
     C                   ENDSR                                                  QUOTE
     C***  END   ADD  ***  10/21/99  *********************************
     C*
     C***  Begin Add  ***  10/10/03  *********************************
     C*================================================================
     C* SPLIT  - Split the INFO record to print on report
     C*================================================================
     C     SPLIT         BEGSR
     C*
     C     *LIKE         DEFINE    INFO          XXSTR
     C*
     C                   Z-ADD     80            INFOSZ            3 0
     C                   Z-ADD     40            SPLTSZ            3 0
     C                   MOVEL(P)  INFO(INFO#)   XXSTR
     C*
     C                   Z-ADD     1             S                 3 0
     C*
B01  C     XXINFO        DOUEQ     *BLANK
     C*
     C     S             ADD       SPLTSZ        F                 3 0
     C                   SUB       1             F
     C*
B02  C     F             IFGE      INFOSZ
     C                   Z-ADD     INFOSZ        F
     C     F             ADD       1             F1
X02  C                   ELSE
     C     F             ADD       1             T                 3 0
     C     ' '           CHECK     XXSTR:F       F1                3 0
     C     ' '           CHECK     XXSTR:T       T1                3 0
E02  C                   ENDIF
     C*
B02  C     F             IFEQ      F1
     C     T             ANDEQ     T1
B03  C     F             DOUNE     F1
     C                   SUB       1             F
     C     ' '           CHECKR    XXSTR:F       F1
E03  C                   ENDDO
X02  C                   ELSE
     C                   ADD       1             F
E02  C                   ENDIF
     C*
B02  C     F1            IFEQ      0
     C     F             ADD       1             F1
E02  C                   ENDIF
     C*
     C     F             SUB       S             LEN               3 0
     C     LEN           SUBST(P)  XXSTR:S       XXINFO           40
     C*
B02  C     LEN           IFEQ      SPLTSZ
     C     F1            ANDEQ     F
     C                   SUB       2             F1
E02  C                   ENDIF
     C*
B02  C     XXINFO        IFNE      *BLANK
     C                   EXSR      CHEKOA
     C                   EXCEPT    DET1IN
     C                   MOVE      *ON           INFOPR
E02  C                   ENDIF
     C*
     C     F1            ADD       1             S
     C*
B02  C                   SELECT
S01  C     S             WHENLE    INFOSZ
     C                   SUBST(P)  XXSTR:S       XXINFO
S02  C                   OTHER
     C                   CLEAR                   XXINFO
E02  C                   ENDSL
     C*
E01  C                   ENDDO
     C*
     C                   ENDSR                                                  SPLIT
     C*
      *****************************************************************
     C     SRANNL        BEGSR
     C                   Z-ADD     ELANDT        XXANDT
     C                   ADD       1             XXANYR
B01  C                   SELECT
S01  C     KKST          WHENEQ    'OK'
     C     ELPTDT        ANDLT     1110901
     C* Check opt out flag for OK 1100901 rate change
     C                   Z-ADD     1100901       XXSPDT            7 0
     C                   EXSR      SRAPRT
S01  C     KKST          WHENEQ    'TN'
     C                   Z-ADD     1140701       XXSPDT            7 0
     C                   EXSR      SRAPRT
S01  C     KKST          WHENEQ    'OK'
     C     ELPTDT        ANDLT     1150101
     C* Check opt out flag for OK 1140201 rate change
     C                   Z-ADD     1140201       XXSPDT            7 0
     C                   EXSR      SRAPRT
     C*** end add ***** 07/16/14 ***************************************
S02  C                   OTHER
     C                   MOVE      'N'           xxoptf
E01  C                   ENDSL
     C*** END   ADD ****** 10/18/10 ************************************
      *
      *  IF PAYROLLS WERE NOT ENTERED FOR THE ENTIRE POLICY
      *  PERIOD, ADJUST XXAMT TO COVER THE ENTIRE PERIOD.
      *  THIS WILL NOT NEED TO BE DONE FOR SELF-REPORTING,
      *  BECAUSE MISSING MONTHS ARE ESTIMATED.
      *
B01  C     EPEFDT        IFGT      XXPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXTYPE        ANDNE     'S'
     C     EPEFDT        ANDNE     1030401
     C     EPEFDT        ANDNE     1031001
     C     EPEFDT        ANDNE     1090701
     C     KKST          ANDEQ     'FL'
     C     CLTID         ANDEQ     '139'
     C     EPEFDT        ORGT      XXPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXTYPE        ANDNE     'S'
     C     EPEFDT        ANDNE     1030401
     C     EPEFDT        ANDNE     1031001
     C     KKST          ANDEQ     'FL'
     C     CLTID         ANDNE     '139'
     C     EPEFDT        ORGT      XXPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXTYPE        ANDNE     'S'
     C     EPEFDT        ANDNE     1081001
     C     KKST          ANDEQ     'DE'
     C     EXDE08        ANDNE     'Y'
     C     EPEFDT        ORGT      XXPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXTYPE        ANDNE     'S'
     C     EPEFDT        ANDNE     1100101
     C     KKST          ANDEQ     'AZ'
     C     EPEFDT        ORGT      XXPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXTYPE        ANDNE     'S'
     C     EPEFDT        ANDNE     1100901
     C     EPEFDT        ANDNE     1140201
     C     KKST          ANDEQ     'OK'
     C     xxoptf        ANDEQ     'N'
     C     EPEFDT        ORGT      XXPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXTYPE        ANDNE     'S'
     C     EPEFDT        ANDNE     1140701
     C     KKST          ANDEQ     'TN'
     C     XXOPTF        ANDEQ     'N'
     C     EPEFDT        ORGT      XXPFDT
     C     EPEFDT        ANDNE     XXANDT
     C     XXTYPE        ANDNE     'S'
     C     KKST          ANDNE     'FL'
     C     KKST          ANDNE     'DE'
     C     EXDE08        ANDEQ     'Y'
     C     KKST          ANDNE     'AZ'
     C     KKST          ANDNE     'OK'
     C     KKST          ANDNE     'TN'
      *
      *  CALCULATE THE NUMBER OF DAYS IN THE POLICY (PDAYS)
      *
B02  C     PDAYS         IFEQ      *ZEROS
      *
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     XXPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZEROS        QQDAYS
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      '1'           QQTERM
      *
     C                   CALL      'SRDDIF8'     PRMDIF
      *
      *  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
      *
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
      *
B03  C     XXLPDT        IFLT      X1FROM
     C                   Z-ADD     X1TO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
E03  C                   ENDIF                                                  XXLPDT IFLT
      *
B03  C     XXLPDT        IFLT      X1FROM
     C     XXLPDT        ORGT      X1TO
     C                   Z-ADD     *ZEROS        XXLPDT
E03  C                   ENDIF                                                  XXLPDT IFLT
      *
B03  C     XXLPDT        IFNE      *ZEROS
      *
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      '1'           QQTERM
      *
     C                   Z-ADD     QQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQFROM
      *
B04  C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
E04  C                   ENDIF                                                  QQERR IFNE
E03  C                   ENDIF                                                  XXLPDT IFNE
     C                   Z-ADD     QQDAYS        PDAYS
E02  C                   ENDIF                                                  PDAYS IFEQ
      *
      *  CALCULATE THE NUMBER OF DAYS RATED (RDAYS)
      *
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZEROS        QQDAYS
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      '1'           QQTERM
      *
     C                   CALL      'SRDDIF8'     PRMDIF
      *
      *  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
      *
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
      *
B02  C     XXLPDT        IFLT      X1FROM
     C                   Z-ADD     X1TO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
E02  C                   ENDIF                                                  XXLPDT IFLT
      *
B02  C     XXLPDT        IFLT      X1FROM
     C     XXLPDT        ORGT      X1TO
     C                   Z-ADD     *ZEROS        XXLPDT
E02  C                   ENDIF                                                  XXLPDT IFLT
      *
B02  C     XXLPDT        IFNE      *ZEROS
      *
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      *BLANKS       QQERR
     C                   MOVE      '1'           QQTERM
      *
     C                   Z-ADD     QQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         QQFROM
      *
B03  C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
E03  C                   ENDIF                                                  QQERR IFNE
E02  C                   ENDIF                                                  XXLPDT IFNE
      *
     C                   Z-ADD     QQDAYS        RDAYS
      *
      *  ADJUST THE PAYROLL AMOUNT
      *
B02  C     RDAYS         IFNE      *ZEROS
     C     PDAYS         DIV       RDAYS         XXFACT
     C     XXAMT1        MULT      XXFACT        XXWORK
     C                   Z-ADD     XXWORK        XXAMT1
      *
B03  C     XXTYPE        IFEQ      'E'
      *
B04  C     XXPROR        IFNE      *ZEROS
     C     XXAMT1        DIV(H)    XXPROR        XXWORK           11 0
     C                   Z-ADD     XXWORK        XXAMT1
E04  C                   ENDIF                                                  XXPROR IFNE
E03  C                   ENDIF                                                  XXTYPE IFEQ
E02  C                   ENDIF                                                  RDAYS IFNE
E01  C                   ENDIF                                                  EPEFDT IFGT
      *
     C                   ENDSR                                                  SRANNL
     C*===============================================================
     C* Process HCN Program information
     C*===============================================================
     C     SrHCNPgm      begsr
     C*
     C     keyWMHCL      klist
     C                   kfld                    HCCO#
     C                   kfld                    HCFND
     C                   kfld                    HCST
     C*
     C     key1WMHCL     klist
     C                   kfld                    HCCO#
     C                   kfld                    HCFND
     C                   kfld                    HCST
     C                   kfld                    kkEfDt
     C*
     C     keyWDHCL3     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    XXXYR
     C*
     C     keyWDHCL3     setgt     phc03
     C     keyWDHCL3     readpe    phc03
B01  C                   if        not %eof
     C                              and HCOUT = *zero
     C                   eval      kkEfDt = HCIN
     C                   clear                   HCRENW
     C                   clear                   HCEXDT
     C     key1WMHCL     setgt     mhc00
     C     keyWMHCL      readpe    mhc00
B02  C                   if        HCRENW = 'Y'
     C                              and (HCEXDT = *zero
     C                                   or HCEXDT > ELPFDT)
     C                   time                    dsDtTm
     C                   eval      HCFYR  = ELFYR
     C                   eval      HCIN   = ELPFDT
     C                   eval      HCINCP = 'Y'
     C                   eval      HCPGM  = q1Pgm
     C                   eval      HCUSER = q1User
     C                   eval      HCDT   = xxDate
     C                   eval      HCTIME = dsTime
     C                   eval      HCIPGM = HCPGM
     C                   eval      HCIUSR = HCUSER
     C                   eval      HCIDT  = HCDT
     C                   eval      HCITM  = HCTIME
B03  C                   select
S01  C                   when      xxRToQ = 'Y'
     C                   write     phc00q
S02  C                   other
     C                   write     phc03
E03  C                   endsl
E02  C                   endif
E01  C                   endif
     C*
     C                   endsr
     C***  End   Add  ***  11/10/10  **************************************
      *
      ***** BEG ADD ******* 06/22/04 **********************************
      *===============================================================
      * SRRNTK - Create Renewal Tracking Record
      *===============================================================
      *
     C     SRRNTK        BEGSR
      *
     C                   MOVEL     'WC'          XSKEY             2
     C     XSKEY         CHAIN     SWC01                              60
      *
B01  C     *IN60         IFEQ      *OFF
     C     WCFL26        ANDEQ     'Y'
      *
     C     RTKY          CHAIN     PRT01                              60
      *
B02  C     *IN60         IFEQ      *OFF
     C                   DELETE    PRT01
E02  C                   ENDIF
      ***** BEG ADD ***** 11/15/13 ************************************
     C                   Z-ADD     *ZEROS        RTUWDT
     C                   Z-ADD     *ZEROS        RTQADT
     C                   Z-ADD     *ZEROS        RTAADT
     C                   Z-ADD     *ZEROS        RTPGDT
     C                   Z-ADD     *ZEROS        RTPOLD
     C                   Z-ADD     *ZEROS        RTPAND
      ***** END ADD ***** 11/15/13 ************************************
      *
     C                   Z-ADD     ELCO#         RTCO#
     C                   Z-ADD     ELFND         RTFND
     C                   Z-ADD     ELEMP#        RTEMP#
     C                   Z-ADD     ELFYR         RTFYR
      *
     C                   MOVEL     Q1USER        RTIUSR
     C                   MOVEL     Q1USER        RTUSER
     C                   MOVEL     Q1PGM         RTIPGM
     C                   MOVEL     Q1PGM         RTPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RTIDT
     C                   MOVEL     SRDTO         RTDT
     C                   MOVEL     DSTIME        RTITM
     C                   MOVEL     DSTIME        RTTIME
      *
     C                   Z-ADD     RTDT          RTYCDT
     C                   WRITE     PRT01
E01  C                   ENDIF                                                  WCFL26
      *
     C                   ENDSR                                                  SRRNTK
      *****************************************************************
      *
      * SUBROUTINE TO WRITE EMPLOYER PAYROLL RECORD FOR NEW FUND YEAR
      *
      *****************************************************************
     C     SRWRIT        BEGSR
     C***  Begin Add  ***  10/10/03  *********************************
     C*
     C     *LIKE         DEFINE    *IN60         WRITEP
     C*
     C                   MOVE      *ON           WRITEP
     C*
     C*  Check Payroll Class Master
     C*
     C                   Z-ADD     SVCLAS        X2CLAS
     C                   Z-ADD     0             X2EFDT
      *
      * USE LATEST EFF DATE OF PAYROLL TO HIT PAYROLL TABLE WITH
      *
     C                   MOVE      XXPRDT        X2EFDT
     C*
     C     PRKY1         SETGT     PPR01
     C     PRKY          READPE    PPR01                                  60
     C*
     C*  Class Code not active
     C*
B01  C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDNE     'A'
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDNE     *ZERO
     C     PREXDT        ANDLE     XXSVPF
     C     PRXFER        ANDEQ     *ZEROS
     C     XXSVAN        ANDEQ     *ZEROS
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDNE     *ZERO
     C     PREXDT        ANDLE     XXSVAN
     C     PRXFER        ANDEQ     *ZEROS
     C     XXSVAN        ANDNE     *ZEROS
     C     *IN60         OREQ      *ON
     C                   EXSR      LDINF5
     C                   Z-ADD     5             INFO#
     C                   EXSR      SPLIT
     C                   MOVE      *OFF          WRITEP
     C                   CLEAR                   PRXFER
E01  C                   ENDIF
     C*
     C*  Transfer to new class?
     C*
B01  C     PREXDT        IFLE      XXSVPF
     C     PREXDT        ANDNE     *ZERO
     C     PRXFER        ANDNE     *ZEROS
     C     XXSVAN        ANDEQ     *ZEROS
     C     PREXDT        ORLE      XXSVAN
     C     PREXDT        ANDNE     *ZERO
     C     PRXFER        ANDNE     *ZEROS
     C     XXSVAN        ANDNE     *ZEROS
     C*
     C                   Z-ADD     PRXFER        SVXFER
     C                   Z-ADD     PRXFER        X2CLAS
     C     EPKYL         CHAIN     PEP01                              63
     C                   Z-ADD     0             X2EFDT
     C                   MOVE      XXPRDT        X2EFDT
     C*
     C     PRKY1         SETGT     PPR01
     C     PRKY          READPE    PPR01                                  60
     C*
     C*  New class not active
     C*
B02  C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDNE     'A'
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDLE     XXSVPF
     C     PREXDT        ANDNE     *ZERO
     C     XXSVAN        ANDEQ     *ZEROS
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDLE     XXSVAN
     C     PREXDT        ANDNE     *ZERO
     C     XXSVAN        ANDNE     *ZEROS
     C     *IN60         OREQ      *ON
     C                   EXSR      LDINF3
     C                   EXSR      LDINF4
     C                   Z-ADD     3             INFO#
     C                   EXSR      SPLIT
     C                   Z-ADD     4             INFO#
     C                   EXSR      SPLIT
     C                   MOVE      *OFF          WRITEP
E02  C                   ENDIF
     C*
     C*  New class active
     C*
B02  C     WRITEP        IFEQ      *ON
     C                   EXSR      LDINF2
     C                   Z-ADD     2             INFO#
     C                   EXSR      SPLIT
E02  C                   ENDIF
     C*
E01  C                   ENDIF
      *
B01  C     XXTYPE        IFEQ      'A'
B02  C     XXPROR        IFNE      *ZEROS
     C     XXAMT         DIV(H)    XXPROR        XXWORK           11 0
     C                   Z-ADD     XXWORK        XXAMT
E02  C                   ENDIF                                                  XXPROR IFNE
E01  C                   ENDIF                                                  XXTYPE IFEQ
      *
     C                   MOVE      SVCLAS        EPCLAS
     C                   MOVE      SVEXT         EPEXT
      *
B01  C     SVACNT        IFNE      *ZERO
     C                   Z-ADD     SVACNT        EPECNT
      *
X01  C                   ELSE
      *
     C                   Z-ADD     SVECNT        EPECNT
E01  C                   ENDIF
      *
     C                   Z-ADD     0             EPACNT
     C                   MOVE      SVSPEC        EPSPEC
     C*
B01  C     SVXFER        IFNE      *ZEROS
     C                   Z-ADD     PRCLAS        EPCLAS
     C                   Z-ADD     PRRT          EPARAT
E01  C                   ENDIF
     C*
     C                   MOVE      XXFYR         EPFYR
     C                   MOVE      XXSVPF        EPEFDT
     C                   MOVE      SVSC          EPSC
     C                   MOVE      'E'           EPSAFL
      *
     C                   MOVE      Q1USER        EPUSER
     C                   MOVE      Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
      **** BEGIN ADD **** 12/15/99 ************************************
     C                   Z-ADD     XXDATE        EPIDT
     C                   Z-ADD     DSTIME        EPITM
     C                   MOVEL(P)  Q1PGM         EPIPGM
      **** END   ADD **** 12/15/99 ************************************
     C*
     C                   Z-ADD     DSTIME        EPTIME
     C                   MOVE      *BLANKS       EPCADT
     C                   MOVE      *ZEROS        EPEMPR
     C                   MOVE      *ZEROS        EPAMPR
      **** BEGIN ADD **** 05/20/14 ************************************
     C                   MOVE      *ZEROS        EPENTP
     C                   MOVE      *ZEROS        EPANTP
      **** END   ADD **** 02/20/14 ************************************
      *
B01  C     ELSTAT        IFEQ      'N'
     C                   MOVE      *ZEROS        EPARAT
X01  C                   ELSE
     C                   Z-ADD     SVARAT        EPARAT
E01  C                   ENDIF                                                  ELSTAT IFEQ
      *
     C***  Begin Add  ***  03/22/06  **************************************
     C                   Z-ADD     XXCNT         EPECNT
     C***  End   Add  ***  03/22/06  **************************************
      *
     C                   MOVE      *BLANKS       EPADLD
     C                   MOVE      *BLANKS       EPAAPR
     C***  Begin Add  ***  03/22/06  **************************************
     C                   CLEAR                   EPACNT
     C***  End   Add  ***  03/22/06  **************************************
     C                   MOVE      'A'           EPSTAT
      *
      *   MULTIPLY PAYROLL BY FACTOR
      *
     C     XXAMT         MULT      XXPCT         XXWK92
      *
B01  C     XXIDFL        IFEQ      'D'
     C     XXAMT         SUB       XXWK92        EPEAPR
X01  C                   ELSE
     C     XXAMT         ADD       XXWK92        EPEAPR
E01  C                   ENDIF                                                  XXIDFL IFEQ
      *
      *   ROUND TO NEAREST DOLLAR
      *
     C                   Z-ADD(H)  EPEAPR        XXSAVE           11 0
     C                   Z-ADD     XXSAVE        EPEAPR
      *
B01  C     XXSAVE        IFLT      *ZEROS
     C                   MULT      -1            EPEAPR
E01  C                   ENDIF                                                  XXSAVE IFLT
      *
     C                   MOVE      *BLANKS       EPSAPR
      *
      ***** BEG ADD ***** 03/23/06 ***********************************
     C                   MOVE      QQTYPE        EPRLOP                         ROLLOVER OPTION
B01  C     XDTYPE        IFEQ      'D'
     C     XAPAMT        ANDGT     EPEAPR
     C                   Z-ADD     XAPAMT        EPEAPR
E01  C                   ENDIF                                                  XAPAMT> EPEAPR
     C* Now Clear The Audit Amount before next payroll type
     C                   Z-ADD     *ZEROS        XAPAMT
      ***** END ADD ***** 03/23/06 ***********************************
      *
      * WRITE PAYROLL RECORD TO NEW FYR
      *
     C***  Begin Add  ***  10/10/03  *********************************
B01  C     WRITEP        IFEQ      *ON
      *
     C*+* BEGIN ADD *+*  08/23/06  +*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
     C*
     C*  IF POLICY IS SHORT-TERM AND PRORATA FACTOR IS 1.00, ANNUALIZE
     C*  THE PAYROLL
     C*
     C                   Z-ADD     X2PFDT        XXTODT
     C                   ADD       1             XXTOYR
     C*
B02  C     X2PTDT        IFNE      XXTODT
     C     X2PROR        ANDEQ     1.00
     C     X2FFL9        ANDEQ     'Y'
     C     PRPCF         ANDNE     'O'
     C*
B03  C     P2DAYS        IFNE      *ZERO
     C     365           DIV       P2DAYS        X2FACT            9 6
X03  C                   ELSE
     C                   Z-ADD     1             X2FACT
E03  C                   ENDIF
     C*
     C     EPEAPR        MULT(H)   X2FACT        XXWK90            9 0
     C                   Z-ADD     XXWK90        EPEAPR
     C*
E02  C                   ENDIF
     C*
     C*+* END ADD *+*+*+ 08/23/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*
B02  C     XXRTOQ        IFEQ      'Y'
     C                   WRITE     PEP01Q
X02  C                   ELSE
     C***  End   Add  ***  10/10/03  *********************************
     C                   WRITE     PEP01
     C***  Begin Add  ***  10/10/03  *********************************
E02  C                   ENDIF
     C*
E01  C                   ENDIF
     C***  End   Add  ***  10/10/03  *********************************
      *
     C                   ENDSR                                                  SRWRIT
      *
      *
      *
      *****************************************************************
      *
      *   FIRST CYCLE - IF OPTION 2 SELECTED FOR SECOND CHOICE
      *   EXCEPTIONS, PRINT REPORT HEADINGS
      *
      *****************************************************************
     C     SR1ST         BEGSR
      *
     C                   MOVE      *OFF          *IN08
     C                   MOVE      *ON           *IN09
     C***  Begin Add  ***  10/10/03  **************************************
     C                   MOVE      *ON           INFOPR
     C***  End   Add  ***  10/10/03  **************************************
      *
     C     UPKY          CHAIN     PUP01                              60
      *
      * INITIALIZE FIELDS AND MOVE SELECTION CRITERIA
      *    TO ROUTING PAGE
      *
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     QQFTDT        X2FTDT            8 0
     C*
     C                   Z-ADD     X2FTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RSFTDT            6 0
     C*
     C                   MOVEL     QQTTDT        X2TTDT            8 0
     C*
     C                   Z-ADD     X2TTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RSTTDT            6 0
     C*
     C                   MOVE      *BLANKS       RSCHG
      *
B01  C     QQIDFL        IFEQ      'I'
     C                   MOVEL     CHG(1)        RSCHG
X01  C                   ELSE
B02  C     QQIDFL        IFEQ      'D'
     C                   MOVEL     CHG(2)        RSCHG
E02  C                   ENDIF                                                  QQIDFL IFEQ
E01  C                   ENDIF                                                  QQIDFL IFEQ
      *
     C     COKY          CHAIN     PCO01                              60
     C     FNKY          CHAIN(N)  PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
B01  C     *IN60         IFEQ      *OFF
B02  C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
E02  C                   ENDIF                                                  FNANAM<>BLK
E01  C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR PRINTER FILE 0
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*  Left Justify Title in Field
     C*
B01  C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
B02  C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
E02  C                   ENDIF                                                  N<=60
X01  C                   ELSE
     C                   MOVEL     TITLE         ORTITL
E01  C                   ENDIF                                                  ORTITL<>*BLANKS
     C***  End   Add  ***  10/10/03  **************************************
     C*
B01  C     *IN60         IFEQ      '0'
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
     C                   MOVE      *BLANKS       XXFORM           10
     C                   MOVEL     ORFORM        XXFORM
     C                   MOVE      *BLANKS       XXSAV             4
     C                   MOVEL     ORSAVE        XXSAV
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C***  End   Add  ***  10/10/03  **************************************
     C*
E01  C                   END
     C***  Begin Chg  ***  10/10/03  **************************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR PRINTER FILE 1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
B01  C     *IN60         IFEQ      '0'
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*  Left Justify Title in Field
     C*
B02  C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
B03  C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
E03  C                   ENDIF                                                  N<=60
X02  C                   ELSE
     C                   MOVEL     TITLE1        ORTITL
E02  C                   ENDIF                                                  ORTITL<>*BLANKS
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C                   MOVE      *BLANKS       X1TITL           60
     C                   MOVEL     ORTITL        X1TITL
     C                   MOVE      *BLANKS       X1DIS1           40
     C                   MOVEL     ORDIS1        X1DIS1
     C                   MOVE      *BLANKS       X1DIS2           40
     C                   MOVEL     ORDIS2        X1DIS2
     C                   MOVE      *BLANKS       X1DIS3           40
     C                   MOVEL     ORDIS3        X1DIS3
     C                   MOVE      *BLANKS       X1DIS4           40
     C                   MOVEL     ORDIS4        X1DIS4
     C                   MOVE      *BLANKS       X1PNAM           10
     C                   MOVEL     ORPNAM        X1PNAM
     C                   MOVE      *BLANKS       X1FIL#            1
     C                   MOVEL     ORFIL#        X1FIL#
     C                   MOVE      *BLANKS       X1COPY            2 0
     C                   MOVEL     ORCOPY        X1COPY
     C                   MOVE      *BLANKS       X1HOLD            4
     C                   MOVEL     ORHOLD        X1HOLD
     C                   MOVE      *BLANKS       X1OUTQ           10
     C                   MOVEL     OROUTQ        X1OUTQ
     C                   MOVE      *BLANKS       X1FORM           10
     C                   MOVEL     ORFORM        X1FORM
     C                   MOVE      *BLANKS       X1SAVE            4
     C                   MOVEL     ORSAVE        X1SAVE
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
     C***  End   Add  ***  10/10/03  **************************************
     C*
E01  C                   END
     C***  Begin Add  ***  10/10/03  **************************************
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        X1TITL
     C***  End   Add  ***  10/10/03  **************************************
     C*
     C***  Begin Del  ***  10/10/03  **************************************
     C**********
     C****PRINT*THE DISTRIBUTION INFORMATION
     C**********
     C**********           EXCPTROUTE
     C***  End   Del  ***  10/10/03  **************************************
      *
     C                   EXCEPT    HEAD                                                     IC.
      *
     C***** BEGIN ADD **** 06/01/99 ***********************************
     C***  Begin Del  ***  10/10/03  **************************************
     C**********           EXCPTROUTE1
     C***  End   Del  ***  10/10/03  **************************************
     C                   EXCEPT    HEAD1
     C***** END ADD ****** 06/01/99 ***********************************
     C                   ENDSR                                                  SR1ST
      *
      ***** BEG ADD ******* 04/18/06 **********************************
     **********************************************************************
     *  Get Risk State from Rate Set Code Master File
     **********************************************************************
      *
     C     RISKSR        BEGSR
      *
      ***** BEG ADD ***** 12/11/07 **************************************
      *  If state reporting flag = "Y" process here.
B01  C     QQSRDF        IFEQ      'Y'
      * Save the state reporting division
     C                   Z-ADD     *ZEROS        KKDIV
     C     ELKY8         CHAIN     PEM01                              60
B02  C     *IN60         DOWEQ     *OFF
B03  C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         KKDIV
     C     ELKY9         CHAIN     PEL01                              60
B04  C     *IN60         IFEQ      *OFF
     C                   MOVE      ELRTCD        @@SKST            3
E04  C                   ENDIF                                                  *IN60 = *OFF
      *  Get Rating State
     C     RSKY1         CHAIN     PRS01                              60
     C  N60              MOVEL(P)  RSJRST        RISKST
      *
E03  C                   ENDIF                                                  EMSTRP= 'Y'
     C     ELKY8         READE     PEM01                                  60
E02  C                   ENDDO                                                  EMKY4
X01  C                   ELSE
      ***** END ADD ***** 12/11/07 **************************************
     C     WMRSLK        CHAIN     WMRSL                              61
      *
B02  C     *IN61         IFEQ      *OFF
     C                   MOVE      RSJRST        RISKST            2
X02  C                   ELSE
     C                   MOVE      *BLANKS       RISKST
E02  C                   ENDIF
      ***** BEG ADD ***** 12/11/07 **************************************
E01  C                   ENDIF                                                  QQSRDF = "Y"
      ***** END ADD ***** 12/11/07 **************************************
      *
     C                   ENDSR
      ***** END ADD ******* 04/18/06 **********************************
      *
      ***** BEG ADD ******* 09/22/99 **********************************
      *****************************************************************
      * SRRNW - Get Auto Renewal flags
      *****************************************************************
     C     SRRNW         BEGSR
      *
     C                   Z-ADD     ELCO#         KKCO#
     C                   Z-ADD     ELFND         KKFND
      *
      *  Get Rating State
      *
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL(P)  RSJRST        KKST
     C   60              MOVE(P)   *BLANKS       KKST
      *
      ***** BEG ADD ***** 06/05/07 ************************************
      *  Check for Statistical Rpt/rating record equal to renewal year
      *  If so, check fields for codes/headings and if not exist set
      *  flags to clear fields.
     C                   MOVEL     XXPFDT        KKEFDT
      *
     C                   MOVE      *BLANKS       ZZRWCC            1            CONTR CREDT CDE
     C                   MOVE      *BLANKS       ZZRWD1            1            DEV # 1
     C                   MOVE      *BLANKS       ZZRWD2            1            DEV # 2
     C                   MOVE      *BLANKS       ZZR1AC            1            SUB PREM CRT 1
     C                   MOVE      *BLANKS       ZZR1BC            1            SUB PREM CRT 2
     C                   MOVE      *BLANKS       ZZDBCD            1            SCHED DEBIT CDE
     C                   MOVE      *BLANKS       ZZCRCD            1            SCHED CREDT CDE
     C*** BEGIN ADD ***  07/05/07  ****************************************
     C                   MOVE      *BLANK        ZZDISC            1            STOCK DISC CODE
     C                   Z-ADD     0             ZZDSPC            5 2          STOCK DISC PERC
     C                   MOVE      *BLANK        ZZSFAP            1            SAFETY FEE LEVEL
     C*** END ADD *****  07/05/07  ****************************************
      *
     C     RRKY1         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
      *
B01  C     *IN60         IFEQ      *OFF
     C***  Begin Del  ***  06/14/07  **************************************
     C********** RREFDT    ANDEQXXPFDT
     C***  End   Del  ***  06/14/07  **************************************
B02  C     RRDCCD        IFEQ      *ZEROS
     C                   MOVE      'N'           ZZRWCC
E02  C                   ENDIF
B02  C     RRDV1C        IFEQ      *ZEROS
     C                   MOVE      'N'           ZZRWD1
E02  C                   ENDIF
B02  C     RRDV2C        IFEQ      *ZEROS
     C                   MOVE      'N'           ZZRWD2
E02  C                   ENDIF
B02  C     RR1ACD        IFEQ      *ZEROS
     C                   MOVE      'N'           ZZR1AC
E02  C                   ENDIF
B02  C     RR1BCD        IFEQ      *ZEROS
     C                   MOVE      'N'           ZZR1BC
E02  C                   ENDIF
B02  C     RRDBCD        IFEQ      *ZEROS
     C                   MOVE      'N'           ZZDBCD
E02  C                   ENDIF
B02  C     RRCRCD        IFEQ      *ZEROS
     C                   MOVE      'N'           ZZCRCD
E02  C                   ENDIF
     C*** BEGIN ADD ***  07/05/07  ****************************************
     C                   MOVE      RRDISC        ZZDISC
     C                   Z-ADD     RRDSPC        ZZDSPC
     C                   MOVE      RRSFAP        ZZSFAP
     C*** END ADD *****  07/05/07  ****************************************
      *
E01  C                   ENDIF                                                  RREFDT=XXPFDT
      *
      ***** END ADD ***** 06/05/07 ************************************
      *
     C***  Begin Del  ***  06/14/07  **************************************
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     KKEFDT
     C***  End   Del  ***  06/14/07  **************************************
     C*** BEGIN ADD ***  07/05/07  ****************************************
     C                   Z-ADD     SVPFDT        KKEFDT
     C*** END ADD *****  07/05/07  ****************************************
      *
      *  Get auto renewal flags from Statistical Reporting / Rating file
      *
     C     RRKY1         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  71
      *
      *  Get auto renewal flag from Deductible Discount file
      *
     C                   MOVEL(P)  ELDCDE        KKCODE
      *
     C                   SETOFF                                       72
     C***  BEG ADD  ***  09/22/09 ***************************************
     C* GET RATING DATE BASED ON ARD LOGIC
     C*** BEGIN CHG ****** 09/23/11 ************************************
     C**************     MOVE      ELDCEF        Q1DATE
     C                   MOVE      ELDCEF        Q1DAT7
     C*** END   CHG ****** 09/23/11 ************************************
     C                   MOVE      ELCO#         QQCO#
     C                   MOVE      ELFND         QQFND
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
     C*** BEGIN CHG ****** 09/23/11 ************************************
     C*************      PARM                    Q1DATE            7
     C                   PARM                    Q1DAT7            7
     C*************      MOVE      Q1DATE        KKEFDT
     C                   MOVE      Q1DAT7        KKEFDT
     C*** END   CHG ****** 09/23/11 ************************************
     C***  END ADD  ***  09/22/09 ***************************************
     C     DDKY1         SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  72
      *
B01  C     *IN72         DOWEQ     *OFF
     C     DDSTAT        ANDEQ     'I'
     C     DDKY2         READPE    PDD01                                  72
E01  C                   ENDDO                                                  DDSTAT
     C*
     C***  BEGIN ADD  ***  04/07/05 *************************************
     C     *LIKE         DEFINE    DDRNW         XX1RNW
     C     *LIKE         DEFINE    DDRNW         XX2RNW
     C*
     C                   CLEAR                   XX1RNW
     C                   CLEAR                   XX2RNW
     C*
B01  C     *IN72         IFEQ      *OFF
     C                   MOVE      DDRNW         XX1RNW
     C***  BEG ADD  ***  09/22/09 ***************************************
     C* POPULATE 1ST DEDUCTIBLE INFO FIELDS                            **
B02  C     ELDCDE        IFNE      *BLANKS
     C                   MOVE      DDTYPE        ELDEDP
     C                   MOVE      DDAMPO        ELAMPO
     C                   MOVE      DDAMAG        ELAMAG
E02  C                   ENDIF
     C***  END ADD  ***  09/22/09 ***************************************
E01  C                   ENDIF
     C*
     C                   MOVEL(P)  ELDCD2        KKCODE
      *
     C***  BEG ADD  ***  09/22/09 ***************************************
     C* GET RATING DATE BASED ON ARD LOGIC
     C*** BEGIN CHG ****** 09/23/11 ************************************
     C*************      MOVE      EL2DEF        Q1DATE
     C                   MOVE      EL2DEF        Q1DAT7
     C*** END   CHG ****** 09/23/11 ************************************
     C                   MOVE      ELCO#         QQCO#
     C                   MOVE      ELFND         QQFND
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
     C*** BEGIN CHG ****** 09/23/11 ************************************
     C*************      PARM                    Q1DATE            7
     C                   PARM                    Q1DAT7            7
     C*************      MOVE      Q1DATE        KKEFDT
     C                   MOVE      Q1DAT7        KKEFDT
     C*** END   CHG ****** 09/23/11 ************************************
     C***  END ADD  ***  09/22/09 ***************************************
      *
     C     DDKY1         SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  72
      *
B01  C     *IN72         DOWEQ     *OFF
     C     DDSTAT        ANDEQ     'I'
     C     DDKY2         READPE    PDD01                                  72
E01  C                   ENDDO                                                  DDSTAT
     C*
B01  C     *IN72         IFEQ      *OFF
     C                   MOVE      DDRNW         XX2RNW
     C***  BEG ADD  ***  09/22/09 ***************************************
     C* POPULATE 2ND DEDUCTIBLE INFO FIELDS                            **
B02  C     ELDCD2        IFNE      *BLANKS
     C                   MOVE      DDTYPE        EL2DDP
     C                   MOVE      DDAMPO        EL2APO
     C                   MOVE      DDAMAG        EL2AAG
E02  C                   ENDIF
E01  C                   ENDIF
      *
      *  Get auto renewal flag from Increased Limit file
      *
     C                   MOVE      *BLANK        X1LRNW            1
     C                   MOVE      *BLANK        X2LRNW            1
     C*** Begin Add ***  08/01/25  ****************************************
     C                   Z-ADD     SVPFDT        KKEFDT
     C*** End Add *****  08/01/25  ****************************************
      *
     C                   MOVEL(P)  ELILCD        KKKEY
      *
     C                   SETOFF                                       73
     C     LMKY1         SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  73
      *
B01  C     *IN73         DOWEQ     *OFF
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  73
E01  C                   ENDDO                                                  LMSTAT
B01  C     *IN73         IFEQ      *OFF
     C                   MOVEL     LMRNW         X1LRNW
E01  C                   ENDIF
      *
      *  Check 2nd Increased Limits auto renewal flag
     C                   MOVEL(P)  ELINL2        KKKEY
     C                   SETOFF                                       73
     C     LMKY1         SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  73
      *
B01  C     *IN73         DOWEQ     *OFF
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  73
E01  C                   ENDDO                                                  LMSTAT
B01  C     *IN73         IFEQ      *OFF
     C                   MOVEL     LMRNW         X2LRNW
E01  C                   ENDIF
      *
     C                   ENDSR                                                  SRRNW
     C*================================================================
     C* WRITEQ - Write records to the WAQTP file
     C*================================================================
     C*
     C     WRITEQ        BEGSR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     XXDATE        QTWKDT
     C*
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   Z-ADD     XXDATE        QTDT
     C                   Z-ADD     DSTIME        QTITM
     C*
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTIPGM
     C                   Z-ADD     XXDATE        QTIDT
     C                   Z-ADD     DSTIME        QTITM
     C*
     C                   WRITE     PQT01
     C*
     C                   ENDSR                                                  WRITEQ
      *================================================================
      * WRTPLO - WRITE PAYROLL LOCATION RECORDS
      *================================================================
     C     WRTPLO        BEGSR
      *
      *  Ensure payroll class is valid in new year before writing
      *
     C                   MOVE      *ON           XXWRIT            1            WRITE FLAG
     C*
     C*  Check Payroll Class Master
     C*
     C                   Z-ADD     SVCLAS        X2CLAS
     C                   CLEAR                   SVXFER
     C*
     C     PRKY1         SETGT     PPR01
     C     PRKY          READPE    PPR01                                  60
     C*
     C*  Class Code not active
     C*
     C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDNE     'A'
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDNE     *ZERO
     C     PREXDT        ANDLE     XXSVPF
     C     PRXFER        ANDEQ     *ZEROS
     C     XXSVAN        ANDEQ     *ZEROS
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDNE     *ZERO
     C     PREXDT        ANDLE     XXSVAN
     C     PRXFER        ANDEQ     *ZEROS
     C     XXSVAN        ANDNE     *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *OFF          XXWRIT
     C                   CLEAR                   PRXFER
     C                   ENDIF
     C*
     C*  Transfer to new class?
     C*
     C     PREXDT        IFLE      XXSVPF
     C     PREXDT        ANDNE     *ZERO
     C     PRXFER        ANDNE     *ZEROS
     C     XXSVAN        ANDEQ     *ZEROS
     C     PREXDT        ORLE      XXSVAN
     C     PREXDT        ANDNE     *ZERO
     C     PRXFER        ANDNE     *ZEROS
     C     XXSVAN        ANDNE     *ZEROS
     C*
     C                   Z-ADD     PRXFER        SVCLAS
     C                   Z-ADD     PRXFER        X2CLAS
     C*
     C     PRKY1         SETGT     PPR01
     C     PRKY          READPE    PPR01                                  60
     C*
     C*  New class not active
     C*
     C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDNE     'A'
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDLE     XXSVPF
     C     PREXDT        ANDNE     *ZERO
     C     XXSVAN        ANDEQ     *ZEROS
     C     *IN60         OREQ      *OFF
     C     PREXDT        ANDLE     XXSVAN
     C     PREXDT        ANDNE     *ZERO
     C     XXSVAN        ANDNE     *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *OFF          XXWRIT
     C                   ENDIF
      *
     C                   ENDIF                                                  PREXDT < XXSVPF
      *
     C     XXWRIT        IFEQ      *ON
      *
      *   MULTIPLY PAYROLL BY FACTOR
      *
     C     SVEAPR        MULT      XXPCT         XXWK92
      *
     C     XXIDFL        IFEQ      'D'
     C     SVEAPR        SUB       XXWK92        PLEAPR
     C                   ELSE
     C     SVEAPR        ADD       XXWK92        PLEAPR
     C                   ENDIF                                                  XXIDFL IFEQ
      *
      *   ROUND TO NEAREST DOLLAR
      *
     C                   Z-ADD(H)  PLEAPR        XXSAVE           11 0
     C                   Z-ADD     XXSAVE        PLEAPR
      *
     C     XXSAVE        IFLT      *ZEROS
     C                   MULT      -1            PLEAPR
     C                   ENDIF                                                  XXSAVE IFLT
      *
     C                   Z-ADD     XXFYR         PLFYR
     C                   Z-ADD     SVCLAS        PLCLAS
     C                   MOVE      XXSVPF        PLEFDT
     C                   MOVE      SVSPEC        PLSPEC
     C                   Z-ADD     SVECNT        PLECNT
     C                   Z-ADD     SVCADT        PLCADT
     C                   MOVE      SVSTAT        PLSTAT
     C                   MOVE      Q1USER        PLUSER
     C                   MOVE      Q1PGM         PLPGM
     C                   MOVE      DSTIME        PLTIME
     C                   MOVE      XXDATE        PLDT
     C                   MOVE      PLUSER        PLIUSR
     C                   MOVE      PLPGM         PLIPGM
     C                   MOVE      PLTIME        PLITIM
     C                   MOVE      PLDT          PLIDT
      *
     C                   WRITE     PPL01
     C                   ENDIF
      *
     C                   ENDSR                                                  WRTPLO
     C*================================================================
     C* WRTSHQ - Write records to the WMSHLQ File
     C*================================================================
     C     WRTSHQ        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   CLEAR                   PSH01Q
     C*
     C                   Z-ADD     ELCO#         SHCO#
     C                   Z-ADD     ELFND         SHFND
     C                   Z-ADD     ELEMP#        SHEMP#
     C                   Z-ADD     ELDIV         SHDIV
     C                   Z-ADD     ELFYR         SHFYR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHTRDT
     C*
     C                   MOVEL(P)  DSDTTM        SHUNID
     C                   MOVEL(P)  'P'           SHTYPE
     C                   MOVEL(P)  'O'           SHCODE
     C                   MOVEL(P)  '1'           SHSTAT
     C                   Z-ADD     ELPFDT        SHPFDT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SHDT
     C*
     C                   MOVEL(P)  Q1USER        SHUSER
     C                   MOVEL(P)  Q1PGM         SHPGM
     C                   Z-ADD     DSTIME        SHTIME
     C*
     C                   MOVEL(P)  Q1USER        SHIUSR
     C                   Z-ADD     SHDT          SHIDT
     C*
     C                   WRITE     PSH01Q
     C*
     C                   ENDSR                                                  WRITEQ
      *=============================================================
      * ApplyRateSplit - Check option flag to apply rate split
      *=============================================================
     C     SRAPRT        BEGSR
     C*
     C     RTKYX         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    KKST
     C                   KFLD                    XXSPDT            7 0
     C*
     C                   MOVE      'N'           xxoptf            1
     C     RTKYX         CHAIN     PRT0X                              60
B01  C     *IN60         IFEQ      *OFF
     C     RXOPT         ANDEQ     'Y'
     C                   MOVE      'Y'           xxoptf
E01  C                   ENDIF
     C*
     C                   ENDSR
      *=============================================================
      * SRRollLgDed    - ROLL LARGE DEDUCTIBLE RECORDS TO A NEW YEAR.
      *=============================================================
     C     SRRollLgDed   BEGSR
B01  C     QQMODE        IFEQ      'L'
     C                   OPEN      WMLDL1
     C                   EXSR      MODEL
     C                   CLOSE     WMLDL1
E01  C                   ENDIF
     C                   ENDSR
     C********************************************************************
     C*
     C*  If the 1st Deductible is expired and the 2nd is not, move the 2nd
     C*   information to the first and clear the 2nd.
     C*  If the 1st and 2nd Deductible are expired, clear both.
     C*  In order for there to be a 2nd deductible, the first must expire.
     C*
     C     MODEL         BEGSR
     C*
     C     KEY1          CHAIN     PLD02                              60
B01  C     *IN60         IFEQ      *OFF
B02  C     LDRNW1        IFNE      'Y'
     C                   GOTO      SUBEND
E02  C                   ENDIF
     C*
B02  C                   SELECT
     C*
S01  C     LDEXP1        WHENNE    *ZEROS
     C     LDEXP1        ANDLE     ELPFDT
     C     LDEXP2        ANDEQ     *ZEROS
     C                   EXSR      MVLD
     C                   EXSR      CLRD2
     C*
S02  C     LDEXP1        WHENNE    *ZEROS
     C     LDEXP1        ANDLE     ELPFDT
     C     LDEXP2        ANDNE     *ZEROS
     C                   EXSR      CLRD1
B03  C     LDEXP2        IFNE      *ZEROS
     C     LDEXP2        ANDGT     ELPFDT
     C                   EXSR      MVLD
     C                   EXSR      CLRD2
E03  C                   ENDIF                                                  LDEXP2>QQPFDT
B03  C     LDEXP2        IFNE      *ZEROS
     C     LDEXP2        ANDLE     ELPFDT
     C                   EXSR      CLRD2
E03  C                   ENDIF                                                  LDEXP2<=QQPFDT
E02  C                   ENDSL
     C*
     C*  If a deductible code still exists, the prior years deductible did
     C*   NOT expire, therefor the effective date of the deductible for the
     C*   current year will be the policy from date.
     C*
     C                   MOVE      ELFYR         LDFYR
     C                   Z-ADD     ELPFDT        LDEFD1
B02  C     LDPCT1        IFNE      *ZERO
     C                   MOVE      'Y'           LDRNW1
E02  C                   ENDIF
      *
     C*
B02  C     XXRTOQ        IFEQ      'Y'
     C                   OPEN      WMLDL1Q
     C                   WRITE     PLD02Q
     C                   CLOSE     WMLDL1Q
X02  C                   ELSE
     C                   WRITE     PLD02
     C                   CLOSE     WMLDL1
E02  C                   ENDIF
     C*
E01  C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     SUBEND        ENDSR
     C************************************************************************
     C     MVLD          BEGSR
     C                   MOVE      LDEFD2        LDEFD1
     C                   MOVE      LDEXP2        LDEXP1
     C                   MOVE      LDDES2        LDDES1
     C                   MOVE      LDPCT2        LDPCT1
     C                   MOVE      LDTYP2        LDTYP1
     C                   MOVE      LDAMP2        LDAMP1
     C                   MOVE      LDAMG2        LDAMG1
     C                   MOVE      LDPLN2        LDPLN1
     C                   MOVE      LDVRB2        LDVRB1
     C                   MOVE      LDOCV2        LDOCV1
     C                   MOVE      LDTOT2        LDTOT1
     C                   MOVE      LDCLS2        LDCLS1
     C                   MOVE      LDPLV2        LDPLV1
     C                   MOVE      LDALA2        LDALA1
     C                   MOVE      LDRNW2        LDRNW1
     C                   MOVE      LDR201        LDR101
     C                   MOVE      LDR202        LDR102
     C                   MOVE      LDR203        LDR103
     C                   MOVE      LDR204        LDR104
     C                   MOVE      LDR205        LDR105
     C                   MOVE      LDR206        LDR106
     C                   MOVE      LDR207        LDR107
     C                   MOVE      LDR208        LDR108
     C                   MOVE      LDR209        LDR109
     C                   MOVE      LDR210        LDR110
     C                   ENDSR
     C**************************************************************
     C     CLRD1         BEGSR
     C                   CLEAR                   LDEFD1
     C                   CLEAR                   LDEXP1
     C                   CLEAR                   LDDES1
     C                   CLEAR                   LDPCT1
     C                   CLEAR                   LDTYP1
     C                   CLEAR                   LDAMP1
     C                   CLEAR                   LDAMG1
     C                   CLEAR                   LDPLN1
     C                   CLEAR                   LDVRB1
     C                   CLEAR                   LDOCV1
     C                   CLEAR                   LDTOT1
     C                   CLEAR                   LDCLS1
     C                   CLEAR                   LDPLV1
     C                   CLEAR                   LDALA1
     C                   CLEAR                   LDRNW1
     C                   CLEAR                   LDR101
     C                   CLEAR                   LDR102
     C                   CLEAR                   LDR103
     C                   CLEAR                   LDR104
     C                   CLEAR                   LDR105
     C                   CLEAR                   LDR106
     C                   CLEAR                   LDR107
     C                   CLEAR                   LDR108
     C                   CLEAR                   LDR109
     C                   CLEAR                   LDR110
     C                   ENDSR
     C**************************************************************
     C     CLRD2         BEGSR
     C                   CLEAR                   LDEFD2
     C                   CLEAR                   LDEXP2
     C                   CLEAR                   LDDES2
     C                   CLEAR                   LDPCT2
     C                   CLEAR                   LDTYP2
     C                   CLEAR                   LDAMP2
     C                   CLEAR                   LDAMG2
     C                   CLEAR                   LDPLN2
     C                   CLEAR                   LDVRB2
     C                   CLEAR                   LDOCV2
     C                   CLEAR                   LDTOT2
     C                   CLEAR                   LDCLS2
     C                   CLEAR                   LDPLV2
     C                   CLEAR                   LDALA2
     C                   CLEAR                   LDRNW2
     C                   CLEAR                   LDR201
     C                   CLEAR                   LDR202
     C                   CLEAR                   LDR203
     C                   CLEAR                   LDR204
     C                   CLEAR                   LDR205
     C                   CLEAR                   LDR206
     C                   CLEAR                   LDR207
     C                   CLEAR                   LDR208
     C                   CLEAR                   LDR209
     C                   CLEAR                   LDR210
     C                   ENDSR
     C**************************************************************
     C*===============================================================
     C* Process Employer Fee information
     C*===============================================================
     C     SrEmpFee      begsr
     C*
     C     keyWDEFL1     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    XXXYR
     C*
     C     key1WDEFL1    klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELEMP#
     C                   kfld                    ELDIV
     C                   kfld                    ELFYR
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
     C*
     C     keyWTFIL      klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
     C                   kfld                    kkSt
     C*
     C     key1WTFIL     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    EFTYPE
     C                   kfld                    EFDESC
     C                   kfld                    kkSt
     C                   kfld                    ELPFDT
     C*
     C* Do not process if module not turned on
     C                   clear                   WCFL44
     C     'WC'          chain     swc01
B01  C                   if        WCFL44 <> 'Y'
     C                   leavesr
E01  C                   endif
     C*
B01  C                   if        not %open(WTFIL)
     C                   open      WTFIL
E01  C                   endif
     C*
B01  C                   if        not %open(WDEFL1)
     C                   open      WDEFL1
E01  C                   endif
     C*
B01  C                   if        not %open(WDEFL1Q) and xxRToQ = 'Y'
     C                   open      WDEFL1Q
E01  C                   endif
     C*
     C* Get jurisidiction state
     C                   clear                   RSJRST
     C     rsKy          chain     prs01
     C                   eval      kkSt = RSJRST
     C*
     C     keyWDEFL1     setgt     pef01
B01  C                   dou       %eof
     C     keyWDEFL1     readpe    pef01
B02  C                   if        not %eof
     C* Ensure fee still available at group level
     C     key1WTFIL     setgt     pfi00
     C     keyWTFIL      readpe    pfi00
B03  C                   if        %eof
     C                              or (FIEXDT <> *zero
     C                                  and FIEXDT <= ELPFDT)
     C                   iter
E03  C                   endif
     C* Check to ensure fee doesn't exist at new year
B03  C                   select
S01  C                   when      xxRToQ = 'Y'
     C     key1WDEFL1    chain     pef01q
S02  C                   other
     C     key1WDEFL1    chain     pef01
E03  C                   endsl
B03  C                   if        not %found
     C                   time                    dsDtTm
     C                   eval      EFFYR  = ELFYR
     C                   eval      EFPGM  = q1Pgm
     C                   eval      EFUSER = q1User
     C                   eval      EFDT   = xxDate
     C                   eval      EFTIME = dsTime
     C                   eval      EFIPGM = EFPGM
     C                   eval      EFIUSR = EFUSER
     C                   eval      EFIDT  = EFDT
     C                   eval      EFITM  = EFTIME
B04  C                   select
S01  C                   when      xxRToQ = 'Y'
     C                   write     pef01q
S02  C                   other
     C                   write     pef01
E04  C                   endsl
E03  C                   endif
E02  C                   endif
E01  C                   enddo
     C*
B01  C                   if        %open(WTFIL)
     C                   close     WTFIL
E01  C                   endif
     C*
B01  C                   if        %open(WDEFL1)
     C                   close     WDEFL1
E01  C                   endif
     C*
B01  C                   if        %open(WDEFL1Q)
     C                   close     WDEFL1Q
E01  C                   endif
     C*
     C                   endsr
     C**** Beg Add ***** 08/20/18 ************************************
     C*===============================================================
     C* Renew Waivers to Quote
     C*===============================================================
     C     RollWaivers   begsr
     C***  Begin Add  ***  07/18/22  **********************
        RenewWaivers(ELco#:ELfnd:ELfyr:xxKyr:ELemp#:%char(ELdiv));
       Endsr;
     C*===============================================================
     C* Renew Waivers to Quote - Old Routine
     C*===============================================================
       Begsr RollWaivers_old;
     C***  End   Add  ***  07/18/22  **********************
      /free
         //*** Beg Add ***** 12/14/18 *************************
         //*** Beg Chg ***** 04/15/19 *************************
         //Clear arrFormDiv;
         Clear arrForm;
         //*** End Chg ***** 04/15/19 *************************
         //*** End Add ***** 12/14/18 *************************
         Setll (xxco#:xxFnd:xxKyr:xxEmp#) pmu06;
         Reade (xxco#:xxFnd:xxKyr:xxEmp#) pmu06;
         Dow not %eof(wmmul6);
            Chain (muCo#:muFnd:muForm) pen00;
            If %Found(wtenl);
               If ENtype = 'Waiver';
                  //*** Beg Dlt ***** 12/14/18 *************************
                  //mufyr  = elFyr;
                  //mueedt = elpfdt;
                  //muuser = q1user;
                  //muiusr = q1user;
                  //mupgm  = q1pgm;
                  //muipgm = q1pgm;
                  //mutime  = %Dec(%Char(%Time():*iso0):6:0);
                  //muidt   = %Dec(%Char(%Date():*cymd0):7:0);
                  //mudt    = muidt;
                  //Write pmu01Q;
                  //*** End Dlt ***** 12/14/18 *************************
                  //*** Beg Add ***** 12/14/18 *************************
                  StrForm = muForm;
                  //*** Beg Chg ***** 04/15/19 *************************
                  //StrDiv  = %editc(muDiv:'X');
                  //index = %lookup(StrFormDiv:arrFormDiv);
                  index = %lookup(StrForm:arrForm);
                  //*** End Chg ***** 04/15/19 *************************
                  If index = 0;                        //Not Found
                     //*** Beg Chg ***** 04/15/19 *************************
                     //index = %lookup(' ':arrFormDiv);
                     //arrFormDiv(index) = strFormDiv;
                     index = %lookup(' ':arrForm);
                     arrForm(index) = strForm;
                     //*** End Chg ***** 04/15/19 *************************
                     muend# = 0;
                     museq# = 0;
                     mufyr  = elFyr;
                     mueedt = elpfdt;
                     muuser = q1user;
                     muiusr = q1user;
                     mupgm  = q1pgm;
                     muipgm = q1pgm;
                     mutime  = %Dec(%Char(%Time():*iso0):6:0);
                     muidt   = %Dec(%Char(%Date():*cymd0):7:0);
                     mudt    = muidt;
                     Write pmu01Q;
                  Endif;
                  //*** End Add ***** 12/14/18 *************************
               Endif;
            Endif;
            Reade (xxco#:xxFnd:xxKyr:xxEmp#) pmu06;
         Enddo;
         //*** Beg Add ***** 11/12/18 ******************************
         //Copy waiver details
         //*** Beg Add ***** 12/14/18 ***************************
         intWav# = 1;
         //*** End Add ***** 12/14/18 ***************************
         Setll (xxco#:xxFnd:xxKyr:xxEmp#) pwv00;
         Reade (xxco#:xxFnd:xxKyr:xxEmp#) pwv00;
         Dow not %eof(wmwvl);
            WVfyr  = elFyr;
            WVefdt = elpfdt;
            //*** Beg Add ***** 12/14/18 ***************************
            WVwav# = intWav#;
            intWav# = intWav# + 1;
            WVend# = 0;
            WVprt# = 0;
            //*** End Add ***** 12/14/18 ****************************
            WVuser = q1user;
            WVpgm  = q1pgm;
            WVtime = %Dec(%Char(%Time():*iso0):6:0);
            WVdt   = %Dec(%Char(%Date():*cymd0):7:0);
            WViusr = q1user;
            WVipgm = q1pgm;
            WVitm  = wvtime;
            WVidt  = wvdt;
            Write pwv01Q;
            Reade (xxco#:xxFnd:xxKyr:xxEmp#) pwv00;
         Enddo;
         //*** End Add ***** 11/12/18 ******************************
        //*** Beg Add ***** 06/14/19 *********************/
         Setll (xxco#:xxFnd:xxKyr:xxEmp#) pfm01;
         Dou %Eof;
         Reade (xxco#:xxFnd:xxKyr:xxEmp#) pfm01;
            If %Eof;
               Leave;
            EndIf;
            Chain (F3Co#:F3Fnd:F3Form) pen00;
            If %Found and ENtype = 'Waiver';
               F3end# = 0;
               F3seq# = 0;
               F3fyr  = ELFyr;
               F3eedt = ELpfdt;
               F3user = Q1user;
               F3iusr = Q1user;
               F3pgm  = Q1pgm;
               F3ipgm = Q1pgm;
               F3time = %Dec(%Char(%Time():*Iso0):6:0);
               F3idt  = %Dec(%Char(%Date():*Cymd0):7:0);
               F3dt   = F3idt;
               Write pfm11;
            Endif;
         Enddo;
        //*** End Add ***** 06/14/19 *********************/
      /End-free
     C                   endsr
     C**** End Add ***** 08/20/18 ************************************
      /Free
       //*** Begin Add *** 10/29/18 ***********************************
       //===============================================================
       // Get Pay Plan Weeks
       //===============================================================
       Begsr GetPlanWeeks;

       //Check day of billing start date.
       SRdte = %Dec(%Char(%date(SVbsdt:*CYMD):*USA0):8:0);
       SRDAY8(SRdte:SRsday:SRlday:SRerr);
       xxDay = %subst(SRsday:1:2);

       //Find 1st Billing Date
       If xxDay = BSdowk;
          NxtBillDate = SVbsdt;
       Else;

       x1 = %Lookup(xxDay:DAY);
       x2 = %Lookup(BSdowk:DAY:x1);
       x3Cnt = x2 - x1;
       NxtBillDate = %Dec(%Char(%date(SVbsdt:*cymd) + %days(x3Cnt):*cymd0):7:0);
       Endif;

       If NxtBillDate >= SVptdt;
          NxtBillDate = SVbsdt;
       Endif;

       //Find remaining billing dates
       T = *Zeros;

       Dow NxtBillDate <= SVPTDT;
           T = T + 1;
           WKD(T) = NxtBillDate;
           NxtBillDate = %Dec(%Char(%date(NxtBillDate:*cymd)
                         + %days(7):*cymd0):7:0);
       //*** begin add ***  09/22/23  *************************************
           If t = 99;
              Leave;
           Endif;
       //*** end add *****  09/22/23  *************************************
       Enddo;

       Endsr;
       //*** End  Add *** 10/29/18 ***********************************

      /End-Free
      *****************************************************************
      *
     OQSYSPRT   E            ROUTE          1 06
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
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE          2
     O                                           20 'COMPANY:'
     O                       XXCO#         Z     39
     O          E            ROUTE          2
     O                                           18 'GROUP:'
     O                       XXFND         Z     39
     O          E            ROUTE          2
     O                                         + 12 'RENEW TO QUOTE:'
     O                       XXRTOQ              37
     O          E            ROUTE          2
     O                                           34 'EXPIRATION DATE RANGE:'
     O                       RSFTDT              44 '  /  /  '
     O                                           47 'TO'
     O                       RSTTDT              56 '  /  /  '
     O          E            ROUTE          2
     O                                         + 12 'EMPLOYER:             '
     O                       RSEMP#            +002
     O          E            ROUTE          2
     O                                           26 'ROLLOVER TYPE:'
     O                       QQTYPE              37
     O          E            ROUTE          2
     O                                           27 'PAYROLL CHANGE:'
     O                       XXPCT1        M     43
     O                                           44 '%'
     O                       RSCHG               55
     O          E            ROUTE          2
     O                  21                       27 'INCLUDED:'
     O                  22                       27 'EXCLUDED:'
     O                       QQST1             +  1
     O                       QQST2             +  1
     O                       QQST3             +  1
     O                       QQST4             +  1
     O                       QQST5             +  1
     O                       QQST6             +  1
     O                       QQST7             +  1
     O                       QQST8             +  1
     O                       QQST9             +  1
     O                       QQST10            +  1
      ***** END ADD ******* 04/18/06 **********************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E  160           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                             +000 'VE DATA SOLUTIONS. INC,'
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
     O                       XXTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
     O          E            HEAD           2
     O                                           09 'EMPLOYER'
     O                                           15 'DIV'
     O                                           29 'EMPLOYER NAME'
     O                                           65 'CURRENT EFF/EXP DATES'
     O                                           78 'EXPLANATION'
     O          E            DET            1
     O                       EMEMP#        Z     09
     O                       EMDIV         Z     15
     O                       XXNAME              41
     O                       XXEFFD              53 '  /  /    '
     O                                           55 '-'
     O                       XXEXPD              66 '  /  /    '
     O                       XXMSG              132
     O          E            DETMSG         1
     O                       XXMSG1             132
     O          E            TOT         1  1
     O                                           31 'TOTAL NUMBER LISTED:'
     O                       XXTOT         Z     43
     O*
     O**** DETAIL LINE FOR 'NO RECORDS MEET SELECTION CRITERIA'
     O          E            NOREC       2  1
     O                                           60 '*** NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA ***'
     O*
     OQSYSPRT1  E            ROUTE1         1 06
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O*
     O          E            ROUTE1      2  2
     O*
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE1         2
     O                                           20 'COMPANY:'
     O                       XXCO#         Z     39
     O          E            ROUTE1         2
     O                                           18 'GROUP:'
     O                       XXFND         Z     39
     O          E            ROUTE1         2
     O                                         + 12 'RENEW TO QUOTE:'
     O                       XXRTOQ              37
     O          E            ROUTE1         2
     O                                           34 'EXPIRATION DATE RANGE:'
     O                       RSFTDT              44 '  /  /  '
     O                                           47 'TO'
     O                       RSTTDT              56 '  /  /  '
     O          E            ROUTE1         2
     O                                         + 12 'EMPLOYER:             '
     O                       RSEMP#            +002
     O          E            ROUTE1         2
     O                                           26 'ROLLOVER TYPE:'
     O                       QQTYPE              37
     O          E            ROUTE1         2
     O                                           27 'PAYROLL CHANGE:'
     O                       XXPCT1        M     43
     O                                           44 '%'
     O                       RSCHG               55
     O          E            ROUTE1         2
     O                  21                       27 'INCLUDED:'
     O                  22                       27 'EXCLUDED:'
     O                       QQST1             +  1
     O                       QQST2             +  1
     O                       QQST3             +  1
     O                       QQST4             +  1
     O                       QQST5             +  1
     O                       QQST6             +  1
     O                       QQST7             +  1
     O                       QQST8             +  1
     O                       QQST9             +  1
     O                       QQST10            +  1
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
     O*
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       X1TITL              96
     O                                          126 'PAGE:'
     O                       PAGE1         Z    132
     O          E            HEAD1          2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
     O          E            HEAD1          2
     O                                           18 'REPORT SELECTIONS:'
     O                                           47 'ROLLOVER TYPE:'
     O                       X1TYPE              49
     O                                           66 'INC PAYROLL PCT:'
     O                       XXPCT1        L     74
     O          E            HEAD1          1
     O                                           09 'EMPLOYER'
     O                                           15 'DIV'
     O                                           29 'EMPLOYER NAME'
     O                                           65 'CURRENT EFF/EXP DATES'
     O                                           87 'NEW EFF/EXP DATES'
     O                                          111 'RENEWAL INFORMATION'
     O          E            DET1        1  0
     O                       EMEMP#        Z     09
     O                       EMDIV         Z     15
     O                       XXNAME              41
     O                       XXEFFD              53 '  /  /    '
     O                                           55 '-'
     O                       XXEXPD              66 '  /  /    '
     O                       XXNEFD              77 '  /  /    '
     O                                           79 '-'
     O                       XXNEXD              90 '  /  /    '
     O          E            DET1IN         1
     O                       XXINFO             132
     O          E            EXTRA1         1
     O          E            TOT1        1  1
     O                                           31 'TOTAL NUMBER LISTED:'
     O                       XXTOT1        Z     43
     O*
     O          E            NOREC1      2  1
     O                                           60 '*** NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA ***'
      *//   Begin Add 05/13/19 ***************************************
      *
      *//=============================================================
      *// S/R Annualization
      *//=============================================================
     P SRANNS          B
     D SRANNS          PI             5  0
     D  ClassEffDt                    7  0
     D  DivExpDt                      7  0

     D  Rundate        s               D   INZ
     D  ClassDays      s              5  0 INZ

       Rundate = %date();
       If  %date(DivExpDt:*cymd) < Rundate or Rundate < %date(ClassEffDt:*cymd);
           Rundate = %date(DivExpDt:*cymd);
       Endif;
       ClassDays = %diff(Rundate:%date(ClassEffDt:*cymd):*days);
       Return ClassDays;
     P SRANNS          E
      *//   End Add 05/13/19 ***************************************
      *
**  MSG                                                            |
NO SELF REPORTED PAYROLLS - ROLLED EST. PAYROLL CURRENT YR FOR CLASS
PRIOR YEAR NOT AUDITED - ROLLED ESTIMATED PAYROLLS CURRENT YR
EMPLOYER NOT RENEWED: PENDING CANCELLATION - EFFECTIVE xx/xx/xxxx
NOT ROLLED: NO GROUP DETAIL FOR THE NEW EMPLOYER POLICY YR
NOT ROLLED: NEW POLICY EFF DT IS NOT WITHIN GRP POLICY DATE RANGE
NOT ROLLED: DO NOT RENEW FLAG SET IN DIV OR STATE REP DIV EMP DETAIL
NOT ROLLED: EMPLOYER ALREADY EXISTS IN QUOTE ENV WITH THIS NUMBER
NOT ROLLED: RATE SET CODE IS NO LONGER ACTIVE FOR THIS EMPLOYER     ***11/19/13
**  INFO                               |
CLASS CODE xxxxx EXPIRED DURING THE CURRENT POLICY PERIOD AND WAS NOT RENEWED
CLASS CODE xxxxx WAS TRANSFERED TO CLASS CODE xxxxx
CLASS CODE xxxxx DESIGNATED TO TRANSFER TO CLASS xxxxx, HOWEVER, THE NEW
 TRANSFER CLASS IS NOT VALID FOR THE NEW POLICY YEAR
CLASS CODE xxxxx HAS BEEN INACTIVATED AT THE PAYROLL CLASS MASTER LEVEL
AGENT RATE xxx INFORMATION WAS INACTIVE AT THE TIME OF RENEWAL, DEFAULT RATE
 xxx ASSIGNED
NEW AGENT RENEWAL RATE xxx DOES NOT MATCH THE DEFAULT RATE xxx ESTABLISHED
 FOR AGENT xxxxx DEFAULT RATE WILL BE ASSIGNED
**  CHG
INCREASE
DECREASE
**  MSG1
POLICY FROM AND TO DATE WITHIN SAME YEAR PREVENTING CREATION OF
RENEWAL YEAR - REWRITE OF POLICY REQUIRED TO RENEW.
** DAYS OF WEEK FOR ARRAY                                              ***
MOTUWETHFRSASUMOTUWETHFRSA
