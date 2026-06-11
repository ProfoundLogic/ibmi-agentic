     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H/TITLE WTPOLDTL - Policy Detail (Profound)
     H DFTNAME(WTPOLDTL)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
      //===============================================================*
      //  Created   : 02/07/12                                         *
      //  Programmer: C:065                                            *
      //  Project   : WC2116                                           *
      //===============================================================*
      // Revisions:                                                    *
      //
      // 12/17/25 C:349 Support: 120177 (UBIC)
      //                Add date check for adoption of 3 digit rates.  *
      //                If the policy is not effective on/after the    *
      //                date entered, or there is no date, then the    *
      //                payroll rates will be calculated to 2 digits.  *
      //                                                               *
      // 09/03/25 C:333 Support: 119615 (SCS)                          *
      //                Sapiens - PRA field empty.                     *
      //                                                               *
      // 06/19/25 C:65  Project: 146WKR (Phw#119246)                   *
      //                Add popup alerting user collateral change will *
      //                affect entire policy.                          *
      //                                                               *
      // 05/28/25 C:333 Support: PHW#118908 (Vital)                    *
      //                BO - Noted Issues / Data Set Request           *
      //                - When policy effective date changed, effective*
      //                date in WAQTP should match with changed date.  *
      //                                                               *
      // 03/06/25 C:291 Project: SY250                                 *
      //                Disable Experience Rating field in Inquiry mode*
      //                Display file WTPOLDTL:Changed Disabled Property*
      //                Indicator on scExpRat from DspMod to Inquiry.  *
      //                                                               *
      // 10/18/24 C:65  Support: Phw#117783 (Vital)                    *
      //                Apply billing plan disabling to bill on eff    *
      //                day checkbox.                                  *
      //                                                               *
      // 07/22/24 C:079 Support: Ticket #117017                        *
      //                Set choices 1-3 to omit inactive codes         *
      //                                                               *
      // 08/05/24 C:65  Project: WC2951                                *
      //                Add javascript to OnChange event on experience *
      //                rating dropdown (display file change) to       *
      //                default the report to NCCI checkbox as checked *
      //                when rating changes to '1' or '2' value.       *
      //                                                               *
      // 06/24/24 C:333 Project: 146WKR                                *
      //                Collateral Processing.                         *
      //                Dspf : rcsAddInfo Changes                      *
      //                -Adding CntCollateralInfo, Onload event.       *
      //                                                               *
      // 06/17/24 C:344 Project: WC2952                                *
      //                Increase Rate field to zone 8.3.               *
      //                                                               *
      // 06/12/24 C:062 Project: 146WKQ                                *
      //                Add Bill assessments for S/R.                  *
      //                                                               *
      // 05/09/24 C:300 Project: WC2433, Support: 115562               *
      //                Expand Endorsement Forms grid to show more rows*
      //                                                               *
      // 05/02/24 C:333 Support: 111739 (modification)                 *
      //                Only display error "Self-report not allowed    *
      //                after cancellation Date" on a cancelled policy *
      //                otherwise it doesn't allow to enter Reporting  *
      //                period on active policy.                       *
      //                                                               *
      // 05/01/24 C:300 Project: WC2433, QA testing issues             *
      //                Pass WDELP rrn of State Reporting Division,    *
      //                while building URL for W1054R02 (Endorsement   *
      //                Form Additional Info), referred from WTPOLTWRP.*
      //                So that Add'l Info data in WLEDP file is created
      //                with correct division number                   *
      //                                                               *
      // 04/16/24 C:300 Project: WC2433, Support: 115407               *
      //                scCO# is part of every klists and its getting  *
      //                cleared in procOther() on Clearing rcdEndAdd   *
      //                Fix: renaming scCO# to scComp# in rcdEndAdd    *
      //                                                               *
      // 04/12/24 C:344 Project: PHW115407                             *
      //                The Tab Index for the 'Agency History' Tab     *
      //                was affected by the recent addition of the     *
      //                'Endorsement Forms' Tab. The Tab Index for     *
      //                'Agency History' was incremented from 5 to 6.  *
      //                                                               *
      // 03/22/24 C:300 Project: WC2433                                *
      //                Toolbar: Policy Endorsement Forms/CS Standards *
      //                Added rcdEndFrm, rcdEndAdd, rcdEndCpy and      *
      //                rcdEndInfo in DSPF                             *
      //                                                               *
      // 03/04/24 C:079 Project: WC2951                                *
      //                Add Exp Rating and Report to NCCI fields       *
      //                                                               *
      // 10/25/23 C:340 Project: WC2922                                *
      //                Add/Remove Invoices based on contested flag    *
      //                and Division is either not summarized OR       *
      //                its Summarized and state reporting.            *
      //                                                               *
      //                                                               *
      // 10/20/23 C:65  Support: Phw#114397 (Sunz)                     *
      //                Clear ANC checkbox disable variable so its     *
      //                on status does not stick between divisions.    *
      //                                                               *
      // 10/16/23 C:340 Project: 153WAO                                *
      //                Add Prior Policy Number.                       *
      //                Only for Client # 153.                         *
      //                                                               *
      // 07/30/23 C:062 Project: WC2918                                *
      //                Add not rated logic for exp mod 2 and 3        *
      //                                                               *
      // 06/15/23 C:342 Support: 107237                                *
      //                clear out the error message edit conditions    *
      //                if user exits from the sub-window for self     *
      //                reporting payrolls.                            *
      //                                                               *
      // 06/09/23 C:291 Support: 111739                                *
      //                Only display error "Self-report not allowed    *
      //                after cancellation Date" on a cancelled policy *
      //                otherwise it doesn't allow to enter Reporting  *
      //                period on active policy.                       *
      //                                                               *
      // 05/18/23 C:342 Support: 107237                                *
      //                Show error on screen for cancelled policy      *
      //                while using Self reporting payrolls process.   *
      //                If reporting date is greater than cancellation *
      //                date of policy then error will be displayed on *
      //                screen after entering reporting date/year/month*
      //                Below is the error message to display.         *
      //                "Self-report not allowed after cancellation    *
      //                Date".                                         *
      //                                                               *
    // 03/23/23 C:65  Support: Phw#107697 (Sunz) - WC2302            *
    //                Pass elRRN to VendorPEdit procedure call to    *
    //                prevent error displaying window on edit mode.  *
    //                                                               *
      // 08/29/22 C:342 Support: 103419                                *
      //                Payroll Class Code Descriptions Not Displaying *
      //                in Payroll Grid                                *
      //                                                               *
    // 06/23/22 C:65  Support: Phw#91924   (Sunz)                    *
    //                When updating all policies/divisions check the *
      //                maximum schedule db/cr allowed for state.      *
      //                                                               *
      // 04/11/22 C:333 Project: SY434                                 *
      //                - Update Grids for Sorting Filtering Upgrade   *
      //                                                               *
      // 03/01/22 C:65  Project: WC2819 (Phw#101215/74814)             *
      //                Revert back the coding to call SRPREM for      *
      //                non-CareWest policies. They have special       *
      //                coding to use net rate from wmepp instead of   *
      //                calculating them for the reporting period.     *
      //                                                               *
      // 01/19/22 C:33  Project: WC2837                                *
      //                Set the net rate flag based on group settings  *
      //                 and not whether the payroll is calculated.    *
      //                                                               *
      // 10/28/21 C:300 Support: 99094 (InHouse)                       *
      //                Information showing at the bottom of the screen*
      //                where Large Deductibles aren't valid           *
      //                Only DSPF changes -- Added visibility indicator*
      //                on Claim Details Panel in rcdLrgDed record fmt *
      //                                                               *
      // 09/13/21 C:300 Support: 97658 (PNIC-SCS)                      *
      //                WMLDP-Large Detuctible table issue             *
      //                When a policy/quote is set up with a large deductible
      //                billing plan and it is then switched to a non-deductible plan,
      //                the large deductible record should be deleted. *
      //                                                               *
      // 09/09/21 C:295 Project: WC2302                                *
      //                Auto Tab Closure on Deletion                   *
      //                                                               *
      // 08/02/21  C:298 Project: WC2820                               *
      //                 Add Valen on Analytics Tab                    *
      //                                                               *
      // 04/21/21 C:300 Support: 95299 (FMIC)                          *
      //                Increased Limits Midterm Endorsement Error     *
      //                "00" is a valid value and load its description *
      //                                                               *
      // 04/06/21 C:300 Support: 94604 (SUNZ)                          *
      //                Policy rating screen allows old deductible     *
      //                descriptions to be selected.                   *
      //                DSPF Changes on Deductible Discount Code 1 & 2 fields
      //                1) Increased the length of DDchoices to 500 (from 80)
      //                2) Changed the DataBase File to (WTDDP D1)     *
      //                                                               *
      // 03/19/21 C:295 Support: 94885                                 *
      //                Correct the error message for Prorate or       *
      //                Short rate.                                    *
      //                                                               *
      // 02/02/21  C:136 Project: WC2757                               *
      //                 Add checkbox and text area for                *
      //                 Blanket Waiver Description                    *
      //                                                               *
      // 09/02/20  C:180 Project: WC2730                               *
      //                 Add Include Medical/Indemnity Claims          *
      //                 deductible option to RcdLrgDed screen.        *
      //                                                               *
      // 06/09/20  C:298 Project: SY420                                *
      //                 Mask SSN                                      *
      //                                                               *
      // 06/17/20 C:65  Project: WC2447                                *
      //                Do not show data saved popup in reviesd bill.  *
      //                                                               *
      // 05/04/20 C:33  Project: 133WRW                                *
      //                Add 'Analytics' tab                            *
      //                                                               *
      // 03/03/20 C:180 Project WC2702                                 *
      //                Increase Payroll grid size to 12               *
      //                                                               *
      // 02/11/20 C:180 Project WC2702                                 *
      //                Move to V20R1 by disabling Estimated button    *
      //                                                               *
      // 01/23/20 C:65  Support: 86879 (LUBA)                          *
      //                If 3C verbiage is not checked to be overridden *
      //                then clear the fields in employer detail.      *
      //                                                               *
      // 12/16/19 C:180 Project: 150WEH                                *
      //                Allow SR payrolls as Audited Payrolls          *
      //                                                               *
      // 10/22/19 C:295 Project: 81720                                 *
      //                To Prevent Mass Update Across Policies on      *
      //                Combined PEO Group.                            *
      //                                                               *
      // 10/21/19 C:180 Project: WC2702                                *
      //                Add button for estimated payroll input grid.   *
      //                                                               *
      // 02/06/19 C:062 Ticket: 74814(136)                             *
      //                Take into account net rate.                    *
      //                                                               *
      // 02/08/19 C:33  Project: SY406                                 *
      //                Add 'Rating Questions' tab                     *
      //                                                               *
      // 01/29/19 C:291 Project: WC2206                                *
      //                Add Exp Mod 1 Not Rated hidden screen field    *
      //                to rcdRating2 and populate.                    *
      //                                                               *
      // 01/22/19 C:298 Support: PHW 76288                             *
      //                Added a tooltip on rcdRating2 in WTPOLDTL      *
      //                display file over fields SCDCDE,SCDCD2         *
      //                                                               *
      // 01/16/19 C:267 Support: 76243                                 *
      //                Revise Choice DB File selection criteria in the*
      //                display file to use an RPG bound field for RTCD.
      //                Add logic to determine where Rate Set code     *
      //                values will be retrieved from - WMGRP or WMRSP.*
      //                                                               *
      // 01/08/19 C:33  Project: SY407                                 *
      //                Add score information                          *
      //                                                               *
      // 11/29/18 C:136 Project: 150WDI                                *
      //                Calculate S/R premium to the nearest penny     *
      //                (Sunz only)                                    *
      //                                                               *
      // 11/07/18 C:136 Project: 150WDK                                *
      //                Sunz wants terrorism calculated on non-large   *
      //                deductible policies only.                      *
      //                                                               *
      // 07/17/18 C:079 Support: 150 69713                             *
      //                If max records (9999) are loaded to sfl give   *
      //                a message.                                     *
      //                                                               *
      // 07/13/18 C:180 Support: 50007                                 *
      //                When limits change on a mid-term endorsement,  *
      //                and prior limits were STD, report the STD      *
      //                limits in the BEFORE section of the            *
      //                ENDORSE form.                                  *
      //                                                               *
      // 07/12/18 C:291 Project: 148WAV                                *
      //                Turn off Access to Deviation when flagged in   *
      //                WDRRP.                                         *
      //                                                               *
      // 05/31/18 C:062 Project: 133WQN                                *
      //                Add Suppress Premium Notice in Mass Billing    *
      //                                                               *
      // 05/29/18 C:136 Support: 70680                                 *
      //                Retrofit 69830 from V18R1 to fix S/R prem calc *
      //                and various other issues.                      *
      //                                                               *
      // 05/16/18 C:291 Project: 150WCB                                *
      //                Added hidden field scOvrMP rcdRating for       *
      //                for PEO Master/MCP override.                   *
      //                                                               *
      // 05/02/18 C:33  Support: 70670                                 *
      //                Save current year informaiton when changed     *
      //                                                               *
      // 04/13/18 C:136 Support: 69830                                 *
      //                Correct premium calculation on S/R grid        *
      //                                                               *
      // 04/10/18 C:33  Project: 150WCF  (Moved to V18R2)              *
      //               -For PEO groups, only allow Payroll Vendors to  *
      //                 be updated for Emp 1 Div 0                    *
      //                                                               *
      // 03/16/18 C:65  Support: 63144 (RMLA)                          *
      //                Allow safety fee entries if there is a heading *
      //                (for amount) or a premium level (for percent). *
      //                Do not constantly default the safety fees      *
      //                since they may not be desired on policy.       *
      //                                                               *
      // 02/28/18 C:136 Support: 64554                                 *
      //                Print to S/R Subfile if only one S/R record    *
      //                exists. Also, exclude terrorism from S/R       *
      //                premium calculation for Sunz                   *
      //                                                               *
      // 02/06/18 C:291 Support: 63953                                 *
      //                Change Division dropdown "max choices" property*
      //                to 100,000. Screen change only.                *
      //                                                               *
      // 02/01/18 C:062 Support: 664115(146)                           *
      //                Don't load sr grid if no sr payrolls.          *
      //                                                               *
      // 01/23/18 C:267 Support: 63896                                 *
      //                Schedualed rating tab only available for CSI.  *
      //                                                               *
      // 01/18/18 C:291 Support: 50657                                 *
      //                Changed field Aggregate Amount SCLDAMG1 under  *
      //                Large Deductible tab to "Show Zero Balance".   *
      //                DSPF change only.                              *
      //                                                               *
      // 01/08/18 C:136 Support: 50628                                 *
      //                Remove accidental lock on WMEML3 left by       *
      //                work on 45968                                  *
      //                                                               *
      // 12/20/17 C:291 Support: 50647                                 *
      //                Change Vendor Number field on Payroll Vendors  *
      //                grid to not use 1000 Separator on Vendor Number*
      //                Change to DSPF only. No programming changes.   *
      //                                                               *
      // 10/13/17 C:33  Support: 50254                                 *
      //                Default large deductible info on add mode      *
      //                                                               *
      // 09/08/17 C:65  Project: 150WAR                                *
      //                Display info edit if % of manual premium value *
      //                differs from employer 1 division 0 for group.  *
      //                                                               *
      // 08/15/17 C:062 Project: 150WAP                                *
      //                Add combined peo processing.                   *
      //                                                               *
      // 08/10/17 C:180 Project: 150WAV/150WBL - Client 150 only.      *
      //                Add deductible aggregate %.                    *
      //                Add variable aggregate Flag.                   *
      //                                                               *
      // 08/03/17 C:65  Project: 150WAR                                *
      //                Add a text box for % of manual premium to be   *
    //                displayed for client 150 only (both regular    *
      //                screen and popup window).                      *
      //                                                               *
      // 08/01/17 C:291 Project WC2259                                 *
      //               -Add Employee Payroll Filter                    *
      //                                                               *
      // 07/28/17 C:267 Project WC2259                                 *
      //               -Added INMODE var to add/edit procedure for     *
      //                Employee Payroll                               *
      //               -Change "Schedule Rating Information" to        *
      //                "Schedule Rating" tab heading                  *
      //                                                               *
      // 07/27/17 C:062 Project 150WAL                                 *
      //                Use EPARAT instead of PRRT.                    *
      //                                                               *
      // 07/24/17 C:267 Project WC2259                                 *
      //                Added new Employee Payroll grid to Payroll     *
      //                (Self Reporting) tab.                          *
      //                                                               *
      // 04/07/17 C:062 Project 136WHF                                 *
      //                Add Deviation #2 field for CareWest.           *
      //                                                               *
      // 04/03/17 C:306 Support: 45968                                 *
      //                Added logic to update the inception date, if   *
      //                user changes the Policy effective date range.  *
      //                                                               *
      // 03/14/17 C:306 Support: 46014                                 *
      //                Added logic to grayed out the Safety fee       *
      //                and membership fee, if setup is not available  *
      //                at group level in rating and statistics.       *
      //                                                               *
      // 09/21/16 C:307 Support: 43452                                 *
      //                Added logic to total the calculated premium.   *
      //                                                               *
      // 08/17/16 C:298 Project: WC2070                                *
      //                Added logic to populate Audit Non Compliance   *
      //                check box over Additional info tab.            *
      //                                                               *
      // 06/01/16 C:298 Phaseware: 41499                               *
      //                Display file change only. Change text          *
      //                to "Data successfully saved" for rcdSave       *
      //                                                               *
      // 04/27/16 C:306 Project: WC2424                                *
      //                Added a logic to populate the Experience       *
      //                Modifier- Not final status flag value.         *
      //                                                               *
      // 02/01/16 C:136 Support: 31832                                 *
      //                Don't display Update All Division window       *
      //                unless an updated value is actually changed    *
      //                                                               *
      // 01/15/16 C:136 Phaseware: 39182                               *
      //                Ensure DARTS pull correct policy in            *
      //                Revised Billing                                *
      //                                                               *
      // 01/07/16 C:65  Project: WC2401                                *
      //                Change the coding done under WC2254 to call an *
    //                API instead of directly calling the various    *
      //                programs for windows here.                     *
      //                                                               *
      //    09/17/15 C:302 Project: WC2254                             *
      //                   Add a framework to be able to call any      *
      //                   Programn as a popup window rather than as   *
      //                   a new tab.                                  *
      //                                                               *
      // 08/25/15 C:302 Project: WC2385                                *
      //                Perform a check at the end of the revised bill *
      //                when a user attempts to submit the revised     *
      //                bill to check if the live files have been      *
      //                updated since the revised bill started.        *
      //                                                               *
      // 09/24/15 C:308 Support : 32186                                *
      //                Self Reporting Entry not allow for Future Month*
      //                                                               *
      // 05/12/15 C:306 Phaseware: 29738 LUBA                          *
      //                Recompile only                                 *
      //                                                               *
      // 04/16/15 C:65  Project: SY251                                 *
      //                Add API call to launch new Department          *
      //                letters.                                       *
      //                                                               *
      // 04/09/15 C:242 Project: 132WQS                                *
      //                CompSuite Retrofit of Custom Code to Base      *
      //                Turn off Deviation#1 when RRST = NM.           *
      //                                                               *
      // 03/30/15 C:290 Support: 33886                                 *
      //                Added a hidden field scPFDT3 in WTPOLDTL       *
      //                display file to capture the effective date of  *
      //                the state reporting division.Changed JavaScript*
      //                associated with Experience Rating Modifiers.   *
      //                Added logic to populate the rate information   *
      //                effective dates with current div's effective   *
      //                date. (contains DSPF changes of WTPOLDTL)      *
      //                                                               *
      // 12/16/14 C:295 Redmine: 4645                                  *
      //                To make the drop down selection choice to pick *
      //                the exact valid list discount flag for user.   *
      //                                                               *
      // 12/02/14 C:180 Support: WA129 87376                           *
      //                Populate endorsement from date in CYYMMDD      *
      //                format.
      //                                                               *
      // 10/14/14 C:295 RED 4645                                       *
      //                If Large Deductible processing is not allowed  *
      //                then not perform edit of Large Deductible tab. *
      //                                                               *
      // 10/08/14 C:291 RED 3161                                       *
      //                Change the date definition on the grid from    *
      //                Number to date data type.                      *
      //                                                               *
      // 09/15/14 C:283 Support: WA099 86748                           *
      //                Changed the width of the Reporting MMYYYY field*
      //                on the display file. Re-Compile Only.          *
      //                                                               *
      // 08/20/14 C:136 Support: WA133 85107                           *
      //                Reset WMEMP and WDELP in SpreadFlag subroutine *
      //                to ensure selected division is being processed *
      //                for midterm endorsements                       *
      //                                                               *
      // 07/28/14 C:295 Project: 114WMD                                *
      //                Compilation only as the changes are in         *
      //                Action Panel                                   *
      //                                                               *
      // 07/11/14 C:279 Project: 111WBL                                *
      //                Added dividend amount field.                   *
      //                                                               *
      // 07/09/14 C:136 Support: WA129 86141                           *
      //                ARAP charges aren't saving to WDELP            *
      //                                                               *
      //  06/05/14 C:65  Project: 136WFZ (Redmine #4276)               *
      //                 Recalculate the commission multiplier unless  *
      //                 the program is in display mode - to match     *
      //                 the coding in W0051R02.                       *
      //                                                               *
      // 04/24/14 C:298 Support: WA136 85424                           *
      //                Added logic to enable Payment Plan (Billing    *
      //                Schedule Code) saved across all divisions.     *
      //                                                               *
      // 04/03/14 C:279 Redmine: 4088                                  *
      //                Change 'Commission Mod Factor' label to        *
      //                'Schedule Debit/Credit'.                       *
      //                                                               *
      // 04/02/14 C:279 Redmine: 4096                                  *
      //                Populate Stock Discount heading based on State *
      //                Reporting/Rating setup.                        *
      //                                                               *
      // 03/20/14 C:291 Redmine: 3801                                  *
      //                Fixed Type of Policy field heading to display  *
      //                with a golden asterisk as required.            *
      //                                                               *
      // 03/11/14 C:279 Project: 129WSB  Priss#: 27                    *
      //                Add validation to 'Revised Bill' process for   *
      //                EFT only on Billing plan change process.       *
      //                                                               *
      // 02/10/14 C:290 Support: WA147 84590                           *
      //                Changed logic so that producer change is       *
      //                allowed even if the commision exists. And      *
      //                restricts user from leaving agency history     *
      //                tab, if more than one records are active.      *
      //                                                               *
      // 02/28/14 C:65  Project: WC1454  Priss#:46                     *
      //                When deleting a record from WXUAP because the  *
      //                update is invalid for the division delete all  *
      //                the updates associated with that field as      *
      //                well.  An update to any of the fields will     *
      //                trigger an update to happen.                   *
      //                Also, only delete an update if there is an     *
      //                error and the update is not clearing the       *
      //                field.  There is no error in clearing a field  *
      //                that does not apply to a division.             *
      //                                                               *
      // 02/06/14 C:279 Project: 129WSB  Priss#: 27                    *
      //                Add validation to 'Revised Bill' process for   *
      //                EFT only.                                      *
      //                                                               *
      // 02/05/14 C:65  Project: SY336 Priss# 45 (display file change) *
      //                Add a blank option to premium level for large  *
      //                deductible so a choice has to be made to       *
      //                populate the field - and that blank is not     *
      //                mixed up with manual.                          *
      //                Additionally, fix the edit around the call to  *
      //                edit large deductible info (blank <> *off).    *
      //                                                               *
      //  01/20/14 C:279 Project: WC1454  Priss#:46                    *
      //                 Modified to update only applicable divisions. *
      //                                                               *
      //  01/13/14 C:190 Project: 136WFZ                               *
      //                 Add checkbox "Exclude from primary/secondary  *
      //                 agent rate" on rcdRating and "Commission Mod  *
      //                 Multiplier" field on rcdRating2 for           *
      //                 client = 136.                                 *
      //                                                               *
      // 01/08/14 C:65  Project: 25WSS                                 *
      //                Fix javascript in stabilization factor text    *
      //                box so it properly triggers effective date     *
      //                defaulting (display file change only).         *
      //                                                               *
      // 01/06/14 C:65  Project: SY266 Redmine:3838                    *
      //                Move file definition for the 11/29/13 change   *
      //                as it was erronously inserted between the      *
      //                group master file and its data structure.      *
      //                                                               *
      // 11/29/13 C:290 Support: 83322  WA146                          *
      //                Modified to populate Contested Audit Flag      *
      //                correctly in Audit Log File.                   *
      //                                                               *
      // 11/14/13 C:279 Project: WC1454  Priss#:45                     *
      //                Modified to update all divisions               *
      //                                                               *
      // 10/07/13 C:190 Project: 25WSS                                 *
      //                - From the Policy Year Detail/ Rating Tab      *
      //                  replace "Deviation #2" with  Stabilization   *
      //                  Factor                                       *
      //                - Add Claims Liability and Claims Allowance    *
      //                  fields to the Policy Year Detail/Large       *
      //                  Deductible Tab.                              *
      //                                                               *
    // 10/07/13 C:279 Project: WC1454  Priss#:44                     *
    //                Modified to display pop-up correctly.          *
    //                                                               *
    // 09/02/13 C:279 Project: WC1454                                *
    //                Added missing changes for project WC1454.      *
    //                                                               *
    // 08/07/13 C:33  Redmine: 3508                                  *
    //                Do not display inactive rates.                 *
    //                                                               *
    // 07/24/13 C:267 Support: Redmine #1180 (Haifa Kealy)           *
    //                UI - Added two new columns - Agency Name and   *
    //                Producer Name.                                 *
    //                                                               *
      // 07/12/13 C:180 SUPPORT  82623 WA114                           *
      //                Populate Billing Auxiliary upon return         *
      //                from prompt.                                   *
    //                                                               *
      // 05/29/13 C:283 Support: 82425 WA146                           *
      //                Do not process the Large Deductible Edits if   *
      //                LD Module is not installed.                    *
      //                                                               *
    // 05/27/13 C:136 Project: 147WAI                                *
    //                Add Dividend Plan Processing                   *
    //                                                               *
      // 05/13/13 C:283 Support: 82302  WA099                          *
      //                Transform - Payroll Grid not Updating after    *
      //                Audit Payroll is completed. Reload the Payroll *
      //                grid always before running the Audit Payroll.  *
      //                                                               *
      // 05/08/13 C:189 Support: 81376  WA102                          *
      //            283 Endorsements not printing properly. Endorsement*
      //                record needs to be created.                    *
      //                                                               *
      // 04/30/13 C:180 SUPPORT WA114 82203                            *
      //                Fix parms for BillAuxiliary                    *
      //
    // 04/26/13 C:279 Redmine: 2801                                  *
    //                Modified to load Self Reporting detail grid    *
    //                for selected row only.                         *
    //                                                               *
    // 04/08/13 C:65  Project: 46WGO                                 *
    //          C:136 Add a message beside S/R payroll grid if any   *
    //                owners/officers have an ownership % and are    *
    //                included (base).                               *
    //                                                               *
    // 03/12/13 C:279 Project: SY266 Redmine:2456                    *
    //                Policy Year Detail/Rating - Deductible Bill on *
    //                Cancelled Check Box changed to Radio Button.   *
    //                                                               *
    // ------------------------------------------------------------- *
    //  NOTE: Please add comments to the top of Revisions.           *
    // ------------------------------------------------------------- *
    //===============================================================*

     fwtpoldtl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(PayrollSFL:rrnPayroll)
     f                                     Sfile(ExcludeSFL:rrnExclude)
     f                                     Sfile(ElementSFL:rrnElement)
     f                                     Sfile(sflSelfRpt:rrnSRRAudt)
     f                                     Sfile(sflSrGrid1:rrnSrGrid1)
     f                                     Sfile(sflSrGrid2:rrnSrGrid2)
     f                                     Sfile(sflVendorP:rrnVendorP)
     f                                     Sfile(agencySFL:rrnAgency)
     f                                     Sfile(sflEmpPayr:rrnEmpPayr)
     f                                     Sfile(sflQuest:rrnQuest)
      //***  Begin Add  ***  05/04/20  *******************************
     f                                     Sfile(sflDrivers:rrnDrivers)
      //***  End   Add  ***  05/04/20  *******************************
      *** Beg Add *** 03/22/24 *** *************************************
     f                                     Sfile(sflEndFrm:rrnEndFrm)
      *** End Add *** 03/22/24 *** *************************************
      *** Beg Add *** 06/24/24 *** *************************************
     f                                     Sfile(CollateSFL:rrnCollate)
      *** End Add *** 06/24/24 *** *************************************
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdPolPnl)
     f                                     include(rcdQtePnl)
     fwpipolpop cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdAgtMsg)

     FWDELP     IF   E             DISK    rename(PEL01:PEL0P)
     FWMEMP     IF   E             DISK    rename(PEM01:PEM0P)
     FWDELL     UF   E           K DISK    rename(PEL01:PEL00)
     F                                     infds(dbfds)
     FWDELL1    UF   E           K DISK    rename(PEL01:PEL01)
     FWDELL5    iF   E           K DISK    rename(PEL01:PEL05) prefix(L5)
     F                                     infds(dbfdsl5)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     FWDF4L     IF   E           K DISK    rename(PF401:PF400)
     FWMRPL     UF A E           K DISK    rename(PRP01:PRP00)
     F                                     infds(dbfds8)
     FWMRPL3    IF   E           K DISK    rename(PRP01:PRP03)
     FWMGCL     IF   E           K DISK    rename(PGC01:PGC00)
     FWDCXL1    IF   E           K DISK    rename(PCX01:PCX01)
     FWMFNL     IF   E           K DISK    rename(PFN01:PFN00)
     F                                     infds(dbfds11)
     Fwmaul2    uf   e           k disk    rename(pau01:pau02)
     fwmdvl     uf a e           k disk    rename(pdv01:pdv00)
     fwdefl1    if   e           k disk
     fwdell8    if   e           k disk    rename(pel01:pel08) prefix(E2:2)
     fwdell1q   if   e           k disk    rename(pel01:pel0q) prefix(E3:2)
     fwxual     if a e           k disk    rename(pua01:pua00)
     fwxual2    uf   e           k disk    rename(pua01:pua02) prefix(U1:2)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)
     FWDF1L2    UF   E           K DISK    rename(PF101:PF102) prefix(L2)
     FWMRSL     IF   E           K DISK    rename(PRS01:PRS00)
     FWDRRL     IF   E           K DISK    rename(PRR01:PRR00)
     FWTTBL     IF   E           K DISK    rename(PTB01:PTB00)
     FWTAVL     IF   E           K DISK    rename(PAV01:PAV00)
     FWTRTL1    IF   E           K DISK    rename(PRT01:PRT01)
     FWMPFL     IF   E           K DISK    rename(PPF01:PPF00)
     FWMMGL     IF   E           K DISK    rename(PMG01:PMG00)
     FWMLDL1    UF A E           K DISK    rename(PLD01:PLD01)
     F                                     infds(dbfds9)
     FWMLDL2    UF   E           K DISK    rename(PLD01:PLD02)
     FWMLDL4    UF   E           K DISK    rename(PLD01:PLD04)
     FWTLML1    IF   E           K DISK    rename(PLM01:PLM01)
     FWTDDL2    IF   E           K DISK    rename(PDD01:PDD02)
     FWMEPL     IF   E           K DISK    rename(PEP01:PEP00)
     f                                     infds(dbfds2)
     FWMEPL1E   IF   E           K DISK    rename(PEP01:PEP01E)
     FWMEPP     IF   E             DISK    rename(PEP01:PEP0P)
     Fwmepl9    if   e           k disk    Rename(pep01:pep09) Prefix(E9:2)
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPR01)
     FWMPXL1    IF   E           K DISK    rename(PPX01:PPX01)
     FWDXSL     IF   E           K DISK    rename(PXS01:PXS00)
     f                                     infds(dbfds3)
     FSTSTL3    IF   E           K DISK    rename(PST01:PST03)
     FWMEXL     IF   E           K DISK    rename(PEX01:PEX00)
     FWDFHL     IF   E           K DISK    rename(PFH01:PFH00)
     FWTABL     IF   E           K DISK
     F                                     ignore(PRT01)
     FWTGTL     IF   E           K DISK    rename(pgt01:PGT00)
     FWTGIL     IF   E           K DISK    rename(pgi01:PGI00)
     FWMCMLC    IF   E           K DISK    rename(PCM01:PCM0C)
     FWDDDL1    IF   E           K DISK    rename(PDD01:PDD01)
     FWMEAL6    IF   E           k disk    rename(PEA01:PEA06)
     FWMEAL10   IF   E           k disk    rename(PEA01:PEA10)
     FWMEALC    IF   E           K DISK    rename(PEA01:PEA0C)
     FWMGRL     IF   E           K DISK    rename(PGR01:PGR00)
     FWMDSL3    IF   E           K DISK    rename(PDS01:PDS03)
     FWDVHL     IF   E           K DISK    rename(PVH01:PVH01)
     f                                     infds(dbfds4)
     FWASRP     IF   E             DISK    rename(PSR01:PSR0P)
     FWASRL3    IF   E           K DISK    rename(PSR01:PSR03)
     FWASRL6E   IF   E           K DISK    rename(PSR01:PSR06E)
     f                                     infds(dbfds5)
     FWASRL9    IF   E           K DISK    rename(PSR01:PSR09)
     f                                     infds(dbfds6)
     FWMVPL     IF   E           K DISK    rename(PVP01:PVP01)
     FWMAHP     IF   E             DISK    rename(PAH01:PAH0P)
     FWMAHL     IF   E           K DISK    rename(PAH01:PAH00)
     F                                     infds(dbfds7)
     FWMARL     IF   E           K DISK    rename(PAR01:PAR00)
     FWDHEL     IF   e           K DISK    rename(PHE01:PHE01)
     FWMBXP     IF   e           K DISK    rename(PBX01:PBX0P)
     FWAQTL7    IF   E           K DISK    rename(PQT01:PQT07)
     FWTQSL1    IF   E           K DISK    rename(PQS01:PQS01)
     FWMRFL     UF A E           K DISK    rename(RFP01:RFP00)
     F                                     infds(dbfds10)
     FWMUPL     IF   E           K DISK    rename(PUP01:PUP00)
     FWMOOL     IF   E           K DISK    rename(POO01:POO00)
     FWMENL3    IF A E           K DISK    rename(PEN01:PEN03)
     FWMDXL     IF   E           K DISK    rename(PDX01:PDX00)
     FWTMYP     IF   E             DISK
     FWDF2L     IF   E           K DISK    rename(PF201:PF200)
     FWMEEL1    IF   E           K DISK
     f                                     infds(dbfds12)
     FSSYLF     IF   E           K DISK
     F                                     ignore(SSY01)
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
     fwmeml3    uf   e           k Disk    rename(pem01:pem03)
     f                                     prefix(l3_)
      *** Beg Add *** 03/22/24 *** *************************************
      * Policy Endorsement Form File
     fwmmulb    if   e           k disk    rename(pmu01:pmu0b)
     f                                     infds(dbfds16)
     fwmmup     uf a e             disk    rename(pmu01:pmu0p)
     f                                     infds(dbfds17)
      * Endorsement Form Master Table File
     fwtenl     if   e           k disk    rename(pen01:pen00)
     f                                     prefix(en_)
      * Endorsement Form Data File
     fwledl     uf a e           k disk    rename(ped01:ped00)
      * Waiver Informstion File
     fwmwvl1    uf a e           k disk
      * Forms Data File
     fwxfml11   uf a e           k disk
      *** End Add *** 03/22/24 *** *************************************
      //*** Beg Add ***** 05/28/25 ************************
     fwaqtl12   uf a e           k disk    rename(PQT01:PQT12)
      //*** End Add ***** 05/28/25 ************************

      /include SCOPYPOLPR

     D PSDS          ESDS                  extname(SZQ1P)
     D dbfds           ds
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D dbfds3          ds
     D  dbrrn3               397    400b 0
     D dbfds4          ds
     D  dbrrn4               397    400b 0
     D dbfds5          ds
     D  dbrrn5               397    400b 0
     D dbfds6          ds
     D  dbrrn6               397    400b 0
     D dbfds7          ds
     D  dbrrn7               397    400b 0
     D dbfds8          ds
     D  dbrrn8               397    400b 0
     D dbfds9          ds
     D  dbrrn9               397    400b 0
     D dbfds10         ds
     D  dbrrn10              397    400b 0
     D dbfds11         ds
     D  dbrrn11              397    400b 0
     D dbfds12         ds
     D  dbrrn12              397    400b 0
     D dbfdsl5         ds
     D  dbrrnl5              397    400b 0
      *** Beg Add *** 03/22/24 *** *************************************
     D dbfds16         ds
     D  dbrrn16              397    400b 0
     D dbfds17         ds
     D  dbrrn17              397    400b 0
      *** End Add *** 03/22/24 *** *************************************
     D***  Begin Add  ***  05/04/20  *****************************
     DdsWIGSP        e ds                         extname(WIGSP) qualified inz
     D***  End   Add  ***  05/04/20  *****************************
      ***** Beg Add ***** 08/02/21 ***********************************
     DdsWIVSP        e ds                         extname(WIVSP) qualified inz
      ***** End Add ***** 08/02/21 ***********************************

     D STA             S             75    DIM(11) CTDATA PERRCD(1)             Stat data inf msg
      //
     D                 DS
     D  LN                     1    825
     D                                     DIM(11)                              SCR03 LINE OUTPUT
     D  SCDS01                 1     75
     D  SCDS02                76    150
     D  SCDS03               151    225
     D  SCDS04               226    300
     D  SCDS05               301    375
     D  SCDS06               376    450
     D  SCDS07               451    525
     D  SCDS08               526    600
     D  SCDS09               601    675
     D  SCDS10               676    750
     D  SCDS11               751    825
      //
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D  QCFND                  4      6  0

     D                 DS
     D  xxData                 1     80
     D  xxType2                1      1
     D  xxCo#2                 2      4
     D  xxUser2                5     14
     D  xxTime2               15     20
     D  xxDate2               21     28
     D  ##Outq2               29     38

     D                 DS
     D  tmpDate                1      6
     D  tmpDate1               1      2
     D  tmpDate2               3      6

     D                 DS
     D  scSrGr1Rpt             1     10
     D  tmpDate3               1      2
     D  tmpDate4               3      3
     D  tmpDate5               4      5
     D  tmpDate6               6      6
     D  tmpDate7               7     10

     D                 DS
     D  tmpDate_1              1      8
     D  tmpDate1_1             1      2
     D  tmpDate2_1             3      4
     D  tmpDate3_1             5      8

     D                 DS
     D  reason                 1     50
     D  reason1                1     25
     D  reason2               26     50

      // Employee SS#
     D                 DS
     D  EmpSS#_ds              1      9
     D  ESS1                   1      3
     D  ESS2                   4      5
     D  ESS3                   6      9
      //
     D                 DS
     D  XXEXMD                 1      5
     D  XXEXM1                 1      1
     D  XXEXM4                 2      5

     D                 DS
     D  XXWKDT                 1      8
     D  XXWKMM                 1      2
     D  XXWKDD                 3      4
     D  XXWKCC                 5      6
     D  XXWKYY                 7      8
     D  XXWKCY                 5      8

     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *** Beg Add *** 03/22/24 *** *************************************
     D  ldForm                41     55
      *** End Add *** 03/22/24 *** *************************************

     D ASCTXT          C                   CONST('ACTIVITY STATUS CODE CHANGED')
     D ASCTX1          C                   CONST('Activity Status Changed from')
     D ASCTX2          C                   CONST(' by user ')

     DarrTabNames      s            100    varying inz dim(20)
     DisPortal         s              1n   inz
     DisShowRatingQuest...
     D                 s              1n   inz('0')
     D***  Begin Add  ***  05/04/20  *****************************
     DisShowAnalytics...
     D                 s              1n   inz('0')
     D***  End   Add  ***  05/04/20  *****************************
     d xxRrn           s              9  0
     d elRRN           s              9  0
     d epRRN           s              9  0
     d eeRRN           s              9  0
      //** Beg add ***** 09/09/21 ********************************
     d WrkRRN          s              9  0 export
      //** End add ***** 09/09/21 ********************************
     d srRRN           s              9  0
     d vhRRN           s              9  0
     d xsRRN           s              9  0
     d xxAHRRN         s              9  0
     d srFltDt         s              8  0
     d srFltDt1        s              6  0
     d dateFlt         s              8  0
     d savePayRRN      s              9  0
     d qteAgtMode      s              1
     d agtMode         s              1
     D dspMode         s               n   inz(*off)
     D qteMode         s               n   inz(*off)
     D quoteMode       s              1    inz(' ')
     D inMode          s              1    inz(' ')
     D WrkName         s                   like(scEmpPNam)
     D BtnFiltCnl      s               n   inz(*off)
     D BtnFiltApl      s               n   inz(*off)
     D clientID        s              3    inz('000')
     D process         s               n   inz(*off)
     D xpEMP#          s                   like(ELEMP#)
     D xpDIV           s                   like(ELDIV)
     D svBsdt          s                   like(ELBSDT)
     D svBscd          s                   like(ELBSCD)
     D svASRM          s                   like(EXASRM)
     D svRLFL          s                   like(ELRLFL)
     D s2RLFL          s                   like(ELRLFL)
     D*** begin add ***  06/19/25  *************************************
     D numDivs         s              5  0 inz(0)
     D*** end add *****  06/19/25  *************************************
     D manPremium      s             11  0 inz(0)
     D totPremium      s             11  0 inz(0)
     D terrPayroll     s             15  4 inz(0)
     D ratingState     s              2
     D qqrrn           s                   like(ELRRN)
     D qqrpmy          s              6  0
     D qqtodt          s              8  0
     D qqmapr          s                   like(totPremium)
     D qqadld          s                   like(totPremium)
     D qqpayr          s                   like(totPremium)
     D qqprem          s                   like(totPremium)
     D qqtax           s                   like(totPremium)
     D totTax          s                   like(totPremium)
     D xxEmrrn         s              9  0
     D tempCalTot      s             11  0 inz(0)
     d qqPPrm          s             11  0
     d qqPTax          s             11  0
     d qqFact          s            143
     d qqPrev          s             11  2
     d qqSrOn          s              1
     d qqInfo          s             39
     d qqHdg           s            270
     d qqPro           s            100
     d qqPPrm2         s             11  2
     d qqPTax2         s             11  2
     d qqFact2         s             48
     d totPremium2     s             11  2
     d totTax2         s             11  2

     D clearWMLDP      s              1n
     D first           s              1n   inz(*on)
     DdeductInstalled  s              1n
     D flgRcdUpdated   s              1
     D achFlg          s              1n
     D vndrFl          s              1n
     D xsKey           s              2
     D xxSt            s              2
     D xxPxSt          s              2
     D xxType          s              1
     D compName        s             40
     D fundName        s             40
     D secType         s              1
     D secLevel        s              3  0
     D inQnm           s             10
     D inLib           s             10
     D inFldl          s              5  0
     D inData          s             80
     D inKL            s              1
     D inoutNum        s             80
     D inEdtMsk        s             10
     D isLargeDedRcd   s              1n
     D frequency       s             10
     D schdTyp         s              1
     D svDate          s              5  0
     D pos#            s              3  0
     D svDate1         s              7  0
     D svDoWk          s              2
     D  dspAllDiv      s              1n   inz
     D  dspAllPol      s              1n   inz
     D  isCltFlg       s              1n   inz
     D  isFirst        s              1n   inz(*off)
     D  isChgFlag1     s              1n   inz
     D  isChgFlag2     s              1n   inz
     D  intEmp#        s                   like(UAemp#)
     D  intDiv         s                   like(UAdiv)
     d intRrn#         s              9  0
     d intFctr         s              5  4
     D  s2user         s                   like(ENrusr)
     D  s2date         s                   like(EMefd1)
     D  s2time         s                   like(ELtime)
     D  svFyr          s                   like(ELfyr)
     D  svPfDt         s                   like(ELpfdt)
     D  svPtDt         s                   like(ELptdt)
     D  svArSm         s                   like(ELarsm)
     D  svIfFl         s                   like(ELiffl)
     D  svQTyp         s                   like(ELqtyp)
     D  svAdvr         s                   like(ELadvr)
     D  s@RlFl         s                   like(ELrlfl)
     D  svStat         s                   like(ELstat)
     D  svIlCd         s                   like(ELilcd)
     D  svIlEf         s                   like(ELilef)
     D  svIlEx         s                   like(ELilex)
     D  svDcDe         s                   like(ELdcde)
     D  svDcEf         s                   like(ELdcef)
     D  svDcEx         s                   like(ELdcex)
     D  svCxBl         s                   like(ELcxbl)
     D  svAsp2         s                   like(ELasp2)
     D  svDvEf         s                   like(ELdvef)
     D  svD1S2         s                   like(ELd1s2)
     D  svD1D2         s                   like(ELd1d2)
     D  svD1X2         s                   like(ELd1x2)
     D  svD2S2         s                   like(ELd2s2)
     D  svD2D2         s                   like(ELd2d2)
     D  svD2X2         s                   like(ELd2x2)
     D  svDvEx         s                   like(ELdvex)
     D  svAsp1         s                   like(ELasp1)
     D  svCrEf         s                   like(ELcref)
     D  svCrEx         s                   like(ELcrex)
     D  svAspm         s                   like(ELaspm)
     D  svMcEf         s                   like(ELmcef)
     D  svMcEx         s                   like(ELmcex)
     D  svMcCd         s                   like(ELmccd)
     D  svMcGp         s                   like(ELmcgp)
     D  svDev2         s                   like(ELdev2)
     D  svStfc         s                   like(ELstfc)
     D  svD2Ef         s                   like(ELd2ef)
     D  svD2Ex         s                   like(ELd2ex)
     D  svExM1         s                   like(ELexm1)
     D  svEmD1         s                   like(ELemd1)
     D  svExM2         s                   like(ELexm2)
     D  svEmD2         s                   like(ELemd2)
     D  svExM3         s                   like(ELexm3)
     D  svEmD3         s                   like(ELemd3)
     D  svAsPs         s                   like(ELasps)
     D  svScEf         s                   like(ELscef)
     D  svScEx         s                   like(ELscex)
     D  svSrPr         s                   like(ELsrpr)
     D  svAsP3         s                   like(ELasp3)
     D  svDisc         s                   like(ELdisc)
     D  svProm         s                   like(ELprom)
     D  svDbCr         s                   like(ELdbcr)
     D  svSdEf         s                   like(ELsdef)
     D  svSdEx         s                   like(ELsdex)
     D  svCxDy         s                   like(ELcxdy)
     D  svPp3c         s                   like(ELpp3c)
     D  svPpC3         s                   like(ELppc3)
     D  svMFee         s                   like(ELmfee)
     D  svSfPc         s                   like(ELsfpc)
     D  svSfam         s                   like(ELsfam)
     D  svSfmn         s                   like(ELsfmn)
     D  svAddm         s                   like(ELaddm)
     D  svAnDt         s                   like(ELandt)
     D  svDcd2         s                   like(ELdcd2)
     D  sv2DEf         s                   like(EL2def)
     D  sv2DEx         s                   like(EL2dex)
     D  svInl2         s                   like(ELinl2)
     D  svI2Ef         s                   like(ELi2ef)
     D  svI2Ex         s                   like(ELi2ex)
     D  svExMf         s                   like(ELexmf)
     D  svWvTp         s                   like(ELwvtp)
     D  svWvFl         s                   like(ELwvfl)
     D  svWvPc         s                   like(ELwvpc)
     D  svWMin         s                   like(ELwmin)
     D  svFChg         s                   like(ELfchg)
     D  svWMax         s                   like(ELwmax)
     D  svASts         s                   like(ELascd)
     D  svCoFl         s                   like(ELcofl)
     D  svTypB         s                   like(ELtypb)
     D  svPFin         s                   like(ELpfin)
     D  svCtSt         s                   like(ELctst)
     D  svExCf         s                   like(EFexcf)
     D  svExCo         s                   like(EFexco)
     D  svScBd         s                   like(EFscbd)
      *** Begin Add *** 02/02/21 ************************************
     D  svBlnkWd       s                   like(ELblnkWd)
      *** End   Add *** 02/02/21 ************************************
     D  qqUser         s             10
     D  qqDate         s              7  0
     D  qqTime         s              6  0
     D  qqMode         s              1
     D  qqCxFl         s              1
     D  qqName         s             50
     D  qqEdn          s             50
     D  qqMpo          s                    like(FNpeof)
     D  #TotDivM       s              5  0
     D  xxNtUpd        s              5  0
     D  xxName         s             40
     D  xxEDN          s             80
      *** Beg Add *** 03/22/24 *** *************************************
     d isAttResp       s               n   inz
     d xxovr           s              1    inz
     d svEndFrm        s             15    inz
     d saveDT4         s                   like(MUdt)
     d saveTM4         s                   like(MUtime)
     d charMsg         s            100
     d strErrMsg       s            256    inz
     d svrrnEndFrm     s              9  0 inz
     d inInclude       s             10
     d AddInfoPgm      s             10    inz
     d strUrlTabName   s             50    inz
     d ##kc            s              1
     d ##kl            s              1
     d ##mod           s              1
     d ##rrn1          s              9
     d ##rrn2          s              9
      *** beg add ***** 05/01/24 **************************
     d ##stdivrrn      s              9    inz
      *** end add ***** 05/01/24 **************************
     d ##env           s              1
     d ##rtn           s              1
     d ##EndFrm        s             15
     d ##attach        s              1
     d chrAddPgm       s             10
     d chrPrtPgm       s             10
     d chrState        s              2    Inz('TX')
     d chrWvBlkPrjDs   s            500
     d chrWvrForm      s             15
     D xxco#           s              3    inz
     D xxfnd           s              3    inz
     D xxemp#          s              9    inz
     D xxdiv           s              5    inz
     D xxfyr           s              3    inz
     D ##fyr2          s              3  0 inz
     d isUseScript     s              1n   inz
     d processType     c                   const('Policy Page')
     d xxkc            s              1
     d xxkl            s              1
     d xxcall          s              1
     d xxenv           s              1
     D ##wav#          s              5  0 inz
     D nxtwav#         s              5  0 inz

      // calling parameter for WtPolARInq
     d @@next          s             10
     d @@tabind        s              5
     d @@Qte2          s              1
     d @@PostDt        s              8  0
     d @@PostAmt       s             11  2
      *** End Add *** 03/22/24 *** *************************************

     D xx#Div          s                   like(ELDIV)
     D xxKey           s                   like(BSKEY)
     D saveDT          s                   like(ELDT)
     D saveTM          s                   like(ELTIME)
     D saveDT1         s                   like(ELDT)
     D saveTM1         s                   like(ELTIME)
     D saveDT2         s                   like(ELDT)
     D saveTM2         s                   like(ELTIME)
     D saveDT3         s                   like(ELDT)
     D saveTM3         s                   like(ELTIME)
     D xxDate          s                   like(ELPFDT)
     D xxDat1          s                   like(ELPFDT)
     D xxDat2          s                   like(ELPFDT)
     D xxEfDt          s                   like(ELPFDT)
     D x2EfDt          s                   like(ELPFDT)
     D x3EfDt          s                   like(ELPFDT)
     D xxDDcd          s                   like(ELDCDE)
     D xxTBID          s                   like(GTTBID)
     D xxTBCD          s                   like(TBTBCD)
     D xxFCOD          s                   like(PFFCOD)
     D xxMCGP          s                   like(ELMCGP)
     D xxMXEX          s                   like(FNMXEX)
     D xxMXCY          s                   like(X7MXCY)
     D xxArty          s              1
     D xxRate          s              3  0
     D xtotalDedB      s             14  2
     D payAddMode      s               n   inz(*off)
     D exlAddMode      s               n   inz(*off)
     D date8           s              8  0
     D reload          s               n   inz(*off)
     D error           s               n   inz(*off)
     D errorfldt       s               n   inz(*off)
     D xxErrorTab      s              5  0 inz(-1)
     D exlError        s               n   inz(*off)
     d  xxStDiv        s              3  0 inz
     d charNormPrem    s             15
     d isContribution  s              1
     d poltypcde       s              1
     d outCo#          s              3
     d outFnd#         s              3
     d outEftType      s              1    inz('E')
     d outEftKey       s             30    inz
     d outDate1        s              7
     d outPreNote      s              1
     d ##key#          s             10
     d errMsg          s            100
     D kkBscd          s                   like(ELBSCD)
     D x4Date          s                   like(ELPFDT)
     Dstr9999Msg       s            256
     Dxx9999rsp        s               n
     D isMaxEmpRcds    s               n
      //*** Beg Add *** 08/02/21 *** *************************************
     DstrScoreVendor   s             50
     DstrRptHead       s             50    inz('Valen Scoring Report')
      //*** End Add *** 08/02/21 *** *************************************
      //*** Beg Add *** 05/18/23 *** *************************************
     d xxCxDt7         s              7  0 inz
     d xxCxMYD         s              7  0 inz
      //*** End Add *** 05/18/23 *** *************************************
      //*** Beg Add *** 03/04/24 *** *************************************
        dcl-s intStRptDiv Like(ELdiv);
        dcl-s svNcci char(1);
        dcl-s isUpdNcci char(1);
      //*** End Add *** 03/04/24 *** *************************************
      //*** Beg Add *** 06/24/24 *****************************************
        dcl-s xxOcol Like(EXoCol);
        dcl-s svColwp Like(ELcolwp);
        dcl-s svColwa Like(ELcolwa);
        dcl-s svColrp Like(ELcolrp);
        dcl-s svColTyp Like(ELcolTyp);
        dcl-s svColad Like(ELcolad);
        dcl-s svCollib Like(ELcollib);
        dcl-s svColln Like(ELcolln);
        dcl-s svCollid Like(ELcollid);
        dcl-s svColled Like(ELcolled);
        dcl-s svColsic Like(ELcolsic);
        dcl-s svColsn Like(ELcolsn);
        dcl-s svColsid Like(ELcolsid);
        dcl-s svColsed Like(ELcolsed);
        dcl-s xxRepPrem Like(EPSapr);
        dcl-s isUpdColl char(1);
      //*** End Add *** 06/24/24 *****************************************
     D                 DS
     D  BS                     1    108  8
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
     D  AR                     1     90  2 DIM(10)
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
      //
     D                 DS
     D  EATRDT                 1      7  0
     D  xxTRYM                 1      5  0
     D                 DS
     D  EAPODT                 1      7  0
     D  xxPOYM                 1      5  0
     D                 DS
     D  xxxDTE                 1      7  0
     D  xxCUYM                 1      5  0
     D                 DS
     D  xxDT                   1      7  0
     D  xxYY                   1      3  0
     D  xxMM                   4      5  0
     D  xxDD                   6      7  0
     D  xxMDD                  4      7  0
     D  xxMMDD                 4      7  0
     D                 DS
     D  x7MXDT                 1      7  0
     D  x7MXCY                 1      3  0
     D  x7MXMM                 4      5  0

     d dsRating      e ds                  extname(wtpoldtl:rcdRating) inz
     d ptrRating       s               *   inz

     d dsRating2     e ds                  extname(wtpoldtl:rcdRating2) inz
     d ptrRating2      s               *   inz

     d dsAddInfo     e ds                  extname(wtpoldtl:rcdAddInfo) inz
     d ptrAddInfo      s               *   inz

     d dsLargeDed    e ds                  extname(wtpoldtl:rcdLrgDed) inz
     d ptrLargeDed     s               *   inz

     d dsRetroRate   e ds                  extname(wtpoldtl:rcdRetro) inz
     d ptrRetroRate    s               *   inz

     d dsSchedRate   e ds                  extname(wtpoldtl:rcdSched) inz
     d ptrSchedRate    s               *   inz

      // Proto-types
     d CalcSrPrem      pr                  ExtPgm('SRPREM')
     d  rrn                           9  0
     d  rpmy                          6  0
     d  todt                          8  0
     d  mapr                         11  0
     d  adld                         11  0
     d  payr                         11  0
     d  prem                         11  0
     d  tax                          11  0
     d  pprm                         11  0
     d  ptax                         11  0
     d  fact                        143
     d  prev                         11  2
     d  sron                          1
     d  info                         39
     d  hdg                         270
     d  pro                         100
     d  prem2                        11  2
     d  tax2                         11  2
     d  pprem2                       11  2
     d  ptax2                        11  2
     d  fact2                        48
      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
      // Get the LCM factor
     d GetLCM          pr                  extpgm('SRLCM')
     d  chrCompany#                   3
     d  chrGroup#                     3
     d  chrRateCode                   3
     d  chrClassCode                  5
     d  chrEffDate                    7
     d  outLCM                        5
     d  chrCompany#    s              3
     d  chrGroup#      s              3
     d  chrRateCode    s              3
     d  chrClassCode   s              5
     d  chrEffDate     s              7
     d  outLCM         s              5
     d  decLCM         s              5  4

      // Get the Territorial Differential Factor
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d  outTDF         s              5  4

      // Get the Original Rate
     d**** Beg Chg **** 06/17/24 *******************************************
     d***GetOrgRate      pr             7  2
     d GetOrgRate      pr             8S 3
     d**** End Chg **** 06/17/24 *******************************************

      // Check EFT setup
     d chkEFT          pr
      // Program Start
     d ProgramStart    pr
     D processMe                       n

      // Main Processing
     d Main            pr

      *** Beg Add *** 03/22/24 *** *************************************
      // Check if "ALL STATES" Endorsement is Appropriate for Policy Coverage
     d ChkAllSt        pr                  ExtPgm('SRCKES')
     d ##co#2                         3  0
     d ##fnd2                         3  0
     d ##emp#2                        9  0
     d ##fyr2                         3  0
     d ##EndFrm                      15
     d ##attach                       1

      // Delete "Additional Information" data associated with policy page endorsement
     d DelAddInfo      pr                  ExtPgm('W1054R04')
     d ##co#2                         3  0
     d ##fnd2                         3  0
     d ##emp#2                        9  0
     d ##fyr2                         3  0
     d ##EndFrm                      15

      // Load Endorsement Form Grid
     d LoadEndFrm      pr
     d inIncludeFrm                  10    const

      * Print Policy Page Endorsement Form
     d PrintEndFrm     pr                  ExtPgm('W1054R')
     d xxkc                           1
     d xxkl                           1
     d xxcall                         1
     d xxrrn                          9  0
     d xxenv                          1

      * Process Addition of Policy Endorsement Form
     d ProcessAdd      pr

      * Process Copy Endorsement Form
     d ProcessCopy     pr

      * Process other Options for Endorsement Form Grid
     d ProcOther       pr

      * Process Performed Action from the action dropdown
     d ProcPerAct      pr

      // Re-attach Endorsement Forms
     d Reattach        pr                  ExtPgm('W1053R3')
     d xxco#                          3
     d xxfnd                          3
     d xxemp#                         9
     d xxfyr                          3
     d ##rtn                          1

     * Validate Endorsement State
     d ValEndSt        pr                  ExtPgm('SRVAES')
     d ##EndFrm                      15
     d ##rtn                          1

     * Validate Action procedure.
     d ValidateAct     pr              n
     d strmode                        1

     * Validate Copy Policy Endorsement Form
     d ValidateCpy     pr              n

     * Validate Policy Endorsement Forms procedure.
     d ValidateEnd     pr              n
     d strmode                        1

      * Policy A/R Inquiry
     d WtPolARInq      pr                  ExtPgm('WTPOLARINQ')
     d xxrrn1                         9
     d @@mode                         1
     d @@next                        10
     d @@tabind                       5
     d @@Qte2                         1
     d @@PostDt                       8  0
     d @@PostAmt                     11  2
      *** End Add *** 03/22/24 *** *************************************

      // Load Selection fields
     d LoadSelection   pr

      // Load Policy Detail fields
     d LoadPolicyDet   pr

      // Load Payroll Grid
     d LoadPayrollGrd  pr

      // Load Self Reporting Payroll Grid.
     d LoadSrRptPrGrd  pr

      // Load Self Reporting Payroll Grid.
     d LoadSrRptGrd3   pr

      // Load Payroll Vendors.
     d LoadVendorPGrd  pr

      // Load Employee Payroll
     d LoadEmpPayrGrd  pr

      // Employee Payroll Filter Popup
     d DspEmpPayFlt    pr

      // Employee Payroll Filter
     d EmpPayFilter    pr

      // Clear Employee Payroll Filter
     d ClrEmpPayFlt    pr

      // Policy Detail Error Check
     d PolicyCheck     pr

      // Payroll Filter Popup
     d PayrollFilter   pr

     D RcdAddInfoDsp   pr
     D  indValue                      1n   const

     D RcdPayrollDsp   pr
     D  indValue                      1n   const

     D RcdRateChgDsp   pr
     D  indValue                      1n   const

     D RcdAgencyDsp    pr
     D  indValue                      1n   const

      // S/R Payroll Filter Popup
     d SelfRptFilter   pr

      // Load Excluded States Grid
     d LoadExcludeGrd  pr

      // Load Agency Grid
     d LoadAgencyGrid  pr

      *** Beg Add ***** 06/24/24 ***************************
      // Load Collateral Information Grid
     d LoadCollateral  pr
      *** End Add ***** 06/24/24 ***************************

      // Check if endorsment record needs to be written
     d SaveEndorse     pr

      // Save Policy Detail fields
     d SavePolicyDet   pr

      // Get Manual Premium
     d getManPrem      pr            11  0
     d   outNetRated                   n
      //
     d getHarborExp    pr                  like(HELHNF)
      //
     d getLossCost     pr             5  4
     d  class                              like(SRCLAS)
     d  effDt                              like(SRTODT)

      // Clear all Errors
     d ClearErrors     pr

     DAcceptAgg        pr             1n
     D inEffDt                             like(scLDEfd1) const
     D inAgg                               like(LDAMG1)   const

      // Effective Date
     d SrEffd          pr             7  0
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d inFYR                          3  0 const
     d inEMP#                         9  0 const
     d inDIV                          5  0 const
     d inDate                         7  0
     d inType                         1    options(*nopass)
     d inClas                         5  0 options(*nopass)
      // SReffd API prototype
     d SReffdAPI       pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5
     d ComAPI          pr                  extpgm('WA001R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##Conm                      40
     d   ##Fnam                      40
     d   ##Lvl                        3  0
     d   ##Type                       1

     d callW1551C      pr                  extpgm('W1551C')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##QUE                       10
     d   ##QTE                        1

     d WA029R          pr                  extpgm('WA029R')
     d   ##KL                         1

     d WA003R          pr                  extpgm('WA003R')
     d   ##Num                       80
     d   ##EdtMsk                    10

     d QSNDDTAQ        pr                  extpgm('QSNDDTAQ')
     d   XXQNM                       10
     d   XXLIB                       10
     d   XXFLDL                       5  0
     d   XXDATA                      80

     DWriteDarts       PR
     D  dkey                         40
     D  comm                        256
     D  rmcd                         10

     d AgtInfo         pr                  extpgm('WA002R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##EMP#                       9  0
     d   ##DIV                        5  0
     d   ##FYR                        3  0
     d   ##AGT#                       5  0
     d   ##ANAM                      60
     d   ##PROD                       5  0
     d   ##PNAM                      60
     d EDNInfo         pr                  extpgm('WA005R')
     d ##Emp#                         9  0
     d ##Div                          5  0
     d ##Name                        40
     d ##Edn                         80

     d w0051r2         pr                  extpgm('W0051R2')
     d qqUser                        10
     d qqDate                         7  0
     d qqTime                         6  0
     d qqMode                         1
     d qqCxFl                         1
     d qqEmp#                              like(ELemp#)
     d qqFnd                               like(ELfnd)
     d qqDiv                               like(ELdiv)
     d qqName                        50
     d qqEdn                         50
     d qqMpo                               like(FNpeof)
     d qq#Div                              like(ELDiv)
     d #TotDivM                       5  0
     d xxNtUpd                        5  0
     d strMsg          s            256
     d outNetRated     s               n   Inz(*Off)
     d*** Begin Add *** 06/12/24 *************************************
     d chrBsCd         s             10
     d chrBillSchedType...
     d                 s              1
     d*** End   Add *** 06/12/24 *************************************

     d GetDev1CrWst    pr                  extpgm('WA060R')
     d intRrn#                        9  0
     d intFctr                        5  4
      // Check if effective dates are same for active agents
     d CheckAgtEfDt    pr
     d CheckCallStack  pr                  extPgm('SRCHKPGM')
     d pgmInStack                    10    const
     d inStack                        1n

       //*** Beg Add *** 10/16/23 *** 153WAO ***************************
     d CallWA069R      pr                  extpgm('WA069R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##EMP#                       9  0
     d   ##FYR                        3  0
     d   ##RFlg                       1
       //*** End Add *** 10/16/23 *** 153WAO ***************************

     D/copy sPrDate
     D/copy sPrModule
     D***  Begin Add  ***  05/04/20  *******************************
     D/copy sPrSql
     D***  End   Add  ***  05/04/20  *******************************
     D/copy wPrAgent
     D/copy wPrProduce
     D/copy wPrGroup
     D/copy wPrRateSet
     d/copy WprEmpLr
     d/copy wprcarrier
     d/copy wprPolPop
     d/copy wprPolErr
     d/copy sPrAtrium
     d/copy sPrGenPop
     d/copy wPrDivPlan
      /copy wprBank
      /copy wprEft
     d/copy sPrLetter
     d/copy wPrPolCpy
     d*** Begin Add *** 06/12/24 *************************************
     d/copy wPrTable
     d*** End   Add *** 06/12/24 *************************************

      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
      //*** beg add ***** 02/02/21 *************************
     d/copy wPrEndorse
      //*** end add ***** 02/02/21 *************************
      //*** Beg Add *** 10/25/23 *** WC2922 ****************
     d/copy wPrPmtSvc
      //*** End Add *** 10/25/23 *** WC2922 ****************
      *** Beg Add *** 03/22/24 *** *************************************
     d/copy sPrMessage
     d/copy sPrXML
      *** End Add *** 03/22/24 *** *************************************

     d dsEpayFilter    ds                  likerec(rcdEPayFlt) inz

     d xxSDat7         s                   Like(EESDT)
     d xxEDat7         s                   Like(EEEDT)
     d xxCkDat7        s                   Like(EECKDT)
     d meetFlt         s               n
     d xxEmpFCLs2      s             13a

     D count           s              2  0
     d errAgtEfDt      s               n
      //** uncomment to time delay for debug
      //********
      //waittime        s              3    inz('120')
      //cmdstring       s            256
      //cmdlength       s             15  5
      //********          eval      cmdstring = 'dlyjob ' + waittime
      //********          eval      cmdlength = %len(%trim(cmdstring))
      //********          call(e)   'QCMDEXC'
      //********          parm                    cmdstring
      //********          parm                    cmdlength
      //** uncomment to time delay for debug
      //
      //   KEY LIST FOR THE USER PARAMETER FILE
      //
     C     keyWMUPL      Klist
     C                   Kfld                    Q1USER
      //
      // Table File Key List
      //
     C     keyWTABL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
      //
      // State File Key List
      //
     C     keySTSTL3     Klist
     C                   Kfld                    xxKey
      //
      // Activity Status File Key List
      //
     C     keyWTAVL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
      //
      // Reserve Code File Key List
      //
     C     keyWTRTL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
      //
      // Generic Table File Key List
      //
     C     keyWTGTL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
      //
     C     keyWTGIL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbid
      //
      // Type of Building File Key List
      //
     C     keyWTTBL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxTbcd
      //
      // Premium Finance Company File Key List
      //
     C     keyWMPFL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxFcod
     C                   Kfld                    xxDate
      //
     C     key1WMPFL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxFcod
      //
      // Group Printing Text File Key List
      //
     C     keyWDF4L      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxKey
     C                   Kfld                    xxDate
      //
     C     key1WDF4L     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxKey
      //
      // MCO Group File Key List
      //
     C     keyWMMGL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scMcgp
     C                   Kfld                    xxDate
      //
     C     key1WMMGL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scMcgp
      //
      // Group Master File Key List
      //
     C     keyWMFNL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
      //
      // Group Detail File Key List
      //
     C     keyWDF1L      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
      //
      // Group Carrier Information File Key List
      //
     C     keyWMGCL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
      //
      // Employer Master File Key List
      //
     C     keyWMEML      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
      //
     C     key1WMEML     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
      //
     C     keyPWMEML     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xpEMP#
     C                   Kfld                    xpDIV
      //
      // Employer Detail File Key List
      //
     C     keyWDELL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
     C     key1WDELL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
      //
     C     key2WDELL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    EMDIV
     C                   Kfld                    scFYR
      //
     C     key3WDELL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    EADIV
     C                   Kfld                    scFYR
      //
     C     keyWDELL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
      //
     C     key1WDELL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
      //
      // Employer Payroll File Key List
      //
     C     keyWMEPL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
      // Employee Payroll File Key List
      //
     C     keyWMEEL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
      // Payroll Class File Key List
      //
     C     keyWMPRL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
     C                   Kfld                    EPCLAS
     C                   Kfld                    xxEfDt

     C     key1WMPRL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
     C                   Kfld                    EPCLAS
      //
     C     keyWMPRL1_1   Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
     C                   Kfld                    SRclas
     C                   Kfld                    xxEfDt
     C     key1WMPRL1_1  Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
     C                   Kfld                    SRclas
      //
     C     key4WMPRL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
      //
      // Payroll Description File Key List
      //
     C     keyWMPXL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    EPCLAS
     C                   Kfld                    EPEXT
     C                   Kfld                    xxPxSt
     C                   Kfld                    EPEFDT
     C     key1WMPXL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    EPCLAS
     C                   Kfld                    EPEXT
     C                   Kfld                    xxPxSt
      //
     C     keyWMPXL1_1   Klist
     C                   Kfld                    scCO#
     C                   Kfld                    SRclas
     C                   Kfld                    SRext
     C                   Kfld                    xxPxSt
     C                   Kfld                    SRefdt
     C     key1WMPXL1_1  Klist
     C                   Kfld                    scCO#
     C                   Kfld                    SRclas
     C                   Kfld                    SRext
     C                   Kfld                    xxPxSt

      // Large Deductible File Key List
      //
     C     keyWMLDL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
      // Deductible Detail File Key List
      //
     C     keyWDDDL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
      // Agency History File Key List
      //
     C     keyWMAHL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
      // Agent Rate File Key List
      //
     C     keyWMARL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    xxRate
     C                   Kfld                    AHEfdt
      //
     C     key1WMARL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    xxRate
      //
      // Quote Status File Key List
      //
     C     keyWAQTL7     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scDiv
     C                   Kfld                    qteAgtMode
     C                   Kfld                    AHAgt#
      //
      // A/R Detail File Key List
      //
     C     keyWMEAL10    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    xxArty
      //
     C     keyWMEAL6     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
     C     keyWMEALC     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
      //
      // Scheduled Rating File Key List
      //
     C     keyWMRFL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
      //  CARRIER/STATE REPORTING MASTER FILE
      //
     C     keyWMRPL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      //
     C     keyWMRPL3     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRPCAP#
      //
      // Cancellation Detail File Key List
      //
     C     keyWDCXL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
      //
      // Group Rate Set Code File Key List
      //
     C     keyWMGRL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
      //
     C     key1WMGRL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scRTCD
      //
      //  Rate Set Code File Key List
      //
     C     keyWMRSL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
      //
     C     key1WMRSL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    ELRTCD
      //
      //  Statistical Reporting/Rating File Key List
      //
     C     keyWDRRL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDate
      //
     C     key1WDRRL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
      //
      //  Increased Limits File Key List
      //
     C     keyWTLML1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxKey
     C                   Kfld                    xxDate
      //
     C     key1WTLML1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxKey
      //
     C     key2WTLML1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
      //
      //  Deductible Discount File Key List
      //
     C     keyWTDDL2     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDDcd
     C                   Kfld                    xxDate
      //
     C     key1WTDDL2    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDDcd
      //
     C     key2WTDDL2    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
      //
      //  Stock Discount File Key List
      //
     C     keyWMDSL3     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    scDISC
     C                   Kfld                    xxDate
      //
     C     key1WMDSL3    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    scDISC
      //
      // Excluded States File Key List
      //
     C     keyWDXSL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scFYR
      //
      // Admin/Darts File Key List
      //
     C     keyWMEXL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
      //
      // Flexible Heading File Key List
      //
     C     keyWDFHL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
      //
      //  Claims Master
      //
     C     keyWMCMLC     KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    scFND
     C                   KFLD                    scEMP#
     C                   KFLD                    scDIV
     C                   KFLD                    xxDate
      //
     C     key1WMCMLC    KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    scFND
     C                   KFLD                    scEMP#
     C                   KFLD                    scDIV
      //
      // Employer Payroll Self-Reporting Transaction File
      //

     c     keyWASRL6E    Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scFYR
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
      //
     c     keyWASRL6E_1  Klist
     c                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    svDate
      //
     c     keyWASRL9     Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scFYR
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
      //
     c     keyWASRL9_1   Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scFYR
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
     c                   Kfld                    svDate1
      //

      // Vendor History File
      //
     C     keyWDVHL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR

      // Payroll Vendor File
      //
     C     keyWMVPL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    VHvdr#

     C     keyWMOOL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV

      // Audit Log File

     C     keyWMAUL2     Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    ELemp#
     C                   Kfld                    ELdiv
     C                   Kfld                    ELfyr
      //
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
      //
     C     Dtcymdp       Plist
     C                   Parm                    Srdti
     C                   Parm      *BLANKS       Srffmt
     C                   Parm      '*CYMD'       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
      //
     C     Dtmdyyp       Plist
     C                   Parm                    Srdti
     C                   Parm      '*CYMD'       Srffmt
     C                   Parm      *BLANKS       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
      //
      // Entry of Program.
      //
     C     *ENTRY        PLIST
     C                   PARM                    @@rrn             9            wdelp
     C                   PARM                    @@mode            1            mode
     C                   PARM                    @@Next           10            next pgm call
     C                   PARM                    @@TabInd          5            tab index
     C                   PARM                    @@Qte             1            quote
     c
     C     *DTAARA       DEFINE    *LDA          SVLDA
      /FREE
       IN *LOCK *DTAARA;
       OUT *DTAARA;
      /END-FREE
      /free

       GetClientID(clientID);

       ProgramStart(process);

       If process;

          Main();

       Endif;

       *InLr = *On;
      /end-free
       //===================================================================
       // Add information to list
       //===================================================================
     PAddToList        b
     DAddToList        pi           101a   varying
     D inString                     100a   varying const
     D inArray                      100a   varying dim(20)
     D inIndex                       10i 0

     DoutString        s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     PAddToList        e

       //===================================================================
       // CheckAgtEfDt - Check agent history for matching effective dates.
       //===================================================================
     P CheckAgtEfDt    B
     D CheckAgtEfDt    PI
     D svDate          s              7  0
      /free

       // Read agent history file.
       Clear svDate;
       Setll keyWMAHL PAH00;
       Reade keyWMAHL PAH00;
       errAgtEfdt = *off;

       Dow not %eof;
          If AHSTAT = 'A';
             If AHEfdt = svDate;
                errAgtEfDt = *on;
                Leave;
             Else;
                svDate = AHEfdt;
             Endif;
          Endif;

          Reade keyWMAHL PAH00;
       Enddo;

       If errAgtEfDt = *on;
          Clear wdmsg1;
          Clear wdmsg2;
          Clear wdmsg3;
          Clear wdmsg4;
          Clear wdmsg5;

          wdmsg1 = 'Multiple active records exist with the same effective';
          wdmsg2 = 'date. This condition must be fixed prior to exiting';
          wdmsg3 = 'the agent history screen.';
          Clear agtMsgCnl;
          Clear agtMsgCon;

          If xxErrorTab <> 5;
             Dou agtMsgCnl = *on or agtMsgCon = *on;
                Exfmt rcdAgtMsg;
                error = *on;
             Enddo;
          Endif;

       Endif;

      /end-free
     P CheckAgtEfDt    E
     //================================================================
     // GetOrgRate - Get the Original Rate
     //================================================================
     P GetOrgRate      B
     D**** Beg Chg **** 06/17/24 *******************************************
     D****GetOrgRate      PI             7  2
     D GetOrgRate      PI             8S 3
     d***originalRate    s              7  2
     d originalRate    s              8S 3
     D**** End Chg **** 06/17/24 *******************************************
     d ratingState     s              2
      /free

       // Load payroll rate
       originalRate = PRrt;

       // Apply LCM
       chrCompany#  = %EditC(ELco#:'X');
       chrGroup#    = %EditC(ELfnd:'X');
       chrRateCode  = ELrtCd;
       chrClassCode = %EditC(EPclas:'X');
       chrEffDate   = %EditC(ELpfDt:'X');
       outLCM       = ' ';
       GetLCM(chrCompany#:chrGroup#:chrRateCode:chrClassCode:chrEffDate:outLCM);
       decLCM =%Dec(%Subst(outLCM:1:1) + '.' + %Subst(outLCM:2:4):5:4);
       //*** Beg Del *** 12/17/25 ******************************************
       // originalRate = originalRate * decLCM;
       //*** End Del *** 12/17/25 ******************************************

       // Apply Harbor Exposure
       ratingState = RatingSt(ELco#:ELrtCd);

       //*** Beg Add *** 12/17/25 ******************************************
       //Half-adjust to 2 decimals when adoption of 3 digit rate is not yet effective
       If RS3Date = 0 or ELpfDt < RS3Date;
          originalRate = %DecH(originalRate * decLCM:8:2);
       Else;
          originalRate = %DecH(originalRate * decLCM:8:3);
       Endif;
       //*** End Add *** 12/17/25 ******************************************
       If EPspec = 'F' And PRlhCd = 'N';
          SetGT (ELco#:ELfnd:ratingState:ELpfDt) phe01;
          ReadE (ELco#:ELfnd:ratingState)        phe01;
          If Not(%Eof(wdhel));
             originalRate = originalRate * HElhnf;
          EndIf;
       EndIf;

       Return originalRate;

      /End-Free
     P GetOrgRate      e


       //***************************************************************************//
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D processMe                       n

     DdecCount         s             10i 0
      /free

       Chain(n) keyWMUPL PUP00;

       rrnSrGr1Cr = 1;
       // Relaative Record # for employer detail.
       If q1prms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          xxRrn = 3386;
          chgMode=*on;
       Endif;
       APnlHeight = 1450;

       revBill=*off;
       scPolHdg = 'Policy Year Detail';
       // Program Mode.
       If q1prms >= 2;
          scActMode = @@mode;
          Select;
             When @@mode='D';
                dspMode=*on;
             When @@mode='A';
                addMode=*on;
             When @@mode='C';
                chgMode=*on;
             When @@mode='E';
                revBill=*on;
                chgMode=*on;
                scPolHdg = 'Bill Revision';
          Endsl;
       Endif;

       If dspMode;
          inquiry = *on;
          inquiry_ = *on;
          //*** Begin Add *** 06/12/24 *************************************
          inqSRAs   = *On;
          inqSRAs2  = *On;
          //*** End   Add *** 06/12/24 *************************************
          inquiry_A = *on;
          inquiry_D = *on;
          inquiry_R = *on;
          inquiry_S = *on;
       //*** Begin Add *** 06/12/24 ****************************************
       Else;
          inqSRAs   = *Off;
          inqSRAs2  = *Off;
       //*** End   Add *** 06/12/24 ****************************************
       Endif;

       scActRrn = xxRrn;
       scActCltid = clientId;
       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             //*** Beg Chg *** 04/12/24 *** PHW115407 *********************
             //***If tabIndex < 0 or tabIndex > 5;
             If tabIndex < 0 or tabIndex > 6;
             //*** End Chg *** 04/12/24 *** PHW115407 *********************
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       // Quote Environment
       If q1prms >= 5;
          quoteMode = @@qte;
          Select;
             When @@qte='Q';
                qteMode=*on;
                scPolHdg = 'Quote Year Detail';
          Endsl;
       Endif;
       scEnv = quoteMode;
       scRevMode = ' ';
       If revBill = *On;
         scEnv = 'E';
         scRevMode = 'E';
       Endif;

       //  Only display audit payroll button when not in quote and not in revised bill.
       auditPay = *on;
       If revBill = *on or qteMode = *on;
         auditPay = *off;
       Endif;

       // Default payroll filter to Active only.
       scPayFStat = 'A';

       If qteMode;
          qteAgtMode = 'Q';
       Else;
          qteAgtMode = 'W';
       Endif;

       visible102 = *off;
       If clientID = '102';
          visible102 = *on;
       Endif;
       visible150 = *off;
       If clientID = '150';
          visible150 = *on;
       Endif;
       visible136 = *off;
       If clientId = '136';
          visible136 = *on;
       Endif;
       If clientID = '046';
          visible046 = *on;
       else;
          visible046 = *off;
       endif;
       If clientID = '111';
          dspDivInfo = *off;
       Else;
          dspDivInfo = *on;
       Endif;
       If ValidDivClient = *on;
          visDivPlan = *on;
       else;
          visDivPlan = *off;
       endif;
       //*** Beg Add ***** 06/24/24 ************************
       If ClientID = '146';
          visCollate = *On;
          dspColGrid = *On;
          If dspDivInfo = *On and visDivPlan = *Off;
             scContTop = 690;
          Else;
             scContTop = 890;
          Endif;
       Else;
          visCollate = *Off;
       Endif;
       //*** End Add ***** 06/24/24 ************************
       sysDt7 = DateTo7(SystemDate);
       sysDt8 = DateTo8(SystemDate);

       isPortal = IsModEnabled('Portal Module');

       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;

       // Process only if valid employer detail record found.
       If %found;

          scCo#  = ELCO#;
          scFnd  = ELFND;
          scEmp# = ELEMP#;
          scDiv  = ELDIV;
          scFyr  = ELFYR;


          kkDiv = ELDIV;
          kkFyr = ELFYR;

          Clear xxKey;
          PyrlSlfRpt = *Off;
          xxKey = %editc(elbscd : 'X');
          Chain(n) keyWTABL PBS01;
          If BSsefl <> 'E';
             PyrlSlfRpt = *On;
          Endif;

           isContribution = GetPrmContFlg(ELco#:ELfnd);

          Exec sql
           select count(*) into :decCount
            from WIAPPANS
            where APco# = :ELco# and APfnd = :ELfnd and APfyr = :ELfyr
             and APemp# = :ELemp#;
          isShowRatingQuest = decCount > 0;
          // It was decided that this would only be on location master
          //  until we allow access for every year.
          isShowRatingQuest = *off;
          //***** Beg Chg ***** 08/02/21 ********************************
          //***  Begin Add  ***  05/04/20  ******************************
          //Exec sql
           //select count(*) into :decCount
            //from WIGSP g1
            //full join WIGSPDRV g2
             //on g1.GSCO#=g2.GSCO# and g1.GSFND=g2.GSFND
              //and g1.GSEMP#=g2.GSEMP# and g1.GSFYR=g2.GSFYR
            //where (g1.GSco# = :ELco# and g1.GSfnd = :ELfnd
             //and g1.GSfyr = :ELfyr and g1.GSemp# = :ELemp#)
            //or (g2.GSco# = :ELco# and g2.GSfnd = :ELfnd
             //and g2.GSfyr = :ELfyr and g2.GSemp# = :ELemp#);
          //isShowAnalytics = decCount > 0;

          isShowAnalytics = *off;
          strScoreVendor = GetGrScoringVnd(ELco#:ELfnd);
          If strScoreVendor <> *blanks;
             isShowAnalytics = *on;
          Endif;
          //***  End   Add  ***  02/08/19  ******************************
          //***** End Chg ***** 08/02/21 ********************************
          //*** Beg Add ***** 03/04/24 ********************************
          // "Report to NCCI" is only visible if there are no NCCI states
          // on this policy.  Read though WDELP and check each state.
          visRptNCCI = *on;
          Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
          Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
          Dow not %Eof(WDELL8);
             Chain (E2co#:E2rtCd) prs00;
             If %Found(WMRSL);
                Chain (E2co#:'NCCI':RSjrSt) pgt00;
                If %Found(WTGTL);
                   visRptNcci = *off;
                   Leave;
                Endif;
             Endif;
             Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
          Enddo;

          //Selection for Experience Rating dropdown
          scExrtSel = 'GTCO# = ' + %char(scCO#) +
           ' AND GTTBID = ''EXRT'' AND GTSTAT = ''A''';
          //*** End Add ***** 03/04/24 ********************************
          //*** begin add ***  06/19/25  *************************************
          If clientID = '146';
             Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
             Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
             Dow not %Eof(WDELL8);
                numDivs += 1;
                Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
             Enddo;
          Endif;
          //*** end add *****  06/19/25  *************************************

          //*** Beg Add ***** 03/22/24 ********************************
          isUseScript = UseScriptura(processType);

          // Choices for Action Dropdown
          If @@Mode = 'D';
             ChoSelAct = ' ';
          Else;
             // ChoSelAct = '--- Select List Action ---,Copy,Delete,Print,+
             //  Change Include/Exclude';
             ChoSelAct = '--- Select List Action ---';

             // Check Security for Copy Endorsement Forms
             If CheckSecLevel('EndFrm_Cpy':UPtype:UPlvl);
                ChoSelAct = %trim(ChoSelAct) + ',Copy';
             EndIf;

             // Check Security for Delete Endorsement Forms
             If CheckSecLevel('EndFrm_Del':UPtype:UPlvl);
                ChoSelAct = %trim(ChoSelAct) + ',Delete';
             EndIf;

             // Check Security for Print Endorsement Forms
             If CheckSecLevel('EndFrm_Prt':UPtype:UPlvl);
                ChoSelAct = %trim(ChoSelAct) + ',Print';
             EndIf;

             // Check Security for Change Include/Exclude
             If CheckSecLevel('EndFrm_Cie':UPtype:UPlvl);
                ChoSelAct = %trim(ChoSelAct) + ',Change Include/Exclude';
             EndIf;
          Endif;
          scSelAct = '--- Select List Action ---';
          //*** End Add ***** 03/22/24 ********************************
          //*** Beg Add ***** 05/01/24 ********************************
          // Get WDELP rrn of State Reporting Division
          intStRptDiv = GetStRptDv(scCo#:scFnd:scEmp#);
          Chain (scCo#:scFnd:scEmp#:intStRptDiv:scFyr) pel05;
          If %found(WDELL5);
             ##stdivrrn = %Editc(dbrrnl5:'X');
          Endif;
          //*** End Add ***** 05/01/24 ********************************

          processMe = *on;
          Return;

       Else;

          processMe = *off;
          Return;

       Endif;
      /end-free
     P ProgramStart    E

       //***************************************************************************//
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI
     d begDate         s              8  0
     d bscd            s              3  0
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Que         s             10
     d   ##Qte         s              1
     dn1               s             10i 0 inz(1)
     dstrTabNames      s           2000    varying inz
      /free

       //*  Default payroll detail to 1st record.
       savePayRRN = 1;
       dspExcCtl  = *on;
       ratPopup = *off;
       ratPop102 = *off;
       visPop150 = *off;
       lrgPopup = *off;
       retPopup = *off;
       addPopup = *off;
       schdPopup = *off;
       GetClientID(clientID);
       strTabNames += AddToList('Rating':arrTabNames:n1);
       strTabNames += AddToList('Payroll':arrTabNames:n1);
       strTabNames += AddToList('Additional Information':arrTabNames:n1);
       //*** Beg Add *** 03/22/24 *** *************************************
       If qteMode = *on;
          strTabNames += AddToList('Waiver Endorsements':arrTabNames:n1);
       Else;
          strTabNames += AddToList('Endorsement Forms':arrTabNames:n1);
       Endif;
       //*** End Add *** 03/22/24 *** *************************************
       strTabNames += AddToList('Large Deductible':arrTabNames:n1);
       Select;
       When clientID = '114';
          scRatingL  = 'Retrospective/Retention Rating';
          strTabNames += AddToList(scRatingL:arrTabNames:n1);
       Other;
          strTabNames += AddToList('Retrospective Rating':arrTabNames:n1);
          scRatingL  = 'Retrospective Rating Information';
       Endsl;
       strTabNames += AddToList('Agency History':arrTabNames:n1);
       Select;
       When clientID = '114';
          strTabNames += AddToList('Scheduled Rating':arrTabNames:n1);
       Endsl;
       If isShowRatingQuest;
          strTabNames += AddToList('Rating Questions':arrTabNames:n1);
       Endif;
       //***  Begin Add  ***  05/04/20  ************************************
       If isShowAnalytics;
          strTabNames += AddToList('Analytics':arrTabNames:n1);
       Endif;
       //***  End   Add  ***  05/04/20  ************************************
       tbNmRcdPnl = strTabNames;

       isDefault  = *On;
       isStFc     = *Off;
       isCareWest = *Off;
       isFactor   = *Off;

       Select;
       // Use TASB Stabilisation Factor
       When clientId = '025';
          isStFc    = *On;
          isDefault = *Off;
          isFactor   = *On;
       // Use CareWest Territory Rating
       When  clientId = '136';
          isCareWest = *On;
          isDefault  = *Off;
          isFactor   = *On;
       EndSl;
       If isContribution = '1';
          flxMinFct = 'Minimum Contribution Factor';
          flxMaxFct = 'Maximum Contribution Factor';
          flxBasFct = 'Basic Contribution Factor';
          flxExcFct = 'Excess Loss Contribution Factor';
       Else;
          flxMinFct = 'Minimum Premium Factor';
          flxMaxFct = 'Maximum Premium Factor';
          flxBasFct = 'Basic Premium Factor';
          flxExcFct = 'Excess Loss Premium Factor';
       Endif;

       //*** Beg Add *** 10/16/23 *** 153WAO ***************************
       If clientId = '153';
          ShowPPol = *On;
       Else;
          ShowPPol = *Off;
       EndIf;
       //*** End Add *** 10/16/23 *** 153WAO ***************************

       //*  Load screen fields
       LoadActionBar();
       LoadPolicyDet();

       //*  Process until exit button is pressed.

       DoU exitNow = *On;

          //*  Process policy detail screen.

          If scFFLA = 'Y';
             disbPP3C = *off;
          Else;
             disbPP3C = *on;
          Endif;

            CheckAgtEfDt();
            If errAgtEfDt = *on;
               //*** Beg Chg *** 04/12/24 *** PHW115407 *******************
               //***TabIndex = 5;
               TabIndex = 6;
               //*** End Chg *** 04/12/24 *** PHW115407 *******************
            Endif;
          Exsr ExfmtScreens;

          ClearErrors();

          //Perform actions on screen.
          Select;

             //* Exit the program
             When exitNow = *on;
                Leave;

          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
             btnShowWin = *off;

             //* Do nothing - Catch tab click event
             When tabClick=*On;
               tabClick = *Off;

             //* Menu Choices
             When scSelOpt <> *blanks;
               If revBill = *On;
                 PopOneBtn('This action is not allowed while processing revise+
                  d bill':'Error');
                  scSelOpt = *Blanks;
               ElseIf errAgtEfDt = *on;
                  //*** Beg Chg *** 04/12/24 *** PHW115407 ************
                  //***TabIndex = 5;
                  TabIndex = 6;
                  //*** Beg Chg *** 04/12/24 *** PHW115407 ************
                  @@Next = 'WTPOLDTL';
               Else;
                @@Next = scSelOpt;
                Leave;
               Endif;

             //*  Check if new division or policy year has been selected.
             When kkFyr <> scFyr or kkDiv <> scDiv;
                If errAgtEfDt = *on;
                   //*** Beg Chg *** 04/12/24 *** PHW115407 ***********
                   //***TabIndex = 5;
                   TabIndex = 6;
                   //*** End Chg *** 04/12/24 *** PHW115407 ***********
                   kkFyr = scFyr;
                   kkDiv = scDiv;
                Else;
                scFyr = kkFyr;
                scDiv = kkDiv;
                Chain(n) keyWDELL PEL00;
                If not %found;
                   Setgt key1WDELL PEL00;
                   Readpe(n) key1WDELL PEL00;
                   scFyr = ELFYR;
                   kkFyr = ELFYR;
                   kkDiv = ELDIV;
                Endif;

                xxRRN = dbrrn;

                If q1prms >= 1;
                   @@rrn = %editc(dbrrn: 'X');
                Endif;

                savePayRRN = 1;
                //*  Load screen
                LoadActionBar();
                LoadPolicyDet();
                Endif;


             //*  Call billing auxiliary program if billing type selected
             When auxSelect = *on;
                If scBSDT <> 0;
                   begDate = scBSDT;
                Else;
                   begDate = scPFDT;
                Endif;
              bscd = BillAuxiliary(scBSTY:begDate:scPTDT:dbrrn11);
                If bscd <> 0;
                   kkBsCd = bscd;
                   Exsr validEFT;
                   If error = *off;
                   scBSCD = bscd;
                   Else;
                      clear scBSTY;
                   Endif;
                 else;
                Clear scBSTY;
                Endif;
                auxSelect = *off;
             // Validate Payment plan for EFT info
             When plnSelect = *on;
                plnSelect = *off;
                kkBsCd = scBsCd;
                Exsr validEFT;

             //*  Filter Payroll button.
             When filterPay = *on;
                PayrollFilter();
                filterPay = *off;
                If  scPayFStat <> 'A';
                   payFiltOn  = *on;
                Endif;
                LoadPayrollGrd();

             //*  Filter Payroll button.
             When clearPayFt = *on;
                clearPayFt = *off;
                payFiltOn  = *off;
                scPayFStat = 'A';
                LoadPayrollGrd();

             //*  Add new payroll record.
             When addPayroll = *on;
                   elRRN = dbRRN;
                   epRRN = 0;
                   PayrollEdit(epRRN:scRTCD:elRRN);
                   addPayroll = *off;
                   LoadPayrollGrd();

             //*  Add new Final Audit payroll record.
             When addAgent = *on;
                  elRRN = dbRRN;
                  xxAHRRN = 0;
                  agtMode = 'A';
                  Exsr AgentPopup;
                  addAgent = *off;
                  LoadAgencyGrid();

             //*  Add new Final Audit payroll record.
             When BtnAddPyrl = *on;
                  elRRN = dbRRN;
                  epRRN = 0;
                  FinAudtEdit(epRRN:scRTCD:elRRN);
                  BtnAddPyrl = *off;

             When BtnAddPyrl = *on;
                  elRRN = dbRRN;
                  epRRN = 0;
                  FinAudtEdit(epRRN:scRTCD:elRRN);
                  BtnAddPyrl = *off;

             //*  Add new Final Audit payroll record.
             When BtnEstPyrl = *on;
                  elRRN = dbRRN;
                  epRRN = 0;
                  EstPyrEdit(epRRN:scRTCD:elRRN);
                  BtnEstPyrl = *off;

             //*  Add/Edit S/R Payroll button.
             When BtnSrAdEdt = *on;
                //***** Beg Add ***** 06/15/23 ********************************
                Clear rcdSrrFilt;
                //***** Beg Add ***** 06/15/23 ********************************
                Clear scPst1;
                Dou BtnFiltCnl = *on or errorfldt = *off;
                    If frequency <> 'Weekly';
                       monthlyDt = *on;
                       weeklyDt = *off;
                       schdTyp = ' ';
                    Else;
                       monthlyDt = *off;
                       weeklyDt = *on;
                       schdTyp = 'W';
                    Endif;

                    //***** Beg Add ****** 09/09/21 *************************
                       scPF1rrn = dbrrn;
                    //***** End Add ****** 09/09/21 *************************
                    Exfmt rcdSrrFilt;

                    If BtnFiltCnl <> *on;
                       Exsr Errckc;

                       If errorfldt = *off;
                          elRRN = dbRRN;
                          srRRN = 0;
                          srFltDt = dateFlt;
                          SelfRptEdit(srRRN:scRtCd:elRRN:srFltDt:schdTyp:
                                      scLateRpt);
                          LoadSrRptPrGrd();
                          LoadPayrollGrd();
                          BtnSrAdEdt = *off;
                       Endif;
                    Endif;
                Enddo;

             //*  Add new excluded state record.
             When addExclude = *on;
                   elRRN = dbRRN;
                   xsRRN = 0;
                   ExcludeEdit(xsRRN:ELrrn);
                   addExclude = *off;
                   LoadExcludeGrd();

             //*  Add new Employee Payroll record.
             When BtnEmpPAdd = *on;
                  elRRN = dbRRN;
                  eeRRN = 0;
                  inMode= 'A';
                  EmpPayEdit(eeRRN:elRRN:inMode);
                  LoadEmpPayrGrd();
                  BtnEmpPAdd = *off;

             //*  Filter Employee Payroll button.
             When filterEPay = *on;
                filterEPay = *off;
                DspEmpPayFlt();
                LoadEmpPayrGrd();

             //*  Filter Employee Payroll button.
             When clrEpayFlt = *on;
                clrEpayFlt = *off;
                ClrEmpPayFlt();
                LoadEmpPayrGrd();

             //*  Add new Payroll Vendor record.
             When BtnVendAdd = *on;
                  elRRN = dbRRN;
                  vhRRN = 0;
                  VendorPEdit(vhRRN:elRRN);
                  LoadVendorPGrd();
                  BtnVendAdd = *off;

             When btnRefresh = *On;
                btnRefresh =*Off;
                //*  Load screen fields
                LoadActionBar();
                LoadPolicyDet();

          //*** Beg Add *** 03/22/24 *** *************************************
                inInclude = ' ';

          // Perform Action
          When btnPerAct = *on;
             btnPerAct = *off;
             ProcPerAct();

          // Include All
          When btnEndIncl = *on;
             btnEndIncl = *off;
             inInclude = 'Include';

          // Exclude All
          When btnEndExcl = *on;
             btnEndExcl = *off;
             inInclude = ' ';

          // Re-attach
          When btnEndRatt = *on;
             btnEndRatt = *off;
             xxco#  = %Editc(ELco#:'X');
             xxfnd  = %Editc(ELfnd:'X');
             xxemp# = %Editc(ELemp#:'X');
             xxfyr  = %Editc(ELfyr:'X');
             ##rtn = ' ';
             Reattach(xxco#:xxfnd:xxemp#:xxfyr:##rtn);

             If ##rtn <> '1';
                PopOneBtn('Employer has not been billed for +
                 this Policy Year.':'Re-attach Error':'Ok');
             Endif;

          // Add Policy Endorsement Form
          When BtnEndAdd = *On;
             BtnEndAdd = *Off;
             ProcessAdd();
          //*** End Add *** 03/22/24 *** *************************************

          //* Popup Windows

             //* Calculate Contractors Credit popup goes here.
             When calcCClick = *on;
                calcCClick = *off;

             //* Schedule DB/CR Worksheet popup goes here.
             When workSClick = *on;
                workSClick = *off;

             //* Print Scheduled Rating Form
             When schdPrint = *on;
                PolicyCheck();
                If not Error;
                   ##Co# = %editc(scCO#: 'X');
                   ##Fnd = %editc(scFND: 'X');
                   ##Fyr = %editc(scFYR: 'X');
                   ##Emp# = %editc(scEMP#: 'X');
                   ##Div = %editc(scDIV: 'X');
                   ##Que = UPOUTQ;
                   ##Qte = quoteMode;
                   callW1551C(##Co#:##Fnd:##Fyr:##Emp#:##Div:##Que:##Qte);
                   scSchdPnl = 'Print Scheduled Rating Form';
                   scSchdMsg = 'Job has been submitted.';
                   Exfmt rcdSchdErr;
                Endif;
                schdPrint = *off;

             When btnFinaliz = *on;
                btnFinaliz = *off;
                PolicyCheck();


                //*  Save Policy Detail
                If error = *off;
                   SaveEndorse();
                   SavePolicyDet();
                   @@Next = 'FINALIZE';
                   ExitNow = *On;
                Endif;
             When btnRevCan = *on;
                btnRevCan = *off;
                @@Next = 'CANCEL';
                ExitNow = *On;
             //*  Edit Policy Detail for Errors (Enter key or Save button)
             When save = *on;
                If not dspMode;
                   PolicyCheck();

                   //*  Save Policy Detail
                   If error = *off;
                   SaveEndorse();
                      SavePolicyDet();
                      //*  Load screen fields
                      LoadActionBar();
                      LoadPolicyDet();
                   Endif;
                Endif;
                save = *off;

             Other;
               //*** Beg Add *** 03/22/24 *** *************************************
               procOther();                   // Policy Endorsement Form
               //*** End Add *** 03/22/24 *** *************************************

               //* Popup Windows
               Popup();

                //*  Launch Popup for Payroll Edit
               Dou %Eof;
                Readc PayrollSFL;
                If %EOF();
                  Leave;
                Endif;
                If payEdit = *on;
                  epRRN = scPayRRN;
                  PayrollEdit(epRRN:scRTCD);
                  payEdit = *off;
                  LoadPayrollGrd();
                  Leave;
                Endif;
               Enddo;

                //*  Launch Popup for Agent Edit
               Dou %Eof;
                Readc AgencySFL;
                If %EOF();
                  leave;
                Endif;
                If agtEdit = *on;
                   xxAHRRN = scAgtAHRRN;
                   elRRN = dbRRN;
                   agtMode = ' ';
                   Exsr AgentPopup;
                   agtEdit = *off;
                   LoadAgencyGrid();
                   Leave;
                Endif;
               Enddo;

                //*  Launch Popup for Payroll Edit
                Readc sflSrGrid1;
                If not %EOF();
                   If LnkSrEdit = *On or LnkSrCalc = *On;
                      Clear xxKey;
                      xxKey = %editc(elbscd : 'X');
                      Chain keyWTABL PBS01;
                      Select;
                      When BSFreq = 'W';
                         frequency = 'Weekly';
                      When BSFreq = 'M';
                         frequency = 'Monthly';
                      Other;
                         frequency = '  ';
                      Endsl;
                      If frequency <> 'Weekly';
                         schdTyp = ' ';
                         pos# = %scan('/': scSrGr1Rpt);
                         If pos# > 0;
                            Clear tmpdate1;
                            Clear tmpdate2;
                            tmpdate1 = %subst(scSrGr1Rpt : 1 : pos# - 1);
                            tmpdate2 = %subst(scSrGr1Rpt : pos# + 1 : 7);
                            srFltDt = %int(tmpDate);
                         Endif;
                      Else;
                            schdTyp = 'W';
                            tmpDate1_1 = tmpDate3;
                            tmpDate2_1 = tmpDate5;
                            tmpDate3_1 = tmpDate7;
                            srFltDt = %int(tmpDate_1);
                      Endif;

                      srRRN = scSrGr1RRN;
                      If LnkSrCalc = *On;
                         elRRN = dbRRN;
                      Else;
                         elRRN = 0;
                      Endif;
                      SelfRptEdit(srRRN:scRtCd:elRRN:srFltDt:schdTyp);
                      LnkSrEdit = *off;
                      LnkSrCalc = *off;
                      LoadSrRptPrGrd();
                      LoadPayrollGrd();
                      LoadSrRptGrd3();
                   Endif;
                Endif;

                //*  Launch Popup for Employee Payroll Edit
               Dou %Eof;
                Readc sflEmpPayr;
                If %EOF();
                  Leave;
                Endif;

                //Edit link
                If EmpPEdt = *on;
                  eeRRN = scEmpPRRN;
                  inMode= 'E';
                  EmpPayEdit(eeRRN:elRRN:inMode);
                  EmpPEdt = *off;
                  LoadEmpPayrGrd();
                  Leave;
                Endif;

                //Edit link
                If EmpPDlt = *on;
                  eeRRN = scEmpPRRN;
                  inMode= 'D';
                  EmpPayEdit(eeRRN:elRRN:inMode);
                  EmpPDlt = *off;
                  LoadEmpPayrGrd();
                  Leave;
                Endif;
               Enddo;

                //*  Launch Popup for Payroll Vendors Edit
                Readc sflVendorP;
                If not %EOF();
                   If LnkVendEdt = *on;
                         vhRRN = scVndRRN;
                //*** begin chg ***  03/23/23  *****************************************
                //*****  VendorPEdit(vhRRN);
                         elRRN = dbRRN;
                         VendorPEdit(vhRRN:elRRN);
                //*** end chg *****  03/23/23  *****************************************
                         LnkVendEdt = *off;
                         LoadVendorPGrd();
                   Endif;
                Endif;

                //*  Launch Popup for Excluded States delete.
                Readc ExcludeSFL;
                If not %EOF();
                   If excludeDel = *on;
                         xsRRN = scXSrrn;
                         ExcludeEdit(xsRRN);
                         excludeDel = *off;
                         LoadExcludeGrd();
                   Endif;
                Endif;

             //*  When row is clicked, reload
                If rrnSrGr1Cr <> 0;
                   LoadSrRptPrGrd();
                Endif;
          Endsl;

       Enddo;

       //***************************************************************************//
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       APnlHeight = 1450;

          Select;
        //When TabIndex = 0;
          When arrTabNames(tabIndex+1) = 'Rating';
             TabHeight = 1560;
             APnlHeight = TabHeight + 50;
             Exsr WriteAction;
             Write rcdPanel;
             Write rcdRating;
             Exfmt rcdRating2;
             Exsr ReadAction;
             Read rcdRating;
             Read rcdPanel;
          When arrTabNames(tabIndex+1) = 'Payroll';
             If isMaxEmpRcds = *on;
                str9999msg = 'Your search returned too many records. ' +
                'Click on Results to see first 9999 records Or, click ' +
                'Filter to narrow down your search results';
                filterEPay=PopTwoBtn(str9999msg:'Search':'Filter':
                        'Results');
                Dow filterEpay = *on;
                   filterEPay = *off;
                   isMaxEmpRcds = *off;
                   DspEmpPayFlt();
                   LoadEmpPayrGrd();
                   If isMaxEmpRcds = *on;
                      filterEPay=PopTwoBtn(str9999msg:'Search':'Filter':
                           'Results');
                   Endif;
                Enddo;
             Endif;
        //** Begin Change *** 03/03/20 *************************************
        //   TabHeight = 1620;
             TabHeight = 1635;
        //** End   Change *** 03/03/20 *************************************
             APnlHeight = TabHeight + 50;
             LoadPayrollGrd();
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdPayroll;
             Exsr ReadAction;
             Read rcdPanel;
          When arrTabNames(tabIndex+1) = 'Additional Information';
             TabHeight = 995;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdAddInfo;
             Exsr ReadAction;
             Read rcdPanel;
          //*** Beg Add *** 03/22/24 *** *************************************
          When arrTabNames(tabIndex+1) = 'Endorsement Forms'
           or arrTabNames(tabIndex+1) = 'Waiver Endorsements';
             TabHeight = 530;
             // Initialize Screen fields
             scSelAct = '--- Select List Action ---';
             // Load Endorsement Form Grid
             LoadEndFrm(inInclude);
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdEndFrm;
             Exsr ReadAction;
             Read rcdPanel;
          //*** End Add *** 03/22/24 *** *************************************
          When arrTabNames(tabIndex+1) = 'Large Deductible';
             TabHeight = 780;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdLrgDed;
             Exsr ReadAction;
             Read rcdPanel;
          When arrTabNames(tabIndex+1) = 'Retrospective/Retention Rating'
           or arrTabNames(tabIndex+1) = 'Retrospective Rating' ;
             TabHeight = 780;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdRetro;
             Exsr ReadAction;
             Read rcdPanel;
          When arrTabNames(tabIndex+1) = 'Agency History';
             TabHeight = 780;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdAgency;
             Exsr ReadAction;
             Read rcdPanel;
          When arrTabNames(tabIndex+1) = 'Scheduled Rating';
             TabHeight = 780;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdSched;
             Exsr ReadAction;
             Read rcdPanel;
          When arrTabNames(tabIndex+1) = 'Rating Questions';
             dspQuest = isShowRatingQuest;
             TabHeight = 780;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdRateQst;
             Exsr ReadAction;
             Read rcdPanel;
        //***  Begin Add  ***  05/04/20  **************************************
          When arrTabNames(tabIndex+1) = 'Analytics';
             //***** Beg Chg ***** 08/02/21 ***********************************
             //*****dspDrivers = isShowAnalytics;
             dspDrivers = *on;
             //***** End Chg ***** 08/02/21 ***********************************
             TabHeight = 780;
             Exsr WriteAction;
             Write rcdPanel;
             Exfmt rcdAnalyt;
             Exsr ReadAction;
             Read rcdPanel;
        //***  End   Add  ***  05/04/20  **************************************
       EndSl;

       Endsr;
       //***************************************************************************//
       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;

       If qteMode;
          Write rcdQtePnl;
       Else;
          If revBill = *on;
             Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pel05;
             If %found(WDELL5);
                 scActRRN = dbrrnl5;
             Endif;
          Endif;
          Write rcdPolPnl;
       Endif;

       Endsr;
       //***************************************************************************//
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;

       If qteMode;
          Read rcdQtePnl;
       Else;
       scactrrn = scactrrn;
          Read rcdPolPnl;
       Endif;

       xxErrorTab = -1;
       Endsr;
      /end-free

       //===================================================================
       // AgentPopup - Load the Agent Info popup for agent history edit
       //===================================================================
      /FREE
       Begsr AgentPopup;
      /END-FREE
      /free

       AgentEdit(xxAHRRN:qteAgtMode:elRRN:agtMode);

      /end-free
      /FREE
       Endsr;                                                                 //AgentPopup Subr
      /END-FREE
      /free
       //===================================================================
       // validEFT - Validate EFT setup
       //===================================================================
       Begsr validEFT;

          If revBill = *on;
             clear errMsg;
             errMsg = 'Bill Plan must have EFT Information added to +
              the Employer.';
             ChkEFT();
             If error = *on;
                scBsCd = saveScBsCd;
             Endif;
          Endif;

       Endsr;
      /end-free
      //================================================================
      // ErrCkc - Error check control record prior to subfile
      //================================================================
      //
     c     keyWASRL3     Klist
     c                   Kfld                    scCO#
     c                   Kfld                    scFND
     c                   Kfld                    scFYR
     c                   Kfld                    scEMP#
     c                   Kfld                    scDIV
     c                   KFLD                    xxrpmy            5 0

     c     keyWASRL9_2   KLIST
     c                   KFLD                    scCO#
     c                   KFLD                    scFND
     c                   KFLD                    scFYR
     c                   KFLD                    scEMP#
     c                   KFLD                    scDIV
     c                   KFLD                    xxToDt            7 0

      /FREE
       BegSr ErrCkc;
      /END-FREE
       //***** beg add ***** 05/18/23 ********************************
      /FREE
           er4pst2 = *off;
           er3pst1 = *off;
      /END-FREE
       //***** end add ***** 05/18/23 ********************************
     c                   move      *Off          errorfldt
     c                   move      *Off          erpst1
      /FREE
         Clear mspst1;
      /END-FREE
     c                   move      *Off          er1pst1
      /FREE
         Clear ms1pst1;
      /END-FREE
     c                   move      *OFF          er2pst1
      /FREE
         Clear ms2pst1;
      /END-FREE
     c                   move      *Off          erpst2
      /FREE
         Clear mspst2;
      /END-FREE
     c                   move      *Off          er1pst2
      /FREE
         Clear ms1pst2;
      /END-FREE
     c                   move      *Off          er2pst2
      /FREE
         Clear ms2pst2;
      /END-FREE
     c                   move      *Off          er3pst2
      /FREE
         Clear ms3pst2;
         //
         //  Check Reporting for valid month/year
         //
         If frequency <> 'Weekly';
      /END-FREE
     c                   Move      scpst1        srdti
     c                   Call      'SRCVTMY'     DATCHK
     c                   Movel     srdto         scpst1
     c                   Call      'SRCVTMY'     DTCYMDp
     c                   MoveL     SRDTO         xxRPMY            5 0
      /FREE
           dateFlt = scpst1;

           If srerr = *On;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           erpst1
     c                   movel(p)  'WCR0500'     mspst1
      /FREE
           Endif;                                                             //srerr = *on
           //***** beg add ***** 05/18/23 ********************************
           xxCxDt7 = getCxDate(emco#:emfnd:ememp#:emdiv);
           //***** Beg Add ***** 06/09/23 ********************************
           If xxCxDt7 <> 0;
           //***** End Add ***** 06/09/23 ********************************
              xxCxMYD = (xxRPMY * 100) + 01;
              xxCxMYD = DateTo7(getLastDay(cvtToDate(xxCxMYD)));
              If xxCxMYD > xxCxDt7;
                 errorfldt = *on;
                 er3pst1 = *on;
              Endif;
           //***** Beg Add ***** 06/09/23 ********************************
           Endif;
           //***** End Add ***** 06/09/23 ********************************
           //***** end add ***** 05/18/23 ********************************
           //
           //  MONTH/YEAR CANNOT BE ADDED MORE THAN ONCE
           //
           If BtnSrAdEdt = *On;
             Chain keyWASRL3 PSR03;
             If %found;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           er1pst1
     c                   movel(p)  'WC30005'     ms1pst1
      /FREE
             Endif;                                                           //%Found
           Endif;                                                             //BtnSrAdEdt = *On
           //
           //  Month/Year must be within employer policy effective date range
           //
      /END-FREE
     c                   MoveL     ELpfdt        x1PfDt            5 0
     c                   MoveL     ELptdt        x1PtDt            5 0
     c                   Move      ELPtdt        xxTmp             4 0
      //
      /FREE
           If xxTmp = 101;                                                    //1ST OF MONTH
      /END-FREE
     c                   Z-Add     ELptdt        SRDTI
     c                   Call      'SRCVTDAT'    DTMDYYp
     c                   MoveL     SRDTO         ##Dt8             8 0
      //
     c                   CalL      'SRPAST8'
     c                   Parm                    ##DT8
     c                   Parm      1             ##PAST            5 0
     c                   Parm      *Blank        ##ERR             1
      //
     c                   Z-Add     ##DT8         SRDTI
     c                   Call      'SRCVTDAT'    DTCYMDp
     c                   MoveL     SRDTO         x1PtDt
      /FREE
           Endif;                                                             //XXTMP=101
           //
           x4Date = Dateto7(SystemDate());
      /END-FREE
     c                   Movel     x4Date        xxCurdat          5 0
      //
      /FREE
           IF xxRPMY < X1PFDT
           OR xxRPMY > X1PTDT;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           er2pst1
     c                   movel(p)  'WC31373'     ms2pst1
      /FREE
           ElseIf xxRPMY > xxCurdat;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           er2pst1
     c                   movel(p)  'WC40162'     ms2pst1
      /FREE
           Endif;                                                             //XXRPMY
           //
         Else;
           //
           //  VALID DATE IF FREQUENCY IS WEEKLY
           //
      /END-FREE
     c                   Z-Add     scpst2        srdti
     c                   call      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpst2
      /FREE
           dateFlt = scpst2;
           If SRERR = *On;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           erpst2
     c                   movel(p)  'WCR0500'     mspst2
      /FREE
           Endif;                                                             //srerr = *on
           //
           //  REPORTING DATE CANNOT BE ADDED MORE THAN ONCE
           //
      /END-FREE
     c                   Z-Add     scpst2        SRDTI
     c                   Call      'SRCVTDAT'    DTCYMDp
     c                   MoveL     SRDTO         xxToDt
      //
       //***** beg add ***** 05/18/23 ********************************
      /FREE
           xxCxDt7 = getCxDate(emco#:emfnd:ememp#:emdiv);
           //***** Beg Add ***** 05/02/24 ********************************
           If xxCxDt7 <> 0;
           //***** End Add ***** 05/02/24 ********************************
              If xxToDt > xxCxDt7;
                 errorfldt = *on;
                 er4pst2 = *on;
              Endif;
           //***** Beg Add ***** 05/02/24 ********************************
           Endif;
           //***** End Add ***** 05/02/24 ********************************
      /END-FREE
       //***** end add ***** 05/18/23 ********************************
      /FREE
           If BtnSrAdEdt = *On;
             Chain keyWASRL9_2 PSR09;
             If %found;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           er1pst2
     c                   movel(p)  'WC30005'     ms1pst2
      /FREE
             Endif;                                                           //%found
           Endif;                                                             //BtnSrAdEdt = *On
           //
           //  Report date must be within employer policy effective date range
           //
           //
           IF xxToDt < ELpfdt
           OR xxToDt >= ELptdt;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           er2pst2
     c                   movel(p)  'WC31315'     ms2pst2
      /FREE
           Endif;                                                             //XXTODT
           //
           //
           //  Report date day of week must match billing schedule code day of week
           //
      /END-FREE
     c                   MOVE      scpst2        ##RPDT            8 0
     c                   Call      'SRDAY8'
     c                   Parm                    ##RpDt
     c                   Parm      *Blank        ##SDay            4
     c                   Parm      *Blank        ##LDay            9
     c                   Parm      *Blank        ##Err             1
     c                   MoveL     ##SDAY        xxDoWk            2
      //
      /FREE
           IF xxDoWk <> svDoWk;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           er3pst2
     c                   movel(p)  'WC30263'     ms3pst2
      /FREE
           Endif;                                                             //xxDoWk
           //
           //
         Endif;                                                               //freq <> "Weekly"
       Endsr;

       //***************************************************************************//
      /END-FREE
     P Main            E

       //*** Beg Add *** 03/22/24 *** *************************************
       //===================================================================
       // Load Endorsement Form Grid
       //===================================================================
     p LoadEndFrm      b
     d LoadEndFrm      pi
     d inIncludeFrm                  10    const

      /free
        // Clear the subfile
        rrnEndFrm = 0;
        dspEndFrm = *off;
        clrEndFrm = *on;
        Write rcdEndFrm;
        clrEndFrm = *off;
        dspEndFrm = *on;
        scRowSel = 1;

        // Populate the subfile
        Setll (ELco#:ELfnd:ELemp#:scFyr) pmu0b;
        Reade (ELco#:ELfnd:ELemp#:scFyr) pmu0b;
        Dow not %eof(wmmulb);
           rrnEndFrm += 1;
           scEndSel = *off;

           // set the selection on 1st record for first load
           If svrrnEndFrm = *zeros;
              If rrnEndFrm = 1;
                 scEndSel = *on;
              Endif;
           Else;
              If svrrnEndFrm = rrnEndFrm;
                 scEndSel = *on;
              Endif;
           Endif;

           scEndr# = MUend#;
           scEndDiv = %char(MUdiv);
           If MUend# = 0;
              scEndDiv = 'All';
           Endif;

           scForm = MUform;
           scEndDesc = MUdesc;
           If MUdsc2 <> *Blanks;
              scEndDesc = MUdsc2;
           Endif;

           scEedt = CvtToDate(MUeedt);
           scEnst = MUenst;

           scMnau = *Blanks;
           If MUmnau = 'A';
              scMnau = 'Auto';
           Endif;
           If MUmnau = 'M';
              scMnau = 'Manual';
           Endif;

           scPrnt = *Blanks;
           If MUprnt = 'N';
              scPrnt = 'No';
           Endif;
           If MUprnt = 'Y';
              scPrnt = 'Yes';
           Endif;

           scExfl = *Blanks;
           If MUexfl = 'I';
              scExfl = 'Include';
           Endif;
           If MUexfl = 'E';
              scExfl = 'Exclude';
           Endif;

           scEndRRN = dbrrn16;

           // set Visibility of "Add'l Info" Link
           visbAdInf = *off;
           Clear scAdInfURL;
           Chain (MUco#:MUfnd:MUform) pen00;
           If %found(wtenl);
              If en_ENpgm3 <> *Blanks;
                 visbAdInf = *on;

                 // Populate "Add'l Info" Link
                 ##rrn1 = %Editc(dbrrn16:'X');
                 //*** beg chg ***** 05/01/24 ***********************
                 //*** ##rrn2 = @@rrn;
                 ##rrn2 = ##stdivrrn;
                 //*** end chg ***** 05/01/24 ***********************
                 strUrlTabName = 'Endorsement Forms Additional Info';
                 ##kc   = ' ';
                 ##kl   = ' ';
                 ##mod  = 'F';
                 ##env  = ' ';
                 scAdInfURL = CreateURL('W1054R02':strUrlTabName:
                  ##kc:##kl:##mod:##rrn1:##rrn2:##env);
              Endif;
           Endif;

           // Select Checkbox
           If inInclude = 'Include';
              scSelect = 'Y';
              nxtEndFrm = *on;
           Else;
              scSelect = 'N';
              nxtEndFrm = *off;
           Endif;


           Write sflEndFrm;
        Reade (ELco#:ELfnd:ELemp#:scFyr) pmu0b;
        Enddo;

        Clear inInclude;
        rrnEndFrm = 1;
        If svrrnEndFrm <> 0;
           rrnEndFrm = svrrnEndFrm;
           scRowSel = svrrnEndFrm;
           svrrnEndFrm = 0;
        Endif;

      /end-free
     p LoadEndFrm      e

       //===================================================================
       // ProcessAdd - Process Addition of Policy Endorsement Form
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdEndAdd;
       mode = 'A';
       isexit = *off;
       BtnEndCan = *off;
       xxovr = 'N';
       svEndFrm = *Blanks;

       Dow BtnEndCan = *off and isexit = *off;

          EndFrmEdt = *Off;            // Enaable Endorsement Form field
          visEndSvAd = *On;            // Show Save/Add Next Button
          leftBtnCan = 565;             // Set left position on Cancel Button
          scEndHdg = 'Add Policy Endorsement Form';
          //*** Beg Chg ***** 04/16/24 ********************************
          //*** scCo# = ELco#;
          scComp# = ELco#;
          //*** End Chg ***** 04/16/24 ********************************
          scGroup = ELfnd;
          scEnd# = 0;
          scEndMnau = 'Manual';
          scEndExfl = 'Include';

         // Set selection criteria for Endorsement Form field
          scEndFrmCH = 'ENco# = ' + %EditC(ELco#:'X') + ' and ' +
           'ENfnd = ' + %EditC(ELfnd:'X') + ' and ' + 'ENstat <> ' + '''I''';
          // Only allow Waiver Endorsements to be added in Quote mode
          If qteMode = *on;
             scEndFrmCH = 'ENco# = ' + %EditC(ELco#:'X') + ' and ' +
              'ENfnd = ' + %EditC(ELfnd:'X') + ' and ' + 'ENstat <> ' + '''I'''
              + ' and ' + 'ENtype = ' + '''Waiver''';
          Endif;

          Exfmt rcdEndAdd;

          Select;
          // Save/Add Next Button
          When BtnEndSvAd = *on;
             BtnEndSvAd = *off;
             If ValidateEnd(mode);
                Exsr WrtEndFrm;
          //Initialize fields for next add screen if save/add next is pressed
                Clear scEndFrm;
                Clear scEndDt;
                xxovr = 'N';
                svEndFrm = *Blanks;
             EndIf;

          // Save Button
          When BtnEndSav = *on;
             BtnEndSav = *off;
             If ValidateEnd(mode);
                Exsr WrtEndFrm;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnEndCan = *on;
               BtnEndCan = *off;
               isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        MUco#   = ELco#;
        MUfnd   = ELfnd;
        MUemp#  = ELemp#;
        MUdiv   = 0;
        MUfyr   = scFyr;
        MUend#  = scEnd#;
        MUseq#  = 0;
        MUform  = scEndFrm;
        // Get Endorsement Form Details Information
        Chain (MUco#:MUfnd:MUform) pen00;
        If %found(wtenl);
           MUdesc  = en_ENDesc;
           MUdsc2  = en_ENDsc2;
           MUenst  = en_ENst;
           MUprnt  = en_ENprnt;
        Endif;
        MUeedt  = dtCYMD(scEndDt);
        MUexfl  = 'I';
        MUmnau  = 'M';
        MUstat  = 'A';
        MUppgm  = ' ';
        MUnatfm = ' ';

        MUuser  = q1user;
        MUpgm   = q1pgm;
        MUtime  = Timeto6(SystemTime());
        MUdt    = Dateto7(SystemDate());
        MUiusr  = q1user;
        MUipgm  = q1pgm;
        MUidt   = Dateto7(SystemDate());

       EndSr;

       // Writing Policy Endorsement Form subroutine
       BegSr WrtEndFrm;
        Clear pmu0p;
        Exsr MoveScDb;
        Write pmu0p;

        // Check and Process for Additional Info
        If en_ENpgm3 <> *blanks;
           Clear scURL01;

           // Populate URL for "Add'l Info" screen
           ##rrn1 = %Editc(dbrrn17:'X');
           //*** beg chg ***** 05/01/24 ***********************
           //*** ##rrn2 = @@rrn;
           ##rrn2 = ##stdivrrn;
           //*** end chg ***** 05/01/24 ***********************
           strUrlTabName = 'Endorsement Forms Additional Info';
           ##kc   = ' ';
           ##kl   = ' ';
           ##mod  = 'R';                      // Add mode (create)
           ##env  = ' ';
           scURL01 = CreateURL('W1054R02':strUrlTabName:
             ##kc:##kl:##mod:##rrn1:##rrn2:##env);
           Exfmt rcdEndInfo;
        EndIf;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcessCopy - Process Copy of Policy Endorsement Form
       //===================================================================
     P ProcessCopy     B
     D ProcessCopy     PI
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdEndCpy;
       isexit = *off;
       BtnCpyCan = *off;

       Dow BtnCpyCan = *off and isexit = *off;

          scCpyHdg = 'Copy ' + %trim(scForm) + ' Endorsement Form';
          scFrmYear = scFyr;

          Exfmt rcdEndCpy;

          Select;
          When selPolYr = *on;
             erCpyYear = *off;
             msCpyYear = *blanks;
             @@next = 'PostYear';
             ##rrn1 = *blanks;
             ##rrn1 = %char(@@rrn);

             @@mode = 'D';
             @@TabInd = *blanks;
             @@Qte2   = quoteMode;
             @@PostDt = *zeros;
             @@PostAmt = *zeros;

             // Call WtPolARInq for Policy Year
             If ##rrn1 <> *blanks;
                WtPolARInq(##rrn1:@@mode:@@next:@@TabInd:@@Qte2:
                           @@PostDt:@@PostAmt);
                If @@TabInd <> *blanks;
                   scCpyYear = %int(@@TabInd);
                Endif;
             Endif;

          // Save Button
          When BtnCpySav = *on;
             BtnCpySav = *off;
             If ValidateCpy();
                Exsr CpyEndFrm;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnCpyCan = *on;
               BtnCpyCan = *off;
               isexit = *on;
          EndSl;
       Enddo;

       // Copy Policy Endorsement Form subroutine
       BegSr CpyEndFrm;
        // Determine next avail Waiver # for copy TO policy year
        ##wav# = 0;
        nxtwav# = 1;
        Chain (MUco#:MUfnd:MUemp#:MUdiv:scCpyYear) pwv01;
        If %found(wmwvl1);
           nxtwav# = WVwav# + 1;
        Endif;

        // Copy Waiver info records for TO policy year
        Setll (MUco#:MUfnd:MUemp#:MUdiv:scfyr:##wav#) pwv01;
        ReadE (MUco#:MUfnd:MUemp#:MUdiv:scfyr) pwv01;
        Dow not %eof(wmwvl1);
           If WVenm = scForm;
              WVfyr = scCpyYear;
              WVend# = 0;
              WVwav# = nxtwav#;
              nxtwav# = nxtwav# + 1;
              WVuser  = q1user;
              WVpgm   = q1pgm;
              WVtime  = Timeto6(SystemTime());
              WVdt    = Dateto7(SystemDate());
              WViusr  = q1user;
              WVipgm  = q1pgm;
              WVitm   = Timeto6(SystemTime());
              WVidt   = Dateto7(SystemDate());
              Write pwv01;
           Endif;
           ReadE (MUco#:MUfnd:MUemp#:MUdiv:scfyr) pwv01;
        Enddo;

        // Copy Forms Data records for TO policy year
        Chain (MUco#:MUfnd:MUemp#:MUdiv:scfyr:scForm) pfm01;
        If %found(wxfml11);
           F3fyr = scCpyYear;
           F3user  = q1user;
           F3pgm   = q1pgm;
           F3time  = Timeto6(SystemTime());
           F3dt    = Dateto7(SystemDate());
           F3iusr  = q1user;
           F3ipgm  = q1pgm;
           F3idt   = Dateto7(SystemDate());
           Write pfm01;
        Endif;

        // Copy the associated Endorsement Form data in WLEDP
        Setll (MUco#:MUfnd:MUemp#:MUdiv:scfyr:scForm) ped00;
        Reade (MUco#:MUfnd:MUemp#:MUdiv:scfyr:scForm) ped00;
        Dow Not %EOF(wledl);
           FMfyr = scCpyYear;
           FMuser  = q1user;
           FMpgm   = q1pgm;
           FMtime  = Timeto6(SystemTime());
           FMdt    = Dateto7(SystemDate());
           FMiusr  = q1user;
           FMipgm  = q1pgm;
           FMidt   = Dateto7(SystemDate());
           Write ped00;

           Reade (MUco#:MUfnd:MUemp#:MUdiv:scfyr:scForm) ped00;
        EndDo;

        // Copy Endorsement form TO policy year
        Clear pmu0p;
        Chain (ELco#:ELfnd:ELemp#:scfyr:scEndr#:scForm) pmu0b;
        If %found(wmmulb);
           MUfyr = scCpyYear;
           MUeedt = dtCYMD(scCpyEndDt);
           MUuser  = q1user;
           MUpgm   = q1pgm;
           MUtime  = Timeto6(SystemTime());
           MUdt    = Dateto7(SystemDate());
           MUiusr  = q1user;
           MUipgm  = q1pgm;
           MUidt   = Dateto7(SystemDate());

           Write pmu0p;

           strErrMsg = %trim(MUform) + ' form has been Copied to Policy Year '
            + %char(scCpyYear) + '.';
           PopOneBtn(strErrMsg:'Form Copy':'Ok');
        Endif;

       EndSr;

      /end-free
     P ProcessCopy     E

       //===================================================================
       // ProcOther - Process other Options for Endorsement Form Grid
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdEndAdd;
       isexit = *off;
       ReadC sflEndFrm;

       Dow not %eof;

          Select;
          // Processing for Edit Policy Endorsement Form Link
          When scEndEdt = *On;
             scEndEdt = *Off;
             mode = 'C';

             EndFrmEdt = *On;             // Disable Endorsement Form field
             visEndSvAd = *Off;            // Hide Save/Add Next Button
             leftBtnCan = 675;             // Set left position on Cancel Button
             svrrnEndFrm = rrnEndFrm;     //Save Rrn value for edited record

             Chain(n) scEndRRN WMMUP;

             Exsr MoveDbSc;

             scEndHdg = 'Edit Policy Endorsement Form';
             //*** Beg Chg ***** 04/16/24 ********************************
             //*** scCo# = ELco#;
             scComp# = ELco#;
             //*** End Chg ***** 04/16/24 ********************************
             scGroup = ELfnd;

            // Set selection criteria for Endorsement Form field
             scEndFrmCH = 'ENco# = ' + %EditC(ELco#:'X') + ' and ' +
              'ENfnd = ' + %EditC(ELfnd:'X') + ' and ' + 'ENstat <> ' + '''I''';
             // Only allow Waiver Endorsements to be added in Quote mode
             If qteMode = *on;
                scEndFrmCH = 'ENco# = ' + %EditC(ELco#:'X') + ' and ' +
                 'ENfnd = '+ %EditC(ELfnd:'X')+ ' and '+ 'ENstat <> ' + '''I'''
                 + ' and ' + 'ENtype = ' + '''Waiver''';
             Endif;

             Dow isexit = *off;
                Exfmt rcdEndAdd;

                Select;
                // Save Button
                When BtnEndSav = *on;
                   BtnEndSav = *off;
                   // Check if WMMUP updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('WMMUP':dbrrn17:'MUdt':'MUtime':
                    'MUuser':saveDT4:saveTM4:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scEndRRN WMMUP;
                      Exsr MoveDbSc;
                   EndIf;
                   if flgRcdUpdated = '0'; // No error
                      If ValidateEnd(mode);
                         Chain scEndRRN WMMUP;
                         Exsr MoveScDb;
                         Update pmu0p;

                // Check and Process for Additional Info
                         If scAdInfURL <> *blanks;
                            scURL01 = scAdInfURL;
                            Exfmt rcdEndInfo;
                         EndIf;

                         isexit = *on;
                      EndIf;
                   EndIf;

                // Cacel Button
                When BtnEndCan= *on;
                   BtnEndCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          EndSl;
          Unlock WMMUP;
          ReadC sflEndFrm;
       Enddo;

       // Subrotine to move database fields into screen fields
       BegSr MoveDbSc;
        scEndFrm = MUform;
        scEnd# = MUend#;
        scEndFDsc2 = MUdsc2;

        scEndDt = dtMDYY(MUeedt);
        scEndst = MUenst;

        scEndMnau = *Blanks;
        If MUmnau = 'A';
           scEndMnau = 'Auto';
        Endif;
        If MUmnau = 'M';
           scEndMnau = 'Manual';
        Endif;

        scEndPrnt = *Blanks;
        If MUprnt = 'N';
           scEndPrnt = 'No';
        Endif;
        If MUprnt = 'Y';
           scEndPrnt = 'Yes';
        Endif;

        scEndExfl = *Blanks;
        If MUexfl = 'I';
           scEndExfl = 'Include';
        Endif;
        If MUexfl = 'E';
           scEndExfl = 'Exclude';
        Endif;

        saveDT4 = MUdt;
        saveTM4 = MUtime;
       EndSr;

       // Subrotine to move Screen fields into database fields
       BegSr MoveScDb;
        MUeedt = dtCYMD(scEndDt);
        MUuser = q1user;                // User
        MUpgm  = q1pgm;                 // Program
        MUtime = Timeto6(SystemTime()); // Time
        MUdt   = Dateto7(SystemDate()); // Date Mintained
        saveDT4 = MUdt;
        saveTM4 = MUtime;
       EndSr;

      /end-free
     P ProcOther       e

       //===================================================================
       // ProcPerAct - Process Performed Action from the action dropdown
       //===================================================================
     p ProcPerAct      b
     d ProcPerAct      pi
     d  inmod          s              1

      /Free

       Unlock WMMUP;

       ReadC sflEndFrm;
       Dow not %eof;
          If scSelect = 'Y';
             Chain(n) scEndRRN pmu0p;
             Chain q1user pup00;

             Select;
             When scSelAct = 'Copy';
                inmod = 'Y';
                If ValidateAct(inmod);
                   ProcessCopy();
                EndIf;
             When scSelAct = 'Delete';
                inmod = 'D';
                If ValidateAct(inmod);
                   Exsr DeleteEnd;
                EndIf;
             When scSelAct = 'Print';
                inmod = 'P';
                If ValidateAct(inmod);
                   Exsr PrintEnd;
                Endif;
             When scSelAct = 'Change Include/Exclude';
                Chain scEndRRN WMMUP;
                If %found(WMMUP);
                   Select;
                   When MUexfl = 'I';
                      MUexfl = 'E';
                   When MUexfl = 'E';
                      MUexfl = 'I';
                   Endsl;
                   MUuser = q1user;                // User
                   MUpgm  = q1pgm;                 // Program
                   MUtime = Timeto6(SystemTime()); // Time
                   MUdt   = Dateto7(SystemDate()); // Date Mintained
                   Update pmu0p;
                Endif;
             Endsl;
          Endif;

          ReadC sflEndFrm;
       Enddo;


       // Subroutine to Delete Endorsement Form
       Begsr DeleteEnd;
        // Delete the associated Endorsement Form data in WLEDP
        Setll (MUco#:MUfnd:MUemp#:MUdiv:MUfyr:MUform) ped00;
        Reade (MUco#:MUfnd:MUemp#:MUdiv:MUfyr:MUform) ped00;
        Dow Not %EOF(wledl);
           Delete ped00;
           Reade (MUco#:MUfnd:MUemp#:MUdiv:MUfyr:MUform) ped00;
        EndDo;

        // Delete "Addtional Information" data associated with
        // "Manual Attach" policy page endorsement form
        DelAddInfo(MUco#:MUfnd:MUemp#:MUfyr:MUform);

        // Delete Endorsement Form
        Chain scEndRRN WMMUP;
        If %found(WMMUP);
           Delete pmu0p;
           strErrMsg = %trim(MUform) + ' form has been Deleted!';
           PopOneBtn(strErrMsg:'Delete':'Ok');
        Endif;
       Endsr;


       // Subroutine to Print Single Endorsement Form
       Begsr PrintEnd;
        In *Lock *dtaara;
        ldend# = MUend#;
        ldenda = *blanks;
        ldmyrr = 0;
        ldeedt = MUeeDt;
        ldfyr  = ELfyr;
        ldpyrl = '0';
        ldform = MUform;
        Out *dtaara;

        // Submit Print Endorsement Form Job
        Chain (MUco#:MUfnd:MUform) pen00;
        If %found(wtenl);
           If en_ENpgm2 <> *Blanks;
              xxkc   = '0';
              xxkl   = '0';
              xxcall = '6';
              xxenv  = ' ';
              PrintEndFrm(xxkc:xxkl:xxcall:xxrrn:xxenv);
           Endif;
        Endif;
       Endsr;

      /End-Free
     p ProcPerAct      e

      //===================================================================
      // Validate the Action.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateAct     b
     d ValidateAct     pi              n
     d strmode                        1
     d isError         s               n   inz
     d strmsg          s            100    inz
     d strmsgd         s            500    inz
     d strmsgid        s             10    inz
      /Free

        isError = *off;

       // Modes
       // 'Y' -- Copy               -- opt 3
       // 'P' -- Print              -- opt 6
       // 'D' -- Delete             -- opt 4

       // for Copy - Verify that the form is a "manual attach"
        If strmode = 'Y' and MUmnau <> 'M';
           strmsg = *Blanks;
           strmsgid = 'WC30216';
           isError = *on;
        Endif;

       // for Copy - Verify that the endorsement # is equal to zero
        If strmode = 'Y' and MUend# <> 0 and isError = *off;
           strmsg = *Blanks;
           strmsgid = 'WC30217';
           isError = *on;
        Endif;

       // for Print - Verify that System Print is 'Y'
        If strmode = 'P' and MUprnt <> 'Y';
           strmsg = 'Endorsement cannot be Printed.';
           strmsgid = *Blanks;
           isError = *on;
        Endif;

       // for Print - Print not allowed for Endorsement Form WC890600
        If strmode = 'P' and MUform = 'WC890600' and isError = *off;
           strmsg = 'Endorsement not valid for print.';
           strmsgid = *Blanks;
           isError = *on;
        Endif;

       // for Delete - Deletion of Mid-term endorsement records not allowed
        If strmode = 'D' and MUend# <> 0;
           strmsg = *Blanks;
           strmsgid = 'WC32405';
           isError = *on;
        Endif;



      // Return *on indicator when No error exists.
       strErrMsg = *blanks;
       If isError = *on;
          If strmsgid <> ' ';
             strMsgD = RtvMsgD('WMESGG':strmsgid);
             strErrMsg = 'Error on Form ' +  %trim(MUform) +
              ' -- ' + %trim(strMsgD);
          Endif;
          If strmsg <> ' ';
          strErrMsg = 'Error on Form ' +  %trim(MUform) +
           ' -- ' + %trim(strMsg);
          Endif;
          PopOneBtn(strErrMsg:'Error');

          // As sfl grid is not reloaded, keep the row selection (scselect) active
          // Do not update sfl grid for Edit/Delete
          If strmode = 'Y' or strmode = 'P';
             nxtEndFrm = *on;
             Update sflEndFrm;
          EndIf;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateAct     e

      //===================================================================
      // Validate Copy Policy Endorsement Form
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateCpy     b
     d ValidateCpy     pi              n
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erCpyYear = *off;
        msCpyYear = *blanks;
        erCpyEndDt = *off;
        msCpyEndDt = *blanks;

       // Policy Year can't be blank
        If scCpyYear = 0;
           erCpyYear = *on;
           msCpyYear = 'WCR0502';
           isError = *on;
        Endif;
       // Verify that Policy Year exists on the policy
        If scCpyYear <> 0;
           Chain (MUco#:MUfnd:MUemp#:MUdiv:scCpyYear) pel05;
           If not %found(wdell5);
              erCpyYear = *on;
              msCpyYear = 'WCR0639';
              isError = *on;
           Endif;
        Endif;
       // Verify that Form does not already exists for Policy Year entered
        If erCpyYear = *off;
           Chain (MUco#:MUfnd:MUemp#:scCpyYear:scEndr#:scForm) pmu0b;
           If %found(wmmulb);
              erCpyYear = *on;
              msCpyYear = 'WC30005';
              isError = *on;
           Endif;
        Endif;

       // Validate Endorsement Effective Date
        erCpyEndDt = ValidDate(scCpyEndDt:charMsg);
        If erCpyEndDt = *on;
           msCpyEndDt = charMsg;
           isError = *on;
        Endif;
       // Endorsement Effective Date must fall within the policy date range
        If erCpyEndDt = *off;
           Chain (MUco#:MUfnd:MUemp#:MUdiv:scCpyYear) pel05;
           If %found(wdell5);
              If (dtCYMD(scCpyEndDt) < L5ELpfdt
               or dtCYMD(scCpyEndDt) > L5ELptdt);
                 erCpyEndDt = *on;
                 msCpyEndDt = 'WC11234';
                 isError = *on;
              Endif;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateCpy     e


      //===================================================================
      // Validate Policy Endorsement Forms
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateEnd     b
     d ValidateEnd     pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erEndFrm = *off;
        msEndFrm = *blanks;
        erEndFrm2 = *off;
        mgEndFrm2 = *blanks;
        erEndDt = *off;
        msEndDt = *blanks;

        If strmode = 'A';      //Add Mode
          // Endorsement Form can't be blank
           If scEndFrm = *Blanks;
              erEndFrm = *on;
              msEndFrm = 'WCR0503';
              isError = *on;
           Endif;
          // Check if Endorsement Form has already been attached
           If scEndFrm <> *Blanks;
              Chain (ELco#:ELfnd:ELemp#:scFyr:scEnd#:scEndFrm) pmu0b;
              If %found(wmmulb);
                 erEndFrm = *on;
                 msEndFrm = 'WC30005';
                 isError = *on;
              Endif;
           Endif;
          // Check Waiver Info for Waiter type forms
           If scEndFrm <> *Blanks and erEndFrm = *off;
              Chain (ELco#:ELfnd:scEndFrm) pen00;
              If %found(wtenl) and en_ENtype = 'Waiver';
                 chrWvrForm = en_ENform;
                 chrWvBlkPrjDs = ' ';
                 ##fyr2 = scfyr;
                 If GetWaiverInfo(ELco#:ELfnd:chrState:ELpfdt:chrWvrForm:
                  chrPrtPgm:chrAddPgm:chrWvBlkPrjDs:##fyr2:ELemp#) = *Off;
                    erEndFrm2 = *on;
                    mgEndFrm2 = chrWvBlkPrjDs;
                    isError = *on;
                 Endif;
              Endif;
           Endif;
          // Validate Endorsement State
           If scEndFrm <> *Blanks and erEndFrm = *off and isError = *off;
              ##rtn = ' ';
              ValEndSt(scEndFrm:##rtn);
              If ##rtn = 'N';
                 erEndFrm = *on;
                 msEndFrm = 'WCR3209';
                 isError = *on;
              Endif;
           Endif;
          // Check if "ALL STATES" Endorsement is Appropriate for Policy Coverage
           If scEndFrm <> *Blanks and erEndFrm = *off and isError = *off;
              ##attach = ' ';
              ##fyr2 = scfyr;
              ChkAllSt(ELco#:ELfnd:ELemp#:##fyr2:scEndFrm:##attach);
              If (##attach = 'N' and xxovr = 'N') or
               (##attach = 'N' and svEndFrm <> scEndFrm);
                 isAttresp = PopTwoBtn('Endorsement Form not Approved for +
                  States covered with this Policy. Do you want to Attach?':
                  'Confirm Form Attach':'Yes':'No');
                 Select;
                 When isAttresp = *On;
                    xxovr = 'Y';
                    svEndFrm = scEndFrm;
                 When isAttresp = *Off;
                    isError = *on;
                 EndSl;
              Endif;
           Endif;
        Endif;

       // Validate Endorsement Effective Date
        erEndDt = ValidDate(scEndDt:charMsg);
        If erEndDt = *on;
           msEndDt = charMsg;
           isError = *on;
        Endif;
       // Endorsement Effective Date must fall within the policy date range
        If erEndDt = *off;
           If (dtCYMD(scEndDt) < ELpfdt or dtCYMD(scEndDt) > ELptdt);
              erEndDt = *on;
              msEndDt = 'WCR0930';
              isError = *on;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateEnd     e
       //*** End Add *** 03/22/24 *** *************************************

       //***************************************************************************//
       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     P LoadSelection   B
     D LoadSelection   PI
      /free

       Clear fyrChoices;
       fyrChoices = 'ELCO# = ' + %char(ELCO#) + ' AND ELFND = '
        + %char(ELFND) + ' AND ELEMP# = ' + %char(ELEMP#)
        + ' AND ELDIV = ' + %char(ELDIV);

       Clear divChoices;
       divChoices = 'EMCO# = ' + %char(ELCO#) + ' AND EMFND = '
        + %char(ELFND) + ' AND EMEMP# = ' + %char(ELEMP#);

       Clear rteChoices;
       Setll (ELco#:ELfnd) PGR00;
       Dou %Eof;
          Reade (ELco#:ELfnd) PGR00;
          If %Eof Or GRstat = 'A';
             Leave;
          Endif;
       Enddo;
       If Not %Eof(WMGRL);
          rteDBFile = 'WMGRP Join WMRSP' +
           ' On RSco# = GRco# And GRrtcd = RSrtcd';
          rteChoices = 'GRCO# = ' + %char(ELCO#) + ' AND GRFND = ' +
                    %char(ELfnd) + ' AND GRSTAT = ''A''';
       Else;
          rteDBFile = 'WMRSP';
          rteChoices = 'RSCO# = ' + %char(ELCO#) + ' AND RSSTAT = ''A''';
       EndIf;

       Clear premFinSel;
       premFinSel = 'PFCO# = ' + %char(ELCO#) + ' AND PFFND = '
        + %char(ELFND) + ' AND PFEFDT <= ' + %char(ELPFDT);

       Clear ilChoices;
       ilChoices = 'LMCO# = ' + %char(ELCO#) + ' AND LMFND = '
        + %char(ELFND) + ' AND LMST = ''' + xxSt
        + ''' AND LMEFDT <= ' + %char(ELPFDT);

       Clear ddChoices;
       //***** beg chg ****** 04/06/21 ****************************************
       //*****ddChoices = 'DDCO# = ' + %char(ELCO#) + ' AND DDFND = '
       //***** + %char(ELFND) + ' AND DDST = ''' + xxSt
       //***** + ''' AND DDEFDT <= ' + %char(ELPFDT);
       ddChoices = 'DDCO# = ' + %char(ELCO#) + ' AND DDFND = '
        + %char(ELFND) + ' AND DDST = ''' + xxSt
        + ''' AND DDEFDT = (select max(d2.DDefDt) from WTDDP d2 '
        + 'where d1.DDco# = d2.DDco# and d1.DDfnd = d2.DDfnd and d1.DDst = '
        + 'd2.DDst and d1.DDcode = d2.DDcode and d2.DDefdt <= ' + %char(ELPFDT)
        + ' group by DDfnd, DDst, DDcode limit 1)';
       //***** End chg ****** 04/06/21 ****************************************

       Clear mgChoices;
       mgChoices = 'MGCO# = ' + %char(ELCO#) + ' AND MGFND = '
        + %char(ELFND) + ' AND MGEFDT <= ' + %char(ELPFDT);

       Clear dsChoices;
       dsChoices = 'DSCO# = ' + %char(ELCO#) + ' AND DSFND = '
        + %char(ELFND) + ' AND DSST = ''' + xxSt
        + ''' AND DSEFDT <= ' + %char(ELPFDT);

       Clear rtChoices;
       rtChoices = 'RTCO# = ' + %char(ELCO#);

       Clear ldPlnChoic;
       ldPlnChoic = 'GTCO# = ' + %char(ELCO#) + ' AND GTTBID = ''  DT'''
        + ' AND GTSTAT = ''A''';

       //Clear bilChoices;

      /end-free
     P LoadSelection   E

       //***************************************************************************//
       //===================================================================
       // Load Policy Detail - Load the policy detail fields.
       //===================================================================
     P LoadPolicyDet   B
     D LoadPolicyDet   PI
     D intCo#          s              3  0 inz
     D intFnd          s              3  0 inz
      *** beg add ***** 02/02/21 **************************
     d chrWvrForm      s             15
     d chrPrtPgm       s             10
     d chrAddPgm       s             10
     d chrBlnkWdDf     s                   Like(ELblnkWD)
      *** end add ***** 02/02/21 **************************
      /free

       Chain(n) keyWDELL pel00;
       xxDate =  ELPFDT;
       xxOnce =  'N';
       xxOFlg =  'N';
       xxUPDA =  *OFF;
       xxUPDB =  ' ';
       xxUPDC =  ' ';
       xxUPDD =  ' ';
       intCo# = scCo#;
       intFnd = scFnd;
       ComAPI(intCo#:intFnd:compName:fundName:secLevel:secType);

       xx#Div = 0;
       Setll key1WMEML PEM00;
       Reade key1WMEML PEM00;

       Dow not %eof;
          xx#Div = xx#Div + 1;
          Reade key1WMEML PEM00;
       Enddo;

       Chain keyWMEML PEM00;

       xsKey = 'WC';
       Chain xsKey SWC01;
       deductInstalled = WCFL06 = 'Y';
       achFlg = WCFL24 = 'Y';
       vndrFl = WCFL23 = 'Y';

       Clear noRRModule;
       If WCFL11 <> 'Y';
          noRRModule = *on;
       Endif;

       Clear noLDModule;
       If WCFL43 <> 'Y';
          noLDModule = *on;
       Endif;

       Clear noSRModule;
       If WCFL53 <> 'Y';
          noSRModule = *on;
       Endif;

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;
       Chain keyWMEXL PEX00;
       If %found();
          svASRM = EXASRM;
       Endif;
       svASRM = EXASRM;
       xxSt = RatingSt(ELCO#:ELRTCD);
       xxSvSt = RatingSt(ELCO#:ELRTCD);
       SetGt keyWDRRL PRR00;
       Readpe key1WDRRL PRR00;

       //*** begin add ***  10/20/23  ******************************************
       isANCStat = *off;
       //*** end add *****  10/20/23  ******************************************
       If RRancs = 0;
          isANCStat = *on;
       Endif;
       //*  Selection fields
       LoadSelection();

       //***  Policy Detail Tab  **************************************************/

       If isContribution = '1';
          flxExcMin = 'Exclude from $2,500 Minimum Contribution';
          flxFinComp = 'Contribution Finance Company';
          flxExcMin2 = 'Exclude from $2,500 Minimum Contribution';
          flxPolInfo = 'Policy Contribution Information';
          flxLvlInfo = 'Contribution Level';
       Else;
          flxExcMin = 'Exclude from $2,500 Minimum Premium';
          flxFinComp = 'Premium Finance Company';
          flxExcMin2 = 'Exclude from $2,500 Minimum Premium';
          flxPolInfo = 'Policy Premium Information';
          flxLvlInfo = 'Premium Level';
       Endif;
       scPfdt =  dtMDYY(ELPFDT);
       scPtdt =  dtMDYY(ELPTDT);
       scAndt =  dtMDYY(ELANDT);
       scBsdt =  dtMDYY(ELBSDT);
       scManP2 =  ELMANP;

       scCofl2 =  ELCOFL;
       scFfl92 =  ELFFL9;
       scSrpr2 =  ELSRPR;
       scBlfl =  ELBLFL;
       scStat =  ELSTAT;
       If clientID = '102';
          sc25002 =  ELFFL2;
       Endif;
       scEmpy =  ELEMPY;
       scArsm =  ELARSM;
       //*** Begin Add *** 06/12/24 ****************************************
       // Get the bill schedule code type
       chrBsCd          = %EditC(ELbsCd:'X');
       chrBillSchedType = BillSchdType(ELco#:chrBsCd);

       // Set Detail S/R Assessment to group value and protect from change
       If Not dspMode;
          inqSRAs2 = *Off;
       EndIf;

       If clientID = '146';
          Chain (ELco#:ELfnd:ELfyr) PF100;
          If %Found(wdf1l) and F1SRAs = 'Y';
             scSRAs   = 'Y';
             scSRAs2  = 'Y';
             inqSRAs2 = *On;
          Else;
             scSRAs   = ELsrAs;
             scSRAs2  = ELsrAs;
          EndIf;
       Else;
          scSRAs  = ' ';
          scSRAs2 = ' ';
       EndIf;

       // Set which field is displayed
       if ratPopUp = *Off;
          visSRAs  = *Off;
          visSRAs2 = *On;
       Else;
          visSRAs  = *On;
          visSRAs2 = *Off;
       EndIf;

       // Do show S/R Assessments if bill schedule code type is not S/R
       If (chrBillSchedType <> 'S' And ELbsCd <> 0) Or
          (ELbsCd = 0) Or (clientID <> '146');
          visSRAs  = *Off;
          visSRAs2 = *Off;
          scSRAs2  = ' ';
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
       scFfl1 =  ELFFL1;
       scIffl =  ELIFFL;

       scRTCD = ELRTCD;

       scBSTY = ' ';

       scBSCD = ELBSCD;
       s2BSCD = ELBSCD;
       saveScBsCD = scBsCD;
       svRTCD = ELRTCD;

       protBSCD = *off;
       If dspMode;
          protBSCD = *on;
       Endif;
       If clientId = '136';
          scExAgtRat = ELffl2;
       Endif;

       If chgMode = *on and qteMode <> *on and revBill <> *on;
          xxxDTE = sysDt7;
          Setll keyWMEAL6 pea06;
          Reade(n) keyWMEAL6 pea06;
          Dow not %eof;
             If eaarty = 'B' or eaarty = 'A' or eaarty = 'H';
                If xxPoym < xxCuym or xxTrym < xxCuym;
                   protBSCD = *on;
                   Leave;
                Endif;
             Endif;
             Reade(n) keyWMEAL6 pea06;
          Enddo;
       Endif;

       scQTYP = ELQTYP;
       scTYPB = ELTYPB;
       scASCD = ELASCD;
       scPFIN = ELPFIN;
       scMCCD = ELMCCD;
       scMCGP = ELMCGP;
       scILCD = ELILCD;
       scINL2 = ELINL2;
       scDCDE = ELDCDE;
       scDCD2 = ELDCD2;

       // Increased limits 1 and 2
       scIL1efDt =  dtMDYY(ELILEF);
       scIL1exDt =  dtMDYY(ELILEX);
       scIL2efDt =  dtMDYY(ELI2EF);
       scIL2exDt =  dtMDYY(ELI2EX);

       //Disable Increased Limits fields if no active codes are available.
       noILAvail = *off;
       Setll key2WTLML1 PLM01;
       Reade key2WTLML1 PLM01;
       Dow not %eof and LMSTAT = 'I';
       Reade key2WTLML1 PLM01;
       Enddo;

       If %eof;
          noILAvail = *on;
       Endif;

       // Deductible Discount 1 and 2
       scCxbl =  ELCXBL;
       scDD1efDt =  dtMDYY(ELDCEF);
       scDD1exDt =  dtMDYY(ELDCEX);
       scDD2efDt =  dtMDYY(EL2DEF);
       scDD2exDt =  dtMDYY(EL2DEX);

       //Disable Deductible Discount fields if no active codes are available.
       noDDAvail = *off;
       Setll key2WTDDL2 PDD02;
       Reade key2WTDDL2 PDD02;
       Dow not %eof and DDSTAT = 'I';
       Reade key2WTDDL2 PDD02;
       Enddo;

       If %eof;
          noDDAvail = *on;
       Endif;

       // Deviation 1
       deviation1 = *on;
       dev1Split  = *on;
       isSftyPer  = *on;
       isSaftyAmt = *on;
       isSftyHdg  = *off;  // Safety heading grayed out.

       If RRDV1C <> 0 or RRST = 'NM';
          deviation1 = *off;
       Endif;

       If RRDV1S = 'Y';
          dev1Split = *off;
       Endif;

       // Do not allow Deviation 1 entry if 'Disallow Changes' is checked in Stat reporting/Rating
       If RRDV1CHG = 'N';
          deviation1 = *on;                   //Disable Deviation 1 fields
          dev1Split = *on;
       Endif;

       If RRDV1H <> *blanks;
          scDev1H = RRDV1H;
       Else;
          scDev1H = 'Deviation #1';
       Endif;

       scASP2 = ELASP2;
       scDV1efDt =  dtMDYY(ELDVEF);
       scDV1exDt =  dtMDYY(ELDVEX);

       scD1S2 = ELD1S2;
       scD1D2EfDt =  dtMDYY(ELD1D2);
       scD1D2ExDt =  dtMDYY(ELD1X2);

       // Deviation 2
       deviation2 = *on;
       dev2Split  = *on;

       If RRdv2c <> 0 Or clientID = '025' Or clientID = '136';
          deviation2 = *off;
       Endif;

       If RRDV2S = 'Y';
          dev2Split = *off;
       Endif;

       // Do not allow Deviation 2 entry if 'Disallow Changes' is checked in Stat reporting/Rating
       If RRDV2CHG = 'N';
          deviation2 = *on;                   //Disable Deviation 2 fields
          dev2Split = *on;
       Endif;
       If RRDV2H <> *blanks;
          scDev2H = RRDV2H;
       Else;
          Select;
          When clientID = '025';
             scDev2H = 'Stabilization Factor';
          When clientID = '136';
             scDev2H = 'Territory Rating';
          Other;
             scDev2H = 'Deviation #2';
          EndSl;
       Endif;

       scStFc = ELstfc;
       scDev2F = ELdev2F;
       scDEV2 = ELDEV2;
       scDV2efDt =  dtMDYY(ELD2EF);
       scDV2exDt =  dtMDYY(ELD2EX);

       scD2S2 = ELD2S2;
       scD2D2EfDt =  dtMDYY(ELD2D2);
       scD2D2ExDt =  dtMDYY(ELD2X2);

       // Subject Premium Credit 1A
       subject1A = *on;

       If RR1ACD <> 0;
          subject1A = *off;
       Endif;

       If RR1AHD <> *blanks;
          scSbj1AH = RR1AHD;
       Else;
          scSbj1AH = 'Subject Prem CR 1A';
       Endif;

       scASPM = ELASPM;
       scMCefDt =  dtMDYY(ELMCEF);
       scMCexDt =  dtMDYY(ELMCEX);

       // Subject Premium Credit 1B
       subject1B = *on;

       If RR1BCD <> 0;
          subject1B = *off;
       Endif;

       If RR1BHD <> *blanks;
          scSbj1BH = RR1BHD;
       Else;
          scSbj1BH = 'Subject Prem CR 1B';
       Endif;

       scASPS = ELASPS;
       scSCefDt =  dtMDYY(ELSCEF);
       scSCexDt =  dtMDYY(ELSCEX);

       // Contractors Credit
       contractor = *on;
       contrInq = *on;

       If RRCCCD <> 0;
          contractor = *off;
          contrInq = *off;
          If dspMode;
             contrInq = *on;
          Endif;
       Endif;
       If RRCCHD <> *blanks;
          scContCrdH = RRCCHD;
       Else;
          scContCrdH ='Contractor Credit';
       Endif;

       scASP1 = ELASP1;
       scCCefDt =  dtMDYY(ELCREF);
       scCCexDt =  dtMDYY(ELCREX);

       // Schedule DB/CR
       schedule = *on;
       worksheet = *off;

       If RRDBCD <> 0 or RRCRCD <> 0;
          schedule = *off;
       Endif;

       scDBCR = ELDBCR;
       scDBCR2 = ELDBCR;
       scSDefDt =  dtMDYY(ELSDEF);
       scSDexDt =  dtMDYY(ELSDEX);

       Clear FlexHead11;
       FlexHead11 = 'Schedule Debit/Credit';
       If ClientId = '136';
          visibl136 = *on;
          If not dspMode;
             intFctr = 0;
             intRrn# = dbRrn;
             GetDev1CrWst(intRrn#:intFctr);
             If intFctr <> 0;
                scComModM = intFctr;
             Else;
                scComModM = 1;
             Endif;
          Else;
             scComModM = ELcmul;
          Endif;
       Else;
          visibl136 = *off;
       Endif;
       // Experience Modifiers
       scPROM = ELPROM;

       //scEXM1 = ELEXM1;
       scNoExm1 = ELnoexm1;
       If scNoExm1 = 'Y';
          scExm1 = 0;
       Else;
          scExm1 = ELexm1;
       Endif;
       //*** Begin Add *** 07/30/23 ****************************************
       // Set Experience Mod 2 is Rated or Not
       scNoExm2 = ELnoexm2;
       If scNoExm2 = 'Y';
          scExm2 = 0;
       Else;
          scExm2 = ELexm2;
       Endif;
       // Set Experience Mod 3 is Rated or Not
       scNoExm3 = ELnoexm3;
       If scNoExm3 = 'Y';
          scExm3 = 0;
       Else;
          scExm3 = ELexm3;
       Endif;
       //*** End   Add *** 07/30/23 ****************************************
       scEM1Date =  dtMDYY(ELEMD1);
       //*** Begin Del *** 07/30/23 ****************************************
       //*** scEXM2 = ELEXM2;
       //*** End   Del *** 07/30/23 ****************************************
       scEM2Date =  dtMDYY(ELEMD2);
       //*** Begin Del *** 07/30/23 ****************************************
       //*** scEXM3 = ELEXM3;
       //*** End   Del *** 07/30/23 ****************************************
       scEM3Date =  dtMDYY(ELEMD3);

       // ARAP
       arap = *off;

       If xxSt = 'MA';
          arap = *on;
       Endif;

       scARP1 = ELARP1;
       scARP2 = ELARP2;
       scARP3 = ELARP3;

       //*** Experience Modifier- Not final status flag ***
       scExMd1Nf = ELexMd1Nf;
       scExMd2Nf = ELexMd2Nf;
       scExMd3Nf = ELexMd3Nf;
       //*** Beg Add *** 03/04/24 ******************************************
       // Experience Rating
       scExpRat  = ELexRt;

       // Report to NCCI
       scNcci = ELncci;
       svNcci = scNcci;

       // Get Current state and compare state reporting division and
       // if they are the same then enable "Report to NCCI" checkbox
         //*** Beg Chg ***** 03/06/25 ******************************
         //***If dspMod = *on;
         If dspMode = *on;
         //*** End Chg ***** 03/06/25 ******************************
          disRptNcci = *on;
       Else;
          If FNpeof = *Blank;
             intStRptDiv = GetStRptDv(ELco#:Elfnd:Elemp#);
          Else;
             intStRptDiv = 0;
          Endif;
          If ELdiv = intStRptDiv;
             disRptNcci = *off;
          Else;
             disRptNcci = *On;
          Endif;
       Endif;
       //*** End Add *** 03/04/24 ******************************************

       // Stock Discount
       scASP3 = ELASP3;
       scDISC = ELDISC;
       scDISP = ELDISP;

       // Memebership Fees
       scMFEE = ELMFEE;

       // Safety Fees
       Clear scSFHD;
       If RRSFHD <> *blanks;
          scSFHD = RRSFHD;
       Else;
          scSFHD = 'Safety Fees';
       Endif;

       // Stock Discount Heading
       Clear scStcDscHd;
       If RRdshd <> *blanks;
          scStcDscHd = RRdshd;
       Else;
          scStcDscHd = 'Stock Discount';
       Endif;

       // Safety Fee Percentage.
       scSFPC = ELSFPC;
       If RRsfAp <> ' ';
          isSftyPer = *off;
       EndIf;

       // Safety Fee minimum amount.
       scSFMN = ELSFMN;
       If RRsfHd <> ' ';
          isSaftyAmt = *off;
       EndIf;

       If isSftyPer = *On and isSaftyAmt = *On;
          isSftyHdg  = *On;  // Safety heading grayed out.
       EndIf;

       // Waiver Surcharge
       waiver = *on;
       waivInq = *on;

       If RRWVAC <> 0;
          waiver = *off;
          waivInq = *off;
          If dspMode;
             waivInq = *on;
          Endif;
       Endif;

       scWVTP = ELWVTP;
       scWVPC = ELWVPC;
       scFCHG = ELFCHG;
       scWMIN = ELWMIN;
       scWMAX = ELWMAX;
       scWVFL = ELWVFL;

       //*** Begin Add *** 02/02/21 *******************************************
       // Blanket Waiver Description
       scOVRwD  = ELovrWd;
       scBlnkWd = ELblnkWd;

       If GetWaiverInfo(ELco#:ELfnd:RRst:ELpfdt:chrWvrForm:
        chrPrtPgm:chrAddPgm:chrBlnkWdDf) = *On;
          scBlnkWdDf = chrBlnkWdDf;
       Else;
          scBlnkWdDf = *Blanks;
       Endif;

       If scOvrWd <> 'Y';
          scBlnkWd = scBlnkWdDf;
       Endif;
       //*** End   Add *** 02/02/21 *******************************************


       // Facultative Reinsurance
       faculty = *on;

       If RRARCC <> 0;
          faculty = *off;
       Endif;

       scARCP = ELARCP;
       scARCA = ELARCA;
       scAREfDt = dtMDYY(ELAREF);
       scARExDt = dtMDYY(ELAREX);

       // Voluntary Labor
       volLabor = *on;
       Clear scVLCH;

       If RRVLCC <> 0 and RRVLCH <> *blanks;
          volLabor = *off;
          scVLCH = RRVLCH;
       Else;
          scVLCH = 'Voluntary Labor Chrg';
       Endif;

       scVLCA = ELVLCA;
       scScore = ELscore;
       scScoreDt = dtMDYY(ELscoreDt);
       scScoreBP = ELscoreBP;

       isCltFlg = *off;
       If clientID = '132' or clientID ='932';
          isCltFlg = *on;
       Endif;
       Select;
       When svFyr <> ELfyr;
          isFirst = *off;
       Endsl;
       If isFirst = *off;
          svFyr  = ELfyr;
          svPfDt = ELpfdt;
          svPtDt = ELptdt;
          svArSm = ELarsm;
          svIfFl = ELiffl;
          svQTyp = ELqtyp;
          svAdvr = ELadvr;
          s@RlFl = ELrlfl;
          svBsDt = ELbsdt;
          svStat = ELstat;
          svIlCd = ELilcd;
          svIlEf = ELilef;
          svIlEx = ELilex;
          svDcDe = ELdcde;
          svDcEf = ELdcef;
          svDcEx = ELdcex;
          svCxBl = ELcxbl;
          svAsp2 = ELasp2;
          svDvEf = ELdvef;
          svD1S2 = ELd1s2;
          svD1D2 = ELd1d2;
          svD1X2 = ELd1x2;
          svD2S2 = ELd2s2;
          svD2D2 = ELd2d2;
          svD2X2 = ELd2x2;
          svDvEx = ELdvex;
          svAsp1 = ELasp1;
          svCrEf = ELcref;
          svCrEx = ELcrex;
          svAspm = ELaspm;
          svMcEf = ELmcef;
          svMcEx = ELmcex;
          svMcCd = ELmccd;
          svMcGp = ELmcgp;
          svDev2 = ELdev2;
          svStfc = ELstfc;
          svD2Ef = ELd2ef;
          svD2Ex = ELd2ex;
          svExM1 = ELexm1;
          svEmD1 = ELemd1;
          svExM2 = ELexm2;
          svEmD2 = ELemd2;
          svExM3 = ELexm3;
          svEmD3 = ELemd3;
          svAsPs = ELasps;
          svScEf = ELscef;
          svScEx = ELscex;
          svSrPr = ELsrpr;
          svAsP3 = ELasp3;
          svDisc = ELdisc;
          svProm = ELprom;
          svDbCr = ELdbcr;
          svSdEf = ELsdef;
          svSdEx = ELsdex;
          svCxDy = ELcxdy;
          svPp3c = ELpp3c;
          svPpC3 = ELppc3;
          svMFee = ELmfee;
          svSfPc = ELsfpc;
          svSfam = ELsfam;
          svSfmn = ELsfmn;
          svAddm = ELaddm;
          svAnDt = ELandt;
          svDcd2 = ELdcd2;
          sv2DEf = EL2def;
          sv2DEx = EL2dex;
          svInl2 = ELinl2;
          svI2Ef = ELi2ef;
          svI2Ex = ELi2ex;
          svExMf = ELexmf;
          svWvTp = ELwvtp;
          svWvFl = ELwvfl;
          //***** Beg Add ***** 02/02/21 ***************************************
          svBlnkWd = ELblnkWd;
          //***** End Add ***** 02/02/21 ***************************************
          svWvPc = ELwvpc;
          svWMin = ELwmin;
          svFChg = ELfchg;
          svWMax = ELwmax;
          svASts = ELascd;
          svCoFl = ELcofl;
          svTypB = ELtypb;
          svPFin = ELpfin;
          svCtSt = ELctst;
          svBsCd = ELbscd;

          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pef01;
          If %found;
             svExCf = EFexcf;
             svExCo = EFexco;
             svScBd = EFscbd;
          Endif;

          isFirst = *on;
       Endif;
       //***  Payroll Tab  ********************************************************/

       scSTRP = EMSTRP;
       scRTCD_D = ELRTCD + ' - ' + RateCdDesc(ELCO#:ELRTCD);

       Clear xxKey;
       xxKey = %editc(elbscd : 'X');
       Chain keyWTABL PBS01;
       scBSCD_D = %trimr(xxKey) + ' - ' + BSDESC;
       If first;
          saveBSLPGM = BSLPGM;
       Endif;
       first = *off;
       // Get the Territorial Differential Factor
       GetTDF(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:outTDF);
       If outTDF = 0;
          scTDF = 'none';
       Else;
          scTDF = %EditW(outTDF:'0 .    ');
       EndIf;

       LoadPayrollGrd();
       If PyrlSlfRpt = *On;
          offMsgDsp = *off;
          Setll keyWMOOL POO00;
          Reade keyWMOOL POO00;
          Dow not(%eof);
             If OOstat <> 'I' and
                OOexfl = 'I'  and
                OOofpc <> 0   and
                OOefdt <> OOexdt and
                OOefdt <= ELptdt and
                (OOexdt = 0 or OOexdt > ELpfdt);
                   OffMsgDsp = *on;
                   leave;
             Endif;
          Reade keyWMOOL POO00;
          Enddo;
          LoadSrRptPrGrd();
          LoadSrRptGrd3();
       Endif;
       LoadVendorPGrd();

          LoadEmpPayrGrd();

       //***  Additional Info Tab  ************************************************/

       // Carrier Info
       Clear scRPCARR;
       Clear scRPCAP#;
       Clear scRPISDT;
       Clear scRPEFDT;
       Clear scRPEXDT;
       Clear scRPCLT#;
       Clear scRPTPA#;
       Clear scRPBRCH;
       Clear scRPCNTR;
       // Dividend Plan Info
       Clear scDivdPlan;
       Clear scFlatPrct;
       Clear scFlatDef;
       Clear scFlatDVal;
       // Dividend Amount Info
       Clear scDivAmt;
      //*** Beg Add *** 10/16/23 *** 153WAO ***************************
       Clear scRPPPOL;
      //*** End Add *** 10/16/23 *** 153WAO ***************************
      //*** Beg Add ***** 06/24/24 *************************
       // Collateral Information
       Clear scColwp;
       Clear scColwa;
       Clear scColrp;
       Clear scColTyp;
       Clear scColad;
       Clear scCollib;
       Clear scColln;
       Clear scCollid;
       Clear scColled;
       Clear scColsic;
       Clear scColsn;
       Clear scColsid;
       Clear scColsed;
      //*** End Add ***** 06/24/24 *************************

       Chain(n) keyWMRPL PRP00;

       If %found;
          scRPCARR = RPCARR;
          scRPCAP# = RPCAP#;
          scRPISDT = dtMDYY(RPISDT);
          scRPEFDT = dtMDYY(RPEFDT);
          scRPEXDT = dtMDYY(RPEXDT);
          scRPCLT# = RPCLT#;
          scRPTPA# = RPTPA#;
          scRPBRCH = RPBRCH;
          scRPCNTR = RPCNTR;
          saveDT1  = RPdt;
          saveTM1  = RPtime;
      //*** Beg Add *** 10/16/23 *** 153WAO ***************************
         scRPPPOL = RPPPOL;
      //*** End Add *** 10/16/23 *** 153WAO ***************************
       Endif;

       // Excluded States
       LoadExcludeGrd();

       scPnlTitle = 'Billing Division - '+%Editc(ELdiv:'X')+' '+
        GetEmpName(ELemp#:ELdiv:xxEmrrn:ELfnd:ELco#);

       // Policy Page 3C
       scFFLA = ELFFLA;
       scPP3C = ELPP3C;
       scPPC3 = ELPPC3;

       Clear scPP3CSav1;
       Clear scPP3CSav2;

       xxKey = 'PP3C';
       Setgt keyWDF4L PF400;
       Readpe key1WDF4L Pf400;
       Dow not %eof and F4Stat <> 'A';
          Readpe key1WDF4L Pf400;
       Enddo;
       If not %eof;
          scPP3CSav1 = F4TXT1;
          scPP3CSav2 = F4TXT2;
       Endif;

       If scFFLA <> 'Y';
          scPP3C = scPP3CSav1;
          scPPC3 = scPP3CSav2;
       Endif;

       // Cancellations
       scCXDY = ELCXDY;
       scADDM = ELADDM;
       scADVR = ELADVR;

       scRLFL = ELRLFL;
       svRLFL = ELRLFL;
       s2RLFL = ELRLFL;

       // Claims
       scEXMF = ELEXMF;

       // Audits
       scCTST = ELCTST;
       scAPFL = ELAPFL;
       scAAFL = ELAAFL;
       scAnCf = ELancf;
       // Dividend Plan Info
       scDivdPlan = ELdvcd;
       scFlatPrct = ELdvpc;
       scFlatDef  = ELdvdf;
       scFlatDVal = ELdvpr;
       charNormPrem = %char(GetTotPrem(ELco#:ELfnd:ELfyr:ELemp#:'C':'N'));
       Select;
       When ELfaf <> *blank;
          divPlChoic = 'DVPLID = (SELECT A.DVPLID FROM WDDVP A WHERE A.DVCO# = '
           + %CHAR(ELCO#) + ' AND WTDVP1.DVPLID = A.DVPLID AND A.DVMNLV <= '
           + %TRIM(CHARNORMPREM)+ ' AND A.DVMXLV > ' + %TRIM(CHARNORMPREM)+ ')';
       When ELfaf = *blank and ELnwrw = 'R';
          divPlChoic = 'DVCO# = ' + %CHAR(ELCO#) + ' AND DVPLID = '''
           +  ELDVCD + '''';
       Endsl;
       divPrChoic = 'GTCO# = ' + %CHAR(ELCO#) + ' AND GTTBID = ''FDPC''';
       Chain(n) (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pdv00;
       If %found and clientID = '111';
          scDivAmt = DVamt;
       Endif;

      //*** Beg Add ***** 06/24/24 ***************************
       // Collateral Information
       If clientID = '146';

          Exec sql
          Select EXocol into :xxOcol
           from WMEXP
           where EXco# = :ELco# and EXfnd = :ELfnd;

          If xxoCol = 'Y';                                                     // If EXocol = 'Y'
             scColwp = ELColwp;
             svColwp = scColwp;
             scColwa = ELColwa;
             svColwa = scColwa;
             scColrp = ELColrp;
             svColrp = scColrp;
             scColTyp = ELColTyp;
             svColTyp = scColTyp;
             scColad = ELColad;
             svColad = scColad;
             scCollib = ELCollib;
             svCollib = scCollib;
             scColln = ELColln;
             svColln = scColln;
             scCollid = dtMDYY(ELCollid);
             svCollid = scCollid;
             scColled = dtMDYY(ELColled);
             svColled = scColled;
             scColsic = ELColsic;
             svColsic = scColsic;
             scColsn = ELColsn;
             svColsn = scColsn;
             scColsid = dtMDYY(ELColsid);
             svColsid = scColsid;
             scColsed = dtMDYY(ELColsed);
             svColsed = scColsed;
             disableCol = *Off;
             scCoTlTip = ' ';
             LoadCollateral();
          Else;
             disableCol = *On;
             scCoTlTip = 'Collateral has not been set up for this group.';
             clrCollate = *On;
          Endif;                                                               // EndIf EXocol = 'Y'
       Endif;

      //*** End Add ***** 06/24/24 ***************************

       // Other Policy
       flex1 = *off;
       flex2 = *off;
       flex3 = *off;
       flex4 = *off;
       flex5 = *off;
       flexBox1 = *off;
       flexBox2 = *off;
       flexBox3 = *off;
       flexBox4 = *off;
       flexBox5 = *off;
       flexDrop1 = *off;
       flexDrop2 = *off;
       flexDrop3 = *off;
       flexDrop4 = *off;
       flexDrop5 = *off;
       flex = 'N';
       flex1Req = *off;
       flex2Req = *off;
       flex3Req = *off;
       flex4Req = *off;
       flex5Req = *off;
       flex6Req = *off;

       Clear scFlexHdg1;
       Clear scFlexHdg2;
       Clear scFlexHdg3;
       Clear scFlexHdg4;
       Clear scFlexHdg5;

       Clear flex1Choic;
       Clear flex2Choic;
       Clear flex3Choic;
       Clear flex4Choic;
       Clear flex5Choic;

       If EXEHD1 <> *blanks;
          scFlexHdg1 = EXEHD1;
          flex1 = *on;
          If EXTBD1 <> *blanks;
             //*** Beg Chg *** 07/22/24 *********************************   ***
             //***flex1Choic = 'GTTBID = ''' + EXTBD1 + '''';
             flex1Choic = 'GTTBID = ''' + EXTBD1 + '''' +
              ' and GTSTAT <> ''I''';
             //*** End Chg *** 07/22/24 *********************************   ***
             flexDrop1 = *on;
             scFFDD1 = ELFFD1;
          Else;
             flexBox1 = *on;
             scFFD1 = ELFFD1;
          Endif;
       Endif;

       // Suppress Premium Notices in Mass Billing
       scPrmNtMas = ELprmNtMas;

       If EXEHD2 <> *blanks;
          scFlexHdg2 = EXEHD2;
          flex2 = *on;
          If EXTBD2 <> *blanks;
             //*** Beg Chg *** 07/22/24 *********************************   ***
             //***flex2Choic = 'GTTBID = ''' + EXTBD2 + '''';
             flex2Choic = 'GTTBID = ''' + EXTBD2 + '''' +
              ' and GTSTAT <> ''I''';
             //*** End Chg *** 07/22/24 *********************************   ***
             flexDrop2 = *on;
             scFFDD2 = ELFFD2;
          Else;
             flexBox2 = *on;
             scFFD2 = ELFFD2;
          Endif;
       Endif;

       scPolTypHd = 'Type of Policy';

       Chain keyWDFHL PFH00;

       If %found;

          flex = 'Y';

          If FHFH#1 <> *blanks;
             scFlexHdg1 = FHFH#1;
             flex1 = *on;
             If FHF1TB <> *blanks;
                //*** Beg Chg *** 07/22/24 ************************************
                //***flex1Choic = 'GTTBID = ''' + FHF1TB + '''';
                //*** Beg Chg ***** 09/03/25 **********************************
                //*** flex1Choic = 'GTTBID = ''' + FHF1TB + ''''
                 //*** + GTSTAT <> 'I';
                flex1Choic = 'GTTBID = ''' + FHF1TB + '''' +
                 ' and GTSTAT <> ''I''';
                //*** End Chg ***** 09/03/25 **********************************
                //*** End Chg *** 07/22/24 ************************************
                flexDrop1 = *on;
                scFFDD1 = ELFFD1;
             Else;
                flexBox1 = *on;
                scFFD1 = ELFFD1;
             Endif;
          Endif;

          If FHFH#2 <> *blanks;
             scFlexHdg2 = FHFH#2;
             flex2 = *on;
             If FHF2TB <> *blanks;
                //*** Beg Chg *** 07/22/24 ************************************
                //***flex2Choic = 'GTTBID = ''' + FHF2TB + '''';
                flex2Choic = 'GTTBID = ''' + FHF2TB + '''' +
                 ' and GTSTAT <> ''I''';
                //*** End Chg *** 07/22/24 ************************************
                flexDrop2 = *on;
                scFFDD2 = ELFFD2;
             Else;
                flexBox2 = *on;
                scFFD2 = ELFFD2;
             Endif;
          Endif;

          If FHFH#3 <> *blanks;
             scFlexHdg3 = FHFH#3;
             flex3 = *on;
             If FHF3TB <> *blanks;
                //*** Beg Chg *** 07/22/24 ************************************
                //***flex3Choic = 'GTTBID = ''' + FHF3TB + '''';
                flex3Choic = 'GTTBID = ''' + FHF3TB + '''' +
                 ' and GTSTAT <> ''I''';
                //*** End Chg *** 07/22/24 ************************************
                flexDrop3 = *on;
                scFFDD3 = ELFFD3;
             Else;
                flexBox3 = *on;
                scFFD3 = ELFFD3;
             Endif;
          Endif;

          If FHFH#4 <> *blanks;
             scFlexHdg4 = FHFH#4;
             flex4 = *on;
             flexBox4 = *on;
             scFFD4 = ELFFD4;
          Endif;

          If FHFH#5 <> *blanks;
             scFlexHdg5 = FHFH#5;
             flex5 = *on;
             flexBox5 = *on;
             scFFD5 = ELFFD5;
          Endif;

          If FHFH#6 <> *blanks;
             scPolTypHd = FHFH#6;
          Endif;

       Endif;

       // Do not allow a Dividend Plan to be selected if a bill hasn't been run. If the renewal
       // has rolled with a new plan, let it be edited before running a bill.
       If ELfaf =  *blank and ELdvcd = *blank;
          dsbDivdPln = *On;
       Endif;

      /end-free
      //
      // Setup for Flex fields editing.
      //
      /FREE
       select;
       when qteMode;
         if flex = 'N';
           xxRQD1 = EXERQ1;
           xxRQD2 = EXERQ2;
         endif;
         //
         if flex = 'Y';
           xxRQD1 = FHFH1Q;
           xxRQD2 = FHFH2Q;
           xxRQD3 = FHFH3Q;
           xxRQD4 = FHFH4Q;
           xxRQD5 = FHFH5Q;
           xxRQD6 = FHFH6Q;
         endif;                                                               //flex = 'Y'
       other;
         if flex = 'N';
           xxRQD1 = EXERW1;
           xxRQD2 = EXERW2;
         endif;
         //
         if flex = 'Y';
           xxRQD1 = FHFH1W;
           xxRQD2 = FHFH2W;
           xxRQD3 = FHFH3W;
           xxRQD4 = FHFH4W;
           xxRQD5 = FHFH5W;
           xxRQD6 = FHFH6W;
         endif;                                                               //flex ='Y'
       endsl;
      /END-FREE
      /free

       If xxRQD1 = 'Y';
          flex1Req = *on;
       Endif;

       If xxRQD2 = 'Y';
          flex2Req = *on;
       Endif;

       If xxRQD3 = 'Y';
          flex3Req = *on;
       Endif;

       If xxRQD4 = 'Y';
          flex4Req = *on;
       Endif;

       If xxRQD5 = 'Y';
          flex5Req = *on;
       Endif;

       If xxRQD6 = 'Y';
          flex6Req = *on;
       Endif;

       //***  Large Deductible Tab  **********************************************/

       // Large Deductible
       largeDeduc = *off;
       NoLargeDed = *off;
       VisibAggrP  = *off;
       VisibVarA     = *off;

       Clear scLDEFD1;
       Clear scLDEXP1;
       Clear scLDRNW1;
       Clear scLDDES1;
       Clear scLDPCT1;
       Clear scLDTYP1;
       Clear scLDAMP1;
       Clear scLDAMG1;
       Clear scLDVRB1;
       Clear scLDOCV1;
       Clear scLDCLS1;
       Clear scLDPLV1;
       Clear scLDPLN1;
       Clear scLDTOT1;
       Clear scLDALA1;
       Clear scLDR101;
       Clear scLDR102;
       Clear scLDR103;
       Clear scLDR104;
       Clear scLDR105;
       Clear scLDR106;
       Clear scLDR107;
       Clear scLDR108;
       Clear scLDR109;
       Clear scLDR110;
       Clear scADP1;
       Clear scVarA1;

       Clear scLDEFD2;
       Clear scLDEXP2;
       Clear scLDDES2;
       Clear scLDPCT2;
       Clear scLDTYP2;
       Clear scLDAMP2;
       Clear scLDAMG2;
       Clear scLDVRB2;
       Clear scLDOCV2;
       Clear scLDCLS2;
       Clear scLDPLV2;
       Clear scLDPLN2;
       Clear scLDTOT2;
       Clear scLDALA2;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR102;
       Clear scLDR210;
       Clear scADP2;
       Clear scVarA2;
       Clear scClmLib;
       Clear scClmAlw;

       Clear xxKey;
       xxKey = %editc(elbscd : 'X');
       Chain keyWTABL PBS01;
       If BSLPGM = 'L';
          largeDeduc = *on;
          VisibAggrP = *off;
          VisibVarA   = *off;
          If ClientId = '150';
             VisibAggrP = *on;
             VisibVarA = *on;
          Endif;

       Endif;

       If BSLPGM <> 'L' and WCFL43 = 'Y';
          NoLargeDed = *on;
       Endif;
       poltypcde = ' ';
       visbClmLib = *Off;
       visbClmAlw = *Off;
       If ClientId = '025'and largeDeduc = *on;
          xxTbid = 'PTYP';
          xxkey = ELqtyp;
          Chain keyWTGTL PGT00;
          If %Found(WTGTL);
             poltypcde = GTfg01;
             If poltypcde = 'A' or poltypcde = 'M';
                visbClmLib = *On;
             Endif;
             If poltypcde = 'M';
                visbClmAlw = *On;
             Endif;
          Endif;
       Endif;

       //*** Begin Del *** 09/02/20 ****************************************
       //*** If visbClmLib = *On or visbClmAlw = *On;
       //***    visible025 = *On;
       //*** Else;
       //***    visible025 = *Off;
       //*** Endif;
       //*** End   Del *** 09/02/20 ****************************************

       Chain(n) keyWMLDL1 PLD01;
       isLargeDedRcd = %found;
       If not %found  and LDemp# = 0 and addMode;
          Clear RSjrSt;
          chain (scCo#:scRtCd) prs00;
          setgt (scCo#:scFnd:RSjrSt:0:scPfDt) pld04;
          readpe (scCo#:scFnd:RSjrSt:0) pld04;
          if not %eof and LDdes1 <> ' ';
             isLargeDedRcd = *on;
             LDefD1 = ELpfDt;
             if LDdes2  <>  ' ';
                LDefd2 = ELpfDt;
             Endif;
             LDexp1 = *zero;
             LDexp2 = *zero;
             LDdt   = *zero;
             LDtime = *zero;
          Endif;
       Endif;


       If isLargeDedRcd;
          scLDEFD1 =  dtMDYY(LDEFD1);
          scLDEXP1 =  dtMDYY(LDEXP1);
          scLDRNW1 = LDRNW1;
          scLDDES1 = LDDES1;
          scLDPCT1 = LDPCT1;
          scLDTYP1 = LDTYP1;
          scLDAMP1 = LDAMP1;
          scLDAMG1 = LDAMG1;
          scLDVRB1 = LDVRB1;
          scLDOCV1 = LDOCV1;
          scLDCLS1 = LDCLS1;
          scLDPLV1 = LDPLV1;
          scLDPLN1 = LDPLN1;
          scLDTOT1 = LDTOT1;
          scLDALA1 = LDALA1;
          scLDR101 = LDR101;
          scLDR102 = LDR102;
          scLDR103 = LDR103;
          scLDR104 = LDR104;
          scLDR105 = LDR105;
          scLDR106 = LDR106;
          scLDR107 = LDR107;
          scLDR108 = LDR108;
          scLDR109 = LDR109;
          scLDR110 = LDR110;
          scADP1   = LDadp1;
          scVarA1  = LDvarA1;


          scLDEFD2 =  dtMDYY(LDEFD2);
          scLDEXP2 =  dtMDYY(LDEXP2);
          scLDDES2 = LDDES2;
          scLDPCT2 = LDPCT2;
          scLDTYP2 = LDTYP2;
          scLDAMP2 = LDAMP2;
          scLDAMG2 = LDAMG2;
          scLDVRB2 = LDVRB2;
          scLDOCV2 = LDOCV2;
          scLDCLS2 = LDCLS2;
          scLDPLV2 = LDPLV2;
          scLDPLN2 = LDPLN2;
          scLDTOT2 = LDTOT2;
          scLDALA2 = LDALA2;
          scLDR201 = LDR201;
          scLDR202 = LDR202;
          scLDR203 = LDR203;
          scLDR204 = LDR204;
          scLDR205 = LDR205;
          scLDR206 = LDR206;
          scLDR207 = LDR207;
          scLDR208 = LDR208;
          scLDR209 = LDR209;
          scLDR210 = LDR210;
          saveDT2  = LDdt;
          saveTM2  = LDtime;
          scADP2   = LDadp2;
          scVarA2  = LDvarA2;
          scClmLib = LDlirt;
          scClmAlw = LDclal;
       //*** Begin Add *** 09/02/20 **********************************
          scIncMdIn = LDincMdIn;
          If scIncMdIn = *blank;
             scIncMdIn = 'B';
          Endif;
       //*** End   Add *** 09/02/20 **********************************

       Endif;

       //***  Retrospective Rating Tab  *******************************************/

       // Retrospective Rating
       retro = *off;
       NoRetro = *off;

       Clear scMNFC;
       Clear scMXFC;
       Clear scBSFC;
       Clear scTAX;
       Clear scTAXF;
       Clear scLCVF;
       Clear scRETN;
       Clear scELP;
       Clear scELPF;
       Clear scRDF1;
       Clear scRDF2;
       Clear scRDF3;
       Clear scSTDT;
       Clear scSTAM;
       Clear scFSET;

       If BSLPGM = 'I' or BSLPGM = 'P' or BSLPGM = 'R';
          retro = *on;

          scMNFC = ELMNFC;
          scMXFC = ELMXFC;
          scBSFC = ELBSFC;
          scTAX  = ELTAX;
          scTAXF = ELTAXF;
          scLCVF = ELLCVF;
          scRETN = ELRETN;

          scELP  = ELELP;
          scELPF = ELELPF;
          scRDF1 = ELRDF1;
          scRDF2 = ELRDF2;
          scRDF3 = ELRDF3;

          scSTDT =  dtMDYY(ELSTDT);
          scSTAM = ELSTAM;
          scFSET = ELFSET;

       Endif;

       If BSLPGM <> 'I' and BSLPGM <> 'P' and BSLPGM <> 'R' and WCFL11 = 'Y';
          NoRetro = *on;
       Endif;


       //***  Agency History Tab  *******************************************/

       // Agency History
       LoadAgencyGrid();

       //***  Scheduled Rating Tab  **********************************************/

       // Scheduled Rating
       schdRating = *off;
       schdRteInq = *off;
       noSchedule = *off;

       Clear scRFPRCA;
       Clear scRFPRDA;
       Clear scRFPRRS;
       Clear scRFCPCA;
       Clear scRFCPDA;
       Clear scRFCPRS;
       Clear scRFMFCA;
       Clear scRFMFDA;
       Clear scRFMFRS;
       Clear scRFSDCA;
       Clear scRFSDDA;
       Clear scRFSDRS;
       Clear scRFEMCA;
       Clear scRFEMDA;
       Clear scRFEMRS;
       Clear scRFMCCA;
       Clear scRFMCDA;
       Clear scRFMCRS;
       Clear scRFMSCA;
       Clear scRFMSDA;
       Clear scRFMSRS;
       Clear scRFLSCF;
       Clear scRFLSDT;
       Clear scRFPEFL;
       Clear scRFPEEM;
       Clear scRFCOM1;
       Clear scRFCOM2;
       Clear scRFCOM3;

       If RRDbcr = 'Y';
          schdRating = *on;
          If not dspMode;
             schdRteInq = *on;
          Endif;
       Endif;

       If RRDbcr <> 'Y' and WCFL53 = 'Y';
          NoSchedule = *on;
       Endif;

       Chain(n) keyWMRFL RFP00;
       If %found;
          scRFPRCA = RFPRCA;
          scRFPRDA = RFPRDA;
          scRFPRRS = RFPRRS;
          scRFCPCA = RFCPCA;
          scRFCPDA = RFCPDA;
          scRFCPRS = RFCPRS;
          scRFMFCA = RFMFCA;
          scRFMFDA = RFMFDA;
          scRFMFRS = RFMFRS;
          scRFSDCA = RFSDCA;
          scRFSDDA = RFSDDA;
          scRFSDRS = RFSDRS;
          scRFEMCA = RFEMCA;
          scRFEMDA = RFEMDA;
          scRFEMRS = RFEMRS;
          scRFMCCA = RFMCCA;
          scRFMCDA = RFMCDA;
          scRFMCRS = RFMCRS;
          scRFMSCA = RFMSCA;
          scRFMSDA = RFMSDA;
          scRFMSRS = RFMSRS;
          scRFLSCF = RFLSCF;
          scRFLSDT =  dtMDYY(RFLSDT);
          scRFPEFL = RFPEFL;
          scRFPEEM = RFPEEM;
          scRFCOM1 = RFCOM1;
          scRFCOM2 = RFCOM2;
          scRFCOM3 = RFCOM3;
          saveDT3  = RFdt;
          saveTM3  = RFtime;

       Endif;

       //*************************************************************************/

       saveDT = ELDT;
       saveTM = ELTIME;

       //**** dsSaveDet = dsPolicyDet;
       //***  Begin Add  ***  05/04/20  ******************************
       isShowAnalytics = *off;
       //***** Beg Add ***** 08/02/21 ********************************
       visGrad = *off;
       visVal = *off;
       //***** End Add ***** 08/02/21 ********************************
       //***** Beg Chg ***** 08/02/21 ********************************
       //*****isShowAnalytics = IsValidFetch(sqlstt);
       strScoreVendor = GetGrScoringVnd(ELco#:ELfnd);
       If strScoreVendor <> *blanks;
          isShowAnalytics = *on;
       Endif;
       //***** End Chg ***** 08/02/21 ********************************

       //***** Beg Chg ***** 08/02/21 ********************************
       If strScoreVendor = 'Gradient';
          visGrad = *on;
          Reset dsWIGSP;
          Exec sql
          select * into :dsWIGSP
           from WIGSP
          where GSco# = :ELco# and GSfnd = :ELfnd and GSfyr = :ELfyr
          and GSemp# = :ELemp#;
          scPrdLos  = dsWIGSP.GSprdLos;
          scQtePrm  = dsWIGSP.GSqtePrm;
          scPLosRat = dsWIGSP.GSpLosRat;
          scPerc    = dsWIGSP.GSperc;
          scRvwTrgs = dsWIGSP.GSrvwTrgs;
          scTrgPLR  = dsWIGSP.GStrgPLR;
          scTrgPrm  = dsWIGSP.GStrgPrm;
          scHRngPrm = dsWIGSP.GShRngPrm;
          scLRngPrm = dsWIGSP.GSlRngPrm;
          scTrgFct  = dsWIGSP.GStrgFct;
          scHRngFct = dsWIGSP.GShRngFct;
          scLRngFct = dsWIGSP.GSlRngFct;
       Endif;
       //***** End Chg ***** 08/02/21 ********************************

       //***** Beg Del ***** 08/02/21 ********************************
       //*****isShowAnalytics = *on;
       //***** End Del ***** 08/02/21 ********************************

       //***  End   Add  ***  05/04/20  ******************************

       //***** Beg Add ***** 08/02/21 ********************************
       If strScoreVendor = 'Valen';
          visVal = *on;
          Reset dsWIVSP;
          Exec sql
          select * into :dsWIVSP
           from WIVSP
           where VSco# = :ELco# and VSfnd = :ELfnd and VSfyr = :ELfyr
           and VSemp# = :ELemp#;

          scRatSc = ELscore;
          scRptID = 'ReportID: ' + dsWIVSP.VSscrid;
          scPDFUrl = strRptHead + BuildUNCUrl(dsWIVSP.VSrptPath);
          scPrcBin = dsWIVSP.VSpriceb;
       Endif;
       //***** End Add ***** 08/02/21 ********************************

      /end-free
     P LoadPolicyDet   E

       //***************************************************************************//
       //===================================================================
       // Load Payroll Grid
       //===================================================================
     P LoadPayrollGrd  B
     D LoadPayrollGrd  PI
      /free

       // Clear the subfile.
       RcdPayrollDsp(*off);
       clrPayroll = *On;
       Write rcdPayroll;
       clrPayroll = *Off;
       RcdPayrollDsp(*on);
       rrnPayroll = 0;

       // Clear payroll totals
       Clear scTotalEst;
       Clear scTotalRep;
       Clear scTotalAud;

       // Populate the subfile
       Setll keyWMEPL PEP00;
       Reade keyWMEPL PEP00;

       Dow not %eof;

          If scPayFStat = ' ' or EPSTAT = scPayFStat;

           //scEPEFDT =  dtMDYY(EPEFDT);
           //scEPCADT =  dtMDYY(EPCADT);
             scEPEFDT =  CvtToDate(EPEFDT);
             scEPCADT =  CvtToDate(EPCADT);
             scEPCLAS =  EPCLAS;
             scEPSPEC =  EPSPEC;
             scEPEXT  =  EPEXT;
             scEPEAPR =  EPEAPR;
             scEPSAPR =  EPSAPR;
             scEPAAPR =  EPAAPR;
             scEPARAT =  EPARAT;
             scPayRRN =  dbrrn2;
             If EPSTAT = 'A';
                scEPStatus = 'Active';
             Else;
                scEPStatus = 'Inactive';
             Endif;

             Clear scEPDesc;
             xxType = 'P';
             xxEfDt = SrEffd(scCO#:scFND:scFYR:scEMP#:scDIV:EPEFDT:xxType:
              EPCLAS);
             Setgt keyWMPRL1 PPR01;
             Readpe key1WMPRL1 PPR01;
             If not %eof;
                scOEPARAT = GetOrgRate();
                //*** Begin Add *** 08/29/22 ******************************
                // Adding it back
                scEPDesc = PRDESC;
                //*** End   Add *** 08/29/22 ******************************
                If EPEXT <> *blanks;
                   Clear xxPxSt;
                   Setgt keyWMPXL1 PPX01;
                   Readpe key1WMPXL1 PPX01;
                   If not %eof;
                      scEPDesc = PXDESC;
                   Else;
                      xxPxSt = xxSt;
                      Setgt keyWMPXL1 PPX01;
                      Readpe key1WMPXL1 PPX01;
                      If not %eof;
                         scEPDesc = PXDESC;
                      Endif;
                   Endif;
                Endif;
             Endif;

             scTotalEst = scTotalEst + EPEAPR;
             scTotalRep = scTotalRep + EPSAPR;
             scTotalAud = scTotalAud + EPAAPR;

             rrnPayroll =  rrnPayroll + 1;
             If rrnPayroll = 1;
               scEPsel = *On;
             Else;
               scEPsel = *Off;
             Endif;
             Write PayrollSFL;

          Endif;

          Reade keyWMEPL PEP00;
       Enddo;
       If isContribution = '1';
          flxEstMan = 'Estimated Manual Contribution';
          flxAudMan = 'Audited Manual Contribution';
       Else;
          flxEstMan = 'Estimated Manual Premium';
          flxAudMan = 'Audited Manual Premium';
       Endif;

       isVisAUDSR = ELaudsr = 'Y';

       //* Position payroll grid at top at all times.
       rrnPayroll = 1;

      /end-free
     P LoadPayrollGrd  E


       //===================================================================
     P LoadSrRptPrGrd  B
     D LoadSrRptPrGrd  PI

      /free
       // rrnSrGr1Cr = 1;
       If rrnSrGr1Cr = 0;
          rrnSrGr1Cr = 1;
       Endif;
       Clear xxKey;
       xxKey = %editc(elbscd : 'X');
       Chain keyWTABL PBS01;
       Select;
       When BSFreq = 'W';
            frequency = 'Weekly';
       When BSFreq = 'M';
            frequency = 'Monthly';
       Other;
            frequency = '  ';
       Endsl;
       svDoWk = BSdowk;

       // Clear the subfile.
       RcdPayrollDsp(*off);
       clrSrGrid1 = *On;
       Write rcdPayroll;
       clrSrGrid1= *Off;
       RcdPayrollDsp(*on);
       rrnSRGrid1 = 0;

       Clear scSrGr1Tot;
       Clear scSrGr1Cal;
       Clear svDate;
       Clear scSrGr1Amt;
       Clear scSrClcPrm;
       Clear totPremium;
       Clear terrPayroll;
       Clear manPremium;

       // Populate the subfile
       If  frequency <> 'Weekly';
           Setll keyWASRL6E  PSR06E;
           Reade keyWASRL6E  PSR06E;
       Else;
           Setll keyWASRL9  PSR09;
           Reade keyWASRL9  PSR09;
       Endif;

       If %Eof;
          Return;
       Endif;

       Clear sflSrGrid1;
       PyrlSlfRpt = *On;
       svDate = SRrpmy;
       svDate1 = SRtodt;

       Dow not %eof;

           If svDate <> SRrpmy or svDate1 <> SRtodt;

              If @@mode <> 'D';
                 NonDspEdtL = *On;
                 NonDspCalL = *On;
              Else;
                 NonDspEdtL = *Off;
                 NonDspCalL = *Off;
              Endif;

              rrnSRGrid1 =  rrnSRGrid1 + 1;
           //*** begin chg ***  03/01/22  ***********************************
           //*****   If Not outNetRated;
              If Not outNetRated or clientID <> '136';
           //*** end chg *****  03/01/22  ***********************************
                 Exsr SRPREM;
              Else;
                 totPremium = manPremium;
              EndIf;
              If clientID = '150';
                 scSrClcPrm  = totPremium2;
                 scSrGr1Cal += totPremium2;
              Else;
                 scSrClcPrm  = totPremium;
                 scSrGr1Cal += totPremium;
              Endif;
              scSrGr1Tot += scSrGr1Amt;
              Write sflSrGrid1;
              Clear scSrGr1Amt;
              Clear scSrClcPrm;
              Clear totPremium;
              Clear terrPayroll;
              Clear manPremium;
              svDate  = SRrpmy;
              svDate1 = SRtodt;
           Endif;

           Exsr LoadSrGrid;

           manPremium += getManPrem(outNetRated);
           scSrGr1Amt += SRamtp;

           If  frequency <> 'Weekly';
               scSrGr1RRN  = dbrrn5;
               Reade keyWASRL6E PSR06E;
           Else;
               scSrGr1RRN  = dbrrn6;
               Reade keyWASRL9 PSR09;
           Endif;

       Enddo;

       // Write last record to grid
       If %Eof;

          If @@mode <> 'D';
             NonDspEdtL = *On;
             NonDspCalL = *On;
          Else;
             NonDspEdtL = *Off;
             NonDspCalL = *Off;
          Endif;

           rrnSRGrid1 += 1;
        //*** begin chg ***  03/01/22  ***********************************
        //*****   If Not outNetRated;
           If Not outNetRated or clientID <> '136';
        //*** end chg *****  03/01/22  ***********************************
              Exsr SRPREM;
           Else;
              totPremium = manPremium;
           EndIf;
           If clientID = '150';
              scSrClcPrm  = totPremium2;
              scSrGr1Cal += totPremium2;
           Else;
              scSrClcPrm  = totPremium;
              scSrGr1Cal += totPremium;
           Endif;
           scSrGr1Tot += scSrGr1Amt;
           Write sflSrGrid1;
       Endif;

       //* List of reported payrolls for the highlighted line item in grid 1

           Chain(n) scSrGr1RRN PSR0P;
           If %Found(wasrp);
              Chain rrnSrGr1Cr sflSrGrid1;
              If %found and (srTodt <> 0 or srRpmy <> 0);
                 Exsr LoadSrRptGrd2;
              Endif;
           Else;
              RcdPayrollDsp(*off);
              clrSrGrid2 = *On;
              Write rcdPayroll;
              RcdPayrollDsp(*on);
              clrSrGrid2= *Off;
              rrnSRGrid2 = 0;
              Clear scSrGr2Tot;
           Endif;
           //* Position SR Payroll Grid 2 at top at all times.
           rrnSRGrid2 = 1;

           If isContribution = '1';
              flxSlfRpGr='Reporting Period,Total Payroll,+
               Calculated Contribution';
           Else;
              flxSlfRpGr='Reporting Period,Total Payroll,Calculated Premium';
           Endif;
       //* Position payroll grid at top at all times.
       rrnSRGrid1 = 1;

       Begsr LoadSrGrid;
             If frequency <> 'Weekly';
                inoutNum   =  %editc(dt6MDYY(SRrpmy):'X');
                inedtMsk = 'RPMY';
                WA003R(inoutNum:inEdtMsk);
                scSrGr1Rpt = inoutNum;
             Else;
                inoutNum = %editc(dtMDYY(SRtodt):'X');
                inedtMsk = 'DATE';
                WA003R(inoutNum:inEdtMsk);
                scSrGr1Rpt = inoutNum;
             Endif;
       Endsr;

       Begsr LoadSrRptGrd2;
             RcdPayrollDsp(*off);
             clrSrGrid2 = *On;
             Write rcdPayroll;
             RcdPayrollDsp(*on);
             clrSrGrid2= *Off;
             rrnSRGrid2 = 0;
             Clear scSrGr2Tot;

             If frequency <> 'Weekly';
                pos# = %scan('/': scSrGr1Rpt);
                If pos# > 0;
                   Clear tmpdate1;
                   Clear tmpdate2;
                   tmpdate1 = %subst(scSrGr1Rpt : 1 : pos# - 1);
                   tmpdate2 = %subst(scSrGr1Rpt : pos# + 1 : 7);
                   svdate = dt5CYMD(%int(tmpDate));
                Endif;
                Setll keyWASRL6E_1  PSR06E;
                Reade keyWASRL6E_1  PSR06E;
             Else;
                tmpDate1_1 = tmpDate3;
                tmpDate2_1 = tmpDate5;
                tmpDate3_1 = tmpDate7;
                svdate1 = dtCYMD(%int(tmpDate_1));
                Setll keyWASRL9_1  PSR09;
                Reade keyWASRL9_1  PSR09;
             Endif;
                Dow not %eof;
                    Exsr LoadDataGrid2;
                    If frequency <> 'Weekly';
                       Reade keyWASRL6E_1 PSR06E;
                    Else;
                       Reade keyWASRL9_1  PSR09;
                    Endif;
                Enddo;
       Endsr;

       Begsr LoadDataGrid2;
             If frequency <> 'Weekly';
                inoutNum   =  %editc(dt6MDYY(SRrpmy):'X');
                inedtMsk = 'RPMY';
                WA003R(inoutNum:inEdtMsk);
                scSrGr2Dt= inoutNum;
             Else;
                inoutNum = %editc(dtMDYY(SRtodt):'X');
                inedtMsk = 'DATE';
                WA003R(inoutNum:inEdtMsk);
                scSrGr2Dt= inoutNum;
             Endif;

             scSrGr2Amt =  SRamtp;
             scSrGr2EDt =  CvtToDate(SRefdt);
             scSrGr2Cls =  SRclas;
             scSrGr2Spc =  SRspec;
             scSrGr2Ext =  Srext;

             Clear scSrGr2Dsc;
             xxType = 'P';
             xxEfDt = SrEffd(scCO#:scFND:scFYR:scEMP#:scDIV:SRefdt:xxType:
              SRclas);
             Setgt  (scCo#:scRtcd:SRclas:xxEfDt) ppr01;
             Readpe (scCo#:scRtcd:SRclas)        ppr01;
             If not %eof;
                scSrGr2Dsc = PRdesc;
                If SRext <> *blanks;
                   xxPxSt = xxSt;
                   Setgt  (scCo#:SRclas:SRext:xxPxSt:SRefdt) ppx01;
                   Readpe (scCo#:SRclas:SRext:xxPxSt) ppx01;
                   If not %eof;
                      scSrGr2Dsc = PXdesc;
                   Else;
                      Clear xxPxSt;
                      Setgt  (scCo#:SRclas:SRext:xxPxSt:SRefdt) ppx01;
                      Readpe (scCo#:SRclas:SRext:xxPxSt) ppx01;
                      If not %eof;
                         scSrGr2Dsc = PXdesc;
                      Endif;
                   Endif;
                Endif;
             Endif;

             scSrGr2Tot= scSrGr2Tot + SRamtp;

             rrnSRGrid2 =  rrnSRGrid2 + 1;
             Write sflSrGrid2;

       Endsr;
       Begsr SrPrem;
        Clear qqAdLd;
        Clear totPremium;
        Clear totTax;
       //*** begin chg ***  03/01/22  *****************************************
       //***** If SRTodt <> 0 or SRRpmy <> 0;
       //*****    qqtodt = dtMDYY(SRtodt);
       //*****    qqrpmy = dt6MDYY(SRrpmy);
       //***** Endif;
           qqtodt = dtMDYY(svDate1);
           qqrpmy = dt6MDYY(svDate);
       //*** end chg *****  03/01/22  *****************************************
        qqRRN = dbRRN;
        qqPayR = terrPayroll;
        CalcSrPrem(qqRRN:qqRpMY:qqToDt:manPremium:qqAdld:qqPayR:
         totPremium:totTax:qqPPrm:qqPTax:qqFact:qqPrev:qqSrOn:
         qqInfo:qqHdg:qqPro:totPremium2:totTax2:qqPPrm2:qqPTax2:
         qqFact2);
       Endsr;
      /End-Free
     P LoadSrRptPrGrd  E
       //***************************************************************************//
       //===================================================================
     P LoadSrRptGrd3   B
     D LoadSrRptGrd3   PI
      /free

       Clear xxKey;
       xxKey = %editc(elbscd : 'X');
       Chain keyWTABL PBS01;
       Select;
       When BSFreq = 'W';
            frequency = 'Weekly';
       When BSFreq = 'M';
            frequency = 'Monthly';
       Other;
            frequency = '  ';
       Endsl;

       // Clear the subfile.
       RcdPayrollDsp(*off);
       clrSRRAudt = *On;
       Write rcdPayroll;
       clrSRRAudt= *Off;
       RcdPayrollDsp(*on);
       rrnSRRAudt = 0;

       // Clear payroll totals
       Clear scTotSRAmt;

       // Populate the subfile
       Setll keyWASRL6E  PSR06E;
       Reade keyWASRL6E  PSR06E;

       Dow not %eof;
           If frequency <> 'Weekly';
              inoutNum   =  %editc(dt6MDYY(SRrpmy):'X');
              inedtMsk = 'RPMY';
              WA003R(inoutNum:inEdtMsk);
              scSrDate = inoutNum;
           Else;
              inoutNum = %editc(dtMDYY(SRtodt):'X');
              inedtMsk = 'DATE';
              WA003R(inoutNum:inEdtMsk);
              scSrDate = inoutNum;
           Endif;

           scSRAmount =  SRamtp;
           scSREffDt  =  CvtToDate(SRefdt);
           scSRCLAS  =  SRclas;
           scSRSPEC  =  SRspec;
           scSREXT   =  Srext;

           Clear scSRDesc;
           xxType = 'P';
           xxEfDt = SrEffd(scCO#:scFND:scFYR:scEMP#:scDIV:SRefdt:xxType:
            SRclas);
           Setgt  (scCo#:scRtcd:SRclas:xxEfDt) ppr01;
           Readpe (scCo#:scRtcd:SRclas)        ppr01;
           If not %eof;
              scSRDesc = PRdesc;
              If SRext <> *blanks;
                 xxPxSt = xxSt;
                 Setgt  (scCo#:SRclas:SRext:xxPxSt:SRefdt) ppx01;
                 Readpe (scCo#:SRclas:SRext:xxPxSt) ppx01;
                 If not %eof;
                    scSRDesc= PXdesc;
                 Else;
                    Clear xxPxSt;
                    Setgt  (scCo#:SRclas:SRext:xxPxSt:SRefdt) ppx01;
                    Readpe (scCo#:SRclas:SRext:xxPxSt) ppx01;
                    If not %eof;
                       scSRDesc = PXdesc;
                    Endif;
                 Endif;
              Endif;
           Endif;

           scTotSRAmt= scTotSRAmt + SRamtp;

           rrnSRRAudt =  rrnSRRAudt + 1;
           Write sflSelfRpt;

          Reade keyWASRL6E  PSR06E;
       Enddo;

       //* Position payroll grid at top at all times.
       rrnSRRAudt = 1;

      /end-free
     P LoadSrRptGrd3   E
       //***************************************************************************//



       //===================================================================
     P LoadVendorPGrd  B
     D LoadVendorPGrd  PI
      /free
       Clear scVndPTool;
       isEdtVendP = *on;
       Select;
       When inquiry = *on;
          isEdtVendP = *off;
       When FNpeoF <> *blank and (scEmp# <> 1 or scDiv <> 0);
          isEdtVendP = *off;
          scVndPTool = 'Payroll Vendor information is maintained on the PEO.';
       Endsl;

       // Clear the subfile.
       RcdPayrollDsp(*off);
       clrVendorP = *On;
       Write rcdPayroll;
       clrVendorP= *Off;
       RcdPayrollDsp(*on);
       rrnVendorP = 0;

       // Populate the subfile
       Setll keyWDVHL PVH01;
       Reade keyWDVHL PVH01;

       Dow not %eof;
           scVendor#  =  VHvdr#;
           scVndEffDt =  CvtToDate(VHefdt);
           scVndExpDt =  CvtToDate(VHexdt);
           scVndRRN =  dbrrn4;
           Chain keyWMVPL PVP01;
           If %found;
              scVendName = VPname;
           Endif;

           rrnVendorP =  rrnVendorP + 1;
             Write sflVendorP;

       Reade keyWDVHL PVH01;
       Enddo;

       //* Position payroll grid at top at all times.
       rrnVendorP = 1;


      /end-free
     P LoadVendorPGrd  E

       //===================================================================
       // Employee Payroll
       //===================================================================
     P LoadEmpPayrGrd  B
     D LoadEmpPayrGrd  PI
      /free

       // Clear the subfile.
       RcdPayrollDsp(*off);
       clrEmpPayr = *On;
       Write rcdPayroll;
       clrEmpPayr= *Off;
       RcdPayrollDsp(*on);
       rrnEmpPayr = 0;

       // Populate the subfile
       Setll keyWMEEL PEE01;
       Reade keyWMEEL PEE01;

       Dow not %eof;
         meetFlt  = *on;
         EmpPayFilter();
         If meetFlt = *on;
           Clear WrkName;
           If Eetitl <> ' ';
              WrkName =  %Trim(Eetitl);
           EndIf;
           If Eefsn <> ' ';
              WrkName =  %Trim(WrkName) + ' ' + %Trim(Eefsn);
           EndIf;
           If Eemid <> ' ';
              WrkName =  %Trim(WrkName) + ' ' + %Trim(Eemid);
           EndIf;
           If Eelsn <> ' ';
              WrkName =  %Trim(WrkName) + ' ' + %Trim(Eelsn);
           EndIf;
           scEmpPNam  =  WrkName;

           //scEmpPNam  =  %Trim(Eetitl) + ' ' + %Trim(Eelsn) + ', ' +
           //              %Trim(Eefsn)  + ' ' + %Trim(Eemid);
           //***  Begin Chg  ***  06/09/20  **********************
           //EmpSS#_ds  =  Eess#;
           //scEmpPSS#  =  ESS1 + '-' + ESS2 + '-' + ESS3;
           scEmpSS#  = DisplaySS#(MaskData('SSN':'Eess#':Eess#));
           //***  End   Chg  ***  06/09/20  **********************

           If Eeclas <> 0   and Eespec <> ' ' and Eeext <> ' ';
              scEmpPCls  =  %Char(Eeclas) + '/' + Eespec + '/' +
                            %Trim(Eeext);
           ElseIf Eeclas <> 0   and Eespec  = ' ' and Eeext <> ' ';
              scEmpPCls  =  %Char(Eeclas) + '/' + Eespec + '/' +
                            %Trim(Eeext);
           ElseIf Eeclas <> 0   and Eespec <> ' ' and Eeext  = ' ';
              scEmpPCls  =  %Char(Eeclas) + '/' + Eespec ;
           ElseIf Eeclas <> 0   and Eespec =  ' ' and Eeext  = ' ';
              scEmpPCls  =  %Char(Eeclas);
           Endif;

           scEmpPSRPy =  Eesapr;
           scEmpPCkDt =  CvtToDate(Eeckdt);
           scEmpPStDt =  CvtToDate(Eesdt);
           scEmpPEdDt =  CvtToDate(Eeedt);
           scEmpPRRN  =  dbrrn12;

           rrnEmpPayr =  rrnEmpPayr + 1;
           If rrnEmpPayr >= 9999;
              isMaxEmpRcds = *on;
              Leave;
           Endif;
           If rrnEmpPayr = 1;
             scEEsel = *On;
           Else;
             scEEsel = *Off;
           Endif;
           Write sflEmpPayr;
         Endif;

        Reade keyWMEEL PEE01;
       Enddo;

       //* Position payroll grid at top at all times.
       rrnEmpPayr = 1;

      /end-free
     P LoadEmpPayrGrd  E

       //===================================================================
       // Employee Payroll Filter Popup
       //===================================================================
     P DspEmpPayFlt    B
     D DspEmpPayFlt    PI
      /free

       //* Remain on payroll tab after any editing is done.
       tabIndex = 1;

       // Choice criteria for class code drop down
       Clear clsChoices;
       clsChoices = 'EPCO# = ' + %char(ELCO#) +
                ' AND EPFND = ' + %char(ELFND) +
                ' AND EPFYR = ' + %char(ELFYR) +
                ' AND EPEMP# = ' + %char(ELEMP#) +
                ' AND EPDIV = ' + %char(ELDIV) +
                ' AND EPSTAT = ''A''' ;

       //* Display popup window for the payroll filter.

       Dou EPayFltApl = *on or EPayFltCnl = *on;
          //***** Beg Add ****** 09/09/21 *************************
            scPF1rrn = dbrrn;
          //***** End Add ****** 09/09/21 *************************
          Exfmt rcdEPayFlt;
          If EPayFltApl = *On;
             dsEpayFilter.scEmpFPLNm  = scEmpFPLNm;
             dsEpayFilter.scEmpFPFNm  = scEmpFPFNm;
             dsEpayFilter.scEmpFMInt  = scEmpFMInt;
             dsEpayFilter.scEmpFTitl  = scEmpFTitl;
             dsEpayFilter.scEmpFSS#   = scEmpFSS#;
             dsEpayFilter.scEmpFCLs2  = scEmpFCLs2;
             dsEpayFilter.scEmpFPos   = scEmpFPos;
             dsEpayFilter.scEmpFStDT  = scEmpFStDT;
             dsEpayFilter.scEmpFEdDT  = scEmpFEdDT;
             dsEpayFilter.scEmpFCkDt  = scEmpFCkDt;
          Endif;
          If EPayFltCnl = *on;
             scEmpFPLNm = dsEpayFilter.scEmpFPLNm;
             scEmpFPFNm = dsEpayFilter.scEmpFPFNm;
             scEmpFMInt = dsEpayFilter.scEmpFMInt;
             scEmpFTitl = dsEpayFilter.scEmpFTitl;
             scEmpFSS#  = dsEpayFilter.scEmpFSS#;
             scEmpFCLs2 = dsEpayFilter.scEmpFCLs2;
             scEmpFPos  = dsEpayFilter.scEmpFPos;
             scEmpFStDT = dsEpayFilter.scEmpFStDT;
             scEmpFEdDT = dsEpayFilter.scEmpFEdDT;
             scEmpFCkDt = dsEpayFilter.scEmpFCkDt;
          Endif;

         If DtCYMD(dsEpayFilter.scEmpFStDT) = *zero and
            DtCYMD(dsEpayFilter.scEmpFEdDT) = *zero and
            DtCYMD(dsEpayFilter.scEmpFCkDt) = *zero and
            dsEpayFilter.scEmpFPLNm = *blanks and
            dsEpayFilter.scEmpFPFNm = *blanks and
            dsEpayFilter.scEmpFMInt = *blanks and
            dsEpayFilter.scEmpFTitl = *blanks and
            dsEpayFilter.scEmpFSS#  = *blanks and
            dsEpayFilter.scEmpFCLs2 = *blanks and
            dsEpayFilter.scEmpFPos  = *blanks;
            EPayFiltOn= *off;
         Else;
            EPayFiltOn= *on;
         Endif;

       Enddo;

      /end-free
     P DspEmpPayFlt    E

       //===================================================================
       // Employee Payroll Filter selection
       //===================================================================
     P EmpPayFilter    B
     D EmpPayFilter    PI
      /free
         If scEmpFPLNm <> *Blanks And scEmpFPLNm <> EELSN;
            meetflt = *Off;
         Endif;
         If scEmpFPFNm <> *Blanks And scEmpFPFNm <> EEFSN;
            meetflt = *Off;
         Endif;
         If scEmpFMInt <> *Blanks And scEmpFMInt <> EEMID;
            meetflt = *Off;
         Endif;
         If scEmpFTitl <> *Blanks And scEmpFTitl <> EETITL;
            meetflt = *Off;
         Endif;
         If scEmpFSS# <> *Blanks And scEmpFSS# <> EESS#;
            meetflt = *Off;
         Endif;

         // xxEmpFCLs2 = %char(EPCLAS) + EPSPEC + EPEXT;
         xxEmpFCLS2 = %char(EEclas) +
                         '/' + EEspec + '/' + EEEXT;
         If scEmpFCLs2 <> *Blanks And scEmpFCLs2 <> xxEmpFCLs2;
            meetflt = *Off;
         Endif;

         If scEmpFPos <> *Blanks And scEmpFPos <> EEPOSN;
            meetflt = *Off;
         Endif;

         If scEmpFStDT <> *zeros;
            xxSdat7 = DtCYMD(scEmpFStDT);
            If eesdt < xxSdat7;
              meetflt = *off;
            Endif;
         Endif;

         If scEmpFEdDT <> *zeros;
            xxEDat7 = DtCYMD(scEmpFEdDT);
            If eeedt > xxEdat7;
              meetflt = *off;
            Endif;
         Endif;

         If scEmpFCkDt <> *zeros;
            xxCkDat7 = DtCYMD(scEmpFCkDt);
            If eeckdt <> xxCkdat7;
              meetflt = *off;
            Endif;
         Endif;

      /end-free
     P EmpPayFilter    E

       //===================================================================
       // Clear Employee Payroll Filter
       //===================================================================
     P ClrEmpPayFlt    B
     D ClrEmpPayFlt    PI
      /free
        Clear dsEpayFilter;
        Clear rcdEPayFlt;
        EPayFiltOn = *off;

      /end-free
     P ClrEmpPayFlt    E

       //===================================================================
       // Payroll Filter Popup
       //===================================================================
     P PayrollFilter   B
     D PayrollFilter   PI
      /free

       //* Remain on payroll tab after any editing is done.
       tabIndex = 1;

       //* Display popup window for the payroll filter.

       Dou payFiltApl = *on or payFiltCnl = *on;
          //***** Beg Add ****** 09/09/21 *************************
            scPF1rrn = dbrrn;
          //***** End Add ****** 09/09/21 *************************
          Exfmt rcdPayFilt;
       Enddo;

       If payFiltApl = *on;
          //*  Default payroll detail to 1st record.
          savePayRRN = 1;
       Endif;

      /end-free
     P PayrollFilter   E


       //===================================================================
       // Load display indicators for sub-files in rcdAddInfo
       //===================================================================
     P RcdAddInfoDsp   b
     D RcdAddInfoDsp   pi
     D  indValue                      1n   const
      /Free
       dspExcSfl  = indValue;
       dspExcCtl  = indValue;
       //*** Beg Add ***** 06/24/24 **************************
       If clientID = '146';
          dspCollSfl = indValue;
          dspCollCtl = indValue;
       Endif;
       //*** End Add ***** 06/24/24 **************************
      /End-Free
     P RcdAddInfoDsp   e


       //===================================================================
       // Load display indicators for sub-files in rcdAgency
       //===================================================================
     P RcdAgencyDsp    b
     D RcdAgencyDsp    pi
     D  indValue                      1n   const
      /Free
       dspAgtSfl  = indValue;
       dspAgtCtl  = indValue;
      /End-Free
     P RcdAgencyDsp    e


       //===================================================================
       // Load display indicators for sub-files in rcdPayroll
       //===================================================================
     P RcdPayrollDsp   b
     D RcdPayrollDsp   pi
     D  indValue                      1n   const
      /Free
       dspPaySfl  = indValue;
       dspPayCtl  = indValue;
       dspSGr1Sfl = indValue;
       dspSGr1Ctl = indValue;
       dspSGr2Sfl = indValue;
       dspSGr2Ctl = indValue;
       dspSrRSfl  = indValue;
       dspSrRCtl  = indValue;
       dspVndSfl  = indValue;
       dspVndCtl  = indValue;
       dspEmpPSfl  = indValue;
       dspEmpPCtl  = indValue;
      /End-Free
     P RcdPayrollDsp   e


       //===================================================================
       // Load display indicators for sub-files in rcdRateChg
       //===================================================================
     P RcdRateChgDsp   b
     D RcdRateChgDsp   pi
     D  indValue                      1n   const
      /Free
       dspElmSfl  = indValue;
       dspElmCtl  = indValue;
      /End-Free
     P RcdRateChgDsp   e


       //===================================================================
       // S/R Payroll Filter Popup
       //===================================================================
     P SelfRptFilter   B
     D SelfRptFilter   PI
      /free

       //* Remain on Payroll tab after any editing is done.
       tabIndex = 1;

       //* Display popup window for the self reporting filter.

       Dou BtnFiltCnl = *on or BtnFiltApl = *on;
           Dou BtnFiltCnl = *on or errorfldt = *off;
               If frequency <> 'Weekly';
                  monthlyDt = *on;
                  weeklyDt = *off;
                  schdTyp = ' ';
               Else;
                  monthlyDt = *off;
                  weeklyDt = *on;
                  schdTyp = 'W';
               Endif;
               Exfmt rcdSrrFilt;

               If BtnFiltApl = *on;
      /end-free
     c                   move      *Off          errorfldt
     c                   move      *Off          erpst1
      /FREE
       Clear mspst1;
      /END-FREE
     c                   move      *Off          erpst2
      /FREE
       Clear mspst2;
       If frequency <> 'Weekly';
      /END-FREE
     c                   move      scpst1        srdti
     c                   call      'SRCVTMY'     DATCHK
     c                   movel     srdto         scpst1
      /FREE
         dateFlt = scpst1;
         IF SRERR = *On;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           erpst1
     c                   movel(p)  'WCR0500'     mspst1
      /FREE
         Endif;                                                               //srerr = *on
       Else;
      /END-FREE
     c                   Z-Add     scpst2        srdti
     c                   call      'SRCVTDAT'    DATCHK
     c                   movel     srdto         scpst2
      /FREE
         dateFlt = scpst2;
         IF SRERR = *On;
      /END-FREE
     c                   move      *On           errorfldt
     c                   move      *On           erpst2
     c                   movel(p)  'WCR0500'     mspst2
      /FREE
         Endif;                                                               //srerr = *on
       Endif;                                                                 //frequency <>
      /END-FREE
      /free
               Endif;
           Enddo;
       Enddo;

          //*  Default payroll detail to 1st record.
          rrnSRRAudt = 1;

      /end-free
     P SelfRptFilter   E
       //***************************************************************************//


       //===================================================================
       // Load Excluded States Grid
       //===================================================================
     P LoadExcludeGrd  B
     D LoadExcludeGrd  PI
      /free

       // Clear the subfile.
       RcdAddInfoDsp(*off);
       clrExclude = *On;
       Write rcdAddInfo;
       clrExclude = *Off;
       RcdAddInfoDsp(*on);
       rrnExclude = 0;

       // Populate the subfile
       Setll keyWDXSL PXS00;
       Reade keyWDXSL PXS00;

       Dow not %eof;

          scXSST   =  XSST;
          scXSrrn =  dbrrn3;

          Clear scXSDesc;
          Clear xxKey;
          xxKey = XSST;
          Chain keySTSTL3 PST03;
          If %found;
             scXSDesc = STNAME;
          Endif;

          rrnExclude =  rrnExclude + 1;
          Write ExcludeSFL;

          Reade keyWDXSL PXS00;
       Enddo;
       If isContribution = '1';
          flxPrmAcrs = 'Accumulate Contribution Across Divisions';
       Else;
          flxPrmAcrs = 'Accumulate Premium Across Divisions';
       Endif;

       //* Position exclude grid at top at all times.
       rrnExclude = 1;

      /end-free
     P LoadExcludeGrd  E


       //*** Beg Add ***** 06/24/24 ************************
       //===================================================================
       // Load Collateral Information Grid
       //===================================================================
     P LoadCollateral  B
     D LoadCollateral  PI
      /free

       // Clear the subfile.
       RcdAddInfoDsp(*Off);
       clrCollate = *On;
       Write rcdAddInfo;
       clrCollate = *Off;
       RcdAddInfoDsp(*On);
       rrnCollate = 0;

       // Populate the subfile
       Setll key1WDELL PEL05;
       Reade key1WDELL PEL05;

       //*svFyr = L5ELfyr;

       Clear CollateSfl;
       DoW not %eof(WDELL5);

          scCollYear = *Zeros;
          scCollWP = *Zeros;
          scCollFC = *Zeros;
          scCollRP = *Zeros;
          scTotColl = *Zeros;
          scWrtPrem = *Zeros;
          scRepPrem = *Zeros;
          scTotLoss = *Zeros;
          scTotDedBi = *Zeros;
          scDedPColl = *Zeros;
          scRemColl = *Zeros;

          scCollYear = L5ELfyr;                                           //* Year
          scCollWP = L5ELcolwp;                                           //* Written Percent
          scCollFC = L5ELcolwa;                                           //* Flat Charge
          scCollRP = L5ELcolrp;                                           //* Reported Percent

          //* Total Collateral
          Exec sql
          Select sum(Eaamt) into :scTotColl
           from WMEAP
           where EAarcd in (Select EXcolb from WMEXP where
            EXco# = :L5ELco# and EXfnd = :L5ELfnd) and
            EAco# = :L5ELco# and EAfnd = :L5ELfnd and
            EAfyr = :L5ELfyr and EAemp# = :L5ELemp#;

          scWrtPrem = L5ELmapr;                                           //* Written Premium

          //* Reported Premium
          Exec sql
          Select sum(SRmanprm) into :scRepPrem
           from WASRP
           where SRco# = :L5ELco# and SRfnd = :L5ELfnd and
            SRfyr = :L5ELfyr and SRemp# = :L5ELemp#;

          If scRepPrem = *Zeros;                                           // If Sum of SRmanprm = 0
             Setll (L5ELco#:L5ELfnd:L5ELfyr:L5ELemp#) pep01e;
             Reade (L5ELco#:L5ELfnd:L5ELfyr:L5ELemp#) pep01e;
             DoW not %eof(WMEPL1E);
                Setgt (EPco#:ELrtcd:EPclas:EPefdt) ppr01;
                Readpe (EPco#:ELrtcd:EPclas) ppr01;
                Select;
                When PRpcf = 'P';
                   Eval(h) xxRepPrem = EPSapr * EParat/100;
                When PRpcf = 'C';
                   Eval(h) xxRepPrem = EPSapr * EParat;
                Endsl;

                scRepPrem += xxRepPrem;

                Reade (L5ELco#:L5ELfnd:L5ELfyr:L5ELemp#) pep01e;
             Enddo;
          Endif;                                                               // EndIf SRmanprm = 0

          //* Total Losses
          Exec sql
          Select (CMar01 + CMar02 + CMar03 + CMar04 +
           CMar05 + CMar06 + CMar07 + CMar08 +
           CMar09 + CMar10) into :scTotLoss
           from WMCMP
           where CMco# = :L5ELco# and CMfnd = :L5ELfnd and
            CMfyr = :L5ELfyr and CMemp# = :L5ELemp#;

          //* Total Deductible billed
          Exec sql
          Select sum(DDamtb) into :scTotDedBi
           from WDDDP
           where DDco# = :L5ELco# and DDfnd = :L5ELfnd and
            DDfyr = :L5ELfyr and DDemp# = :L5ELemp#;

          //* Deductible Paid from Collateral
          Exec sql
          Select sum(abs(Eaamt)) into :scDedPColl
           from WMEAP
           where EAarcd in (Select EXcolt from WMEXP where
            EXco# = :L5ELco# and EXfnd = :L5ELfnd) and
            EAco# = :L5ELco# and EAfnd = :L5ELfnd and
            EAfyr = :L5ELfyr and EAemp# = :L5ELemp#;

          //* Remaining Collateral
          Exec sql
          Select sum(Eaamt) into :scRemColl
           from WMEAP
           where (EAarcd in (Select EXcolp from WMEXP
            where EXco# = :L5ELco# and EXfnd = :L5ELfnd) or
            EAarcd in (Select EXcolt from WMEXP
            where EXco# = :L5ELco# and EXfnd = :L5ELfnd)) and
            EAco# = :L5ELco# and EAfnd = :L5ELfnd and
            EAfyr = :L5ELfyr and EAemp# = :L5ELemp#;

          scCollRrn = dbrrnl5;
          rrnCollate += 1;
          Write CollateSfl;
          Reade key1WDELL PEL05;
       Enddo;

     P LoadCollateral  E
       //*** End Add ***** 06/24/24 ************************

       //===================================================================
       // Load Agency Grid
       //===================================================================
     P LoadAgencyGrid  B
     D LoadAgencyGrid  PI
      //
     D agentName       s             40
     D prodName        s             40
     D agtFein         s              9  0
     D AgtMod          s              3  0
     D xxAGrr          s              9  0
     D xxAArr          s              9  0
     D xxAPrr          s              9  0
      /free

       // Clear the subfile.
       RcdAgencyDsp(*off);
       clrAgency = *On;
       Write rcdAgency;
       clrAgency = *Off;
       RcdAgencyDsp(*on);
       rrnAgency = 0;

       // Populate the subfile
       Setll keyWMAHL PAH00;
       Reade keyWMAHL PAH00;
       xxErrorTab = -1;

       Dow not %eof;

        //scAgtEffDt =  dtMDYY(AHEfdt);
          scAgtEffDt =  CvtToDate(AHEfdt);

          Clear agentName;
          agentName  = GetAgtName(AHAgt#:xxAGrr:xxAArr:AHFnd:AHCo#);

          scAgtAgent = agentName;
          scAgtNum#  = AHagt#;

          Clear prodName;
          agtFein    = GetAgtFein(AHCo#:AHFnd:AHAgt#:agtMod);
          ProdName   = GetProdNm(agtFein:agtMod:AHProd:xxAPrr:AHCo#);

          scAgtProd  = prodName;
          scProdNum  = AHprod;

          xxRate = AHRT#;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          Dow not %eof and ARSTAT <> 'A';
             Readpe key1WMARL PAR00;
          Enddo;

          If %eof;
             Clear ARNWRT;
             Clear ARRNRT;
          Endif;

          scAgtNewRt =  ARNWRT;
          scAgtRnwRt =  ARRNRT;

          scAgtofRec =  AHAGRC;

          If AHStat = 'A';
             scAgtStat = 'Active';
          Else;
             scAgtStat = 'Inactive';
          Endif;

          Clear scAgtQStat;
          Setgt keyWAQTL7 PQT07;
          Readpe keyWAQTL7 PQT07;
          If not %eof;
             If QTtype = 'T' and QTxFer <> *blanks;
                QTtype = QTxFer;
             Endif;
             Clear QSused;
             Clear QSdesc;
             Chain (scCo#:QTType) pqs01;
             If QSused <> *blank;
                Chain (scCo#:QSused) pqs01;
             Endif;
             scAgtQStat = QSdesc;
          Endif;

          scAgtAHrrn =  dbrrn7;

          rrnAgency =  rrnAgency + 1;
          If rrnAgency = 1;
            scAgtSel = *On;
          Else;
            scAgtSel = *Off;
          Endif;
          Write agencySFL;

          Reade keyWMAHL PAH00;
       Enddo;

       //* Position exclude grid at top at all times.
       rrnAgency = 1;

      /end-free
     P LoadAgencyGrid  E


       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0 const
     d   inFND                        3  0 const
     d   inFYR                        3  0 const
     d   inEMP#                       9  0 const
     d   inDIV                        5  0 const
     d   inDate                       7  0
     d   inType                       1    options(*nopass)
     d   inClas                       5  0 options(*nopass)
      //
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Date        s              7
     d   ##Type        s              1
     d   ##Clas        s              5
     d   outDate       s              7  0
      //

      /free

       ##Type = *blank;
       If %parms >= 7;
          ##Type = inType;
       Endif;

       ##Clas = *blanks;
       If %parms >= 8;
          ##Clas = %editc(inClas: 'X');
       Endif;

       ##CO# = %editc(inCO#: 'X');
       ##FND = %editc(inFND: 'X');
       ##FYR = %editc(inFYR: 'X');
       ##EMP# = %editc(inEMP#: 'X');
       ##DIV = %editc(inDIV: 'X');
       ##Date = %editc(inDate: 'X');

       SReffdAPI(##CO#:##FND:##FYR:##EMP#:##DIV:##Date:##Type:##Clas);

       outDate = %int(##Date);

       Return outDate;
      /end-free
     P SrEffd          E
       //***************************************************************************//

       //***************************************************************************//
       //===================================================================
       // Edit Policy Detail Data.
       //===================================================================
     P PolicyCheck     B
     D PolicyCheck     PI

     D svDate          s              7  0
     D count           s              2  0
     D found           s              3  0
     D index           s              3  0
     D agt2            s              5  0 DIM(999)
     D agt3            s              5  0 DIM(999)
     D inEditData      s            500
     D message         s            100
     D TOTPCC          s                   like(RFMSCA)                         CREDIT APPLIED
     D TOTPCD          s                   like(RFMSCA)                         DEBIT APPLIED
     D TOTALC          s                   like(SCDBCR)                         TOT CRDT APPLIED
     D TOTALD          s                   like(SCDBCR)                         TOT DEBIT APPLIED
     D TOTPCT          s                   like(SCDBCR)                         CATEGORY DB/CR
     D inStack         s               n   inz(*off)

      /free

       //* Clear errors then process each tab's set of edits.
       ClearErrors();
       xxErrorTab = -1;

       // Check to be sure screen has not been updated since entry
       chain(n) keyWDELL pel00;

       CheckCallStack('WTPOLRBILL':inStack);
       If not inStack;

       // Check if WDELP updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WDELP':dbrrn:'ELdt':'ELtime':
        'ELuser':saveDT:saveTM);
       Select;
       When flgRcdUpdated = '0'; // No error

          // Check if WMRPP updated since screen displayed
          flgRcdUpdated = RcdUpdatedErr('WMRPP':dbrrn8:'RPdt':'RPtime':
           'RPuser':saveDT1:saveTM1);
          Select;
          When flgRcdUpdated = '0'; // No error

             // Check if WMLDP updated since screen displayed
             flgRcdUpdated = RcdUpdatedErr('WMLDP':dbrrn9:'LDdt':'LDtime':
              'LDuser':saveDT2:saveTM2);
             Select;
             When flgRcdUpdated = '0'; // No error

                // Check if WMRFP updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('WMRFP':dbrrn10:'RFdt':
                    'RFtime':'RFuser':saveDT3:saveTM3);
                Select;
                When flgRcdUpdated = '0'; // No error
                When flgRcdUpdated = '1'; // Error
                   error = *on;
                When flgRcdUpdated = '2'; // Error with refresh
                Endsl;
             When flgRcdUpdated = '1'; // Error
                error = *on;
             When flgRcdUpdated = '2'; // Error with refresh
             Endsl;
          When flgRcdUpdated = '1'; // Error
             error = *on;
          When flgRcdUpdated = '2'; // Error with refresh
          Endsl;
       When flgRcdUpdated = '1'; // Error
          error = *on;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       EndIf;

       //* If printing Scheduled Rating Form, only edit Scheduled Rating tab.
       If schdPrint <> *on;

          //* Process tab 1 edits
          Exsr EditRatingTab;
          If revBill = *on;
             // Exsr ChkEFT;

             clear errMsg;
             errMsg = 'EFT Information must be added to Employer.';
             kkBsCd = scBsCd;
             chkEFT();
          Endif;
          If error = *On;
            xxErrorTab = 0;
          Endif;

          //* Process tab 2 edits
          Exsr EditPayrollTab;
          If error = *On And xxErrorTab=-1;
            xxErrorTab = 1;
          Endif;

          //* Process tab 3 edits
          Exsr EditAddInfoTab;
          If error = *On And xxErrorTab=-1;
            xxErrorTab = 2;
          Endif;

          //* Process tab 4 edits
          If noLDModule <> *on and NoLargeDed = *off;
          Exsr EditLrgDedTab;
          If error = *On And xxErrorTab=-1;
            xxErrorTab = 3;
          Endif;
          Endif;

          //* Process tab 5 edits
          Exsr EditRetroTab;
          If error = *On And xxErrorTab=-1;
            xxErrorTab = 4;
          Endif;

          //* Process Agency History tab edits
          CheckAgtEfDt();
          If errAgtEfDt = *on And xxErrorTab=-1;
            xxErrorTab = 5;
          Endif;
       Endif;

       //* Process tab 6 edits
       If schdRating = *on;
          Exsr EditSchdTab;
          If error = *On And xxErrorTab=-1;
             xxErrorTab = 6;
          Endif;
       Endif;

       //* If printing Scheduled Rating Form, only edit Scheduled Rating tab.
       If schdPrint <> *on;

          //* Process popup error messages last
          If not Error;
             Exsr PopupErrors;
             If error = *On And xxErrorTab=-1;
               xxErrorTab = 0;
             Endif;
          Endif;

       Else;

          //* If no errors generated from Scheduled Rating tab, then save the information.
          If not Error;
             Exsr SaveSchdInfo;
          Endif;

       Endif;

       //* If an error has been found then set tab index to it
       If xxErrorTab >-1;
          //*** Beg Add ***** 03/22/24 ********************************
          // Since new tab inserted at 4th position, remaining tab's position
          // needs to increase by 1
          If xxErrorTab >= 3;
             xxErrorTab = xxErrorTab + 1;
          Endif;
          //*** End Add ***** 03/22/24 ********************************
         TabIndex = xxErrorTab;
       Endif;

      /end-free

      //*************************************************************
      //  Save Scheduled Rating Information
      //*************************************************************
      /FREE
       Begsr SaveSchdInfo;
      /END-FREE
      /free

       // Scheduled Rating Information

       If schdRating = *on;

          Chain keyWMRFL RFP00;

          If not %found;
             Clear RFP00;

             RFCO#  = scCo#;
             RFFND  = scFnd;
             RFFYR  = scFyr;
             RFEMP# = scEmp#;
             RFDIV  = scDiv;

             RFIPGM = Q1PGM;
             RFIUSR = Q1USER;
             RFIDT  = Dateto7(SystemDate());
          Endif;

          RFPRCA = scRFPRCA;
          RFPRDA = scRFPRDA;
          RFPRRS = scRFPRRS;
          RFCPCA = scRFCPCA;
          RFCPDA = scRFCPDA;
          RFCPRS = scRFCPRS;
          RFMFCA = scRFMFCA;
          RFMFDA = scRFMFDA;
          RFMFRS = scRFMFRS;
          RFSDCA = scRFSDCA;
          RFSDDA = scRFSDDA;
          RFSDRS = scRFSDRS;
          RFEMCA = scRFEMCA;
          RFEMDA = scRFEMDA;
          RFEMRS = scRFEMRS;
          RFMCCA = scRFMCCA;
          RFMCDA = scRFMCDA;
          RFMCRS = scRFMCRS;
          RFMSCA = scRFMSCA;
          RFMSDA = scRFMSDA;
          RFMSRS = scRFMSRS;
          RFLSCF = scRFLSCF;
          RFLSDT = dtCYMD(scRFLSDT);
          RFPEFL = scRFPEFL;
          RFPEEM = scRFPEEM;
          RFCOM1 = scRFCOM1;
          RFCOM2 = scRFCOM2;
          RFCOM3 = scRFCOM3;

          RFPGM  = Q1PGM;
          RFUSER = Q1USER;
          RFTIME = Timeto6(SystemTime());
          RFDT   = Dateto7(SystemDate());

          If %found;
             Update RFP00;
          Else;
             Write  RFP00;
             Feod   WMRFL;
          Endif;
       Endif;
      /end-free
      /FREE
       Endsr;
       //*************************************************************

       //*************************************************************
       //  Rating Tab Edits
       //*************************************************************
       Begsr EditRatingTab;
         //
      /END-FREE
      /free

       xxMailSt = EMMST;
       xxPhysSt = EMPST;

       inEditData = dsRateEdit;

       ptrRating = %Addr(dsRating);
       ptrRating2 = %Addr(dsRating2);

       If not errPolRating('P':' ':quoteMode:ptrRating:scCo#:scFnd:scEmp#:
               scDiv:scFyr:xxRRN:inEditData:ptrRating2);
          error = *on;
          If scDcDe <> ' ' and BSlpgm = 'L';
             Clear tipDedDis1;
             tipDedDis1 = 'Deductible codes not allowed on policies flagged +
                           for large deductible billing plans.';
          Endif;
          If scDcD2 <> ' ' and BSlpgm = 'L';
             Clear tipDedDis2;
             tipDedDis2 = 'Deductible codes not allowed on policies flagged +
                           for large deductible billing plans.';
          Endif;
       Endif;

       dsRateEdit = inEditData;

       // Update display field on scheduled rating tab.
       scDBCR2 = scDBCR;

      /end-free
      //
      /FREE
       EndSr;


       //*******************************************************
       //// Payroll Tab Edits
       //*******************************************************
       BegSr EditPayrollTab;
       EndSr;


       //*******************************************************
       //// Additional Tab Edits
       //*******************************************************
       BegSr EditAddInfoTab;
         //
      /END-FREE
      /free

       inEditData = dsRateEdit;

       ptrAddInfo = %Addr(dsAddInfo);

       If not errAddInfo('A':' ':quoteMode:ptrAddInfo:scCo#:scFnd:scEmp#:
               scDiv:scFyr:xxRRN:inEditData);
          error = *on;
       Endif;

       dsRateEdit = inEditData;

      /end-free
      //
      /FREE
       EndSr;


       //**********************************************************
       //// Large Deductible/Retro Tab Edits
       //***********************************************************
       BegSr EditLrgDedTab;
         //
      /END-FREE
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         x5PFDT
      //
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         x5PTDT
      //
      /free

       inEditData = dsRateEdit;

       ptrLargeDed = %Addr(dsLargeDed);

       If not errLargeDed('D':' ':quoteMode:ptrLargeDed:scCo#:scFnd:scEmp#:
               scDiv:scFyr:xxRRN:inEditData);
          error = *on;
       Endif;

       dsRateEdit = inEditData;

      /end-free
      //
      /FREE
       EndSr;
       //*********************************************************
       //// Retrospective Rating Tab Edits
       //*********************************************************
       BegSr EditRetroTab;
         //
      /END-FREE
      /free

       inEditData = dsRateEdit;

       ptrRetroRate = %Addr(dsRetroRate);

       If not errRetroRate('E':' ':' ':ptrRetroRate:scCo#:scFnd:scEmp#:
               scDiv:scFyr:xxRRN:inEditData);
          error = *on;
       Endif;

       dsRateEdit = inEditData;

      /end-free
      //
      /FREE
       EndSr;

       //*******************************************************
       //// Scheduled Rating Tab Edits
       //*******************************************************
       BegSr EditSchdTab;
         //
      /END-FREE
      /free

       inEditData = dsRateEdit;

       ptrSchedRate = %Addr(dsSchedRate);

       If not errSchedRate('S':' ':' ':ptrSchedRate:scCo#:scFnd:scEmp#:
               scDiv:scFyr:xxRRN:inEditData);
          error = *on;
       Endif;

       dsRateEdit = inEditData;

      /end-free
      //
      //
      // Add up percentages to compare to Rate factor entered.
      //
      /FREE
         if not Error;
           //
           CLEAR TOTPCC;
           CLEAR TOTPCD;
           CLEAR TOTALC;
           CLEAR TOTALD;
           CLEAR TOTPCT;
           //
      /END-FREE
     C                   ADD       scRFPRDA      TOTPCD
     C                   ADD       scRFPRCA      TOTPCC
     C                   ADD       scRFCPDA      TOTPCD
     C                   ADD       scRFCPCA      TOTPCC
     C                   ADD       scRFMFDA      TOTPCD
     C                   ADD       scRFMFCA      TOTPCC
     C                   ADD       scRFSDDA      TOTPCD
     C                   ADD       scRFSDCA      TOTPCC
     C                   ADD       scRFEMDA      TOTPCD
     C                   ADD       scRFEMCA      TOTPCC
     C                   ADD       scRFMCDA      TOTPCD
     C                   ADD       scRFMCCA      TOTPCC
     C                   ADD       scRFMSDA      TOTPCD
     C                   ADD       scRFMSCA      TOTPCC
      //
      /FREE
           TOTALC = (TOTPCC * -1) / 100;
           TOTALD = (TOTPCD / 100);
           //
           IF scDBCR <> *ZEROS;
             TOTPCT = 1.000 + TOTALC + TOTALD;
           ELSE;
             TOTPCT = TOTALC + TOTALD;
           ENDIF;                                                             //SCDBCR = *ZEROS
           //
           // Check TOTPCT = SCDBCR, if not is Error
           //
           If TOTPCT <> SCDBCR;
      /END-FREE
     C                   move      *on           error
      /FREE
             Clear scSchdMsg;
             Clear scSchdPnl;
      /END-FREE
      /free
       scSchdPnl = 'Scheduled Rating Information Error';

       scSchdMsg = 'The sum of the DB/CR percentages of the rating form ' +
        'does not equal the Schedule DB/CR Factor.  The sum of the category ' +
        'percentages is ' + %editc(totpct : '3') + ' and the Schedule DB/CR ' +
        'Factor is ' + %editc(scdbcr : '3') + '.';
      /end-free
      /FREE
             Exfmt rcdSchdErr;
             //
           ENDIF;                                                             //TOTPCT<>SCDBCR
           //
         Endif;
         //
       EndSr;


       //*******************************************************
       //// Popup Error Messages
       //*******************************************************
       BegSr PopupErrors;
         //
         //  Perform popup window edits only if no errors have been encountered.
         //
         If not Error;
           EXSR ClaimDt;
         Endif;
         //
         If not Error;
           EXSR ARDDiff;
         Endif;
         //
         If not Error;
           EXSR BSCDChg;
         Endif;
         //
         If not Error;
           EXSR SpreadFlag;
         Endif;
         If not Error;
           Exsr ManualPercent;
         Endif;
         //
         If not Error;
           EXSR UpdateDIL;
         Endif;
         //
         If not Error;
           EXSR RenewFlag;
         Endif;
         //
         If not Error;
           EXSR AggregateAmt;
         Endif;
         //
      /END-FREE
      /free
       If not Error;
         CheckAgtEfDt();
       Endif;
         If not Error;
           EXSR CheckSameAgt;
         Endif;
       //*** begin add ***  06/19/25  ***********************************
       If not error and numDivs > 1;
          Exsr CheckCollateral;
       Endif;
       //*** end add *****  06/19/25  ***********************************
         //
         //
         //  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
         //
         If not Error;
           chain(n) keyWDELL pel00;

       CheckCallStack('WTPOLRBILL':inStack);
       If not inStack;

       // Check if WDELP updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WDELP':dbrrn:'ELdt':'ELtime':
        'ELuser':saveDT:saveTM);
       Select;
       When flgRcdUpdated = '0'; // No error

          // Check if WMRPP updated since screen displayed
          flgRcdUpdated = RcdUpdatedErr('WMRPP':dbrrn8:'RPdt':'RPtime':
           'RPuser':saveDT1:saveTM1);
          Select;
          When flgRcdUpdated = '0'; // No error

             // Check if WMLDP updated since screen displayed
             flgRcdUpdated = RcdUpdatedErr('WMLDP':dbrrn9:'LDdt':'LDtime':
              'LDuser':saveDT2:saveTM2);
             Select;
             When flgRcdUpdated = '0'; // No error

                // Check if WMRFP updated since screen displayed
                flgRcdUpdated = RcdUpdatedErr('WMRFP':dbrrn10:'RFdt':'RFtime':
                 'RFuser':saveDT3:saveTM3);
                Select;
                When flgRcdUpdated = '0'; // No error
                When flgRcdUpdated = '1'; // Error
                   error = *on;
                When flgRcdUpdated = '2'; // Error with refresh
                Endsl;
             When flgRcdUpdated = '1'; // Error
                error = *on;
             When flgRcdUpdated = '2'; // Error with refresh
             Endsl;
          When flgRcdUpdated = '1'; // Error
             error = *on;
          When flgRcdUpdated = '2'; // Error with refresh
          Endsl;
       When flgRcdUpdated = '1'; // Error
          error = *on;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       EndIf;
      /end-free
      //
      //    ELDT          IFGT      saveDT
      //    ELDT          OREQ      saveDT
      //    ELTIME        ANDGT     saveTM
      //
      //    erPFDT        IFEQ      *OFF
      //                  move      *on           error
      //                  move      *on           erPFDT
      //                  movel(p)  'WC30000'     msPFDT
      //                  ENDIF
      //
      //                  ENDIF                                                  ELDT>SAVEDT
      //
      /FREE
         ENDIF;                                                               //If not Error
         //
         // Passed all edits : Write DARTS remark if
         //                - Program is in change mode AND
         //                - DARTS remark is setup AND
         //                - Activity status changed
         //
         IF not error and save = *on
               and chgMode
               AND SVASRM <> *blanks
               AND SCASCD <> ELASCD;
           // process DARTS
           EXSR SRPrcDARTS;
         ENDIF;                                                               //not error
       EndSr;                                                                 //PopupErrors SUBR

      /END-FREE
      /free

      /end-free
      //
      //===============================================================
      // ARDDiff - Anniversary Rating Date Popup
      //===============================================================
      /FREE
       BEGSR ARDDiff;
         //
         //  ANNIV DATE ON EACH DIVISION SHOULD BE CONSISTENT WITH THE
         //  STATE REPORTING DIVISION (IGNORE INFO MSG IF CLAIMS ONLY)
         //
         IF scANDT <> xxSRAD
         AND xxSTRP <> 'Y'
         AND F1CLMS = 'N';
           //
           ardText = 'state reporting division is' +
               %editw(xxSRAD : '  /  /    ') + '.';
           //
           Dou ardCancel = *on or ardConfirm = *on;
             EXFMT rcdAnnivDt;
           ENDDO;
           //
           IF ardCancel = *ON;
      /END-FREE
     C                   move      *on           error
      /FREE
           ENDIF;
           //
         ENDIF;
         //
       ENDSR;
       //
       //===============================================================
       // BSCDChg - BSCD Change on summarized policy popup
       //===============================================================
       BEGSR BSCDChg;
         //
         // POP UP WINDOW INFORMING USER THAT BSCD'S WILL BE CHANGED ON ALL
         // SUMMARIZED DIV IF ST RPT DIV IS CHG'D
         //
         IF ChgMode = *ON
         AND xxSTRP = 'Y'
         AND xxOTAR = 'Y';
           //
           IF scBSCD <> ELBSCD;
             //
             Dou bscCancel = *on or bscConfirm = *on;
               EXFMT rcdBSCDChg;
             ENDDO;
             //
             IF bscCancel = *ON;
      /END-FREE
     C                   move      *on           error
      /FREE
             ELSE;
      /END-FREE
     C                   MOVE      'Y'           xxUPDB
      /FREE
             ENDIF;                                                           //bscCancel IFEQ
             //
           ENDIF;                                                             //scBSCD IFNE
           //
         ENDIF;                                                               //ChgMode IFEQ
         //
      //***** Beg add ****** 09/13/21 ****************************************
      // When a policy/quote is set up with a large deductible billing plan and it is
      // then switched to a non-deductible plan, the large deductible record should be deleted.
      /Free
       If ChgMode = *ON and scBSCD <> ELBSCD;
          Clear xxKey;
          xxKey = %editc(scbscd : 'X');               // for new Billing plan
          Chain keyWTABL PBS01;
          If BSLPGM <> 'L';                           // Not Large Deductible Plan
             Chain keyWMLDL1 PLD01;
             If %found(WMLDL1);
                Delete PLD01;
                // Clear screen fields to avoid AggregateAmt Popup
                Clear rcdLrgDed;
                // Clear following to avoid 'Record lock by another user Error'
                saveDT2  = 0;
                saveTM2  = 0;
             Endif;
          Endif;
       Endif;
      /End-Free
      //***** End add ****** 09/13/21 ****************************************
       ENDSR;
       //
       //===============================================================
       // AggregateAmt - Large Deductible Aggregate Amount Popup
       //===============================================================
       BEGSR AggregateAmt;
         //
         // Notify user if aggregate entered differs from other divisions
         //
         if not AcceptAgg(scLDEfD1:scLDAmg1)
               and erLDAmg1 <> *ON;
      /END-FREE
     C                   move      *on           error
     C                   move      *on           erLDAMG1
     C                   movel(p)  'WC32434'     msLDAMG1
      /FREE
         endif;                                                               //(scCFee <> *blanks)
         //
         // Notify user if aggregate entered differs from other divisions
         //
         if not error;
           //
           if not AcceptAgg(scLDEfD2:scLDAmg2)
                 and erLDAmg2 <> *ON;
      /END-FREE
     C                   move      *on           error
     C                   move      *on           erLDAMG2
     C                   movel(p)  'WC32434'     msLDAMG2
      /FREE
           endif;                                                             //(scCFee <> *blanks)
           //
         endif;
         //
       ENDSR;
       //*** begin add ***  06/19/25  ***********************************
       //===================================================================
       // CheckCollateral - Check if collateral setup changing to confirm.
       //===================================================================
       Begsr CheckCollateral;

          Select;
          When clientID = '146';
          Other;
             LeaveSR;
          Endsl;

          If svcolwp <> scColwp or svcolwa <> scColwa or svcolrp <> scColrp or
           svcolTyp <> scColTyp or svcolad <> scColad or svcollib <> scCollib
           or svcolln <> scColln or svcollid <> scCollid or svcolled <>
           scColled or svcolsic <> scColsic or svcolsn <> scColsn or
           svcolsid <> scColsid or svcolsed <> scColsed;

             strMsg = 'The collateral setup has been updated and the  ' +
              'changes will apply to the entire policy (every division).' +
              ' Press Confirm to continue.';

             If PopTwoBtn(strmsg:'Collateral Setup Change':
              'Cancel':'Confirm');
                error = *on;
                xxErrorTab = 2;
             Endif;

          Endif;

       Endsr;
       //===================================================================
       //*** end add *****  06/19/25  ***********************************

       //===================================================================
       // CheckSameAgt - Check agent history for matching agents.
       //===================================================================
       Begsr CheckSameAgt;

       count  = 0;
       Clear agt2;
       Clear agt3;
       index = 1;

       Setll keyWMAHL PAH00;
       Reade keyWMAHL PAH00;

       Dow not %eof;
          If AHStat = 'A';
             agt2(index) = AHSagt;
             agt3(index) = AH3agt;
             index = index + 1;
          Endif;
          Reade keyWMAHL PAH00;
       Enddo;

       Setll keyWMAHL PAH00;
       Reade keyWMAHL PAH00;

       Dow not %eof;
          xxRate = AHRT#;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          Dow not %eof and ARSTAT <> 'A';
             Readpe key1WMARL PAR00;
          Enddo;
          If not %eof and AHStat = 'A' and ARNWRT <> 0;
             found = %Lookup(AHAgt#:agt2);
             If found <> 0;
                count = 1;
                Leave;
             Endif;
             found = %Lookup(AHAgt#:agt3);
             If found <> 0;
                count = 1;
                Leave;
             Endif;
          Endif;
          Reade keyWMAHL PAH00;
       Enddo;

       If count <> 0;

          Clear wdmsg1;
          Clear wdmsg2;
          Clear wdmsg3;
          Clear wdmsg4;
          Clear wdmsg5;

          wdmsg1 = 'The primary agent and producer are the secondary or';
          wdmsg2 = 'tertiary agent and producer on another record.';
          wdmsg3 = 'This condition must be corrected before leaving';
          wdmsg4 = 'the agent history screen.';

          Clear agtMsgCnl;
          Clear agtMsgCon;

          Dou agtMsgCnl = *on or agtMsgCon = *on;
             Exfmt rcdAgtMsg;
          Enddo;

          If agtMsgCnl = *on;
             error = *on;
          Endif;

       Endif;

      /end-free
      /FREE
       Endsr;                                                                 //CheckSameAgt Subr

       //===============================================================
       // ClaimDt - Claim Accident Date Popup
       //===============================================================
       BEGSR ClaimDt;
         //
         // If policy eff date range change, check for claims out of new
         // policy eff date range.
         //
         If ChgMode;
           //
           IF xxOFLG = 'N';
      /END-FREE
     C                   MOVE      'N'           xxEFLG            1            ERROR FOUND
      //
      /FREE
             CHAIN(N) keyWDELL PEL00;
             *IN60 = NOT %FOUND;
             //
      /END-FREE
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPFDT            7 0
      //
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPTDT            7 0
      //
      //  Check for claims outside policy eff dt range
      //
      /FREE
             IF xxPFDT > ELPFDT;
      /END-FREE
     C                   Z-ADD     ELPFDT        xxDATE
      //
      /FREE
               SETLL keyWMCMLC PCM0C;
               READE key1WMCMLC PCM0C;
               *IN60 = %EOF;
               //
               DOW *IN60 = *OFF
               AND CMACDT < xxPFDT;
      /END-FREE
     C                   XFOOT     AR            xxPMTS           13 2
      //
      /FREE
                 IF xxPMTS <> *ZEROS                                          //CLSD CLM W/PMTS
                 OR CMSTAT <> 'C';                                            //OPN CLM
      /END-FREE
     C                   MOVE      'Y'           xxEFLG            1
      /FREE
                   LEAVE;
                 ENDIF;
                 //
                 READE key1WMCMLC PCM0C;
                 *IN60 = %EOF;
               ENDDO;                                                         //PFDT
               //
             ENDIF;                                                           //PFDT
             //
             IF xxPTDT < ELPTDT
             AND xxEFLG = 'N';
      /END-FREE
     C                   Z-ADD     xxPTDT        xxDATE
      /FREE
               SETLL keyWMCMLC PCM0C;
               READE key1WMCMLC PCM0C;
               *IN60 = %EOF;
               //
               DOW *IN60 = *OFF
               AND CMACDT < ELPTDT;
      /END-FREE
     C                   XFOOT     AR            xxPMTS           13 2
      //
      /FREE
                 IF xxPMTS <> *ZEROS                                          //CLSD CLM W/PMTS
                 OR CMSTAT <> 'C';                                            //OPN CLM
      /END-FREE
     C                   MOVE      'Y'           xxEFLG            1
      /FREE
                   LEAVE;
                 ENDIF;
                 //
                 READE key1WMCMLC PCM0C;
                 *IN60 = %EOF;
               ENDDO;                                                         //PTDT
               //
             ENDIF;                                                           //PTDT
             //
             //  If claims with payments found outside new effective date range
             //
             IF xxEFLG = 'Y';
               //
               //  Check for user authority to override error msg
               //  and condition display of Y/N override on prompt window
               //
               CHAIN keyWMEXL PEX00;
               *IN60 = NOT %FOUND;
               //
      /END-FREE
     C                   move      *off          cadSecLev
      //
      /FREE
               IF *IN60 = *OFF;
                 //
      /END-FREE
     C                   MOVE      secType       ##UTYP
     C                   MOVE      secLevel      ##ULVL
     C                   MOVE      EXPDCT        ##CTYP
     C                   MOVE      EXPDCL        ##CLVL
      //
     C                   CALL      'SRCCSC'
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      *OFF          ##RES             1
      //
      /FREE
                 IF ##RES = *OFF;
      /END-FREE
     C                   move      *on           cadSecLev
      /FREE
                 ENDIF;
                 //
               ENDIF;
               //
               // Display "Claims outside eff date range" err window
               //
      /END-FREE
     C                   MOVE      *BLANK        scCADOver
      //
      //
     C                   move      *off          cadCancel
     C                   move      *off          cadConfirm
      //
      /FREE
               Dou cadCancel = *on or cadConfirm = *on;
                 EXFMT rcdClaimDt;
               ENDDO;
               //
               IF cadCancel = *ON;
      /END-FREE
     C                   move      *on           error
      /FREE
               ELSE;
                 //
                 IF scCADOver = 'Y';
      /END-FREE
     C                   MOVE      'Y'           xxOFlg
      /FREE
                 ELSE;
      /END-FREE
     C                   move      *on           error
      /FREE
                 Endif;                                                       //scCadOver ifeq
                 //
               ENDIF;                                                         //cadCancel ifeq
               //
             ENDIF;                                                           //xxEFlg ifeq
             //
           ENDIF;                                                             //XXOFLG ifeq
           //
         ENDIF;                                                               //if ChgMode
         //
       ENDSR;
       //===============================================================
       // UpdateDIL - Deductible Discount/Increased Limits Popup
       //===============================================================
       BEGSR UpdateDIL;
         //
         // IF INCREASED LIMITS CODE OR DEDUCTIBLE DISCOUNT CODE HAS CHANGED, OFFER
         // OPTION TO UPDATE ALL DIVISIONS
         //
      /END-FREE
     C                   MOVE      ' '           scUDIAll
     C                   MOVE      ' '           xxUPDD
      //
      /FREE
         IF chgMode = *ON
         AND FNPEOF <> *BLANK
         AND xxSTRP = 'Y'
         AND xx#DIV >= 2;
           //
           IF scILCD <> ELILCD
           OR scINL2 <> ELINL2
           OR scDCDE <> ELDCDE
           OR scDCD2 <> ELDCD2;
             //
      /END-FREE
     C                   MOVE      *OFF          xxCHEK            1
     C                   move      *off          erUDIAll
     C                   move      *blanks       msUDIAll
     C                   MOVE      *ON           *IN90
      //
      /FREE
             DOW *IN90 = *ON;
               //
               // Display Update Deduc/Incr Limits popup window
               //
      /END-FREE
     C                   move      *off          udiCancel
     C                   move      *off          udiConfirm
      //
      /FREE
               Dou udiCancel = *on or udiConfirm = *on;
                 EXFMT rcdUpdtDIL;
               ENDDO;
               //
      /END-FREE
     C                   MOVE      *OFF          *IN90
     C                   move      *off          erUDIAll
     C                   move      *blanks       msUDIAll
      //
      /FREE
               IF udiCancel = *ON;
      /END-FREE
     C                   move      *on           error
      /FREE
               ELSE;
                 //
                 //  ENSURE THAT INC LIMITS AND DED DISC CODES ARE VALID IN OTHER DIVS
                 //
                 IF scUDIAll = 'Y';
                   //
                   // Display info message (once) if any divisions are cancelled.
                   //
                   CHAIN key1WMEML PEM00;
                   *IN60 = NOT %FOUND;
                   DOW *IN60 = *OFF
                   AND xxCHEK = *OFF;
                     //
                     If getCxDate(emco#:emfnd:ememp#:emdiv) <> 0;
      /END-FREE
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30345'     msUDIAll
     C                   MOVE      *ON           XXCHEK
     C                   MOVE      *ON           *IN90
      /FREE
                       Leave;
                     Endif;
                     //
                     READE key1WMEML PEM00;
                     *IN60 = %EOF;
                   ENDDO;                                                     //*IN60 = *OFF
                   //
                   CHAIN(N) key1WDELL1 PEL01;
                   *IN60 = NOT %FOUND;
                   DOW *IN60 = *OFF;
                     //
                     CHAIN key1WMRSL PRS00;
                     *IN60 = NOT %FOUND;
      /END-FREE
     C                   MOVE      RSJRST        xxSt
     C                   Z-ADD     ELPFDT        xxDate
      //
      /FREE
                     IF scILCD <> *BLANKS;
      /END-FREE
     C                   MOVEL(P)  scILCD        xxKey
      /FREE
                       SETGT keyWTLML1 PLM01;
                       READPE key1WTLML1 PLM01;
                       *IN60 = %EOF;
                       DOW *IN60 = '0'
                       AND LMSTAT = 'I';
                         READPE key1WTLML1 PLM01;
                         *IN60 = %EOF;
                       ENDDO;                                                 //*IN60 DOWEQ
                       //
                       IF *IN60 = *ON;
      /END-FREE
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30346'     msUDIAll
     C                   MOVE      *ON           *IN90
      /FREE
                       ENDIF;                                                 //*IN60=*ON
                     ENDIF;                                                   //SCILCD <> *BLANKS
                     //
                     IF scINL2 <> *BLANKS;
      /END-FREE
     C                   MOVEL(P)  scINL2        xxKey
      /FREE
                       SETGT keyWTLML1 PLM01;
                       READPE key1WTLML1 PLM01;
                       *IN60 = %EOF;
                       DOW *IN60 = '0'
                       AND LMSTAT = 'I';
                         READPE key1WTLML1 PLM01;
                         *IN60 = %EOF;
                       ENDDO;                                                 //*IN60 DOWEQ
                       //
                       IF *IN60 = *ON;
      /END-FREE
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30346'     msUDIAll
     C                   MOVE      *ON           *IN90
      /FREE
                       ENDIF;                                                 //*IN60=*ON
                     ENDIF;                                                   //SCINL2 <> *BLANKS
                     //
                     IF scDCDE <> *BLANKS;
      /END-FREE
     C                   MOVEL(P)  scDCDE        xxDdcd
      /FREE
                       SETGT keyWTDDL2 PDD02;
                       READPE key1WTDDL2 PDD02;
                       *IN60 = %EOF;
                       DOW *IN60 = '0'
                       AND DDSTAT = 'I';
                         READPE key1WTDDL2 PDD02;
                         *IN60 = %EOF;
                       ENDDO;                                                 //*IN60 DOWEQ
                       //
                       IF *IN60 = *ON;
      /END-FREE
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30347'     msUDIAll
     C                   MOVE      *ON           *IN90
      /FREE
                       ENDIF;                                                 //*IN60=*ON
                     ENDIF;                                                   //SCDCDE <> *BLANKS
                     //
                     IF scDCD2 <> *BLANKS;
      /END-FREE
     C                   MOVEL(P)  scDCD2        xxDdcd
      /FREE
                       SETGT keyWTDDL2 PDD02;
                       READPE key1WTDDL2 PDD02;
                       *IN60 = %EOF;
                       DOW *IN60 = '0'
                       AND DDSTAT = 'I';
                         READPE key1WTDDL2 PDD02;
                         *IN60 = %EOF;
                       ENDDO;                                                 //*IN60 DOWEQ
                       //
                       IF *IN60 = *ON;
      /END-FREE
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30347'     msUDIAll
     C                   MOVE      *ON           *IN90
      /FREE
                       ENDIF;                                                 //*IN60=*ON
                     ENDIF;                                                   //scDCD2 <> *BLANKS
                     //
                     READE(N) key1WDELL1 PEL01;
                     *IN60 = %EOF;
                   ENDDO;
                   //
                 ENDIF;                                                       //scUDIAll IFEQ
                 //
               ENDIF;                                                         //udiCancel ifeq
               //
             ENDDO;                                                           //*IN90 = *ON
             //
             IF scUDIAll = 'Y';
      /END-FREE
     C                   MOVE      'Y'           xxUPDD
      /FREE
             ENDIF;
             //
             //  RESET EMP MASTER AND EMP DETAIL INFO
             //
             chain keyWMEML pem00;
             chain(n) keyWDELL pel00;
             CHAIN keyWMRSL PRS00;
             *IN60 = NOT %FOUND;
      /END-FREE
     C                   MOVE      RSJRST        xxSt
      //
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDate
      //
      /FREE
           ENDIF;                                                             //scILCD <> ELILCD
           //
         ENDIF;                                                               //if ChgMode
         //
       ENDSR;
       //===============================================================
       // RenewFlag - Do Not Renew Flag Popup
       //===============================================================
       BEGSR RenewFlag;
         //
         // Verify if user wants to update the Do Not Renew flag on all
         // divisions.
         //
      /END-FREE
     C                   MOVE      *OFF          xxUPDA
      //
      /FREE
         If scRLFL <> svRLFL
               and scRLFL <> s2RLFL;
           //
           If ChgMode and xx#Div >= 2;
             //
             CHAIN keyWMEXL PEX00;
             *IN60 = NOT %FOUND;
             //
             IF *IN60 = *OFF
             AND EXUPDA <> 'Y';
               //
               // Display Update Renew flag popup window (If update all functionality is not active)
               //
      /END-FREE
     C                   MOVE      *BLANK        scURFAll
      //
     C                   move      *off          urfCancel
     C                   move      *off          urfConfirm
      //
      /FREE
               Dou urfCancel = *on or urfConfirm = *on;
                 EXFMT rcdRenewF;
               ENDDO;
               //
               IF urfCancel = *ON;
      /END-FREE
     C                   move      *on           error
      /FREE
               ELSE;
                 //
                 IF scURFAll = 'Y';
      /END-FREE
     C                   MOVE      *ON           xxUPDA
      /FREE
                 Endif;                                                       //scURFAll ifeq
                 //
               ENDIF;                                                         //urfCancel ifeq
               //
             ENDIF;                                                           //*IN60 ifeq
             //
           ENDIF;                                                             //if ChgMode
           //
      /END-FREE
     C                   move      scRLFL        s2RLFL
      //
      /FREE
         ENDIF;                                                               //if scRLFL <>
         //
       ENDSR;
       //===============================================================
       // SpreadFlag - Spread Adjustments Flag Popup
       //===============================================================
       BEGSR SpreadFlag;
         //
         // If state reporting division, check other division's (with summarized
         // A/R) Sprd Adj flag. Pop up window informing that other divisions
         // will be changed to match if they are different.
         //
      /END-FREE
     C                   MOVE      *OFF          *IN82
     C                   MOVE      ' '           xxUPDC
      //
      /FREE
         IF xxSTRP = 'Y';
           //
           CHAIN key1WMEML PEM00;
           *IN60 = NOT %FOUND;
           DOW *IN60 = *OFF;
             //
             IF EMSTRP <> 'Y';
               //
               CHAIN(N) key2WDELL PEL00;
               *IN60 = NOT %FOUND;
               IF *IN60 = *OFF
               AND scFFL1 <> ELFFL1;
                 *IN82 = *ON;
                 LEAVE;
               ENDIF;                                                         //SCFFL1 = 'Y'
               //
             ENDIF;                                                           //EMSTRP = 'Y'
             //
             READE key1WMEML PEM00;
             *IN60 = %EOF;
           ENDDO;                                                             //*IN60 = *OFF
       //*** Reset file pointers so that correct division processes
       Chain(n) key1Wdell pem00;
       Chain(n) keyWdell  pel00;
      //
      // Verify user wants to proceed by updating all summarized divs
      // to have new value for spread adjustments flag.
      //
           IF *IN82 = *ON;
             //
             CLEAR scUSFAll;
      /END-FREE
     C                   move      *off          usfCancel
     C                   move      *off          usfConfirm
      //
      /FREE
             Dou usfCancel = *on or usfConfirm = *on;
               EXFMT rcdSpreadF;
             ENDDO;
             //
             IF usfCancel = *ON;
      /END-FREE
     C                   move      *on           error
      /FREE
             ELSE;
               //
               IF scUSFAll = 'Y';
      /END-FREE
     C                   move      'Y'           xxUPDC
      /FREE
               ENDIF;                                                         //scUSFAll = 'Y'
               //
             ENDIF;                                                           //usfCancel ifeq
             //
           ENDIF;                                                             //*IN82 = *ON
           //
         ENDIF;                                                               //xxSTRP IFEQ
         //
       ENDSR;
       //====================================================================
      // Display info window if % of manual premium value differs from default.
       //====================================================================
       Begsr ManualPercent;

          If clientID = '150' and (ELEmp# <> 1 or ELDiv <> 0);

             Chain(n) (ELco#:ELfnd:ELfyr) pf102;
             If %found and L2F1ManP <> scManP2;

                strMsg = 'The % of manual premium value on this division ' +
                 'does not equal the value from PEO (Employer 1 Division 0).' +
                 ' Press Confirm to continue.';

                If PopTwoBtn(strmsg:'Percentage of Manual Premium':
                 'Cancel':'Confirm');
                   error = *on;
                Endif;

             Endif;

          Endif;

      /end-free
      //
      /FREE
       Endsr;
       //
       //====================================================================
      // Process and write DARTS remark
       //====================================================================
       begsr SRPrcDARTS;
         //
      /END-FREE
     C                   MOVE      *blanks       xxLin2           80
     C                   MOVE      *blanks       xxComm          256
     C                   MOVE      *blanks       xxRmcd           10
      //
      // Load Line 1 text
     C                   MOVEL     ASCTXT        xxLin1           35
      // Build and load Line 2 text
      /FREE
         IF ELASCD = *BLANKS;
      /END-FREE
     C                   MOVEL     'blank'       xxFrCd            5
      /FREE
         ELSE;
      /END-FREE
     C                   MOVEL(P)  ELASCD        xxFrCd
      /FREE
         ENDIF;                                                               //ELASCD = *BLANKS
         //
         IF scASCD = *BLANKS;
      /END-FREE
     C                   MOVEL     'blank'       xxToCd            5
      /FREE
         ELSE;
      /END-FREE
     C                   MOVEL(P)  scASCD        xxToCd
      /FREE
         ENDIF;                                                               //SCASCD = *BLANKS
         //
         xxLin2 = ASCTX1 + ' ' + %TRIM(xxFrCd)
             + ' to ' + %TRIM(xxToCd)
             + ASCTX2 + %TRIM(q1user);
         //
         xxComm = xxLin1 + xxLin2;
         //
      /END-FREE
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ELCO#         QCCO#
     C                   MOVE      scFND         QCFND
     C                   MOVE      scEMP#        QCEMP#
     C                   MOVE      scDIV         QCDIV
      //**41****          MOVE      'Q'           QCQTE
     C                   MOVE      *BLANKS       QCQTE
      //
      /FREE
         xxRmcd = SVASRM;
         //
         // Call procedure to write DARTS
         WriteDarts(qckey:xxComm:xxRmcd);
         //
       endsr;                                                                 //srmvar
       //
       //

      /END-FREE
     P PolicyCheck     E

       //***************************************************************************//
       //===================================================================
       // Save Policy Data.
       //===================================================================
     P SavePolicyDet   B
     D SavePolicyDet   PI

     D @ArSmFlg        s              1
     D*** begin add ***  06/17/20  ******************************************
     D inStack         s               n   inz(*off)
     D*** end add *****  06/17/20  ******************************************
       //*** beg add ***** 02/02/21 ***********************
       Dcl-S chrWvrForm Like(ELblnkWD);
       //*** end add ***** 02/02/21 ***********************
       //*** Beg Add *** 10/16/23 *** 153WAO ***************************
     d   ##CO#         s              3  0
     d   ##FND         s              3  0
     d   ##FYR         s              3  0
     d   ##EMP#        s              9  0
       //*** Beg Add *** 10/16/23 *** 153WAO ***************************

      /free

       Chain keyWDELL PEL00;

       //*** Beg Add ***** 05/28/25 ************************
       Chain(E) (scCO#:scFND:scEMP#:scDiv) pqt12;
       If %Found(WAQTL12);
          QTefdt = dtCYMD(scPFDT);
          QTuser = Q1USER;
          QTpgm  = Q1PGM;
          QTtime = Timeto6(SystemTime());
          QTdt   = Dateto7(SystemDate());
          Update pqt12;
       Endif;
       //*** End Add ***** 05/28/25 ************************
       ELPFDT =  dtCYMD(scPFDT);
       ELPTDT =  dtCYMD(scPTDT);
       ELANDT =  dtCYMD(scANDT);
       ELBSDT =  dtCYMD(scBSDT);
       ELManP =  scManP2;
       If ELEmp# = 1 and ELDiv = 0 and ELManP <> 0;
          Chain(n) (ELCo#:ELFnd:ELFyr) pf102;
          If %found and L2F1ManP <> ELManP;
             Chain (ELCo#:ELFnd:ELFyr) pf102;
             L2F1ManP = ELManP;
             Update pf102;
          Endif;
       Endif;

       ELCOFL =  scCOFL2;
       ELFFL9 =  scFFL92;
       ELSRPR =  scSRPR2;
       ELBLFL =  scBLFL;
       ELSTAT =  scSTAT;
       If clientID = '102';
          ELFFL2 = sc25002;
       Endif;
       If clientId = '136';
          ELffl2 = scExAgtRat;
          ELcmul = scComModM;
       Endif;
       ELEMPY =  scEMPY;
       ELARSM =  scARSM;
       ELFFL1 =  scFFL1;
       ELIFFL =  scIFFL;

       ELRTCD = scRTCD;
       ELBSCD = scBSCD;
       //*** Begin Add *** 06/12/24 ****************************************
       // Set Detail S/R Assessment to group value and protect from change
       // Get the bill schedule code type
       chrBsCd          = %EditC(ELbsCd:'X');
       chrBillSchedType = BillSchdType(ELco#:chrBsCd);

       If clientID = '146';
          Chain (ELco#:ELfnd:ELfyr) PF100;
          If %Found(wdf1l) and F1SRAs = 'Y';
             ELSRAs  = 'Y';
             inqSRAs = *On;
          Else;
             ELSRAs =  scSRAs2;
          EndIf;
       EndIf;

       // Do show S/R Assessments if bill schedule code type is not S/R

       // Set which field is displayed
       if ratPopUp = *Off;
          visSRAs  = *Off;
          visSRAs2 = *On;
       Else;
          visSRAs  = *On;
          visSRAs2 = *Off;
       EndIf;

       If (chrBillSchedType <> 'S' And ELbsCd <> 0) Or
          (ELbsCd = 0) Or (clientID <> '146');
          visSRAs  = *Off;
          visSRAs2 = *Off;
          ELSRAs   = ' ';
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
       //*** Beg Add ***** 06/24/24 ************************
       If clientID = '146';
          ELColwp =  scColwp;
          ELColwa =  scColwa;
          ELColrp =  scColrp;
          ELColTyp = scColTyp;
          ELColad =  scColad;
          ELCollib = scCollib;
          ELColln =  scColln;
          ELCollid = dtCYMD(scCollid);
          ELColled = dtCYMD(scColled);
          ELColsic = scColsic;
          ELColsn =  scColsn;
          ELColsid = dtCYMD(scColsid);
          ELColsed = dtCYMD(scColsed);

          If ELcolwp <> svColwp or ELcolwa <> svColwa or ELcolrp <> svColrp or
           ELcolTyp <> svColTyp or ELcolad <> svColad or ELcollib <> svCollib
           or ELcolln <> svColln or ELcollid <> svCollid or ELcolled <>
           svColled or ELcolsic <> svColsic or ELcolsn <> svColsn or
           ELcolsid <> svColsid or ELcolsed <> svColsed;
             svColwp = ELColwp;
             svColwa = ELColwa;
             svColrp = ELColrp;
             svColTyp = ELColTyp;
             svColad = ELColad;
             svCollib = ELCollib;
             svColln = ELColln;
             svCollid = dtMDYY(ELCollid);
             svColled = dtMDYY(ELColled);
             svColsic = ELColsic;
             svColsn = ELColsn;
             svColsid = dtMDYY(ELColsid);
             svColsed = dtMDYY(ELColsed);
             isUpdColl = 'Y';
          Else;
             isUpdColl = 'N';
          Endif;

       Else;
          ELColwp =  *zeros;
          ELColwa =  *zeros;
          ELColrp =  *zeros;
          ELColTyp = *blanks;
          ELColad =  *blanks;
          ELCollib = *blanks;
          ELColln =  *blanks;
          ELCollid = *zeros;
          ELColled = *zeros;
          ELColsic = *blanks;
          ELColsn =  *blanks;
          ELColsid = *zeros;
          ELColsed = *zeros;
       Endif;
       //*** End Add ***** 06/24/24 ************************
       ELQTYP = scQTYP;
       ELTYPB = scTYPB;
       ELASCD = scASCD;
       ELPFIN = scPFIN;
       ELMCCD = scMCCD;
       ELMCGP = scMCGP;
       ELILCD = scILCD;
       ELINL2 = scINL2;
       ELDCDE = scDCDE;
       ELDCD2 = scDCD2;

       // Increased limits 1 and 2
       ELILEF =  dtCYMD(scIL1efDt);
       ELILEX =  dtCYMD(scIL1exDt);
       ELI2EF =  dtCYMD(scIL2efDt);
       ELI2EX =  dtCYMD(scIL2exDt);

       // Deductible Discount 1 and 2
       ELCXBL =  scCXBL;
       If ELdcde = *blanks;
          ELcxbl = *blanks;
       Endif;
       ELDCEF =  dtCYMD(scDD1efDt);
       ELDCEX =  dtCYMD(scDD1exDt);
       EL2DEF =  dtCYMD(scDD2efDt);
       EL2DEX =  dtCYMD(scDD2exDt);

       // Deviation 1
       ELASP2 = scASP2;
       If scASP2 <> 0 and scDV1efDt = 0;
          scDV1efDt = scPFDT;
       Endif;
       ELDVEF =  dtCYMD(scDV1efDt);
       ELDVEX =  dtCYMD(scDV1exDt);

       ELD1S2 = scD1S2;
       ELD1D2 =  dtCYMD(scD1D2EfDt);
       ELD1X2 =  dtCYMD(scD1D2ExDt);

       // Deviation 2
       ELDEV2 = scDEV2;
       ELstfc = scStFc;
       ELdev2F = scDev2F;
       ELD2EF =  dtCYMD(scDV2efDt);
       ELD2EX =  dtCYMD(scDV2exDt);

       ELD2S2 = scD2S2;
       ELD2D2 =  dtCYMD(scD2D2EfDt);
       ELD2X2 =  dtCYMD(scD2D2ExDt);

       // Subject Premium Credit 1A
       ELASPM = scASPM;
       ELMCEF =  dtCYMD(scMCefDt);
       ELMCEX =  dtCYMD(scMCexDt);

       // Subject Premium Credit 1B
       ELASPS = scASPS;
       ELSCEF =  dtCYMD(scSCefDt);
       ELSCEX =  dtCYMD(scSCexDt);

       // Contractors Credit
       ELASP1 = scASP1;
       ELCREF =  dtCYMD(scCCefDt);
       ELCREX =  dtCYMD(scCCexDt);

       // Schedule DB/CR
       ELDBCR = scDBCR;
       ELSDEF =  dtCYMD(scSDefDt);
       ELSDEX =  dtCYMD(scSDexDt);

       // Experience Modifiers
       ELPROM = scPROM;

       //ELEXM1 = scEXM1;
       Elnoexm1 = scNoExM1;
       If scNoExM1 = 'Y';
          ELexm1 = 1;
          ELemd1 = ELpfdt;
       Else;
          ELexm1 = scExM1;
          ELemd1 = dtCymd(scEm1Date);
       Endif;
       ELEMD1 =  dtCYMD(scEM1Date);
       //*** Begin Chg *** 07/30/23 ****************************************
       //*** ELEXM2 = scEXM2;
       //*** ELEMD2 =  dtCYMD(scEM2Date);
       //*** ELEXM3 = scEXM3;
       //*** ELEMD3 =  dtCYMD(scEM3Date);
       // Set the Experience Modifier 2 is Rated Flag
       Elnoexm2 = scNoExM2;
       If scNoExM2 = 'Y';
          ELexm2 = 1;
          ELemd2 = dtCymd(scEm2Date);
       Else;
          ELexm2 = scExM2;
          ELemd2 = dtCymd(scEm2Date);
       Endif;
       // Set the Experience Modifier 3 is Rated Flag
       Elnoexm3 = scNoExM3;
       If scNoExM3 = 'Y';
          ELexm3 = 1;
          ELemd3 = dtCymd(scEm3Date);
       Else;
          ELexm3 = scExM3;
          ELemd3 = dtCymd(scEm3Date);
       Endif;
       //*** End   Chg *** 07/30/23 ****************************************
       ELarp1 = scArp1;
       ELarp2 = scArp2;
       ELarp3 = scArp3;
       //*** Experience Modifier- Not final status flag ***
       If scExMd1Nf = 'Y';
          ELexMd1Nf = 'Y';
       else;
          ELexMd1Nf = ' ';
       EndIf;

       If scExMd2Nf = 'Y';
          ELexMd2Nf = 'Y';
       else;
          ELexMd2Nf = ' ';
       EndIf;

       If scExMd3Nf = 'Y';
          ELexMd3Nf = 'Y';
       else;
          ELexMd3Nf = ' ';
       EndIf;
       //*** Beg Add *** 03/04/24 *************************************
       // Experience Rating
       ELexRt = scExpRat;

       // Report to NCCI.  If changed then update all divisions
       ELNcci = scNcci;
       If ELncci <> svNcci;
          svNcci = ELncci;
          isUpdNcci = 'Y';
       Else;
          isUpdNcci = 'N';
       Endif;
       //*** End Add *** 03/04/24 *************************************

       // Stock Discount
       ELASP3 = scASP3;
       ELDISC = scDISC;

       // Memebership Fees
       ELMFEE = scMFEE;

       // Safety Fees
       ELSFPC = scSFPC;
       ELSFMN = scSFMN;

       // Waiver Surcharge
       ELWVTP = scWVTP;
       ELWVPC = scWVPC;
       ELFCHG = scFCHG;
       ELWMIN = scWMIN;
       ELWMAX = scWMAX;
       ELWVFL = scWVFL;

       //*** Begin Add *** 02/02/21 ********************************************
       //Blanket Waiver Description
       If scWvTp <> 'B';
          Clear scOvrWd;
       Endif;
       If scOvrWd <> 'Y';
          Clear scBlnkWd;
       Endif;
       ELovrWd = scOvrWd;
       ELBlnkWd = scBlnkWd;
       //*** End   Add *** 02/02/21 ********************************************


       // Facultative Reinsurance
       ELARCP = scARCP;
       ELARCA = scARCA;
       ELAREF = dtCYMD(scAREfDt);
       ELAREX = dtCYMD(scARExDt);

       ELVLCA = scVLCA;
       ELscoreBP = scScoreBP;

       Chain keyWMRPL PRP00;

       If not %found;
          Clear PRP00;
          RPCO#  = scCO#;
          RPFND  = scFND;
          RPEMP# = scEMP#;
          RPDIV  = scDIV;
          RPFYR  = scFYR;

          RPIPGM = Q1PGM;
          RPIUSR = Q1USER;
          RPITM  = Timeto6(SystemTime());
          RPIDT  = Dateto7(SystemDate());
       Endif;

       RPCARR = scRPCARR;
       RPCAP# = scRPCAP#;
       RPISDT =  dtCYMD(scRPISDT);
       RPEFDT =  dtCYMD(scRPEFDT);
       RPEXDT =  dtCYMD(scRPEXDT);
       RPCLT# = scRPCLT#;
       RPTPA# = scRPTPA#;
       RPBRCH = scRPBRCH;
       RPCNTR = scRPCNTR;

       RPPGM  = Q1PGM;
       RPUSER = Q1USER;
       RPTIME = Timeto6(SystemTime());
       RPDT   = Dateto7(SystemDate());

       If %found;
       //*** Beg Add *** 10/16/23 *** 153WAO ***************************
          If scRPPPOL <> RPPPOL;
             If scRPPPOL <> ' ';
                ELNWRW = 'R';
             Else;
                ##Co# = scCO#   ;
                ##Fnd = scFND   ;
                ##Fyr = scFYR   ;
                ##Emp# = scEMP# ;
                CallWA069R(##Co#:##Fnd:##Emp#:##Fyr:ELNWRW);
             // CallWA069R(scCO#:scFND:scEMP#:##Fyr:ELNWRW);
             EndIf;
             RPPPOL = scRPPPOL;
          EndIf;
       //*** End Add *** 10/16/23 *** 153WAO ***************************

          Update PRP00;
       Else;
          Write PRP00;
          Feod WMRPL;
       Endif;

       // Policy Page 3C
       ELFFLA = scFFLA;
       ELPP3C = scPP3C;
       ELPPC3 = scPPC3;
       //*** begin add ***  01/23/20  ******************************
       If ELFFLA <> 'Y';
          Clear ELPP3C;
          Clear ELPPC3;
       Endif;
       //*** end add *****  01/23/20  ******************************

       // Cancellations
       ELCXDY = scCXDY;
       ELADDM = scADDM;
       ELADVR = scADVR;
       ELRLFL = scRLFL;

       // Claims
       ELEXMF = scEXMF;

       //*** Beg Add *** 10/25/23 *** WC2922 ***************************
       // Call RemoveInvoices when the division is flagging as contested
       // audit and the division is either not summarized or is
       // summarized and it?s the state reporting division

         If scCTST = 'Y' and ELCTST <> 'Y' ;
            If ELARSM = ' ' Or (ELARSM = 'Y' and EMSTRP = 'Y');
               RemoveInvoices(scCo#:scFnd:scEmp#:scDiv:scFyr:'A');
            EndIf;
         EndIf;

         If scCTST <> 'Y' and ELCTST = 'Y' ;
            If ELARSM = ' ' Or (ELARSM = 'Y' and EMSTRP = 'Y');
               ResendInvoices(scCo#:scFnd:scEmp#:scDiv:scFyr:'A');
            EndIf;
         EndIf;

       //*** End Add *** 10/25/23 *** WC2922 ***************************

       // Audits
       ELCTST = scCTST;
       Chain(n) keyWMAUL2 PAU02;
       If %found and Elctst <> AUcnfl;
          Chain keyWMAUL2 PAU02;
          Aucnfl = Elctst;
          If Aucnfl = 'Y';
             Aucntd = DateTo7(SystemDate);
          Else;
             Aucntd = 0;
          Endif;
          Update PAU02;
       Endif;
       ELAPFL = scAPFL;
       ELAAFL = scAAFL;
       ELancf = scAnCf;

       // Other Policy
       If flexBox1 = *on;
          ELFFD1 = scFFD1;
       Else;
          ELFFD1 = scFFDD1;
       Endif;

       // Suppress Premium Notices in Mass Billing
       ELprmNtMas = scPrmNtMas;

       If flexBox2 = *on;
          ELFFD2 = scFFD2;
       Else;
          ELFFD2 = scFFDD2;
       Endif;

       If flexBox3 = *on;
          ELFFD3 = scFFD3;
       Else;
          ELFFD3 = scFFDD3;
       Endif;

       If flexBox4 = *on;
          ELFFD4 = scFFD4;
       Else;
          ELFFD4 = scFFDD4;
       Endif;

       If flexBox5 = *on;
          ELFFD5 = scFFD5;
       Else;
          ELFFD5 = scFFDD5;
       Endif;
       // Dividend Plan Info
       ELdvcd = scDivdPlan;
       ELdvpc = scFlatPrct;
       ELdvdf = scFlatDef;
       ELdvpr = scFlatDVal;
       If clientID = '111';
          Chain (scCo#:scFnd:scFyr:scEmp#:scDiv) pdv00;
          DVamt  = scDivAmt;
          DVpgm  = q1Pgm;
          DVuser = q1User;
          DVdt   = Dateto7(SystemDate());
          DVtime = Timeto6(SystemTime());

          If %found;
             Update pdv00;
          Else;
             DVco#  = scCo#;
             DVfnd  = scFnd;
             DVfyr  = scFyr;
             DVemp# = scEmp#;
             DVdiv  = scDiv;
             DVipgm = q1Pgm;
             DViusr = q1User;
             DVidt  = Dateto7(SystemDate());
             DVtime = Timeto6(SystemTime());

             Write pdv00;
             Feod WMDVL;
          Endif;
       Endif;

       // Large Deductible

       If largeDeduc = *on;

          Chain keyWMLDL1 PLD01;

          If not %found;
             Clear PLD01;
             LDCO#  = scCo#;
             LDFND  = scFnd;
             LDEMP# = scEmp#;
             LDDIV  = scDiv;
             LDFYR  = scFyr;

             LDIPGM = Q1PGM;
             LDIUSR = Q1USER;
             LDITIM = Timeto6(SystemTime());
             LDIDT  = Dateto7(SystemDate());
          Endif;

          LDEFD1 = dtCYMD(scLDEFD1);
          LDEXP1 = dtCYMD(scLDEXP1);
          LDRNW1 = scLDRNW1;
          LDDES1 = scLDDES1;
          LDPCT1 = scLDPCT1;
          LDTYP1 = scLDTYP1;
          LDAMP1 = scLDAMP1;
          LDAMG1 = scLDAMG1;
          LDVRB1 = scLDVRB1;
          LDOCV1 = scLDOCV1;
          LDCLS1 = scLDCLS1;
          LDPLV1 = scLDPLV1;
          LDPLN1 = scLDPLN1;
          LDTOT1 = scLDTOT1;
          LDALA1 = scLDALA1;
          LDR101 = scLDR101;
          LDR102 = scLDR102;
          LDR103 = scLDR103;
          LDR104 = scLDR104;
          LDR105 = scLDR105;
          LDR106 = scLDR106;
          LDR107 = scLDR107;
          LDR108 = scLDR108;
          LDR109 = scLDR109;
          LDR110 = scLDR110;
          LDadp1 = scADP1;
          LDvara1 = scVarA1;

          LDEFD2 = dtCYMD(scLDEFD2);
          LDEXP2 = dtCYMD(scLDEXP2);
          LDDES2 = scLDDES2;
          LDPCT2 = scLDPCT2;
          LDTYP2 = scLDTYP2;
          LDAMP2 = scLDAMP2;
          LDAMG2 = scLDAMG2;
          LDVRB2 = scLDVRB2;
          LDOCV2 = scLDOCV2;
          LDCLS2 = scLDCLS2;
          LDPLV2 = scLDPLV2;
          LDPLN2 = scLDPLN2;
          LDTOT2 = scLDTOT2;
          LDALA2 = scLDALA2;
          LDR201 = scLDR201;
          LDR202 = scLDR202;
          LDR203 = scLDR203;
          LDR204 = scLDR204;
          LDR205 = scLDR205;
          LDR206 = scLDR206;
          LDR207 = scLDR207;
          LDR208 = scLDR208;
          LDR209 = scLDR209;
          LDR210 = scLDR210;
          LDadp2 = scADP2;
          LDvarA2 = scVarA2;
          LDlirt = scClmLib;
          LDclal = scClmAlw;
       //*** Begin Add *** 09/02/20 **********************************
          LDincMdIn = scIncMdIn;
          If LDincMdIn = 'B';
             LDincMdIn = *Blank;
          Endif;
       //*** End   Add *** 09/02/20 **********************************
          LDPGM  = Q1PGM;
          LDUSER = Q1USER;
          LDTIME = Timeto6(SystemTime());
          LDDT   = Dateto7(SystemDate());

          If %found;
             Update PLD01;
          Else;
            Write PLD01;
            Feod WMLDL1;
          Endif;
       Endif;

       // Scheduled Rating Information

       If schdRating = *on;

          Chain keyWMRFL RFP00;

          If not %found;
             Clear RFP00;

             RFCO#  = scCo#;
             RFFND  = scFnd;
             RFFYR  = scFyr;
             RFEMP# = scEmp#;
             RFDIV  = scDiv;

             RFIPGM = Q1PGM;
             RFIUSR = Q1USER;
             RFIDT  = Dateto7(SystemDate());
          Endif;

          If RFPRCA <> scRFPRCA or
           RFPRDA <> scRFPRDA or
           RFPRRS <> scRFPRRS or
           RFCPCA <> scRFCPCA or
           RFCPDA <> scRFCPDA or
           RFCPRS <> scRFCPRS or
           RFMFCA <> scRFMFCA or
           RFMFDA <> scRFMFDA or
           RFMFRS <> scRFMFRS or
           RFSDCA <> scRFSDCA or
           RFSDDA <> scRFSDDA or
           RFSDRS <> scRFSDRS or
           RFEMCA <> scRFEMCA or
           RFEMDA <> scRFEMDA or
           RFEMRS <> scRFEMRS or
           RFMCCA <> scRFMCCA or
           RFMCDA <> scRFMCDA or
           RFMCRS <> scRFMCRS or
           RFMSCA <> scRFMSCA or
           RFMSDA <> scRFMSDA or
           RFMSRS <> scRFMSRS or
           RFLSCF <> scRFLSCF or
           RFLSDT <> dtCYMD(scRFLSDT) or
           RFPEFL <> scRFPEFL or
           RFPEEM <> scRFPEEM or
           RFCOM1 <> scRFCOM1 or
           RFCOM2 <> scRFCOM2 or
           RFCOM3 <> scRFCOM3;
             RFPRTF = ' ';
          Endif;

          RFPRCA = scRFPRCA;
          RFPRDA = scRFPRDA;
          RFPRRS = scRFPRRS;
          RFCPCA = scRFCPCA;
          RFCPDA = scRFCPDA;
          RFCPRS = scRFCPRS;
          RFMFCA = scRFMFCA;
          RFMFDA = scRFMFDA;
          RFMFRS = scRFMFRS;
          RFSDCA = scRFSDCA;
          RFSDDA = scRFSDDA;
          RFSDRS = scRFSDRS;
          RFEMCA = scRFEMCA;
          RFEMDA = scRFEMDA;
          RFEMRS = scRFEMRS;
          RFMCCA = scRFMCCA;
          RFMCDA = scRFMCDA;
          RFMCRS = scRFMCRS;
          RFMSCA = scRFMSCA;
          RFMSDA = scRFMSDA;
          RFMSRS = scRFMSRS;
          RFLSCF = scRFLSCF;
          RFLSDT = dtCYMD(scRFLSDT);
          RFPEFL = scRFPEFL;
          RFPEEM = scRFPEEM;
          RFCOM1 = scRFCOM1;
          RFCOM2 = scRFCOM2;
          RFCOM3 = scRFCOM3;

          RFPGM  = Q1PGM;
          RFUSER = Q1USER;
          RFTIME = Timeto6(SystemTime());
          RFDT   = Dateto7(SystemDate());


          If %found;
             Update RFP00;
          Else;
             Write  RFP00;
             Feod   WMRFL;
          Endif;
       Endif;

       // Retrospective Rating

       ELMNFC = scMNFC;
       ELMXFC = scMXFC;
       ELBSFC = scBSFC;
       ELTAX  = scTAX;
       ELTAXF = scTAXF;
       ELLCVF = scLCVF;
       ELRETN = scRETN;

       ELELP  = scELP;
       ELELPF = scELPF;
       ELRDF1 = scRDF1;
       ELRDF2 = scRDF2;
       ELRDF3 = scRDF3;

       ELSTDT =  dtCYMD(scSTDT);
       ELSTAM = scSTAM;
       ELFSET = scFSET;

       ELPGM  = Q1PGM;
       ELUSER = Q1USER;
       ELTIME = Timeto6(SystemTime());
       ELDT   = Dateto7(SystemDate());

      // Update the inception date with Policy effective (From date)

       Chain(n) (scCO#:scEMP#:scDIV:scFND) pem03;
       If %Found(wmeml3) and l3_EMefd1 = svPfDt;
          Chain (scCO#:scEMP#:scDIV:scFND) pem03;
          l3_EMefD1 = dtCYMD(scPFDT);
          Update pem03 %fields(l3_EMefD1);
       Endif;

       // Update PEO Master/MCP override field
       If scOvrMP <> EMOvrMP;
          Chain (scCO#:scEMP#:scDIV:scFND) pem03;
          If %Found(wmeml3);
             l3_EMOvrMP = scOvrMP;
             Update pem03 %fields(l3_EMOvrMP);
          Endif;
       Endif;

     // If bill start date was same as Policy From date and user made
      // some changes in the Policy From date, then Policy bill date should
      // be update with that date as well.

       If svBsDt = svPfDt and svPfDt <> dtCYMD(scPFDT);
          ELbsDt = dtCYMD(scPFDT);
       EndIf;

       Update PEL00;
      //
      // Update Employer Detail records as needed.
      //
       IF xxUPDA = *ON;
         EXSR UPDALL;
       ENDIF;
       //
       IF XXUPDB = 'Y';
         EXSR SRUPDB;
       ENDIF;
       //
       IF XXUPDC = 'Y';
         EXSR SRUPDC;
       ENDIF;
       //
       IF xxUPDD = 'Y';
         EXSR SRUPDT;
       ENDIF;
       //*** Beg Add *** 03/04/24 *************************************
       If isUpdNcci = 'Y';
          Exsr SrUpdNcciAllDiv;
       ENDIF;
       //*** End Add *** 03/04/24 *************************************
       //*** Beg Add *** 06/24/24 *************************************
       If isUpdColl = 'Y';
          Exsr SrUpdCollAllDiv;
       ENDIF;
       //*** End Add *** 06/24/24 *************************************

       chain keyWMEML pem00;
       chain(n) keyWDELL pel00;

       If chgMode = *on;
          Chain (ELco#:ELfnd) pex00;
          If %found and EXupda = 'Y';
             @ArSmFlg = '0';
             If svArSm = '@';
                svArSm = ' ';
                @ArSmFlg = '1';
             Endif;
             clear EFexcf;
             clear EFexco;
             clear EFscbd;
             Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pef01;

             isChgFlag1 = *off;
             If svPfDt <> ELpfdt or svPtDt <> ELptdt or svArSm <> ELarsm or
              svIfFl <> ELiffl or svQTyp <> ELqtyp or svAdvr <> ELadvr or
              s@RlFl <> ELrlfl or svBsDt <> ELbsdt or svStat <> ELstat or
              svIlCd <> ELilcd or svIlEf <> ELilef or svIlEx <> ELilex or
              svDcDe <> ELdcde or svDcEf <> ELdcef or svDcEx <> ELdcex or
              svCxBl <> ELcxbl or svAsp2 <> ELasp2 or svDvEf <> ELdvef or
              svDvEx <> ELdvex or svAsp1 <> ELasp1 or svCrEf <> ELcref or
              svCrEx <> ELcrex or svAspm <> ELaspm or svMcEf <> ELmcef or
              svMcEx <> ELmcex or svMcCd <> ELmccd or svMcGp <> ELmcgp or
              svDev2 <> ELdev2 or svStfc <> ELstfc or svD2Ef <> ELd2ef or
              svD2Ex <> ELd2ex or svAsPs <> ELasps or svScEf <> ELscef or
              svScEx <> ELscex or svSrPr <> ELsrpr or svAsP3 <> ELasp3 or
              svDisc <> ELdisc or svProm <> ELprom or svDbCr <> ELdbcr or
              svSdEf <> ELsdef or svSdEx <> ELsdex or svCxDy <> ELcxdy or
              svPp3c <> ELpp3c or svPpC3 <> ELppc3 or svMFee <> ELmfee or
              svSfPc <> ELsfpc or svSfam <> ELsfam or svSfmn <> ELsfmn or
              svAddm <> ELaddm or svAnDt <> ELandt or svDcd2 <> ELdcd2 or
              sv2DEf <> EL2def or sv2DEx <> EL2dex or svInl2 <> ELinl2 or
              svI2Ef <> ELi2ef or svI2Ex <> ELi2ex or svExMf <> ELexmf or
              svWvTp <> ELwvtp or svWvFl <> ELwvfl or svWvPc <> ELwvpc or
              svWMin <> ELwmin or svFChg <> ELfchg or svWMax <> ELwmax or
              svExCf <> EFexcf or svExCo <> EFexco or svScBd <> EFscbd or
              svD1S2 <> ELd1s2 or svD1D2 <> ELd1d2 or svD1X2 <> ELd1x2 or
              svD2S2 <> ELd2s2 or svD2D2 <> ELd2d2 or svD2X2 <> ELd2x2 or
              svBsCd <> ELbscd;
                isChgFlag1 = *on;
             Endif;

             //*** beg add ***** 02/02/21 *************
             If svBlnkWd <> ELblnkWd And ELwvtp = 'B';
                If GetWaiverInfo(ELco#:ELfnd:xxSt:ELpfdt:chrWvrForm) = *On;
                   UpdateBlkWvrDft(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
                    chrWvrForm:ELblnkWd);
                Endif;
             Endif;
             //*** end add ***** 02/02/21 *************

             isChgFlag2 = *off;
             If svASts <> ELascd or svCoFl <> ELcofl or svTypB <> ELtypb or
              svPFin <> ELpfin or svCtSt <> ELctst or svExM1 <> ELexm1 or
              svEmD1 <> ELemd1 or svExM2 <> ELexm2 or svEmD2 <> ELemd2 or
              svExM3 <> ELexm3 or svEmD3 <> ELemd3;
                isChgFlag2 = *on;
             Endif;

             dspAllDiv = *off;
             dspAllPol = *off;
             Chain (ELco#:ELfnd) pfn00;
             If %found;
                If (FNpeof = 'M' Or FNpeoF = 'C') And
                   isChgFlag1 = *on and isChgFlag2 = *on;
                   dspAllDiv = *off;
                   dspAllPol = *on;
                Else;
                   If (FNpeof = 'S' or FNpeof = ' ') and xx#Div > 1;
                      dspAllDiv = *on;
                      dspAllPol = *off;
                   Endif;
                Endif;
                If (FNpeoF = 'M' Or FNpeoF = 'C') And xx#Div > 1 And
                   isChgFlag1 = *Off and isChgFlag2 = *On;
                   dspAllDiv = *on;
                Endif;
                If (FNpeoF = 'M' Or FNpeoF = 'C') And xx#Div > 1 And
                   isChgFlag1 = *On And isChgFlag2 = *Off;
                   dspAllPol = *on;
                Endif;
             Endif;

             If dspAllPol = *on;
                scLblUpd1 = 'Apply Changes To All Policies And Divisions';
                scLblUpd2 = 'Include Cancelled Policies And Divisions As Well';
                If isChgFlag2 = *On And (FNpeoF = 'M' Or FNpeoF = 'C');
                   scLblUpd1b = '(Discount changes will occur across all ' +
                                'policies within the group)';
                EndIf;
             Endif;

             If dspAllDiv = *on;
                scLblUpd1 = 'Apply Changes To All Divisions';
                scLblUpd2 = 'Include Cancelled Divisions As Well';
                If isChgFlag2 = *On And (FNpeoF = 'M' Or FNpeoF = 'C');
                   scLblUpd1b = '(Policy Status changes will only occur to ' +
                                'the divisions on this policy.)';
                EndIf;
             Endif;

             If (dspAllPol = *on or dspAllDiv = *on) and
              (isChgFlag1 = *on or isChgFlag2 = *on);
                scUpdAll = 'N';
                scCnxInc = 'N';
                updConfirm = *off;
                Exfmt WdwUpdAll;

                If updConfirm = *on and scUpdAll = 'Y';
                   Exsr CHKFA;
                   Exsr UPKFA;
                Endif;
             Endif;
          Endif;
       Endif;
       isFirst = *off;
       CheckCallStack('WTPOLRBILL':inStack);
       If not inStack;
       //*** end add *****  06/17/20  ********************************************
          Exfmt rcdSave;
       //*** begin add ***  06/17/20  ********************************************
       Endif;
       //*** end add *****  06/17/20  ********************************************
       //===============================================================
        // chkfa - verify field not be available to copy in division
       //===============================================================
        BegSr chkfa;

        // save user,date and time
        s2user = q1user;
        s2date = DateTo7(SystemDate);
        s2time = Timeto6(SystemTime());

        // write dummy records in WXAUP (For Divisions)
        If FNpeof <> 'M';
           Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Dow not %eof;
              If ELilcd <> svIlCd;
                 UAdata = ' ';
                 UAdata = 'Increased Limits Code';
                 Exsr wrtfa1;
              Endif;

              If ELilef <> svIlEf;
                 UAdata = ' ';
                 UAdata = 'Increased Limits Effective Date';
                 Exsr wrtfa1;
              Endif;

              If ELilex <> svIlEx;
                 UAdata = ' ';
                 UAdata = 'Increased Limits Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELdcde <> svDcde;
                 UAdata = ' ';
                 UAdata = 'Deductible Code';
                 Exsr wrtfa1;
              Endif;

              If ELdcef <> svDcEf;
                 UAdata = ' ';
                 UAdata = 'Deductible Effective Date';
                 Exsr wrtfa1;
              Endif;

              If ELdcex <> svDcEx;
                 UAdata = ' ';
                 UAdata = 'Deductible Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELasp2 <> svAsp2;
                 UAdata = ' ';
                 UAdata = 'Deviation';
                 Exsr wrtfa1;
              Endif;

              If ELdvef <> svDvEf;
                 UAdata = ' ';
                 UAdata = 'Deviation Effective Date';
                 Exsr wrtfa1;
              Endif;

              If ELdvex <> svDvEx;
                 UAdata = ' ';
                 UAdata = 'Deviation Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELd1s2 <> svD1s2;
                 UAdata = ' ';
                 UAdata = 'Deviation 1 Split 2';
                 Exsr wrtfa1;
              Endif;

              If ELd1d2 <> svD1D2;
                 UAdata = ' ';
                 UAdata = 'Deviation 1 Split 2 Eff Date';
                 Exsr wrtfa1;
              Endif;

              If ELd1x2 <> svD1X2;
                 UAdata = ' ';
                 UAdata = 'Deviation 1 Split 2 Exp Date';
                 Exsr wrtfa1;
              Endif;

              If ELdev2 <> svDev2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2';
                 Exsr wrtfa1;
              Endif;

              If ELstfc <> svStFc;
                 UAdata = ' ';
                 UAdata = 'Stabilization Factor';
                 Exsr wrtfa1;
              Endif;

              If ELd2ef <> svD2Ef;
                 UAdata = ' ';
                 If clientID <> '025';
                    UAdata = 'Deviation 2 Effective Date';
                 Else;
                    UAdata = 'Stabilization Factor Eff Date';
                 Endif;
                 Exsr wrtfa1;
              Endif;

              If ELd2ex <> svD2Ex;
                 UAdata = ' ';
                 If clientID <> '025';
                    UAdata = 'Deviation 2 Expiration Date';
                 Else;
                    UAdata = 'Stabilization Factor Exp Date';
                 Endif;
                 Exsr wrtfa1;
              Endif;

              If ELd2s2 <> svD2S2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2 Split 2';
                 Exsr wrtfa1;
              Endif;

              If ELd2d2 <> svD2D2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2 Split 2 Eff Date';
                 Exsr wrtfa1;
              Endif;

              If ELd2x2 <> svD2X2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2 Split 2 Exp Date';
                 Exsr wrtfa1;
              Endif;

              If ELexm1 <> svExM1;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier 1';
                 Exsr wrtfa1;
              Endif;

              If ELemd1 <> svEmD1;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier Eff Date 1';
                 Exsr wrtfa1;
              Endif;

              If ELexm2 <> svExM2;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier 2';
                 Exsr wrtfa1;
              Endif;

              If ELemd2 <> svEmD2;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier Eff Date 2';
                 Exsr wrtfa1;
              Endif;

              If ELexm3 <> svExM3;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier 3';
                 Exsr wrtfa1;
              Endif;

              If ELemd3 <> svEmD3;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier Eff Date 3';
                 Exsr wrtfa1;
              Endif;

              If ELpfdt <> svPfDt;
                 UAdata = ' ';
                 UAdata = 'Policy Effective Date Errors';
                 Exsr wrtfa1;
              Endif;

              If ELptdt <> svPtDt;
                 UAdata = ' ';
                 UAdata = 'Policy Expiration Date Errors';
                 Exsr wrtfa1;
              Endif;

              If ELarsm <> svarsm;
                 UAdata = ' ';
                 UAdata = 'Summarize A/R To One Div';
                 Exsr wrtfa1;
              Endif;

              If ELiffl <> svIfFl;
                 UAdata = ' ';
                 UAdata = 'Override On Charging Installment Fee';
                 Exsr wrtfa1;
              Endif;

              If ELqtyp <> svQTyp;
                 UAdata = ' ';
                 UAdata = 'Policy Type';
                 Exsr wrtfa1;
              Endif;

              If ELadvr <> svAdvr;
                 UAdata = ' ';
                 UAdata = 'Adverse Account';
                 Exsr wrtfa1;
              Endif;

              If ELrlfl <> s@RlFl;
                 UAdata = ' ';
                 UAdata = 'Do Not Renew To New Year';
                 Exsr wrtfa1;
              Endif;

              If ELbsdt <> svBsDt;
                 UAdata = ' ';
                 UAdata = 'Billing Start Date Errors';
                 Exsr wrtfa1;
              Endif;

              If ELstat <> svstat;
                 UAdata = ' ';
                 UAdata = '"A" Rating';
                 Exsr wrtfa1;
              Endif;

              If ELcxbl <> svCxBl;
                 UAdata = ' ';
                 UAdata = 'Deductible Bill If Cancelled';
                 Exsr wrtfa1;
              Endif;

              If ELmccd <> svMcCd;
                 UAdata = ' ';
                 UAdata = 'MCO Type Code';
                 Exsr wrtfa1;
              Endif;

              If ELmcgp <> svMcGp;
                 UAdata = ' ';
                 UAdata = 'MCO Group Code';
                 Exsr wrtfa1;
              Endif;

              If ELsrpr <> svSrPr;
                 UAdata = ' ';
                 UAdata = 'Short Rate/ Prorata';
                 Exsr wrtfa1;
              Endif;

              If ELasp3 <> svAsp3;
                 UAdata = ' ';
                 UAdata = 'Discount';
                 Exsr wrtfa1;
              Endif;

              If ELprom <> svProm;
                 UAdata = ' ';
                 UAdata = 'Internal Promulgation';
                 Exsr wrtfa1;
              Endif;

              If ELcxdy <> svCxDy;
                 UAdata = ' ';
                 UAdata = 'CX Notice Days';
                 Exsr wrtfa1;
              Endif;

              If ELpp3c <> svPp3c;
                 UAdata = ' ';
                 UAdata = 'Policy Page Item 3C';
                 Exsr wrtfa1;
              Endif;

              If ELppc3 <> svPpC3;
                 UAdata = ' ';
                 UAdata = 'Policy Page Item 3C Line 2';
                 Exsr wrtfa1;
              Endif;

              If ELsfpc <> svSfPc;
                 UAdata = ' ';
                 UAdata = 'Safety Fee Percent';
                 Exsr wrtfa1;
              Endif;

              If ELsfmn <> svSfMn;
                 UAdata = ' ';
                 UAdata = 'Safety Fee Min Amount';
                 Exsr wrtfa1;
              Endif;

              If ELaddm <> svAddm;
                 UAdata = ' ';
                 UAdata = 'Additional Mailing Days';
                 Exsr wrtfa1;
              Endif;

              If ELandt <> svAnDt;
                 UAdata = ' ';
                 UAdata = 'Anniversary Rating Date Errors';
                 Exsr wrtfa1;
              Endif;

              If ELexmf <> svExMf;
                 UAdata = ' ';
                 UAdata = 'Exclude Medical';
                 Exsr wrtfa1;
              Endif;

              If ELasp1 <> svAsp1;
                 UAdata = ' ';
                 UAdata = 'Contractors Adj Credit';
                 Exsr wrtfa1;
              Endif;

              If ELcref <> svCrEf;
                 UAdata = ' ';
                 UAdata = 'Contractors Effective Date';
                 Exsr wrtfa1;
              Endif;

              If ELcrex <> svCrEx;
                 UAdata = ' ';
                 UAdata = 'Contractors Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELaspm <> svAsPm;
                 UAdata = ' ';
                 UAdata = 'Subj Premium Credit 1A Percent';
                 Exsr wrtfa1;
              Endif;

              If ELmcef <> svMcEf;
                 UAdata = ' ';
                 UAdata = 'Subj Pretium 1A Effective Date';
                 Exsr wrtfa1;
              Endif;

              If ELmcex <> svMcEx;
                 UAdata = ' ';
                 UAdata = 'Subj Premium 1A Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELasps <> svAsPs;
                 UAdata = ' ';
                 UAdata = 'Subj Premium Credit 1B Percent';
                 Exsr wrtfa1;
              Endif;

              If ELscef <> svScEf;
                 UAdata = ' ';
                 UAdata = 'Subj Premium 1B Effective Date';
                 Exsr wrtfa1;
              Endif;

              If ELscex <> svScEx;
                 UAdata = ' ';
                 UAdata = 'Subj Premium 1B Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELdbcr <> svDbCr;
                 UAdata = ' ';
                 UAdata = 'Schedule Db/Cr Factor';
                 Exsr wrtfa1;
              Endif;

              If ELsdef <> svSdEf;
                 UAdata = ' ';
                 UAdata = 'Schedule Db/Cr Effective Date';
                 Exsr wrtfa1;
              Endif;

              If ELscex <> svScEx;
                 UAdata = ' ';
                 UAdata = 'Schedule Db/Cr Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELdisc <> svDisc;
                 UAdata = ' ';
                 UAdata = 'Disc Type S,G';
                 Exsr wrtfa1;
              Endif;

              If ELwvtp <> svWvTp;
                 UAdata = ' ';
                 UAdata = 'Waiver Type';
                 Exsr wrtfa1;
              Endif;

              If ELwvfl <> svWvFl;
                 UAdata = ' ';
                 UAdata = 'Waiver Surcharge Prem Lvl';
                 Exsr wrtfa1;
              Endif;

              If ELwvpc <> svWvPc;
                 UAdata = ' ';
                 UAdata = 'Waiver % (Specific=Net%)';
                 Exsr wrtfa1;
              Endif;

              If ELwmin <> svWMin;
                 UAdata = ' ';
                 UAdata = 'Minimum Waiver Surcharge';
                 Exsr wrtfa1;
              Endif;

              If ELfchg <> svFChg;
                 UAdata = ' ';
                 UAdata = 'Waiver Flat Charge';
                 Exsr wrtfa1;
              Endif;

              If ELwmax <> svWMax;
                 UAdata = ' ';
                 UAdata = 'Waiver Maxium Charge';
                 Exsr wrtfa1;
              Endif;

              If ELdcd2 <> svDcD2;
                 UAdata = ' ';
                 UAdata = '2nd Deductible Discount Code';
                 Exsr wrtfa1;
              Endif;

              If EL2def <> sv2dEf;
                 UAdata = ' ';
                 UAdata = '2nd Deductible Effective Date';
                 Exsr wrtfa1;
              Endif;

              If EL2dex <> sv2dEx;
                 UAdata = ' ';
                 UAdata = '2nd Deductible Expiration Date';
                 Exsr wrtfa1;
              Endif;

              If ELinl2 <> svInl2;
                 UAdata = ' ';
                 UAdata = 'Increased Limits 2 Code';
                 Exsr wrtfa1;
              Endif;

              If ELi2ef <> svI2Ef;
                 UAdata = ' ';
                 UAdata = 'Increased Limits 2 Eff Date';
                 Exsr wrtfa1;
              Endif;

              If ELi2ex <> svI2Ex;
                 UAdata = ' ';
                 UAdata = 'Increased Limits 2 Exp Date';
                 Exsr wrtfa1;
              Endif;

              If ELascd <> svASts;
                 UAdata = ' ';
                 UAdata = 'Activity Status';
                 Exsr wrtfa1;
              Endif;

              If ELcofl <> svCoFl;
                 UAdata = ' ';
                 UAdata = 'Short Term To Establish Concurrency';
                 Exsr wrtfa1;
              Endif;

              If ELtypb <> svTypB;
                 UAdata = ' ';
                 UAdata = 'Type Of Building Code';
                 Exsr wrtfa1;
              Endif;

              If ELpfin <> svPFin;
                 UAdata = ' ';
                 UAdata = 'Premium Finance Code';
                 Exsr wrtfa1;
              Endif;

              If ELctst <> svCtSt;
                 UAdata = ' ';
                 UAdata = 'Contested Audit';
                 Exsr wrtfa1;
              Endif;

              If ELmfee <> svMFee;
                 UAdata = ' ';
                 UAdata = 'Member Fee Amount';
                 Exsr wrtfa1;
              Endif;

              If EFexcf <> svExCf;
                 UAdata = 'Exclude Employer From Claim Fee';
                 Exsr wrtfa1;
              Endif;

              If EFexco <> svExCo;
                 UAdata = 'Exclude Employer From Collateral';
                 Exsr wrtfa1;
              Endif;

              If EFscbd <> svScBd;
                 UAdata = 'Stop Collateral Billing Date';
                 Exsr wrtfa1;
              Endif;
              If ELbscd <> svBsCd;
                 UAdata = ' ';
                 UAdata = 'Billing Schedule Code';
                 Exsr wrtfa1;
              Endif;
              Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Enddo;
        Endif;

        // populate xxst with RSjrst
        // For Divisions
        If FNpeof <> 'M';
           Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Dow not %eof(wdell8);
              If ELdiv <> E2div;
                 RSjrst = ' ';
                 // Chain (E2co#:E2rtcd) prs01;
                 Chain (E2co#:E2rtcd) prs00;
                 If %found;
                    xxSt = RSjrst;
                 Endif;

                 // check ELilcd
                 If ELilcd <> svIlCd;
                    xxKey = ELilcd;
                    Setgt (ELco#:ELfnd:xxSt:xxKey:ELpfdt) plm01;
                    Readpe (ELco#:ELfnd:xxSt:xxKey) plm01;
                    // write record to WXUAL
                    If %eof and ELilcd <> *blanks;
                       UAdata = ' ';
                       UAdata = 'Increased Limits Code';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELdcde
                 If ELdcde <> svDcDe;
                    xxDdCd = ELdcde;
                    SetGt (Elco#:ELfnd:xxSt:xxDdCd:ELpfdt) pdd02;
                    Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                    If %eof and ELdcde <> *blanks;
                       UAdata = ' ';
                       UAdata = 'Deductible Code';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELinl2
                 If ELinl2 <> svInl2;
                    xxKey = ELinl2;
                    SetGt (Elco#:ELfnd:xxSt:xxKey:ELpfdt) plm01;
                    Readpe (Elco#:ELfnd:xxSt:xxKey) plm01;
                    If %eof and ELinl2 <> *blanks;
                       UAdata = ' ';
                       UAdata = 'Increased Limits 2 Code';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELdcd2
                 If ELdcd2 <> svDcD2;
                    xxDdCd = ELdcd2;
                    SetGt (Elco#:ELfnd:xxSt:xxDdCd:ELpfdt) pdd02;
                    Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                    If %eof and ELdcd2 <> *blanks;
                       UAdata = ' ';
                       UAdata = '2nd Deductible Discount Code';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELasp2
                 If ELasp2 <> svAsp2;
                    SetGt (Elco#:ELfnd:xxSt:ELpfdt) prr00;
                    Readpe (Elco#:ELfnd:xxSt) prr00;
                    If %eof;
                       If ELasp2 <> 0;
                          UAdata = ' ';
                          UAdata = 'Deviation';
                          Exsr wrtfa;
                       Endif;
                    Else;
                       If (RRdv1c = *zero and (isCltFlg = *off or
                        isCltFlg = *on and RRst <> 'NM')) or
                        (RRdv1l = ' ' and isCltFlg = *on and RRst = 'NM');
                          If ELasp2 <> 0;
                             UAdata = ' ';
                             UAdata = 'Deviation';
                             Exsr wrtfa;
                          Endif;
                       Endif;
                    Endif;
                 Endif;

                 // check ELd1s2
                 If ELd1s2 <> svd1s2;
                    SetGt (Elco#:ELfnd:xxSt:ELpfdt) prr00;
                    Readpe (Elco#:ELfnd:xxSt) prr00;
                    If %eof;
                       If ELd1s2 <> 0;
                          UAdata = ' ';
                          UAdata = 'Deviation 1 Split 2';
                          Exsr wrtfa;
                       Endif;
                    Else;
                       If (RRdv1c = *zero and (isCltFlg = *off or
                        isCltFlg = *on and RRst <> 'NM')) or
                        (RRdv1l = ' ' and isCltFlg = *on and RRst = 'NM');
                          If ELd1s2 <> 0;
                             UAdata = ' ';
                             UAdata = 'Deviation 1 Split 2';
                             Exsr wrtfa;
                          Endif;
                       Endif;
                    Endif;
                 Endif;

                 SetGt (Elco#:ELfnd:xxSt:ELpfdt) prr00;
                 Readpe (Elco#:ELfnd:xxSt) prr00;
                 // check ELdev2
                 If %eof;
                    If ELdev2 <> svDev2 and ELdev2 <> 0;
                       UAdata = ' ';
                       UAdata = 'Deviation 2';
                       Exsr wrtfa;
                    Endif;
                 Else;
                    If ELdev2 <> svDev2 and RRdv2c = *zero and ELdev2 <> 0;
                       UAdata = ' ';
                       UAdata = 'Deviation 2';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELd2s2
                 If %eof;
                    If ELd2s2 <> svD2S2 and ELd2s2 <> 0;
                       UAdata = ' ';
                       UAdata = 'Deviation 2 Split 2';
                       Exsr wrtfa;
                    Endif;
                 Else;
                    If ELd2s2 <> svD2S2 and RRdv2c = *zero and ELd2s2 <> 0;
                       UAdata = ' ';
                       UAdata = 'Deviation 2 Split 2';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check Stabilization factor
                 If %eof;
                    If ELstfc <> svStFc and ELstfc <> 0;
                       UAdata = ' ';
                       UAdata = 'Stabilization Factor';
                       Exsr wrtfa;
                    Endif;
                 Else;
                    If ELstfc <> svStFc and RRdv2c = *zero and ELstfc <> 0;
                       UAdata = ' ';
                       UAdata = 'Stabilization Factor';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELasp1
                 If %eof;
                    If ELasp1 <> svAsp1 and ELasp1 <> 0;
                       UAdata = ' ';
                       UAdata = 'Contractors Adj Credit';
                       Exsr wrtfa;
                    Endif;
                 Else;
                    If ELasp1 <> svAsp1 and RRCcCd = *zero and ELasp1 <> 0;
                       UAdata = ' ';
                       UAdata = 'Contractors Adj Credit';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELaspm
                 If %eof;
                    If ELaspm <> svAspm and ELaspm <> 0;
                       UAdata = ' ';
                       UAdata = 'Subj Premium Credit 1A Percent';
                       Exsr wrtfa;
                    Endif;
                 Else;
                    If ELaspm <> svAspm and RR1acd = *zero and ELaspm <> 0;
                       UAdata = ' ';
                       UAdata = 'Subj Premium Credit 1A Percent';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELasps
                 If %eof;
                    If ELasps <> svAsps and ELasps <> 0;
                       UAdata = ' ';
                       UAdata = 'Subj Premium Credit 1B Percent';
                       Exsr wrtfa;
                    Endif;
                 Else;
                    If ELasps <> svAsps and RR1bcd = *zero and ELasps <> 0;
                       UAdata = ' ';
                       UAdata = 'Subj Premium Credit 1B Percent';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELdbcr
                 If %eof;
                    If ELdbcr <> svDbCr and ELdbcr <> 0;
                       UAdata = ' ';
                       UAdata = 'Schedule Db/Cr Factor';
                       Exsr wrtfa;
                    Endif;
                 Else;
           //*** begin chg ***  06/24/22  ************************************
           //*****  If ELdbcr <> svDbCr and RRdbcd = *zero and ELdbcr <> 0;
                    If ELdbcr <> svDbCr and ELdbcr <> 0 and ((ELDbcr > 1 and
                     (RRDbcd = 0 or (ELDbcr > RRDmax and RRDmax <> 0))) or
                     (ELDBcr < 1 and (RRCrcd = 0 or (ELDbcr < RRCmax and RRCmax
                     <> 0))));
           //*** end chg *****  06/24/22  ************************************
                       UAdata = ' ';
                       UAdata = 'Schedule Db/Cr Factor';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check Waiver Surcharge
                 If %eof;
                    If ELwvtp <> svWvTp and ELwvtp <> *blanks;
                       UAdata = ' ';
                       UAdata = 'Waiver Type';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If %eof;
                    If ELwvfl <> svWvFl and ELwvfl <> *blanks;
                       UAdata = ' ';
                       UAdata = 'Waiver Surcharge Premium Level';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If %eof;
                    If ELwvpc <> svWvPc and elwvpc <> 0;
                       UAdata = ' ';
                       UAdata = 'Waiver % (Specific=Net%)';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If %eof;
                    If ELwmin <> svWMin and ELwmin <> 0;
                       UAdata = ' ';
                       UAdata = 'Minimum Waiver Surcharge';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If %eof;
                    If ELfchg <> svFChg and ELfchg <> 0;
                       UAdata = ' ';
                       UAdata = 'Waiver Flat Charge';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If %eof;
                    If ELwmax <> svWMax and ELwmax <> 0;
                       UAdata = ' ';
                       UAdata = 'Waiver Maximum Charge';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If not %eof;
                    If ELwvtp <> svWvTp and RRwvac = 0 and ELwvtp <> *blanks;
                       UAdata = ' ';
                       UAdata = 'Waiver Type';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If not %eof;
                    If ELwvfl <> svWvFl and RRwvac = 0 and ELwvfl <> *blanks;
                       UAdata = ' ';
                       UAdata = 'Waiver Surcharge Premium Level';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If not %eof;
                    If ELwvpc <> svWvpc and RRwvac = 0 and ELwvpc <> 0;
                       UAdata = ' ';
                       UAdata = 'Waiver % (Specific=Net%)';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If not %eof;
                    If ELwmin <> svWMin and RRwvac = 0 and ELwmin <> 0;
                       UAdata = ' ';
                       UAdata = 'Minimum Waiver Surcharge';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If not %eof;
                    If ELfchg <> svFChg and RRwvac = 0 and ELfchg <> 0;
                       UAdata = ' ';
                       UAdata = 'Waiver Flat Charge';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 If not %eof;
                    If ELwmax <> svWMax and RRwvac = 0 and ELwmax <> 0;
                       UAdata = ' ';
                       UAdata = 'Waiver Maximum Charge';
                       Exsr wrtfa;
                    Endif;
                 Endif;

                 // check ELdisc
                 SetGt (ELco#:ELfnd:xxSt:ELdisc:ELpfdt) pds03;
                 Readpe (ELco#:ELfnd:xxSt:ELdisc) pds03;
                 If ELdisc <> ' ' and %eof;
                    UAdata = ' ';
                    UAdata = 'Discount Type S,G';
                    Exsr wrtfa;
                 Endif;

                 // policy from date cannot be less than initial eff date(division)
                 If ELpfdt <> svPfDt;
                    Chain (ELco#:E2fnd:E2emp#:E2div) pem00;
                    If %found;
                       If ELpfdt < EMefd1;
                          UAdata = ' ';
                          UAdata = 'Policy Effective Date Errors';
                          Exsr wrtfa;
                       Endif;
                    Endif;

                    // policy from date must be less than or equal to all existing
                    // payroll records(for all divisions)
                    Setll (ELco#:E2fnd:E2emp#:E2div:E2fyr) pep00;
                    Reade (ELco#:E2fnd:E2emp#:E2div:E2fyr) pep00;
                    Dow not %eof;
                       If EPefdt < ELpfdt and EPstat <> 'I';
                          UAdata = ' ';
                          UAdata = 'Policy Effective Date Errors';
                          Exsr wrtfa;
                          Leave;
                       Endif;
                       Reade (ELco#:E2fnd:E2emp#:E2div:E2fyr) pep00;
                    Enddo;
                 Endif;

                 // edit policy to date
                 // dates must be ascending order(for divisions)
                 If ELptdt < ELpfdt;
                    UAdata = ' ';
                    UAdata = 'Policy Expiration Date Errors';
                    Exsr wrtfa;
                 Endif;

                 // Anniv date must be less than or equal to pol from date(for divisions)
                 If ELandt <> svAnDt and ELandt > E2pfdt;
                    UAdata = ' ';
                    UAdata = 'Employer Detail Information';
                    Exsr wrtfa;
                 Endif;

                 // billing start date cannot be greater than policy eff date
                 If ELbsdt <> svBsDt;
                    If ELbsdt > E2pfdt;
                       UAdata = ' ';
                       UAdata = 'Billing Start Date Errors';
                       Exsr wrtfa;
                    Endif;

                    // do not allow billing start date to be prior to current date if
                    // we are changing the value of the field or adding a record.
                    // unless the change is the billing start date to = policy from date
                    sysDt7 = DateTo7(SystemDate);
                    If (addMode = *on and ELpfdt > sysDt7) or (addMode = *off
                     and ELbsdt <> E2bsdt);
                       If ELbsdt<sysDt7 and ELbsdt <> ELpfdt and ELarsm <> 'Y';
                          UAdata = ' ';
                          UAdata = 'Billing Start Date Errors';
                          Exsr wrtfa;
                       Endif;
                    Endif;
                 Endif;
                 qquser = s2user;
                 qqdate = s2date;
                 qqtime = s2time;
                 qqcxfl = scCnxInc;
              Endif;
              Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Enddo;
        Endif;

        // write dummy records in wxaup(for policies)
        If (FNpeoF = 'M' Or FNpeoF = 'C') And isChgFlag2 = *On;
           Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Dow not %eof;
              If ELascd <> svASts;
                 UAdata = ' ';
                 UAdata = 'Activity Status';
                 Exsr wrtfa1;
              Endif;

              If ELcofl <> svCoFl;
                 UAdata = ' ';
                 UAdata = 'Short Term To Establish Concurrency';
                 Exsr wrtfa1;
              Endif;

              If ELtypb <> svTypB;
                 UAdata = ' ';
                 UAdata = 'Type Of Building Code';
                 Exsr wrtfa1;
              Endif;

              If ELpfin <> svPFin;
                 UAdata = ' ';
                 UAdata = 'Premium Finance Code';
                 Exsr wrtfa1;
              Endif;

              If ELctst <> svCtSt;
                 UAdata = ' ';
                 UAdata = 'Contested Audit';
                 Exsr wrtfa1;
              Endif;
              Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Enddo;
        Endif;

        If FNpeoF = 'M' Or FNpeoF = 'C';
           #TotDivM = 0;
           Setll (ELco#:ELfnd:ELfyr) pel0q;
           Reade (ELco#:ELfnd:ELfyr) pel0q;
           Dow not %eof;
              #TotDivM = #TotDivM + 1;

              If ELilcd <> svIlCd;
                 UAdata = ' ';
                 UAdata = 'Increased Limits Code';
                 Exsr wrtfa1_1;
              Endif;

              If ELilef <> svIlEf;
                 UAdata = ' ';
                 UAdata = 'Increased Limits Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELilex <> svIlEx;
                 UAdata = ' ';
                 UAdata = 'Increased Limits Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELdcde <> svDcDe;
                 UAdata = ' ';
                 UAdata = 'Deductible Code';
                 Exsr wrtfa1_1;
              Endif;

              If ELdcef <> svDcEf;
                 UAdata = ' ';
                 UAdata = 'Deductible Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELdcex <> svDcEx;
                 UAdata = ' ';
                 UAdata = 'Deductible Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELasp2 <> svAsp2;
                 UAdata = ' ';
                 UAdata = 'Deviation';
                 Exsr wrtfa1_1;
              Endif;

              If ELdvef <> svDvEf;
                 UAdata = ' ';
                 UAdata = 'Deductible Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELdvex <> svDvEx;
                 UAdata = ' ';
                 UAdata = 'Deductible Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELd1s2 <> svD1S2;
                 UAdata = ' ';
                 UAdata = 'Deviation 1 Split 2';
                 Exsr wrtfa1_1;
              Endif;

              If ELd1d2 <> svD2D2;
                 UAdata = ' ';
                 UAdata = 'Deviation 1 Split 2 Eff Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELd1x2 <> svD1X2;
                 UAdata = ' ';
                 UAdata = 'Deviation 1 Split 2 Exp Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELdev2 <> svDev2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2';
                 Exsr wrtfa1_1;
              Endif;

              If ELstfc <> svStFc;
                 UAdata = ' ';
                 UAdata = 'Stabilization Factor';
                 Exsr wrtfa1_1;
              Endif;

              If ELd2ef <> svD2Ef;
                 UAdata = ' ';
                 If clientID <> '025';
                    UAdata = 'Deviation 2 Effective Date';
                 Else;
                    UAdata = 'Stabilization Factor Effective Date';
                 Endif;
                 Exsr wrtfa1_1;
              Endif;

              If ELd2ex <> svD2Ex;
                 UAdata = ' ';
                 If clientID <> '025';
                    UAdata = 'Deviation 2 Expiration Date';
                 Else;
                    UAdata = 'Stabilization Factor Expiration Date';
                 Endif;
                 Exsr wrtfa1_1;
              Endif;

              If ELd2s2 <> svD2S2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2 Split 2';
                 Exsr wrtfa1_1;
              Endif;

              If ELd2d2 <> svD2d2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2 Split 2 Eff Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELd2x2 <> svD2X2;
                 UAdata = ' ';
                 UAdata = 'Deviation 2 Split 2 Exp Date';
                 Exsr wrtfa1_1;
              Endif;

              If FNpeoF = 'C' and E3emp# = ELemp#;
              If ELexm1 <> svExM1;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier 1';
                 Exsr wrtfa1_1;
              Endif;

              If ELemd1 <> svEmD1;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier Effective Date 1';
                 Exsr wrtfa1_1;
              Endif;

              If ELexm2 <> svExM2;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier 2';
                 Exsr wrtfa1_1;
              Endif;

              If ELemd2 <> svEmD2;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier Effective Date 2';
                 Exsr wrtfa1_1;
              Endif;

              If ELexm3 <> svExM3;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier 3';
                 Exsr wrtfa1_1;
              Endif;

              If ELemd3 <> svEmD3;
                 UAdata = ' ';
                 UAdata = 'Experience Modifier Effective Date 3';
                 Exsr wrtfa1_1;
              Endif;
              Endif;

              If ELpfdt <> svPfDt;
                 UAdata = ' ';
                 UAdata = 'Policy Effective Date Errors';
                 Exsr wrtfa1_1;
              Endif;

              If ELptdt <> svPtDt;
                 UAdata = ' ';
                 UAdata = 'Policy Expiration Date Errors';
                 Exsr wrtfa1_1;
              Endif;

              If ELarsm <> svArSm;
                 UAdata = ' ';
                 UAdata = 'Summarize A/R To One Div';
                 Exsr wrtfa1_1;
              Endif;

              If ELiffl <> svIfFl;
                 UAdata = ' ';
                 UAdata = 'Override On Charging Installment Fee';
                 Exsr wrtfa1_1;
              Endif;

              If ELqtyp <> svQTyp;
                 UAdata = ' ';
                 UAdata = 'Policy Type';
                 Exsr wrtfa1_1;
              Endif;

              If ELadvr <> svAdvr;
                 UAdata = ' ';
                 UAdata = 'Adverse Account Flag';
                 Exsr wrtfa1_1;
              Endif;

              If ELrlfl <> s@RlFl;
                 UAdata = ' ';
                 UAdata = 'Do Not Renew To New Year';
                 Exsr wrtfa1_1;
              Endif;

              If ELbsdt <> svBsDt;
                 UAdata = ' ';
                 UAdata = 'Billing Start Date Errors';
                 Exsr wrtfa1_1;
              Endif;

              If ELstat <> svStat;
                 UAdata = ' ';
                 UAdata = '"A" Rating Flag';
                 Exsr wrtfa1_1;
              Endif;

              If ELcxbl <> svCxBl;
                 UAdata = ' ';
                 UAdata = 'Deductible Bill If Cancelled';
                 Exsr wrtfa1_1;
              Endif;

              If ELmccd <> svMcCd;
                 UAdata = ' ';
                 UAdata = 'MCO Type Code';
                 Exsr wrtfa1_1;
              Endif;

              If ELmcgp <> svMcGp;
                 UAdata = ' ';
                 UAdata = 'MCO Group Code';
                 Exsr wrtfa1_1;
              Endif;

              If ELsrpr <> svSrPr;
                 UAdata = ' ';
                 UAdata = 'Short Rate/ Prorata';
                 Exsr wrtfa1_1;
              Endif;

              If ELasp3 <> svAsp3;
                 UAdata = ' ';
                 UAdata = 'Discount';
                 Exsr wrtfa1_1;
              Endif;

              If ELprom <> svProm;
                 UAdata = ' ';
                 UAdata = 'Internal Promulgation';
                 Exsr wrtfa1_1;
              Endif;

              If ELcxdy <> svCxDy;
                 UAdata = ' ';
                 UAdata = 'CX Notice Days';
                 Exsr wrtfa1_1;
              Endif;

              If ELpp3c <> svPp3C;
                 UAdata = ' ';
                 UAdata = 'Policy Page Item 3C';
                 Exsr wrtfa1_1;
              Endif;

              If ELppc3 <> svPpC3;
                 UAdata = ' ';
                 UAdata = 'Policy Page Item 3C Line 2';
                 Exsr wrtfa1_1;
              Endif;

              If ELsfpc <> svSfPc;
                 UAdata = ' ';
                 UAdata = 'Safety Fee Percent';
                 Exsr wrtfa1_1;
              Endif;

              If ELsfmn <> svSfMn;
                 UAdata = ' ';
                 UAdata = 'Safety Fee Min Amt';
                 Exsr wrtfa1_1;
              Endif;

              If ELaddm <> svAddM;
                 UAdata = ' ';
                 UAdata = 'Additional Mailing Days';
                 Exsr wrtfa1_1;
              Endif;

              If ELandt <> svAnDt;
                 UAdata = ' ';
                 UAdata = 'Anniversary Rating Date Errors';
                 Exsr wrtfa1_1;
              Endif;

              If ELexmf <> svExMf;
                 UAdata = ' ';
                 UAdata = 'Exclude Medical';
                 Exsr wrtfa1_1;
              Endif;

              If ELasp1 <> svasp1;
                 UAdata = ' ';
                 UAdata = 'Contractors Adj Credit';
                 Exsr wrtfa1_1;
              Endif;

              If ELcref <> svCrEf;
                 UAdata = ' ';
                 UAdata = 'Contractors Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELcrex <> svCrEx;
                 UAdata = ' ';
                 UAdata = 'Contractors Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELaspm <> svAsPm;
                 UAdata = ' ';
                 UAdata = 'Subj Premium Credit 1A Percent';
                 Exsr wrtfa1_1;
              Endif;

              If ELmcef <> svMcEf;
                 UAdata = ' ';
                 UAdata = 'Subj Premium 1A Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELmcex <> svMcEx;
                 UAdata = ' ';
                 UAdata = 'Subj Premium 1A Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELasps <> svAsPs;
                 UAdata = ' ';
                 UAdata = 'Subj Premium Credit 1B Percent';
                 Exsr wrtfa1_1;
              Endif;

              If ELscef <> svScEf;
                 UAdata = ' ';
                 UAdata = 'Subj Premium 1B Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELscex <> svScEx;
                 UAdata = ' ';
                 UAdata = 'Subj Premium 1B Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELdbcr <> svDbCr;
                 UAdata = ' ';
                 UAdata = 'Schedule Db/Cr Factor';
                 Exsr wrtfa1_1;
              Endif;

              If ELsdef <> svSdEf;
                 UAdata = ' ';
                 UAdata = 'Schedule Db/Cr Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELscex <> svScEx;
                 UAdata = ' ';
                 UAdata = 'Schedule Db/Cr Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELdisc <> svDisc;
                 UAdata = ' ';
                 UAdata = 'Discount Type S,G';
                 Exsr wrtfa1_1;
              Endif;

              If ELwvtp <> svWvTp;
                 UAdata = ' ';
                 UAdata = 'Waiver Type';
                 Exsr wrtfa1_1;
              Endif;

              If ELwvfl <> svWvFl;
                 UAdata = ' ';
                 UAdata = 'Waiver Surcharge Premium Level';
                 Exsr wrtfa1_1;
              Endif;

              If ELwvpc <> svWvPC;
                 UAdata = ' ';
                 UAdata = 'Waiver % (Specific=Net%)';
                 Exsr wrtfa1_1;
              Endif;

              If ELwmin <> svWMin;
                 UAdata = ' ';
                 UAdata = 'Minimum Waiver Surcharge';
                 Exsr wrtfa1_1;
              Endif;

              If ELfchg <> svFChg;
                 UAdata = ' ';
                 UAdata = 'Waiver Flat Charge';
                 Exsr wrtfa1_1;
              Endif;

              If ELwmax <> svWMax;
                 UAdata = ' ';
                 UAdata = 'Waiver Max Charge';
                 Exsr wrtfa1_1;
              Endif;

              If ELdcd2 <> svDcD2;
                 UAdata = ' ';
                 UAdata = '2nd Deductible Discount Code';
                 Exsr wrtfa1_1;
              Endif;

              If EL2def <> sv2Def;
                 UAdata = ' ';
                 UAdata = '2nd Deductible Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If EL2dex <> sv2Dex;
                 UAdata = ' ';
                 UAdata = '2nd Deductible Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELinl2 <> svInl2;
                 UAdata = ' ';
                 UAdata = 'Increased Limits 2 Code';
                 Exsr wrtfa1_1;
              Endif;

              If ELi2ef <> svI2Ef;
                 UAdata = ' ';
                 UAdata = 'Increased Limits 2 Effective Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELi2ex <> svI2Ex;
                 UAdata = ' ';
                 UAdata = 'Increased Limits 2 Expiration Date';
                 Exsr wrtfa1_1;
              Endif;

              If ELmfee <> svMFee;
                 UAdata = ' ';
                 UAdata = 'Member Fee Amount';
                 Exsr wrtfa1_1;
              Endif;

              If ELbscd <> svBsCd;
                 UAdata = ' ';
                 UAdata = 'Billing Schedule Code';
                 Exsr wrtfa1_1;
              Endif;

              Reade (ELco#:ELfnd:ELfyr) pel0q;
           Enddo;
        Endif;

        // populate xxst with RSjrst
        // For Divisions
        If FNpeoF = 'M' Or FNpeoF = 'C';
           Setll (ELco#:ELfnd:ELfyr) pel0q;
           Reade (ELco#:ELfnd:ELfyr) pel0q;
           Dow not %eof;
              RSjrst = ' ';
              Chain (E3co#:E3rtcd) prs00;
              If %found;
                 xxSt = RSjrst;
              Endif;

              // check ELilcd
              If ELilcd <> svIlCd;
                 xxKey = ELilcd;
                 Setgt (ELco#:ELfnd:xxSt:xxKey:ELpfdt) plm01;
                 Readpe (ELco#:ELfnd:xxSt:xxKey) plm01;
                 // write record to WXUAL
                 If %eof and ELilcd <> *blanks;
                    UAdata = ' ';
                    UAdata = 'Increased Limits Code';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELdcde
              If ELdcde <> svDcDe;
                 xxDdCd = ELdcde;
                 SetGt (Elco#:ELfnd:xxSt:xxDdCd:ELpfdt) pdd02;
                 Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                 If %eof and ELdcde <> *blanks;
                    UAdata = ' ';
                    UAdata = 'Deductible Code';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELinl2
              If ELinl2 <> svInl2;
                 xxKey = ELinl2;
                 SetGt (Elco#:ELfnd:xxSt:xxKey:ELpfdt) plm01;
                 Readpe (Elco#:ELfnd:xxSt:xxKey) plm01;
                 If %eof and ELinl2 <> *blanks;
                    UAdata = ' ';
                    UAdata = 'Increased Limits 2 Code';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELdcd2
              If ELdcd2 <> svDcD2;
                 xxDdCd = ELdcd2;
                 SetGt (Elco#:ELfnd:xxSt:xxDdCd:ELpfdt) pdd02;
                 Readpe (Elco#:ELfnd:xxSt:xxDdCd) pdd02;
                 If %eof and ELdcd2 <> *blanks;
                    UAdata = ' ';
                    UAdata = '2nd Deductible Discount Code';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELasp2
              If ELasp2 <> svAsp2;
                 SetGt (Elco#:ELfnd:xxSt:ELpfdt) prr00;
                 Readpe (Elco#:ELfnd:xxSt) prr00;
                 If %eof;
                    If ELasp2 <> 0;
                       UAdata = ' ';
                       UAdata = 'Deviation';
                       Exsr wrtfa_1;
                    Endif;
                 Else;
                    If (RRdv1c = *zero and (isCltFlg = *off or
                     isCltFlg = *on and RRst <> 'NM')) or
                     (RRdv1l = ' ' and isCltFlg = *on and RRst = 'NM');
                       If ELasp2 <> 0;
                          UAdata = ' ';
                          UAdata = 'Deviation';
                          Exsr wrtfa_1;
                       Endif;
                    Endif;
                 Endif;
              Endif;

              // check ELd1s2
              If ELd1s2 <> svd1s2;
                 SetGt (Elco#:ELfnd:xxSt:ELpfdt) prr00;
                 Readpe (Elco#:ELfnd:xxSt) prr00;
                 If %eof;
                    If ELd1s2 <> 0;
                       UAdata = ' ';
                       UAdata = 'Deviation 1 Split 2';
                       Exsr wrtfa_1;
                    Endif;
                 Else;
                    If (RRdv1c = *zero and (isCltFlg = *off or
                     isCltFlg = *on and RRst <> 'NM')) or
                     (RRdv1l = ' ' and isCltFlg = *on and RRst = 'NM');
                       If ELd1s2 <> 0;
                          UAdata = ' ';
                          UAdata = 'Deviation 1 Split 2';
                          Exsr wrtfa_1;
                       Endif;
                    Endif;
                 Endif;
              Endif;

              SetGt (Elco#:ELfnd:xxSt:ELpfdt) prr00;
              Readpe (Elco#:ELfnd:xxSt) prr00;
              // check ELdev2
              If %eof;
                 If ELdev2 <> svDev2 and ELdev2 <> 0;
                    UAdata = ' ';
                    UAdata = 'Deviation 2';
                    Exsr wrtfa_1;
                 Endif;
              Else;
                 If ELdev2 <> svDev2 and RRdv2c = *zero and ELdev2 <> 0;
                    UAdata = ' ';
                    UAdata = 'Deviation 2';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELd2s2
              If %eof;
                 If ELd2s2 <> svD2S2 and ELd2s2 <> 0;
                    UAdata = ' ';
                    UAdata = 'Deviation 2 Split 2';
                    Exsr wrtfa_1;
                 Endif;
              Else;
                 If ELd2s2 <> svD2S2 and RRdv2c = *zero and ELd2s2 <> 0;
                    UAdata = ' ';
                    UAdata = 'Deviation 2 Split 2';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check Stabilization factor
              If %eof;
                 If ELstfc <> svStFc and ELstfc <> 0;
                    UAdata = ' ';
                    UAdata = 'Stabilization Factor';
                    Exsr wrtfa_1;
                 Endif;
              Else;
                 If ELstfc <> svStFc and ELstfc <> 0;
                    UAdata = ' ';
                    UAdata = 'Stabilization Factor';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELasp1
              If %eof;
                 If ELasp1 <> svAsp1 and ELasp1 <> 0;
                    UAdata = ' ';
                    UAdata = 'Contractors Adj Credit';
                    Exsr wrtfa_1;
                 Endif;
              Else;
                 If ELasp1 <> svAsp1 and RRCcCd = *zero and ELasp1 <> 0;
                    UAdata = ' ';
                    UAdata = 'Contractors Adj Credit';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELaspm
              If %eof;
                 If ELaspm <> svAspm and ELaspm <> 0;
                    UAdata = ' ';
                    UAdata = 'Subj Premium Credit 1A Percent';
                    Exsr wrtfa_1;
                 Endif;
              Else;
                 If ELaspm <> svAspm and RR1acd = *zero and ELaspm <> 0;
                    UAdata = ' ';
                    UAdata = 'Subj Premium Credit 1A Percent';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELasps
              If %eof;
                 If ELasps <> svAsps and ELasps <> 0;
                    UAdata = ' ';
                    UAdata = 'Subj Premium Credit 1B Percent';
                    Exsr wrtfa_1;
                 Endif;
              Else;
                 If ELasps <> svAsps and RR1bcd = *zero and ELasps <> 0;
                    UAdata = ' ';
                    UAdata = 'Subj Premium Credit 1B Percent';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELdbcr
              If %eof;
                 If ELdbcr <> svDbCr and ELdbcr <> 0;
                    UAdata = ' ';
                    UAdata = 'Schedule Db/Cr Factor';
                    Exsr wrtfa_1;
                 Endif;
              Else;
        //*** begin chg ***  06/24/22  ************************************
        //*****  If ELdbcr <> svDbCr and RRdbcd = *zero and ELdbcr <> 0;
                 If ELdbcr <> svDbCr and ELdbcr <> 0 and ((ELDbcr > 1 and
                  (RRDbcd = 0 or (ELDbcr > RRDmax and RRDmax <> 0))) or
                  (ELDBcr < 1 and (RRCrcd = 0 or (ELDbcr < RRCmax and RRCmax
                  <> 0))));
        //*** end chg *****  06/24/22  ************************************
                    UAdata = ' ';
                    UAdata = 'Schedule Db/Cr Factor';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check Waiver Surcharge
              If %eof;
                 If ELwvtp <> svWvTp and ELwvtp <> *blanks;
                    UAdata = ' ';
                    UAdata = 'Waiver Type';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If %eof;
                 If ELwvfl <> svWvFl and ELwvfl <> *blanks;
                    UAdata = ' ';
                    UAdata = 'Waiver Surcharge Premium Level';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If %eof;
                 If ELwvpc <> svWvPc and ELwvpc <> 0;
                    UAdata = ' ';
                    UAdata = 'Waiver % (Specific=Net%)';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If %eof;
                 If ELwmin <> svWMin and ELwmin <> 0;
                    UAdata = ' ';
                    UAdata = 'Minimum Waiver Surcharge';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If %eof;
                 If ELfchg <> svFChg and ELfchg <> 0;
                    UAdata = ' ';
                    UAdata = 'Waiver Flat Charge';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If %eof;
                 If ELwmax <> svWMax and ELwmax <> 0;
                    UAdata = ' ';
                    UAdata = 'Waiver Max Charge';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If not %eof;
                 If ELwvtp <> svWvTp and RRwvac = 0 and ELwvtp <> *blanks;
                    UAdata = ' ';
                    UAdata = 'Waiver Type';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If not %eof;
                 If ELwvfl <> svWvFl and RRwvac = 0 and ELwvfl <> *blanks;
                    UAdata = ' ';
                    UAdata = 'Waiver Surcharge Premium Level';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If not %eof;
                 If ELwvpc <> svWvpc and RRwvac = 0 and ELwvpc <> 0;
                    UAdata = ' ';
                    UAdata = 'Waiver % (Specific=Net%)';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If not %eof;
                 If ELwmin <> svWMin and RRwvac = 0 and ELwmin <> 0;
                    UAdata = ' ';
                    UAdata = 'Minimum Waiver Surcharge';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If not %eof;
                 If ELfchg <> svFChg and RRwvac = 0 and ELfchg <> 0;
                    UAdata = ' ';
                    UAdata = 'Waiver Flat Charge';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              If not %eof;
                 If ELwmax <> svWMax and RRwvac = 0 and ELwmax <> 0;
                    UAdata = ' ';
                    UAdata = 'Waiver Max Charge';
                    Exsr wrtfa_1;
                 Endif;
              Endif;

              // check ELdisc
              SetGt (ELco#:ELfnd:xxSt:ELdisc:ELpfdt) pds03;
              Readpe (ELco#:ELfnd:xxSt:ELdisc) pds03;
              If ELdisc <> ' ' and %eof;
                 UAdata = ' ';
                 UAdata = 'Discount Type S,G';
                 Exsr wrtfa_1;
              Endif;

              // policy from date cannot be less than initial eff date(all policy)
              If ELpfdt <> svPfDt;
                 Chain (ELco#:E3fnd:E3emp#:E3div) pem00;
                 If %found;
                    If ELpfdt < EMefd1;
                       UAdata = ' ';
                       UAdata = 'Policy Effective Date Errors';
                       Exsr wrtfa_1;
                    Endif;
                 Endif;

                 // policy from date must be less than or equal to all existing
                 // payroll records(for all divisions)
                 Setll (ELco#:E3fnd:E3emp#:E3div:E3fyr) pep00;
                 Reade (ELco#:E3fnd:E3emp#:E3div:E3fyr) pep00;
                 Dow not %eof;
                    If EPefdt < ELpfdt and EPstat <> 'I';
                       UAdata = ' ';
                       UAdata = 'Policy Effective Date Errors';
                       Exsr wrtfa_1;
                       Leave;
                    Endif;
                    Reade (ELco#:E3fnd:E3emp#:E3div:E3fyr) pep00;
                 Enddo;
              Endif;

              // edit policy to date
              // dates must be ascending order
              If ELptdt < ELpfdt;
                 UAdata = ' ';
                 UAdata = 'Policy Expiration Date Errors';
                 Exsr wrtfa_1;
              Endif;

              // Anniv date must be less than or equal to pol from date
              If ELandt <> svAnDt and ELandt > E3pfdt;
                 UAdata = ' ';
                 UAdata = 'Employer Detail Information';
                 Exsr wrtfa_1;
              Endif;

              // billing start date cannot be greater than policy eff date
              If ELbsdt <> svBsDt;
                 If ELbsdt > E3pfdt;
                    UAdata = ' ';
                    UAdata = 'Billing Start Date Errors';
                    Exsr wrtfa_1;
                 Endif;

                 // do not allow billing start date to be prior to current date if
                 // we are changing the value of the field or adding a record.
                 // unless the change is the billing start date to = policy from date
                 sysDt7 = DateTo7(SystemDate);
                 If (addMode = *on and ELpfdt > sysDt7) or (addMode = *off
                  and ELbsdt <> E3bsdt);
                    If ELbsdt < sysDt7 and ELbsdt <> ELpfdt and ELarsm <> 'Y';
                       UAdata = ' ';
                       UAdata = 'Billing Start Date Errors';
                       Exsr wrtfa_1;
                    Endif;
                 Endif;
              Endif;
              qquser = s2user;
              qqdate = s2date;
              qqtime = s2time;
              qqcxfl = scCnxInc;
              Reade (ELco#:ELfnd:ELfyr) pel0q;
           Enddo;
        Endif;

        Endsr;
       //==================================================
       // WRTFA - write records to wxaup with appropriate
       //         information
       //==================================================
        BegSr wrtfa;

        UAco#  = E2co#;
        UAdiv  = E2div;
        UAemp# = E2emp#;
        UAfnd  = E2fnd;
        UAfyr  = E2fyr;
        UAtime = S2time;
        UAuser = S2user;
        UAdt   = S2date;
        UAst   = xxSt;
        //*** Beg Add ***** 05/28/25 **************************
        If UAdata = 'Billing Start Date Errors';
           UAst = ' ';
        Endif;
        //*** End Add ***** 05/28/25 **************************
        UAmode = @@mode;
        Write pua00;

        Exsr dltfa;

        Endsr;
       //=======================================================
       // WRTFA1- write dummy records to wxaup with appropriate
       //         information
       //=======================================================
        BegSr wrtfa1;

        clear UAst;
        UAco#  = E2co#;
        UAdiv  = E2div;
        UAemp# = E2emp#;
        UAfnd  = E2fnd;
        UAfyr  = E2fyr;
        UAtime = S2time;
        UAuser = S2user;
        UAdt   = S2date;
        If dspAllDiv = *on;
           UAmode = 'T';
        Endif;
        Write pua00;

        Endsr;
       //==================================================
       // WRTFA_1 - WRITE RECORDS TO WXUAP WITH APPROPIATE
       //           INFORMATION
       //==================================================
        BegSr wrtfa_1;

        UAco#  = E3co#;
        UAdiv  = E3div;
        UAemp# = E3emp#;
        UAfnd  = E3fnd;
        UAfyr  = E3fyr;
        UAtime = S2time;
        UAuser = S2user;
        UAdt   = S2date;
        UAst   = xxSt;
        //*** Beg Add ***** 05/28/25 **************************
        If UAdata = 'Billing Start Date Errors';
           UAst = ' ';
        Endif;
        //*** End Add ***** 05/28/25 **************************
        UAmode = @@mode;
        Write pua00;

        Exsr dltfa;

        Endsr;
       //========================================================
       // WRTFA1_1- WRITE DUMMY RECORDS TO WXUAP WITH APPROPIATE
       //           INFORMATION
       //========================================================
        BegSr wrtfa1_1;

        clear UAst;
        UAco#  = E3co#;
        UAdiv  = E3div;
        UAemp# = E3emp#;
        UAfnd  = E3fnd;
        UAfyr  = E3fyr;
        UAtime = S2time;
        UAuser = S2user;
        UAdt   = S2date;
        If dspAllDiv = *on;
           UAmode = 'T';
        Endif;
        Write pua00;

        Endsr;
       //==================================================
       // UPKFA - call w0051r2 conditionally
       //==================================================
        BegSr upkfa;

        xxntupd = 0;
        Setll (qqUser:qqDate:qqTime) pua00;
        Reade (qqUser:qqDate:qqTime) pua00;
        If UAst <> ' ';
           intEmp# = UAemp#;
           intDiv  = UAdiv;
        Endif;

        Dow not %eof(Wxual);
           If UAst <> ' ';
              If UAemp# <> intEmp# or UAdiv <> intDiv;
                 xxntupd = xxntupd + 1;
                 intEmp# = UAemp#;
                 intDiv  = UAdiv;
              Endif;
           Endif;
           Reade (qqUser:qqDate:qqTime) pua00;
        Enddo;

        qqName = FNname;
        qqMpo  = FNpeof;
        xxName = EMname;
        EDNInfo(ELemp#:ELdiv:xxName:xxEDN);
        qqEDN = xxEDN;

        If UAst = ' ';
           qqMode = 'U';
        Else;
           qqMode = ' ';
        Endif;

        W0051R2(qqUser:qqDate:qqTime:qqMode:qqCxFl:ELemp#:ELfnd:ELdiv:
         qqName:qqEdn:qqMpo:xx#Div:#TotDivM:xxNtuPd);

        Endsr;
       //==================================================
       // DLTFA - Delete records from wxaup having error
       //==================================================
        BegSr dltfa;

        Chain (UAuser:UAdt:UAtime:UAco#:UAfnd:UAfyr:UAemp#:UAdiv:UAdata:' ')
         pua02;
        If %found;
           delete pua02;
        Endif;
        Unlock WXUAL2;
        //  Delete records associated with a code/percentage as well.

        Select;
        When UAdata = 'Increased Limits Code';

           UAdata = 'Increased Limits Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Increased Limits Expiration Date';
           Exsr dltfa_associated;
           UAdata = 'Increased Limits 2 Code';
           Exsr dltfa_associated;
           UAdata = 'Increased Limits 2 Eff Date';
           Exsr dltfa_associated;
           UAdata = 'Increased Limits 2 Exp Date';
           Exsr dltfa_associated;

        When UAdata = 'Deductible Code';

           UAdata = 'Deductible Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Deductible Expiration Date';
           Exsr dltfa_associated;
           UAdata = '2nd Deductible Discount Code';
           Exsr dltfa_associated;
           UAdata = '2nd Deductible Effective Date';
           Exsr dltfa_associated;
           UAdata = '2nd Deductible Expiration Date';
           Exsr dltfa_associated;
           UAdata = 'Deductible Bill If Cancelled';
           Exsr dltfa_associated;

        When UAdata = 'Waiver Type';

           UAdata = 'Waiver Surcharge Prem Lvl';
           Exsr dltfa_associated;
           UAdata = 'Waiver % (Specific=Net%)';
           Exsr dltfa_associated;
           UAdata = 'Minimum Waiver Surcharge';
           Exsr dltfa_associated;
           UAdata = 'Waiver Flat Charge';
           Exsr dltfa_associated;
           UAdata = 'Waiver Maxium Charge';
           Exsr dltfa_associated;

        When UAdata = 'Deviation';

           UAdata = 'Deviation Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Deviation Expiration Date';
           Exsr dltfa_associated;

        When UAdata = 'Contractors Adj Credit';

           UAdata = 'Contractors Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Contractors Expiration Date';
           Exsr dltfa_associated;

        When UAdata = 'Subj Premium Credit 1A Percent';

           UAdata = 'Subj Premium 1A Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Subj Premium 1A Expiration Date';
           Exsr dltfa_associated;

        When UAdata = 'Deviation 2';

           UAdata = 'Deviation 2 Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Deviation 2 Expiration Date';
           Exsr dltfa_associated;

        When UAdata = 'Subj Premium Credit 1B Percent';

           UAdata = 'Subj Premium 1B Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Subj Premium 1B Expiration Date';
           Exsr dltfa_associated;

        When UAdata = 'Schedule Db/Cr Factor';

           UAdata = 'Schedule Db/Cr Effective Date';
           Exsr dltfa_associated;
           UAdata = 'Schedule Db/Cr Expiration Date';
           Exsr dltfa_associated;

        Endsl;

        Endsr;

       //======================================================================
       //  Delete records associated with the code or percentage as well as the
       //  update happens for the entire group of records at once.
       //======================================================================
        BegSr dltfa_associated;

        Chain (UAuser:UAdt:UAtime:UAco#:UAfnd:UAfyr:UAemp#:UAdiv:UAdata:' ')
         pua02;
        If %found;
           delete pua02;
        Endif;
        Unlock WXUAL2;

        Endsr;
      //===============================================================
      // Update all divisions
      //===============================================================
       BEGSR UPDALL;
         //
      /END-FREE
     C     *LIKE         DEFINE    *IN60         SVIN60
     C     *LIKE         DEFINE    *IN60         SVIN61
     C     *LIKE         DEFINE    ELUSER        SVUSER
     C     *LIKE         DEFINE    ELPGM         SVPGM
     C     *LIKE         DEFINE    ELDT          SVDT
     C     *LIKE         DEFINE    ELTIME        SVTIME
      //
     C                   MOVEL     ELRLFL        SVRLFL
     C                   MOVEL     ELUSER        SVUSER
     C                   MOVEL     ELPGM         SVPGM
     C                   Z-ADD     ELDT          SVDT
     C     1             ADD       ELTIME        SVTIME
      //
     C                   MOVE      *IN60         SVIN60
     C                   MOVE      *IN61         SVIN61
      //
      /FREE
         SETLL key1WMEML PEM00;
         *IN60 = NOT %FOUND;
         //
         DOU *IN60 = *ON;
           //
           READE key1WMEML PEM00;
           *IN60 = %EOF;
           //
           IF *IN60 = *ON;
             LEAVE;
           ENDIF;
           //
           CHAIN(N) key2WDELL PEL00;
           *IN61 = NOT %FOUND;
           //
           IF *IN61 = *OFF
           AND ELDIV <> scDIV;
             //
             CHAIN key2WDELL PEL00;
             *IN61 = NOT %FOUND;
             //
      /END-FREE
     C                   MOVEL     SVRLFL        ELRLFL
     C                   MOVEL     SVUSER        ELUSER
     C                   MOVEL     SVPGM         ELPGM
     C                   Z-ADD     SVDT          ELDT
     C                   Z-ADD     SVTIME        ELTIME
      //
      /FREE
             UPDATE PEL00;
             //
      /END-FREE
     C                   ADD       1             SVTIME
      //
      /FREE
           ENDIF;
           //
         ENDDO;
         //
      /END-FREE
     C                   MOVE      *OFF          xxUPDA
      //
     C                   MOVE      SVIN60        *IN60
     C                   MOVE      SVIN61        *IN61
      //
      /FREE
       ENDSR;                                                                 //UPDALL
       //====================================================
       // SRUPDB - UPDATE BSCD'S IN ALL SUMMARIZED DIVISIONS
       //====================================================
       BEGSR SRUPDB;
         //
         SETLL key1WDELL1 PEL01;
         READE key1WDELL1 PEL01;
         *IN60 = %EOF;
         //
         DOW *IN60 = *OFF;
           //
           IF ELARSM = 'Y';
      /END-FREE
     C                   MOVE      scBSCD        ELBSCD
      /FREE
             UPDATE PEL01;
           ENDIF;
           //
           READE key1WDELL1 PEL01;
           *IN60 = %EOF;
         ENDDO;
         //
       ENDSR;
       //====================================================
       // SRUPDC - UPDATE SPREAD FLAG IN ALL DIVISIONS
       //====================================================
       BEGSR SRUPDC;
         //
         SETLL key1WDELL1 PEL01;
         READE key1WDELL1 PEL01;
         *IN60 = %EOF;
         //
         DOW *IN60 = *OFF;
           //
      /END-FREE
     C                   MOVE      scFFl1        ELFFL1
      /FREE
           UPDATE PEL01;
           //
           READE key1WDELL1 PEL01;
           *IN60 = %EOF;
         ENDDO;
         //
       ENDSR;
       //===============================================================
       // SRUPDT - SUBROUTINE TO UPDATE ALL DIVISIONS WITH NEW INCREASED
       //          LIMITS OR DEDUCTIBLE DISCOUNT CODE
       //===============================================================
       //
       BEGSR SRUPDT;
         //
         CHAIN key1WDELL1 PEL01;
         *IN60 = NOT %FOUND;
         DOW *IN60 = *OFF;
      /END-FREE
     C                   MOVEL(P)  scILCD        ELILCD
     C                   MOVEL(P)  scINL2        ELINL2
     C                   MOVEL(P)  scDCDE        ELDCDE
     C                   MOVEL(P)  scDCD2        ELDCD2
      //
      /FREE
           IF scILCD <> *BLANKS;
             //
             IF scIL1EfDt <> *ZERO;
      /END-FREE
     C                   Z-ADD     scIL1EfDT     x8EFDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EFDT            7 0
      /FREE
             ENDIF;
             //
             IF scIL1ExDt <> *ZERO;
      /END-FREE
     C                   Z-ADD     scIL1ExDt     x8EXDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EXDT            7 0
      /FREE
             ENDIF;
             //
             SELECT;
             WHEN x7EFDT > ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   Z-ADD     x7EFDT        ELILEF
     C                   Z-ADD     0             ELILEX
      /FREE
             WHEN x7EFDT > ELPFDT
             AND x7EXDT <> *ZERO;
      /END-FREE
     C                   Z-ADD     x7EFDT        ELILEF
     C                   Z-ADD     x7EXDT        ELILEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     0             ELILEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT < ELPFDT;
      /END-FREE
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
     C                   MOVE      *BLANKS       ELILCD
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT > ELPFDT;
      /END-FREE
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     x7EXDT        ELILEX
      /FREE
             ENDSL;
             //
           ELSE;
      /END-FREE
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
      //
      /FREE
           ENDIF;                                                             //SCILCD <> *BLANKS
           //
           IF scINL2 <> *BLANKS;
             //
             IF scIL2EfDt <> *ZERO;
      /END-FREE
     C                   Z-ADD     scIL2EfDt     x8EFDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EFDT            7 0
      /FREE
             ENDIF;
             //
             IF scIL2ExDt <> *ZERO;
      /END-FREE
     C                   Z-ADD     scIL2ExDt     x8EXDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EXDT            7 0
      /FREE
             ENDIF;
             //
             SELECT;
             WHEN x7EFDT > ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   Z-ADD     x7EFDT        ELI2EF
     C                   Z-ADD     0             ELI2EX
      /FREE
             WHEN x7EFDT > ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT > ELPFDT;
      /END-FREE
     C                   Z-ADD     x7EFDT        ELI2EF
     C                   Z-ADD     x7EXDT        ELI2EX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   MOVE      *BLANKS       ELINL2
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      scINL2        ELILCD
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     0             ELILEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT < ELPFDT;
      /END-FREE
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
     C                   MOVE      *BLANKS       ELILCD
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      *BLANKS       ELINL2
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT > ELPFDT;
      /END-FREE
     C                   MOVE      scINL2        ELILCD
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     x7EXDT        ELILEX
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      *BLANKS       ELINL2
      /FREE
             ENDSL;
             //
           ELSE;
      /END-FREE
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
      //
      /FREE
           ENDIF;                                                             //SCINL2 <> *BLANKS
           //
           IF scDCDE <> *BLANKS;
             //
             IF scDD1EfDT <> *ZERO;
      /END-FREE
     C                   Z-ADD     scDD1EfDt     x8EFDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EFDT            7 0
      /FREE
             ENDIF;
             //
             IF scDD1ExDt <> *ZERO;
      /END-FREE
     C                   Z-ADD     scDD1ExDt     x8EXDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EXDT            7 0
      /FREE
             ENDIF;
             //
             SELECT;
             WHEN x7EFDT > ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   Z-ADD     x7EFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
      /FREE
             WHEN x7EFDT > ELPFDT
             AND x7EXDT <> *ZERO;
      /END-FREE
     C                   Z-ADD     x7EFDT        ELDCEF
     C                   Z-ADD     x7EXDT        ELDCEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT < ELPFDT;
      /END-FREE
     C                   MOVE      *BLANKS       ELDCDE
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT > ELPFDT;
      /END-FREE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     x7EXDT        ELDCEX
      /FREE
             ENDSL;
             //
           ELSE;
      /END-FREE
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
      //
      /FREE
           ENDIF;                                                             //SCDCDE <> *BLANKS
           //
           IF scDCD2 <> *BLANKS;
             //
             IF scDD2EfDt <> *ZERO;
      /END-FREE
     C                   Z-ADD     scDD2EfDt     x8EFDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EFDT            7 0
      /FREE
             ENDIF;
             //
             IF scDD2ExDt <> *ZERO;
      /END-FREE
     C                   Z-ADD     scDD2ExDt     x8EXDT            8 0
      /FREE
               EXSR SRDCVT;
             ELSE;
      /END-FREE
     C                   MOVE      *ZERO         x7EXDT            7 0
      /FREE
             ENDIF;
             //
             SELECT;
             WHEN x7EFDT > ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   Z-ADD     x7EFDT        EL2DEF
     C                   Z-ADD     0             EL2DEX
      /FREE
             WHEN x7EFDT > ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT > ELPFDT;
      /END-FREE
     C                   Z-ADD     x7EFDT        EL2DEF
     C                   Z-ADD     x7EXDT        EL2DEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT = *ZERO;
      /END-FREE
     C                   MOVE      *BLANKS       ELDCD2
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      SCINL2        ELDCDE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT < ELPFDT;
      /END-FREE
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
     C                   MOVE      *BLANKS       ELDCDE
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      *BLANKS       ELDCD2
      /FREE
             WHEN x7EFDT <= ELPFDT
             AND x7EXDT <> *ZERO
             AND x7EXDT > ELPFDT;
      /END-FREE
     C                   MOVE      scDCD2        ELDCDE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     x7EXDT        ELDCEX
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      *BLANKS       ELDCD2
      /FREE
             ENDSL;
             //
           ELSE;
      /END-FREE
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
      //
      /FREE
           ENDIF;                                                             //SCDCD2 <> *BLANKS
           //
      /END-FREE
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
      /FREE
           ELTIME = Timeto6(SystemTime());
           ELDT   = Dateto7(SystemDate());
           //
           UPDATE PEL01;
           //
           READE key1WDELL1 PEL01;
           *IN60 = %EOF;
         ENDDO;
         //
       ENDSR;
       //===============================================================
       // SRDCVT - DATE CONVERSION SUBROUTINE FOR SRUPDT
       //===============================================================
       BEGSR SRDCVT;
         //
      /END-FREE
     C                   Z-ADD     x8EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         x7EFDT
      //
     C                   Z-ADD     x8EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         x7EXDT
      //
      /FREE
       ENDSR;
       //===============================================================
       //*** Beg Add *** 03/04/24 **************************************
       Begsr SRupdNcciAllDiv;
         Setll key1WDELL1 pel01;
         Reade key1WDELL1 pel01;
         Dow Not %EOF(WDELL1);
            ELncci = scNcci;
            ELuser = Q1user;
            ELpgm  = Q1pgm;
            ELdt   = DateTo7(SystemDate);
            ELtime = Timeto6(SystemTime);
            Update pel01;
            Reade key1WDELL1 pel01;
         Enddo;
       Endsr;
       //*** End Add *** 03/04/24 **************************************
       //*** Beg Add *** 06/24/24 **************************************
       Begsr SrUpdCollAllDiv;
         Setll key1WDELL1 pel01;
         Reade key1WDELL1 pel01;
         Dow Not %EOF(WDELL1);
            ELColwp =  scColwp;
            ELColwa =  scColwa;
            ELColrp =  scColrp;
            ELColTyp = scColTyp;
            ELColad =  scColad;
            ELCollib = scCollib;
            ELColln =  scColln;
            ELCollid = dtCYMD(scCollid);
            ELColled = dtCYMD(scColled);
            ELColsic = scColsic;
            ELColsn =  scColsn;
            ELColsid = dtCYMD(scColsid);
            ELColsed = dtCYMD(scColsed);
            ELuser = Q1user;
            ELpgm  = Q1pgm;
            ELdt   = DateTo7(SystemDate);
            ELtime = Timeto6(SystemTime);
            Update pel01;
            Reade key1WDELL1 pel01;
         Enddo;
       Endsr;
       //*** End Add *** 06/24/24 **************************************

     P SavePolicyDet   E

       //***************************************************************************//
       //===================================================================
       // Clear all Errors
       //===================================================================
     P ClearErrors     B
     D ClearErrors     PI
      /free

       error    = *off;
       erPFDT   = *off;
       msPFDT   = *blanks;
       erPTDT   = *off;
       msPTDT   = *blanks;
       erANDT   = *off;
       msANDT   = *blanks;
       erRTCD   = *off;
       msRTCD   = *blanks;
       erRTCD2  = *off;
       mgRTCD   = *blanks;
       erSTAT   = *off;
       msSTAT   = *blanks;
       er25002  = *off;
       ms25002  = *blanks;
       erBLFL   = *off;
       msBLFL   = *blanks;
       erCOFL   = *off;
       msCOFL   = *blanks;
       erMANP   = *off;
       msMANP   = *blanks;
       erMANP2  = *off;
       msMANP2  = *blanks;
       erFFL92  = *off;
       msFFL92  = *blanks;
       erBSCD   = *off;
       msBSCD   = *blanks;
       erBSDT   = *off;
       msBSDT   = *blanks;
       erEMPY   = *off;
       msEMPY   = *blanks;
       erQTYP   = *off;
       msQTYP   = *blanks;
       erSRPR2  = *off;
       msSRPR2  = *blanks;
       erTYPB   = *off;
       msTYPB   = *blanks;
       erFFL1   = *off;
       msFFL1   = *blanks;
       erIFFL   = *off;
       msIFFL   = *blanks;
       erARSM   = *off;
       msARSM   = *blanks;
       erASCD   = *off;
       msASCD   = *blanks;
       erPFIN   = *off;
       msPFIN   = *blanks;

       erILCD   = *off;
       msILCD   = *blanks;
       erIL1EfDt = *off;
       msIL1EfDt = *blanks;
       erIL1ExDt = *off;
       msIL1ExDt = *blanks;
       erINL2   = *off;
       msINL2   = *blanks;
       erIL2EfDt = *off;
       msIL2EfDt = *blanks;
       erIL2ExDt = *off;
       msIL2ExDt = *blanks;
       erDCDE   = *off;
       msDCDE   = *blanks;
       tipDedDis1 = *blanks;
       tipDedDis2 = *blanks;
       erDD1EfDt = *off;
       msDD1EfDt = *blanks;
       erDD1ExDt = *off;
       msDD1ExDt = *blanks;
       erCXBL   = *off;
       msCXBL   = *blanks;
       erDCD2   = *off;
       msDCD2   = *blanks;
       erDD2EfDt = *off;
       msDD2EfDt = *blanks;
       erDD2ExDt = *off;
       msDD2ExDt = *blanks;
       erASP2   = *off;
       msASP2   = *blanks;
       erDV1EfDt = *off;
       msDV1EfDt = *blanks;
       erDV1ExDt = *off;
       msDV1ExDt = *blanks;
       erD1S2   = *off;
       msD1S2   = *blanks;
       erD1D2EfDt = *off;
       msD1D2EfDt = *blanks;
       erD1D2ExDt = *off;
       msD1D2ExDt = *blanks;
       erDEV2   = *off;
       msDEV2   = *blanks;
       erDV2EfDt = *off;
       msDV2EfDt = *blanks;
       erDV2ExDt = *off;
       msDV2ExDt = *blanks;
       erD2S2   = *off;
       msD2S2   = *blanks;
       erD2D2EfDt = *off;
       msD2D2EfDt = *blanks;
       erD2D2ExDt = *off;
       msD2D2ExDt = *blanks;
       erASPS   = *off;
       msASPS   = *blanks;
       erSCEfDt = *off;
       msSCEfDt = *blanks;
       erSCExDt = *off;
       msSCExDt = *blanks;
       erASPM   = *off;
       msASPM   = *blanks;
       erMCEfDt = *off;
       msMCEfDt = *blanks;
       erMCExDt = *off;
       msMCExDt = *blanks;
       erMCCD   = *off;
       msMCCD   = *blanks;
       erMCGP   = *off;
       msMCGP   = *blanks;
       erASP1   = *off;
       msASP1   = *blanks;
       erCCEfDt = *off;
       msCCEfDt = *blanks;
       erCCExDt = *off;
       msCCExDt = *blanks;
       erDBCR   = *off;
       msDBCR   = *blanks;
       erSDEfDt = *off;
       msSDEfDt = *blanks;
       erSDExDt = *off;
       msSDExDt = *blanks;
       erASP3   = *off;
       msASP3   = *blanks;
       erDISC   = *off;
       msDISC   = *blanks;
       erPROM   = *off;
       msPROM   = *blanks;
       erEXM1   = *off;
       msEXM1   = *blanks;
       erEXM2   = *off;
       msEXM2   = *blanks;
       erEXM3   = *off;
       msEXM3   = *blanks;
       erEM1Date = *off;
       msEM1Date = *blanks;
       erEM2Date = *off;
       msEM2Date = *blanks;
       erEM3Date = *off;
       msEM3Date = *blanks;
       erARP1   = *off;
       msARP1   = *blanks;
       erARP2   = *off;
       msARP2   = *blanks;
       erARP3   = *off;
       msARP3   = *blanks;
       erExMd1Nf = *off;
       msExMd1Nf = *blanks;
       erExMd2Nf = *off;
       msExMd2Nf = *blanks;
       erExMd3Nf = *off;
       msExMd3Nf = *blanks;
       erSFPC   = *off;
       msSFPC   = *blanks;
       erSFMN   = *off;
       msSFMN   = *blanks;
       erMFEE   = *off;
       msMFEE   = *blanks;
       erWVTP   = *off;
       msWVTP   = *blanks;
       erWVFL   = *off;
       msWVFL   = *blanks;
       //*** Begin Add *** 02/02/21 *********************************
       erOvrWd  = *Off;
       msOvrWd  = *Blanks;
       erBlnkWd = *Off;
       msBlnkWd = *Blanks;
       erNoWvrDsc = *Off;
       msNoWvrDsc = *Blanks;
       //*** End   Add *** 02/02/21 *********************************
       erWVPC   = *off;
       msWVPC   = *blanks;
       erWMIN   = *off;
       msWMIN   = *blanks;
       erWMAX   = *off;
       msWMAX   = *blanks;
       erFCHG   = *off;
       msFCHG   = *blanks;
       erARCP   = *off;
       msARCP   = *blanks;
       erARCA   = *off;
       msARCA   = *blanks;
       erAREfDt = *off;
       msAREfDt = *blanks;
       erARExDt = *off;
       msARExDt = *blanks;
       erVLCA   = *off;
       msVLCA   = *blanks;

       erRPCARR = *off;
       msRPCARR = *blanks;
       erRPCAP# = *off;
       msRPCAP# = *blanks;
       erRPISDT = *off;
       msRPISDT = *blanks;
       erRPEFDT = *off;
       msRPEFDT = *blanks;
       erRPEXDT = *off;
       msRPEXDT = *blanks;
       erRPCLT# = *off;
       msRPCLT# = *blanks;
       erRPTPA# = *off;
       msRPTPA# = *blanks;
       erRPBRCH = *off;
       msRPBRCH = *blanks;
       erRPCNTR = *off;
       msRPCNTR = *blanks;
       erFFLA   = *off;
       msFFLA   = *blanks;
       erPP3C   = *off;
       msPP3C   = *blanks;
       erPPC3   = *off;
       msPPC3   = *blanks;
       erCXDY   = *off;
       msCXDY   = *blanks;
       erADDM   = *off;
       msADDM   = *blanks;
       erADVR   = *off;
       msADVR   = *blanks;
       erRLFL   = *off;
       msRLFL   = *blanks;
       erEXMF   = *off;
       msEXMF   = *blanks;
       erCTST   = *off;
       msCTST   = *blanks;
       erAPFL   = *off;
       msAPFL   = *blanks;
       erAAFL   = *off;
       msAAFL   = *blanks;
       erFFD1   = *off;
       msFFD1   = *blanks;
       erFFD2   = *off;
       msFFD2   = *blanks;
       erFFD3   = *off;
       msFFD3   = *blanks;
       erFFD4   = *off;
       msFFD4   = *blanks;
       erFFD5   = *off;
       msFFD5   = *blanks;
       erFFDD1  = *off;
       msFFDD1  = *blanks;
       erFFDD2  = *off;
       msFFDD2  = *blanks;
       erFFDD3  = *off;
       msFFDD3  = *blanks;
       erFFDD4  = *off;
       msFFDD4  = *blanks;
       erFFDD5  = *off;
       msFFDD5  = *blanks;

       erMNFC   = *off;
       msMNFC   = *blanks;
       erMXFC   = *off;
       msMXFC   = *blanks;
       erBSFC   = *off;
       msBSFC   = *blanks;
       erTAX    = *off;
       msTAX    = *blanks;
       erTAXF   = *off;
       msTAXF   = *blanks;
       erLCVF   = *off;
       msLCVF   = *blanks;
       erRETN   = *off;
       msRETN   = *blanks;
       erELP    = *off;
       msELP    = *blanks;
       erELPF   = *off;
       msELPF   = *blanks;
       erRDF1   = *off;
       msRDF1   = *blanks;
       erRDF2   = *off;
       msRDF2   = *blanks;
       erRDF3   = *off;
       msRDF3   = *blanks;
       erFSET   = *off;
       msFSET   = *blanks;

       erLDEFD1  = *off;
       msLDEFD1  = *blanks;
       erLDEXP1  = *off;
       msLDEXP1  = *blanks;
       erLDRNW1 = *off;
       msLDRNW1 = *blanks;
       erLDDES1 = *off;
       msLDDES1 = *blanks;
       erLDPCT1 = *off;
       msLDPCT1 = *blanks;
       erLDTYP1 = *off;
       msLDTYP1 = *blanks;
       erLDAMP1 = *off;
       msLDAMP1 = *blanks;
       erLDAMG1 = *off;
       msLDAMG1 = *blanks;
       erLDPLN1 = *off;
       msLDPLN1 = *blanks;
       erLDVRB1 = *off;
       msLDVRB1 = *blanks;
       erLDCLS1 = *off;
       msLDCLS1 = *blanks;
       erLDOCV1 = *off;
       msLDOCV1 = *blanks;
       erLDTOT1 = *off;
       msLDTOT1 = *blanks;
       erLDALA1 = *off;
       msLDALA1 = *blanks;
       erLDPLV1 = *off;
       msLDPLV1 = *blanks;
       erLDR101 = *off;
       msLDR101 = *blanks;
       erLDR102 = *off;
       msLDR102 = *blanks;
       erLDR103 = *off;
       msLDR103 = *blanks;
       erLDR104 = *off;
       msLDR104 = *blanks;
       erLDR105 = *off;
       msLDR105 = *blanks;
       erLDR106 = *off;
       msLDR106 = *blanks;
       erLDR107 = *off;
       msLDR107 = *blanks;
       erLDR108 = *off;
       msLDR108 = *blanks;
       erLDR109 = *off;
       msLDR109 = *blanks;
       erLDR110 = *off;
       msLDR110 = *blanks;
       erADP1   = *off;
       msADP1   = *blanks;
       erLDEFD2  = *off;
       msLDEFD2  = *blanks;
       erLDEXP2  = *off;
       msLDEXP2  = *blanks;
       erLDDES2 = *off;
       msLDDES2 = *blanks;
       erLDPCT2 = *off;
       msLDPCT2 = *blanks;
       erLDTYP2 = *off;
       msLDTYP2 = *blanks;
       erLDAMP2 = *off;
       msLDAMP2 = *blanks;
       erLDAMG2 = *off;
       msLDAMG2 = *blanks;
       erLDPLN2 = *off;
       msLDPLN2 = *blanks;
       erLDVRB2 = *off;
       msLDVRB2 = *blanks;
       erLDCLS2 = *off;
       msLDCLS2 = *blanks;
       erLDOCV2 = *off;
       msLDOCV2 = *blanks;
       erLDTOT2 = *off;
       msLDTOT2 = *blanks;
       erLDALA2 = *off;
       msLDALA2 = *blanks;
       erLDPLV2 = *off;
       msLDPLV2 = *blanks;
       erLDR201 = *off;
       msLDR201 = *blanks;
       erLDR202 = *off;
       msLDR202 = *blanks;
       erLDR203 = *off;
       msLDR203 = *blanks;
       erLDR204 = *off;
       msLDR204 = *blanks;
       erLDR205 = *off;
       msLDR205 = *blanks;
       erLDR206 = *off;
       msLDR206 = *blanks;
       erLDR207 = *off;
       msLDR207 = *blanks;
       erLDR208 = *off;
       msLDR208 = *blanks;
       erLDR209 = *off;
       msLDR209 = *blanks;
       erLDR210 = *off;
       msLDR210 = *blanks;
       erADP2   = *off;
       msADP2   = *blanks;

       erRFPRCA = *off;
       msRFPRCA = *blanks;
       erRFPRDA = *off;
       msRFPRDA = *blanks;
       erRFPRRS = *off;
       msRFPRRS = *blanks;
       erRFCPCA = *off;
       msRFCPCA = *blanks;
       erRFCPDA = *off;
       msRFCPDA = *blanks;
       erRFCPRS = *off;
       msRFCPRS = *blanks;
       erRFMFCA = *off;
       msRFMFCA = *blanks;
       erRFMFDA = *off;
       msRFMFDA = *blanks;
       erRFMFRS = *off;
       msRFMFRS = *blanks;
       erRFSDCA = *off;
       msRFSDCA = *blanks;
       erRFSDDA = *off;
       msRFSDDA = *blanks;
       erRFSDRS = *off;
       msRFSDRS = *blanks;
       erRFEMCA = *off;
       msRFEMCA = *blanks;
       erRFEMDA = *off;
       msRFEMDA = *blanks;
       erRFEMRS = *off;
       msRFEMRS = *blanks;
       erRFMCCA = *off;
       msRFMCCA = *blanks;
       erRFMCDA = *off;
       msRFMCDA = *blanks;
       erRFMCRS = *off;
       msRFMCRS = *blanks;
       erRFMSCA = *off;
       msRFMSCA = *blanks;
       erRFMSDA = *off;
       msRFMSDA = *blanks;
       erRFMSRS = *off;
       msRFMSRS = *blanks;
       erRFLSCF = *off;
       msRFLSCF = *blanks;
       erRFLSDT = *off;
       msRFLSDT = *blanks;
       erRFPEFL = *off;
       msRFPEFL = *blanks;
       erRFPEEM = *off;
       msRFPEEM = *blanks;
       erRFCOM1 = *off;
       msRFCOM1 = *blanks;
       erRFCOM2 = *off;
       msRFCOM2 = *blanks;
       erRFCOM3 = *off;
       msRFCOM3 = *blanks;
       msStFc = *blanks;
       erStFc = *off;
       msDev2F = *blanks;
       erDev2F = *off;
       //*** Beg Add *** 03/04/24 ***************************************
       erExpRat    = *Off;
       msExpRat    = *blanks;
       //*** End Add *** 03/04/24 ***************************************
       //*** Beg Add *** 06/24/24 ***************************************
       //* Collateral Infomation indicators
       errColwp = *Off;
       msColwp = *blanks;
       errColwa = *Off;
       msColwa = *blanks;
       errColrp = *Off;
       msColrp = *blanks;
       errCollid = *Off;
       msCollid = *blanks;
       errColled = *Off;
       msColled = *blanks;
       errColsid = *Off;
       msColsid = *blanks;
       errColsed = *Off;
       msColsed = *blanks;
       //*** End Add *** 06/24/24 ***************************************

      /end-free
     P ClearErrors     E

     //================================================================
     // Clear associated info
     //================================================================
     PAcceptAgg        b
     DAcceptAgg        pi             1n
     D inEffDt                             like(scLDEfD1) const
     D inAgg                               like(LDAMG1)   const
      //
     D effDt7          s                   like(LDEFD1)
     D compareAgg      s                   like(LDAMG1)
     D accepted        s              1n   inz('0')

     C     keyWMLDL2     klist
     C                   kfld                    scCO#
     C                   kfld                    scFND
     C                   kfld                    scFYR
     C                   kfld                    scEMP#

     C                   z-add     inEffDt       srDtI
     C                   call      'SRCVTDAT'    DtCYMDp
     C                   movel     srDtO         effDt7

      /FREE
       Setll keyWMLDL2 wmldl2;
       //
       Dou %eof;
         Reade keyWMLDL2 wmldl2;
         //
         If not %eof;
           //
           If LDDIV = ELDIV;
             Iter;
           Endif;
           Exsr GetAggregate;
           //
           If inAgg <> compareAgg;
             scLAAamt1 = inAgg;
             scLAAamt2 = compareAgg;
             scLAAdiv2 = LDDIV;
             //
             Dou laaCancel = *on or laaConfirm = *on;
               EXFMT rcdAggAmt;
             ENDDO;
             //
             IF laaConfirm = *ON;
               accepted = *on;
             ENDIF;
             //
             Leave;
           Endif;
           //
         Endif;
         //
       Enddo;

       If %eof;
         accepted = *on;
       Endif;

       Return accepted;
      //================================================================
      // Determine comparison aggregate
      //================================================================
       begsr getAggregate;

         select;
         when effDt7 >= LDEFD1
               and (effDt7 <= LDEXP1 or LDEXP1 = *zero);
           compareAgg = LDAMG1;
         when effDt7 >= LDEFD2
               and (effDt7 <= LDEXP2 or LDEXP2 = *zero);
           compareAgg = LDAMG2;
         other;
           clear compareAgg;
         endsl;

       endsr;

      /END-FREE
     PAcceptAgg        e

      //====================================================================
     // Write DARTS remark
      //====================================================================
     PwriteDarts       b

     DwriteDarts       pi
     D inDkey                        40
     D inComm                       256
     D inRmcd                        10

      //
     D q#apid          s              2
     D q#cdat          s              8
     D q#code          s              2
     D q#comm          s            256
     D q#com2          s             19
     D q#csid          s              2
     D q#dcid          s             12
     D q#dctp          s              5
     D q#difl          s              1
     D q#fl01          s              1
     D q#fl02          s              1
     D q#fold          s             80
     D q#idoc          s             12
     D q#imgf          s              1
     D q#ipgm          s             10
     D q#ipth          s             10
     D q#ique          s             10
     D q#key           s             40
     D q#pgm           s             10
     D q#pidx          s              5
     D q#pnam          s             10
     D q#qpty          s              1
     D q#rmcd          s              5
     D q#sbyl          s              1
     D q#secl          s              3
     D q#seq#          s              5
     D q#user          s             10
     //
      /FREE
       q#Key  = inDKey;
       q#Comm = inComm;
       q#RmCd = inRmcd;
       //
      /END-FREE
     C                   movel     sysDt8        q#cdat
      //
      /FREE
       q#PNam = 'W0050R';
       q#User = q1User;
       q#Pgm  = q1pgm;
       q#IPgm = q1pgm;
       q#Seq# = '00000';
       q#Sbyl = 'N';
       q#ImgF = ' ';
       //
       q#Fold = *blanks;
       q#DcId = *blanks;
       q#DiFl = ' ';
       //
       clear q#com2;
       clear q#secl;
       clear q#fl01;
       clear q#fl02;
       clear q#apid;
       clear q#idoc;
       clear q#ique;
       clear q#ipth;
       clear q#pidx;
       clear q#qpty;
       clear q#dctp;
       clear q#csid;
       clear q#code;
      //
      /END-FREE
     C                   call      'DA700R'
     C                   parm                    q#key
     C                   parm                    q#comm
     C                   parm                    q#cdat
     C                   parm                    q#dcid
     C                   parm                    q#pnam
     C                   parm                    q#secl
     C                   parm                    q#user
     C                   parm                    q#pgm
     C                   parm                    q#fold
     C                   parm                    q#difl
     C                   parm                    q#ipgm
     C                   parm                    q#fl01
     C                   parm                    q#fl02
     C                   parm                    q#rmcd
     C                   parm                    q#seq#
     C                   parm                    q#sbyl
     C                   parm                    q#com2
     C                   parm                    q#apid
     C                   parm                    q#idoc
     C                   parm                    q#ique
     C                   parm                    q#imgf
     C                   parm                    q#ipth
     C                   parm                    q#pidx
     C                   parm                    q#qpty
     C                   parm                    q#dctp
     C                   parm                    q#csid
     C                   parm                    q#code

     PwriteDarts       e

     //============================================================================================
      // Get manual premium for the record
     //============================================================================================
     p getManPrem      b
     d getManPrem      pi            11  0
     d outNetRated                     n

     d manPrem         s             11  0
     d payRate         s                   like(PRRT)
     d lossCostMult    s              5  4
     d harborExpRate   s                   like(HELHNF)
     d netRated        s               n   Inz(*Off)

     c     prky          klist
     c                   kfld                    scco#
     c                   kfld                    ELRTCD
     c                   kfld                    SRCLAS

     c     prky1         klist
     c                   kfld                    scCO#
     c                   kfld                    ELRTCD
     c                   kfld                    SRCLAS
     c                   kfld                    xxefdt

      /free
       //***  Begin Chg  ***  01/19/22  ********************
       //If EPeNtR <> 0;
       If IsUseNetRate(EPco#:EPfnd:EPemp#:EPdiv:EPfyr)
        and EPeNtR <> 0;
       //***  End   Chg  ***  01/19/22  ********************
          netRated = *On;
       EndIf;
       outNetRated = netRated;

       Chain (SRco#:SRfnd:SRfyr:SRemp#:SRdiv:SRclas:SRefdt:SRspec) pep09;
       If %Found;
          xxEfDt = SrEffd(scCO#:scFND:scFYR:scEMP#:scDIV:SRefdt:xxType:
           SRclas);
          Setgt  prKy1 ppr01;
          Readpe prKy  ppr01;
          If %Eof;
             Return 0;
          Endif;
          Select;
          When PRpcf = 'P';
             Eval(h) terrPayroll += SRamtp;
          //*** begin chg ***  03/01/22  *************************************
          //*****   If netRated;
             If netRated and clientID = '136';
          //*** end chg *****  03/01/22  *************************************
                Eval(h) manPrem = (SRamtp * E9eNtR/100);
             Else;
                Eval(h) manPrem = (SRamtp * E9arat/100);
             EndIf;
          When PRpcf = 'C'; // and clientID <> '150';
          //*** begin chg ***  03/01/22  *************************************
          //*****   If netRated;
             If netRated and clientID = '136';
          //*** end chg *****  03/01/22  *************************************
                Eval(h) manPrem = (SRamtp * E9eNtR);
             Else;
                Eval(h) manPrem = (SRamtp * E9arat);
             EndIf;
          Endsl;
          Return manPrem;
       Else;
          Return 0;
       Endif;
      /End-Free

     p getManPrem      e
     //============================================================================================
      // Gets loss cost multiplier for payroll class being reported  (returns 1 if no LCM)
     //============================================================================================
     p getLossCost     b
     d getLossCost     pi             5  4
     d  class                               like(SRCLAS)
     d  effDt                               like(SRTODT)

     d qqco#           s              3
     d qqfnd           s              3
     d qqrtcd          s              3
     d qqclas          s              5
     d qqefdt          s              7
     d qqlcm           s              5
     d lossCostMult    s              5  4       inz(1)

     c                   move(p)   ELco#         qqco#
     c                   move(p)   ELfnd         qqfnd
     c                   move(p)   ELrtcd        qqrtcd
     c                   move(p)   class         qqclas
     c                   move(p)   effDt         qqefdt

     c                   call      'SRLCM'
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqrtcd
     c                   parm                    qqclas
     c                   parm                    qqefdt
     c                   parm      *blanks       qqlcm

      /FREE
       if (qqlcm <> *blanks);
      /END-FREE
     c                   move      qqlcm         lossCostMult
      /FREE
       endif;

       return lossCostMult;

      /END-FREE
     p getLossCost     e
     //============================================================================================
      // Gets harbor exposure rate for payroll class being reported (returns 1 if no harbor exp rate
     //============================================================================================
     p getHarborExp    b
     d getHarborExp    pi                  like(HELHNF)

     d harborExpRate   s                   like(HELHNF) inz(1)

     c     heky1         klist
     c                   kfld                    scCO#
     c                   kfld                    SRfnd
     c                   kfld                    ratingState
     c                   kfld                    ELPFDT

     c     heky          klist
     c                   kfld                    scCO#
     c                   kfld                    SRfnd
     c                   kfld                    ratingState

      /FREE
       ratingState = RatingSt(ELCO#:ELRTCD);
       if SRSPEC = 'F' and PRLHCD = 'N';
         setgt heky1 phe01;
         readpe heky phe01;
         *IN60 = %EOF;
         if not(%eof(wdhel));
           harborExpRate = HELHNF;
         endif;
       endif;

       return harborExpRate;

      /END-FREE
     p getHarborExp    e
      //===============================================================
      //  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
      //===============================================================
     p SaveEndorse     b
     D SaveEndorse     pi
      //
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      //
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      //
     C     F2KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      //
      //  INCR LIMIT TABLE FILE
      //
     C     LMKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXPFDT
      //
     C     LMKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
      //
     C     AGTPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      //
     C     *LIKE         DEFINE    ENREQ#        XXREQ#
     C
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPFDT            7 0
     C
      /FREE
       CHAIN F2KY PF200;
       *IN60 = NOT %FOUND;
       //
       IF LDENDA <> *ZEROS
       AND LDENDA <> *BLANKS;
         //
         If scExM1 = 0 and scExM2 <> 0;
            scNoExM1 = 'Y';
         Else;
            scNoExM1 = *Blank;
         Endif;
         //*** Begin Add *** 07/30/23 *************************************
         // Set No Rate for Experience Modifier 2
         If scExM2 = 0 and scEm2Date <> 0;
            scNoExM2 = 'Y';
         Else;
            scNoExM2 = *Blank;
         Endif;
         // Set No Rate for Experience Modifier 3
         If scExM3 = 0 and scEm3Date <> 0;
            scNoExM3 = 'Y';
         Else;
            scNoExM3 = *Blank;
         Endif;
         //*** End   Add *** 07/30/23 *************************************
         If scIlCd <> ELilcd
         or scInL2 <> ELinl2
         or scExM1 <> ELexm1 and ELnoexm1 = *Blank
         or scNoExM1 <> ELnoexm1
         or scExM2 <> ELexm2
         or scExM3 <> ELexm3;
           //
           //  GET ENDORS/DARTS INFORMATION
           //
           CHAIN DXKY PDX00;
           *IN60 = NOT %FOUND;
           //
           AGTINFO(##CO#:ELFND:ELEMP#:ELDIV:##FYR:##AGT#
               :##ANAM:##PROD:##PNAM);
           //
           //  SETUP FOR NEXT REQUEST NUMBER
           //
           SETGT ENKY PEN03;
           READPE ENKY PEN03;
           *IN60 = %EOF;
           IF *IN60 = *ON;
      /END-FREE
     C                   Z-ADD     *ZERO         XXREQ#
      /FREE
           ELSE;
      /END-FREE
     C                   Z-ADD     ENREQ#        XXREQ#
      /FREE
           ENDIF;
           //
           CLEAR PEN03;
           //
      /END-FREE
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
      //
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDP
     C                   MOVEL     SRDTO         ENEEDT
      //
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
      //
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
      //
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
      //
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
     C                   MOVEL     EMCNTY        ENCNT1
      //
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
      //
      /FREE
           CHAIN LDMYRR PMY01;
           *IN60 = NOT %FOUND;
      /END-FREE
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
      //
      /FREE
           ENTIME = Timeto6(SystemTime());
           ENDT   = Dateto7(SystemDate());


      /END-FREE
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C                   MOVE      F2CAR#        ENCAR1
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         ENPFD1
      //
      //  DETERMINE WHICH TYPE(S) OF CHANGES HAVE OCCURRED
      //
      //    INCREASED LIMIT CHANGE
      //
      /FREE
           IF SCILCD <> ELILCD
           OR SCINL2 <> ELINL2;
             //
             //  SET INCREASED LIMITS CHANGE FLAG
      /END-FREE
     C                   MOVEL     'Y'           ENILFL
     C                   MOVEL     DXPINL        ENRMCD
      //  LOAD OLD VALUES
     C                   MOVEL     ELILCD        ENIC11
      //
      //  GET INCREASED LIMIT #1 DESCRIPTION  FOR OLD VALUE
      //
      /FREE
       //***** beg chg ****** 04/21/21 ****************************************
       //*** IF ELILCD <> *ZEROS;
             IF ELILCD <> *BLANKS;
       //***** End chg ****** 04/21/21 ****************************************
               //
      /END-FREE
     C                   MOVEL(P)  ELILCD        XXKEY
      /FREE
               SETGT LMKY PLM01;
               READPE LMKY2 PLM01;
               *IN60 = %EOF;
               DOW *IN60 = '0'
               AND LMSTAT = 'I';
                 READPE LMKY2 PLM01;
                 *IN60 = %EOF;
               ENDDO;                                                         //*IN60 DOWEQ
      /END-FREE
     C  N60              MOVEL(P)  LMDESC        ELILDS           30
     C   60              MOVEL     *BLANKS       ELILDS
      //
      /FREE
             ENDIF;                                                           //SCILCD<>*ZEROS
             //
      /END-FREE
     C                   MOVEL     ELILDS        ENID11
     C                   MOVEL     ELINL2        ENIC21
      //
      //  GET INCREASED LIMIT #2 DESCRIPTION  FOR OLD VALUE
      //
      /FREE
       //***** beg chg ****** 04/21/21 ****************************************
       //*** IF ELINL2 <> *ZEROS;
             IF ELINL2 <> *Blanks;
       //***** End chg ****** 04/21/21 ****************************************
               //
      /END-FREE
     C                   MOVEL(P)  ELINL2        XXKEY
      /FREE
               SETGT LMKY PLM01;
               READPE LMKY2 PLM01;
               *IN60 = %EOF;
               DOW *IN60 = '0'
               AND LMSTAT = 'I';
                 READPE LMKY2 PLM01;
                 *IN60 = %EOF;
               ENDDO;                                                         //*IN60 DOWEQ
      /END-FREE
     C  N60              MOVEL(P)  LMDESC        ELI2DS           30
     C   60              MOVEL     *BLANKS       ELI2DS
      //
      /FREE
             ENDIF;                                                           //ELINL2<>*ZEROS
      /END-FREE
     C                   MOVEL     ELI2DS        ENID21
      //  LOAD NEW VALUES
      //
     C                   MOVEL     SCILCD        ENIC12
      //
      //  GET INCREASED LIMIT #1 DESCRIPTION  FOR NEW VALUE
      //
      /FREE
       //***** beg chg ****** 04/21/21 ****************************************
       //*** IF SCILCD <> *ZEROS;
             IF SCILCD <> *BLANKS;
       //***** End chg ****** 04/21/21 ****************************************
               //
      /END-FREE
     C                   MOVEL(P)  SCILCD        XXKEY
      /FREE
               SETGT LMKY PLM01;
               READPE LMKY2 PLM01;
               *IN60 = %EOF;
               DOW *IN60 = '0'
               AND LMSTAT = 'I';
                 READPE LMKY2 PLM01;
                 *IN60 = %EOF;
               ENDDO;                                                         //*IN60 DOWEQ
      /END-FREE
     C  N60              MOVEL(P)  LMDESC        SCILDS           30
     C   60              MOVEL     *BLANKS       SCILDS
      //
      /FREE
             ENDIF;                                                           //SCILCD<>*ZEROS
      /END-FREE
     C                   MOVEL     SCILDS        ENID12
     C                   MOVEL     SCINL2        ENIC22
      //
      //  GET INCREASED LIMIT #2 DESCRIPTION  FOR NEW VALUE
      //
      /FREE
       //***** beg chg ****** 04/21/21 ****************************************
       //*** IF SCINL2 <> *ZEROS;
             IF SCINL2 <> *BLANKS;
       //***** End chg ****** 04/21/21 ****************************************
               //
      /END-FREE
     C                   MOVEL(P)  SCINL2        XXKEY
      /FREE
               SETGT LMKY PLM01;
               READPE LMKY2 PLM01;
               *IN60 = %EOF;
               DOW *IN60 = '0'
               AND LMSTAT = 'I';
                 READPE LMKY2 PLM01;
                 *IN60 = %EOF;
               ENDDO;                                                         //*IN60 DOWEQ
      /END-FREE
     C  N60              MOVEL(P)  LMDESC        SCI2DS           30
     C   60              MOVEL     *BLANKS       SCI2DS
      //
      /FREE
             ENDIF;                                                           //ELINL2<>*ZEROS
             //
      /END-FREE
     C                   MOVEL     SCI2DS        ENID22
      //  CLEAR EXP MOD CHG FILEDS
     C                   MOVEL     *BLANKS       ENERFL
     C                   MOVE      *ZEROS        ENEM11
     C                   MOVE      *ZEROS        ENEM21
     C                   MOVE      *ZEROS        ENEM31
     C                   MOVE      *ZEROS        ENEM12
     C                   MOVE      *ZEROS        ENEM22
     C                   MOVE      *ZEROS        ENEM32
     C                   MOVE      *ZEROS        ENED11
     C                   MOVE      *ZEROS        ENED21
     C                   MOVE      *ZEROS        ENED31
     C                   MOVE      *ZEROS        ENED12
     C                   MOVE      *ZEROS        ENED22
     C                   MOVE      *ZEROS        ENED32
      //
      //  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
      //
     C                   ADD       1             XXREQ#
     C                   Z-ADD     XXREQ#        ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
      //
      /FREE
             IF SCILCD <> ELILCD;
      /END-FREE
     C     ELILCD        CAT(P)    ELILDS:1      ENBLIN
     C     SCILCD        CAT(P)    SCILDS:1      ENALIN
      /FREE
       // Get STD limits
       If ENblin = *blanks;
          Setgt (ELco#:ELfnd:xxST:xxPFDT) PRR00;
          Readpe (ELco#:ELfnd:xxST) PRR00;
          If not %eof(WDRRL);
             ENblin = %Trim(RREMLB);
          Endif;
       Endif;
               WRITE PEN03;
      /END-FREE
     C                   ADD       1             ENSEQ#
      /FREE
             ENDIF;                                                           //SCILCD <> ELILCD
             //
             IF SCINL2 <> ELINL2;
      /END-FREE
     C     ELINL2        CAT(P)    ELI2DS:1      ENBLIN
     C     SCINL2        CAT(P)    SCI2DS:1      ENALIN
      /FREE
               WRITE PEN03;
      /END-FREE
     C                   ADD       1             ENSEQ#
      /FREE
             ENDIF;                                                           //SCINL2 <> ELINL2
             //
           ENDIF;                                                             //SCILCD <> ELILCD
           //
           //    EXPERIENCE MOD CHANGE
           IF SCEXM1 <> ELEXM1
           OR SCEXM2 <> ELEXM2
           OR SCEXM3 <> ELEXM3;
             //  SET EXP MOD CHANGE FLAG
      /END-FREE
     C                   MOVE      'Y'           ENERFL
     C                   MOVEL     DXPEXM        ENRMCD
      //  LOAD OLD VALUES
      /Free
        If ELnoexm1 = 'Y';
           ENem11 = 0;
           ELemd1 = ELpfdt;
        Else;
           ENem11 = ELexm1;
        Endif;
      /End-Free
     C                   MOVE      ELEXM2        ENEM21
     C                   MOVE      ELEXM3        ENEM31
     C
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ELEXD1            8 0
     C
     C                   MOVE      ELEXD1        ENED11
     C
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ELEXD2            8 0
     C
     C                   MOVE      ELEXD2        ENED21
     C
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ELEXD3            8 0
     C
     C                   MOVE      ELEXD3        ENED31
      //  LOAD NEW VALUES
     C                   MOVE      SCEXM1        ENEM12
     C                   MOVE      SCEXM2        ENEM22
     C                   MOVE      SCEXM3        ENEM32
      //
     C                   Z-ADD     scEM1Date     SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ENED12
      //
     C                   Z-ADD     scEM2Date     SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ENED22
      //
     C                   Z-ADD     scEM1Date     SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYP
     C                   MOVEL     SRDTO         ENED32
      //
      //  CLEAR INCREASED LIMITS CHG FIELDS
     C                   MOVE      *BLANKS       ENILFL
     C                   MOVE      *BLANKS       ENIC11
     C                   MOVE      *BLANKS       ENID11
     C                   MOVE      *BLANKS       ENIC21
     C                   MOVE      *BLANKS       ENID21
     C                   MOVE      *BLANKS       ENIC12
     C                   MOVE      *BLANKS       ENID12
     C                   MOVE      *BLANKS       ENIC22
     C                   MOVE      *BLANKS       ENID22
      //
      //  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
      //
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
      //  EXP MOD 1
      /FREE
             IF ELEXM1 = *ZERO;
      /END-FREE
     C                   MOVEL     *BLANKS       ENBLIN
      /FREE
             ELSE;
      /END-FREE
     C                   MOVE      ELEXM1        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENBLIN
     C                   CAT       XXEXM4:0      ENBLIN
     C                   MOVE      ELEXD1        XXWKDT
     C                   CAT       '-':1         ENBLIN
     C                   CAT       XXWKMM:1      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKDD:0      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKCY:0      ENBLIN
      /FREE
             ENDIF;
             IF SCEXM1 = *ZERO;
      /END-FREE
     C                   MOVEL     *BLANKS       ENALIN
      /FREE
             ELSE;
      /END-FREE
     C                   MOVE      SCEXM1        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENALIN
     C                   CAT       XXEXM4:0      ENALIN
     C                   MOVE      scEM1Date     XXWKDT
     C                   CAT       '-':1         ENALIN
     C                   CAT       XXWKMM:1      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKDD:0      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKCY:0      ENALIN
      /FREE
             ENDIF;
             WRITE PEN03;
      /END-FREE
     C                   ADD       1             ENSEQ#
      //  EXP MOD 2
      /FREE
             IF ELEXM2 <> *ZERO
             OR SCEXM2 <> *ZERO;
               IF ELEXM2 = *ZERO;
      /END-FREE
     C                   MOVEL     *BLANKS       ENBLIN
      /FREE
               ELSE;
      /END-FREE
     C                   MOVE      ELEXM2        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENBLIN
     C                   CAT       XXEXM4:0      ENBLIN
     C                   MOVE      ELEXD2        XXWKDT
     C                   CAT       '-':1         ENBLIN
     C                   CAT       XXWKMM:1      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKDD:0      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKCY:0      ENBLIN
      /FREE
               ENDIF;
               IF SCEXM2 = *ZERO;
      /END-FREE
     C                   MOVEL     *BLANKS       ENALIN
      /FREE
               ELSE;
      /END-FREE
     C                   MOVE      SCEXM2        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENALIN
     C                   CAT       XXEXM4:0      ENALIN
     C                   MOVE      scEM2Date     XXWKDT
     C                   CAT       '-':1         ENALIN
     C                   CAT       XXWKMM:1      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKDD:0      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKCY:0      ENALIN
      /FREE
               ENDIF;
               WRITE PEN03;
      /END-FREE
     C                   ADD       1             ENSEQ#
      /FREE
             ENDIF;                                                           //ELEXM2<>0
             //  EXP MOD 3
             IF ELEXM3 <> *ZERO
             OR SCEXM3 <> *ZERO;
               IF ELEXM3 = *ZERO;
      /END-FREE
     C                   MOVEL     *BLANKS       ENBLIN
      /FREE
               ELSE;
      /END-FREE
     C                   MOVE      ELEXM3        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENBLIN
     C                   CAT       XXEXM4:0      ENBLIN
     C                   MOVE      ELEXD3        XXWKDT
     C                   CAT       '-':1         ENBLIN
     C                   CAT       XXWKMM:1      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKDD:0      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKCY:0      ENBLIN
      /FREE
               ENDIF;
               IF SCEXM3 = *ZERO;
      /END-FREE
     C                   MOVEL     *BLANKS       ENALIN
      /FREE
               ELSE;
      /END-FREE
     C                   MOVE      SCEXM3        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENALIN
     C                   CAT       XXEXM4:0      ENALIN
     C                   MOVE      scEM3Date     XXWKDT
     C                   CAT       '-':1         ENALIN
     C                   CAT       XXWKMM:1      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKDD:0      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKCY:0      ENALIN
      /FREE
               ENDIF;
               WRITE PEN03;
      /END-FREE
     C                   ADD       1             ENSEQ#
      /FREE
             ENDIF;                                                           //ELEXM3<>0
             //
           ENDIF;                                                             //SCEXM1 <> ELEXM1
           //
         ENDIF;                                                               //SCRTCD <> ELRTCD
         //
       ENDIF;                                                                 //LDENDA <> *ZERO
       //
       //
      /END-FREE
     P SaveEndorse     e
      //===============================================================
      // ChkEFT - Check EFT setup.
      //===============================================================
     p chkEFT          b
     d chkEFT          pi

      /free
       If quoteMode <> 'Q';
          outEftKey = %Editc(ELemp#:'X') + %Editc(ELdiv:'X') + '  ';
          outDate1 = %Editc(DateTo7(SystemDate):'X');
          outCo#  = %editc(ELco#:'X');
          outFnd# = %editc(ELfnd:'X');
          If not GetEftStatus(outCo#:OutEftType:outFnd#:outEftKey:outDate1:
           outPreNote);

             If kkBsCd <> *zero;
                ##key# = %editc(kkBsCd:'X');
                Chain (ELco#:##key#) pbs01;
                If %found and BSeftf = 'Y';
                   PopOneBtn(errMsg:'Error');
                   error = *on;
                Endif;
             Endif;
          Endif;
       Endif;

      /end-free
     p chkEFT          e

       //***************************************************************************//

      // utility procedure definitions
      /COPY SCOPYPOLPI
** Statistical Data Information messages
Standard Liability Limits that print on the bill/policy page
Premium level to which "
Premium level to which "
Premium level to which the Aircraft Surcharge is applied
Safety Fees information
State Tax information
Waiver Surcharge defaults
First assessment values
Second assessment values
Deductible Discount information
Stock discount information
