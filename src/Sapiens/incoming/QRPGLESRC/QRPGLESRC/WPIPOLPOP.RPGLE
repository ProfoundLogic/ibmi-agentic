     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H/TITLE WPIPOLPOP - Policy Popups Service Program (Profound)
     H DFTNAME(WPIPOLPOP)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H/DEFINE   PROFOUNDUI
      *===============================================================*
      *  Created   : 05/18/12                                         *
      *  Programmer: C:65                                             *
      *  Project   : WC2116 - Policy Transform                        *
      *===============================================================*
      *  Description:  Service program which contains the screen files
      *                and processing for Policy related popups.
      *
      *  The popups included are:
      *    Payroll            (for adding/editing payroll classes)
      *    Excluded States    (for adding/deleting excluded states)
      *    Rating Information (for editing rating info on policy detail)
      *    Large Deductible   (for editing large deduc info on policy detail)
      *    Retro Rating       (for editing retro info on policy detail)
      *    Additional Info    (for editing additional info on policy detail)
      *    Agent History      (for editing agent history records)
      *    Billing Auxiliary  (for selecting payment plans w/billing auxiliary)
      *    City Demographics  (for selecting city demographics record)
      *    Scheduled Rating   (for editing scheduled rating info on policy detail)
      *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      *  11/21/25 C:333 Support: 119896 (Amwins)                      *
      *                 WCSTAT - AMX3160199001 state code 9812 and 9848
      *                 appears to be duplicated.                     *
      *                 -Changed setCondition78 conditions for displaying
      *                  minimum invoke message.                      *
      *                 -Changed setCondition8/9 inFlag='H' section to*
      *                  consider only actual increased limit fields. *
      *                                                               *
      *  10/21/25 C:65  Support: 119967/WC2952   (Prescient)          *
      *                 Check the adoption date on 3 digit rates when *
      *                 calculating the payroll rate.                 *
      *                                                               *
      *  09/11/25 C:65  Support: 119699/WC2918   (Amwins)             *
      *                 Store the not rated flag for exp mods 2 and 3 *
      *                 back into the employer detail file.  Load the *
      *                 2nd/3rd modifiers properly as well.           *
      *                                                               *
      *  06/19/25 C:65  Project: 146WKR (Phw#119246)                  *
      *                 Add popup alerting user collateral change     *
      *                 will affect entire policy.  Move the clearing *
      *                 of the collateral error fields as they were   *
      *                 added to the wrong procedure.                 *
      *                                                               *
      * 03/06/25 C:291 Project: SY450                                 *
      *                Experience Rating field was available in       *
      *                Inquiry mode. Remove DspMod check as Rating    *
      *                popup will not be called from Inquiry mode.    *
      *                                                               *
      * 02/06/25 C:300 Support: 118187 & 118486 (SUNZ)                *
      *                Display Balance to Minimum correctly on Policy *
      *                Summary Premium History (Tab 3)                *
      *                                                               *
      * 11/11/24 C:333 Project: WC2985                                *
      *                Apply Deductible discount to Discounted Premium*
      *                                                               *
      * 11/04/24 C:333 Support: 117108 (SUNZ)                         *
      *                QA V24R0 - Min Prem Display WC2953.            *
      *                - Changes for printing manual premium.         *
      *                                                               *
      * 10/14/24 C:333 Support: 117108 (SUNZ)                         *
      *                QA V24R0 - Min Prem Display WC2953.            *
      *                - Calcualtion error in prorata.                *
      *                - Calcualtion error in Balance to Min (NJ)     *
      *                - Calcualtion error in Terrorism & Domestic    *
      *                  Terrorism for prorata <> 0 & <> 1.           *
      *                - Calcualtion error in Standard and Normal     *
      *                  Premium for prorata <> 0 & <> 1.             *
      *                - Including Total                              *
      *                                                               *
      * 09/24/24 C:342 Support: 117652 (Prescient)                    *
      *                 Issue with Premium History screen.            *
      *                 1. made changes to pull in the old field      *
      *                    if the new field is zero.                  *
      *                    Old fields - Write XXXX and Inforce XXXX   *
      *                    New fields - Write Actual XXXX and
      *                    Inforce Actual XXXX                        *
      *                 2. made changes if inflag is D when           *
      *                    pulling information from WDMPP file fields *
      *                 3. made changes to load "Balance to Minumum"  *
      *                    heading in setCondition77 procedure.       *
      *                    It was overriding by "Standard Premium".   *
      *                                                               *
      * 08/27/24 C:333 Support: 117108 (SUNZ)                         *
      *                QA V24R0 - Min Prem Display WC2953.            *
      *                                                               *
      * 08/14/24 C:291 Project: WC2974                                *
      *                Add Actual Increased Limits amount and Balance *
      *                to Increased Limits Minimum when building      *
      *                the premium Information for Policy Summary.    *
      *                                                               *
      * 08/05/24 C:65  Project: WC2951                                *
      *                Default report to NCCI as 'Y' for Sunz.        *
      *                                                               *
      * 07/21/24 C:300 Support: 114005 (AmWins)                       *
      *                Premium History Balance to Minimum (WC2953)    *
      *                Show pre-minimum amount in Schedule DB/CR      *
      *                                                               *
      * 06/24/24 C:333 Project: 146WKR                                *
      *                Collateral Processing.                         *
      *                DSPF - Recompile                               *
      *                                                               *
      * 06/12/24 C:062 Project: 146WKQ                                *
      *                Add Bill assessments for S/R.                  *
      *                                                               *
      * 04/18/24 C:291 Project: WC2961                                *
      *                Add Schedule DB/CR against Manual Premium.     *
      *                                                               *
      *  03/04/24 C:079 Project: WC2951                               *
      *                 Add Exp Rating and Report to NCCI fields      *
      *                                                               *
      * 02/27/24 C:333 PROJECT: WC2953                                *
      *                Premium History Balance to Minimum             *
      *                                                               *
      *  10/25/23 C:340 Project: WC2922                               *
      *                 Add/Remove Invoices based on contested flag   *
      *                 and Division is either not summarized OR      *
      *                 its Summarized and state reporting.           *
      *                                                               *
      * 10/20/23 C:65  Support: Phw#114397 (Sunz)                     *
      *                Clear ANC checkbox disable variable so its     *
      *                on status does not stick between divisions.    *
      *                In addition move the coding to the correct     *
      *                procedure - the one for additional info.       *
      *                                                               *
      * 10/16/23 C:340 Project: 153WAO                                *
      *                Add Prior Policy Number.                       *
      *                Only for Client # 153.                         *
      *                                                               *
      * 07/30/23 C:062 Project: WC2918                                *
      *                Add not rated logic for exp mod 2 and 3.       *
      *                                                               *
      * 03/23/22 C:079 Support: PHW101574                             *
      *                Use RRN passed to the procedure                *
      *                                                               *
      * 02/23/22 C:136 Support: 99500 (Clt146)                        *
      *                Pull retrosettlement amount from A/R for       *
      *                3rd Party Billing.                             *
      *                                                               *
      * 01/28/22 C:333 Support: 92958                                 *
      *                WC2734 - Split Not Showing on Premium History  *
      *                Screen for No Mod values.                      *
      *                - Premium History Screen to Show '*' instead of*
      *                  blank modifier.                              *
      *                                                               *
      * 11/10/21 C:33  Project: WC2302 #99350                         *
      *                Use the appropriate RRN value.                 *
      *                                                               *
      * 06/22/21 C:295 Project: WC2302                                *
      *                Auto Tab Closure on Deletion.                  *
      *                                                               *
      *                                                               *
      * 04/06/21 C:300 Support: 94604 (SUNZ)                          *
      *                Policy rating screen allows old deductible     *
      *                descriptions to be selected.                   *
      *                Recompile over WTPOLDTL DSPF changes           *
      *                                                                *
      * 03/19/21 C:295 Support: 94885                                 *
      *                Correct the error message for Prorate or       *
      *                Short rate.                                    *
      *                                                               *
      * 01/05/21 C:65  Project: WC2447                                 *
      *                Softcode the height and button placement on     *
      *                premium info/unbilled premium window.           *
      *                                                                *
      * 02/02/21 C:180 Project: WC2757                                 *
      *                Add checkbox and text area for                  *
      *                Blanket Waiver Description                      *
      *                                                                *
      * 09/02/20 C:180 Project: WC2730                                 *
      *                Add Include Medical/Indemnity Claims            *
      *                deductible option to RcdLrgDed screen.          *
      *                                                                *
      * 06/22/20 C:33  Project: SY406                                  *
      *                Update EMFQLK based on the quote status change  *
      *                                                                *
      * 06/02/20 C:33  Project: SY406                                  *
      *                Use service program to check producer states    *
      *                                                                *
      * 05/14/20 C:65  Project: WC2447                                 *
      *                Add new mode for DisplayPremium to use WAELP.   *
      *                                                                *
      * 01/23/20 C:65  Support: 86879 (LUBA)                           *
      *                If 3C verbiage is not checked to be overridden  *
      *                then clear the fields in employer detail.       *
      *                                                                *
      * 12/24/19 C:318 Support: 85252                                  *
      *                Corrected error while adding multiple divisions *
      *                during quote entry procees.                     *
      *                Refreshed RRN if rec not prsent in WMRPL        *
      *                                                                *
      * 10/30/19 C:135 Project: SY406 (DE536)                          *
      *                Add waiver info to heading                      *
      *                                                                *
      * 06/25/19 C:295 Support: 80159                                  *
      *                To calculate waiver amount correctly based on   *
      *                modifiers.                                      *
      *                                                                *
      * 02/08/19 C:291 Project: WC2206                                 *
      *                Print the 2nd modifier if the first modifier is *
      *                not rated.                                      *
      *                                                                *
      *  01/22/19 C:298 Support: PHW 76288                             *
      *                 Added a tooltip on rcdRating2 in WTPOLDTL      *
      *                 display file over fields SCDCDE,SCDCD2         *
      *                                                                *
      * 01/02/19 C:180 Project: 150WDR                                *
      *                Add confirmation popup for per capita class.   *
      *                                                                *
      * 07/12/18 C:291 Project: 148WAV                                 *
      *                Turn off Access to Deviation when flagged in    *
      *                WDRRP.                                          *
      *                                                                *
      * 06/22/18 C:65  Project: 150WAL                                 *
      *                Change rounding on territory factor to round    *
      *                after applying both LCM and territory.          *
      //                                                               *
      // 05/31/18 C:062 Project: 133WQN                                *
      //                Add Suppress Premium Notice in Mass Billing    *
      //                                                               *
      // 04/03/18 C:33  Project: 150WCF                                *
      //                Large Deductible default updates               *
      //                                                               *
      // 03/16/18 C:65  Support: 63144 (RMLA)                          *
      //                Allow safety fee entries if there is a heading *
      //                (for amount) or a premium level (for percent). *
      //                Do not constantly default the safety fees      *
      //                since they may not be desired on policy.       *
      *                                                                *
      * 02/21/18 C:TXO Project: WC2583                                 *
      *                Update drop down selection criteria in WPIPOLPOP*
      *                                                                *
      //                                                               *
      // 02/01/18 C:136 Project: 148WAH                                *
      //                Add premium line for policy fee                *
      //                                                               *
      // 02/07/18 C:291 Support: 63953                                 *
      //                Change Division dropdown "max choices" property*
      //                to 100,000. Screen change only.                *
      //                Modified records: rcdPayEdt & rcdAgtEdit       *
      //                                                               *
      // 11/08/17 C:33  Support: 50956                                 *
      //                Default carrier policy # on add mode           *
      //                                                               *
      // 10/13/17 C:33  Support: 50254                                 *
      //                Default large deductible info on add mode      *
      *                                                               *
      * 09/27/17 C:33  Project: 150WAV/150WBL - CLT150 - 49390        *
      *                Add deductible aggregate %.                    *
      *                Add variable aggregate Flag.                   *
      *
      * 09/08/17 C:65  Project: 150WAR
      *                Display info edit if % of manual premium value
      *                differs from employer 1 division 0 for group.
      *
      * 08/03/17 C:65  Project: 150WAR
      *                Add a text box for % of manual premium to be
    *                displayed for client 150 only (both regular
      *                screen and popup window).
      *
      * 07/26/17 C:062 Project: 150WAL
      *                Teritorial Differential Factor Processing
      *
      * 06/30/17 C:136 Support: 47194
      *                Clear increased limits and deductible screen fields if no
      *                set up exists for them - prevents error from being thrown
      *                when adding states to policy via quote entry.
      *
      * 06/22/17 C:65  Phaseware: 47465  (CoveRisk)
      *                Display the ARAP factors on the ARAP line item.  Also fix the
      *                placement of the subject premium line item (uncommented).
      *
      * 05/10/17 C:306 Phaseware: 47164
      *                Recompile for screen change to show zeros in
      *                the agent edit window division drop down field
      *
      * 03/22/17 C:212 Support : 46117
      *                Fix display of Aircraft description to not show
      *                error message if the field is not visible
      *                no DSPF changes
      *
      * 04/07/17 C:062 Project  136WHF
      *                Use CareWest Territory Rating
      *
      * 01/31/17 C:306 Project : WC2410
      *                Make the Extension field mandatory for WA state
      *                If extension exist on payroll class code.
      *
      * 01/23/17 C:136 Support: 44025
      *                Prevent CA policies from getting a payroll
      *                record with no class (EPCLAS = 0)
      *
      * 08/17/16 C:298 Project: WC2070                                *
      *                Add ANC Charge to Premium History              *
      *                                                               *
      * 04/27/16 C:306 Project: WC2424                                *
      *                Added a logic to populate the Experience       *
      *                Modifier- Not final status flag value.         *
      *                                                               *
      * 04/19/16 C:242 Project: WC2069                                *
      *                Add Billing Fees to Premium History            *
      *                                                               *
      * 10/20/15 C:302 PROJECT: WC2364                                *
      *                Add non-ratable class code and occupational    *
      *                desease line item.                             *
      *                                                               *
      * 10/01/15 C:306 Project: WC2311                                *
      *                CA Requiring extension on payroll class        *
      *                                                               *
      * 06/18/15 C:298 Phaseware: 32565                               *
      *                Added 'Enter' as shortcut key on Save button   *
      *                on Payroll Edit pop up in display file         *
      *                                                               *
      * 05/12/15 C:306 Phaseware: 29738 LUBA                          *
      *                Recompile only                                 *
      *                                                               *
      * 05/07/15 C:65  Support: 34745
      *                Exclude inactive records for payroll class box SQL.
      *
      * 04/09/15 C:242 Project: 132WQS                                *
      *                CompSuite Retrofit of Custom Code to Base      *
      *                Turn off Deviation#1 when RRST = NM.           *
      *                                                               *
      * 03/30/15 C:290 Support: 33886
      *                Added a hidden field scPFDT3 in WTPOLDTL
      *                display file to capture the effective date of
      *                the state reporting division.Changed JavaScript
      *                associated with Experience Rating Modifiers.
      *                Added logic to populate the rate information
      *                effective dates with current div's effective
      *                date. (contains DSPF changes of WTPOLDTL)
      *
      * 10/31/14 C:283 RedMine: 4741
      *                Save the Increased Limits and Deductible Discounts
      *                only for the applicable states.
      *
      * 10/14/14 C:33  Project: 114WPE
      *               -Only edit rates if they change.
      *               -Include 'Written Premium' as a 'Plan Type'
      *
      * 08/29/14 C:283 Support: WA129 86635
      *                Save the ARAP field in Quote Entry mode.
      *
      * 08/01/14 C:283 Support: WA114 85632
      *                Correct the visibility of the Aircraft Desc field.
      *                Error handling for Aircraft Desc field corrected.
      *
      * 07/11/14 C:279 Project: 111WBL
      *                Added dividend amount field.
      *
      *  06/05/14 C:65  Project: 136WFZ (Redmine #4276)
      *                 Recalculate the commission multiplier when
      *                 rating popup is displayed.  There is no display
      *                 mode capability currently.
      *
      *  05/08/14 C:279 Redmine: 4091
      *                 Modified to make Primary Agent# non-editable
      *                 in edit mode for Live.
      *
      *  05/02/14 C:302 Support : WA099 85305
      *                 Add Subject Premium amounts to the premiumInfo Procedure
      *
      *  04/03/14 C:279 Redmine: 4088
      *                 Change 'Commission Mod Factor' label to
      *                 'Schedule Debit/Credit'.
      *
      *  04/02/14 C:279 Redmine: 4096
      *                 Populate Stock Discount heading based on State
      *                 Reporting/Rating setup.
      *
      *  04/01/14 C:279 Redmine: 4091
      *                 Corrected Agent history edit.
      *
      *  03/04/14 C:282 Project: 136WFZ Priss#19
      *                 Checked the "show zero balance" option on the
      *                 bound program field scPayEDiv.
      *                 Recompile the program.
      *
      *  02/19/14 C:190 Project: 136WFZ Priss#22
      *                 Add checkbox "Exclude from primary/secondary
      *                 agent rate" on rcdRating and "Commission Mod
      *                 Multiplier" field on rcdRating2 for
      *                 client = 136.
      *
      * 02/10/14 C:290 Support: WA147 84590
      *                Changed logic so that producer change is allowed
      *                even if the commision exists. And restricts user
      *                from leaving agency history tab, if more than one
      *                records are active.
      *
      * 02/05/14 C:65  Project: SY336 Priss# 45 (display file change)
      *                Add a blank option to premium level for large
      *                deductible so a choice has to be made to
      *                populate the field - and that blank is not
      *                mixed up with manual.
      *
      * 01/27/14 C:282 Redmine:2930
      *                Removed the PayEditMod disabled flag from
      *                Aircraft Description field in display file
      *                WPIPOLPOP.
      *                Only Recompile the program WPIPOLPOP.
     *
      * 01/24/14 C:279 Project: SY336 Priss #29
      *                 Modified to not display inactive agent and producer.
      *
      * 01/07/14 C:65  Support: Redmine SY266/3843
      *                Change logical used for SQL on payroll class box to WMPRL1 as
     *                WMPRL8 appeared to be excluding classes where the latest eff
     *                date was equal to the policy effective date.
      *
      * 12/12/13 C:180 Support: WA146 82907
      *                New quote statuses
     *
      * 10/21/13 C:65  Support: WA099 83792
      *                Change processPayrollClass; to processPayrollClass(); on the
     *                onSelect event property of the payroll class text box, to
     *                allow the original functionality to work again with the added
     *                coding of the aircraft description field.  No RPG changes.
     *
      * 10/07/13 C:190 Project: 25WSS
      *                - From the Policy Year Detail/ Rating Tab replace
      *                  "Deviation #2" with  Stabilization Factor.
      *                - Change the program to determine if
      *                  Premium or Contribution should be printed
      *                  /displayed based on Group setup.
      *
     * 09/27/13 C:136 Project WC2193 (Premium Info)
     *                Priss 9 -  Do not display a '*' unless there are multiple
     *                           modifiers
     *
      * 08/09/13 C:283 Support: WA133 83028
      *                Validate the AOR against the agent is in effect as of the
      *                system date not the effective date of the primary agent.
      *                Same edit for the AOR Rate and Producer.
      *
      * 07/30/13 C:282 Redmine:2930
      *                Added the AirCraft Description handling.
      *
      * 07/15/13 C:136 Project: 147WAI
      *                Add Dividend Plan Processing
      *
      * 06/14/13 C:190 Project: WC2193
     *                Add two new procedures to serve the premium information.
     *                - PremiumInfo.
     *                - DisplayPremium.
     *
      * 04/10/13 C:65  Project: SY266 Redmine:2700
      *                Apply LCM to payroll rate before writing/updating record.
     *
      * 03/12/13 C:279 Project: SY266 Redmine:2456
      *                Policy Year Detail/Rating - Deductible Bill on Cancelled
      *                Check Box changed to Radio Button.
      *
      * 03/05/13 C:65  Project:  WC2116 (Redmine 2454)
      *                Change the save/add next functionality to work in edit
      *                mode but change button name to Save/Add New.  Once window
     *                is redisplayed change button back to Save/Add Next.
      *****************************************************************************************

     H nomain

     FWPIPOLPOP CF   E             WORKSTN extfile('WPIPOLPOP')
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(DemoLosSFL:rrnDemoSel)
     F                                     usropn
     fwtpoldtl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdRating2)
     f                                     include(rcdLrgDed)
     f                                     include(rcdRetro)
     f                                     include(rcdAddInfo)
     f                                     include(rcdSched)
     f                                     include(rcdSchdErr)
     f                                     include(ExcludeSFL)
     f                                     include(rcdUpdtDIL)
     f                                     include(rcdRenewF)
     f                                     include(rcdAggAmt)
     f                                     Sfile(ExcludeSFL:rrnExclude)
     F                                     usropn
     FWDELP     UF   E             DISK    rename(PEL01:PEL0P)
     FWDELL     IF   E           K DISK    rename(PEL01:PEL00)
     F                                     infds(dbfds)
     FWDELL1    UF   E           K DISK    rename(PEL01:PEL01)
     FWAELP     IF   E             DISK    rename(PEL01:PEL0PA)
     FWMEML     IF   E           K DISK    rename(PEM01:PEM00)
     f                                     infds(dbfds4)
     FWMLDL1    UF A E           K DISK    rename(PLD01:PLD01)
     f                                     infds(dbfds8)
     FWMLDL2    IF   E           K DISK    rename(PLD01:PLD02)
     FWMLDL4    UF   E           K DISK    rename(PLD01:PLD04) usropn
     f                                     extfile(@wmld)
     FWMRFL     UF A E           K DISK    rename(RFP01:RFP00)
     FWMEXL     IF   E           k disk    rename(PEX01:PEX00)
     FWMEAL     IF   E           k disk    rename(PEA01:PEA00)
     FWMEAL10   IF   E           k disk    rename(PEA01:PEA10)
     FWDDDL1    IF   E           K DISK    rename(PDD01:PDD01)
     FWTRTL1    IF   E           K DISK    rename(PRT01:PRT01)
     FWTGTL     IF   E           K DISK    rename(pgt01:PGT00)
     FWTGIL     IF   E           K DISK    rename(pgi01:PGI00)
     FWDFHL     IF   E           K DISK    rename(PFH01:PFH00)
     FWMGCL     IF   E           K DISK    rename(PGC01:PGC00)
     FWMRPL     UF A E           K DISK    rename(PRP01:PRP00)
     f                                     infds(dbfds9)
     FWMRPL3    IF   E           K DISK    rename(PRP01:PRP03)
     FWMFNL     IF   E           K DISK    rename(PFN01:PFN00)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)
     FWDF1L2    UF   E           K DISK    rename(PF101:PF102) prefix(L2)
     FWMRSL     IF   E           K DISK    rename(PRS01:PRS00)
     FWDRRL     IF   E           K DISK    rename(PRR01:PRR00)
     FWMEPL     IF   E           K DISK    rename(PEP01:PEP00)
     f                                     infds(dbfds2)
     FWMEPL1E   IF   E           K DISK    rename(PEP01:PEP01E)
     FWMEPP     UF A E             DISK    rename(PEP01:PEP0P) usropn
     FWMPRL1    IF   E           K DISK    rename(PPR01:PPR01)
     FWMPXL1    IF   E           K DISK    rename(PPX01:PPX01)
     FWDXSL     IF   E           K DISK    rename(PXS01:PXS00)
     f                                     infds(dbfds3)
     FWDXSP     UF A E             DISK    rename(PXS01:PXS0P)
     FWDF4L     IF   E           K DISK    rename(PF401:PF400)
     FWMAHP     UF A E             DISK    rename(PAH01:PAH0P)
     FWMAHL     IF   E           K DISK    rename(PAH01:PAH00)
     f                                     prefix(A2)
     f                                     infds(dbfds5)
     FWMARL     IF   E           K DISK    rename(PAR01:PAR00)
     FSTSTL3    IF   E           K DISK    rename(PST01:PST03)
     FWMMGL     IF   E           K DISK    rename(PMG01:PMG00)
     FWMDSL3    IF   E           K DISK    rename(PDS01:PDS03)
     FWTLML1    IF   E           K DISK    rename(PLM01:PLM01)
     FWTDDL2    IF   E           K DISK    rename(PDD01:PDD02)
     FWMACL     IF   E           K DISK    rename(PAC01:PAC00)
     FWMAAL1    IF   E           K DISK    rename(PAA01:PAA01)
     FWMAPL     IF   E           K DISK    rename(PAP01:PAP00)
     FWMPSL     IF   E           K DISK    rename(PPS01:PPS00)
     FWTABL     IF   E           K DISK
     F                                     ignore(PRT01)
     FWMBXP     IF   E             DISK
     FWMCYL1    IF   E           K DISK    rename(PCY01:PCY01)
     f                                     infds(dbfds6)
     FWMCYL2    IF   E           K DISK    rename(PCY01:PCY02)
     f                                     infds(dbfds7)
     FWMREL     IF   E           K DISK    rename(PRE01:PRE00)
     FWMCPL     IF   E           K DISK    rename(PCP01:PCP00)
     FWMUPL     IF   E           K DISK    rename(PUP01:PUP00)
     FWAQTL7    UF A E           K DISK    rename(PQT01:PQT07)
     FWABLL     UF   E           K DISK    rename(PBL01:PBL00)
     fwdehp     if   e             disk    rename(peh01:peh0p)
     fwxelp     if   e             disk    rename(pel01:pxelp)
     Fwmeple    if   e           k disk    rename(pep01:pep0E)
     fwmdvl     uf a e           k disk    rename(pdv01:pdv00)
     fwmwvl1    if   e           k disk    rename(pwv01:pwv01)
     FSSYLF     IF   E           K disk
     F                                     ignore(SSY01)
     F                                     ignore(SPC01)
     F                                     ignore(SFB01)
     F                                     ignore(SRC01)
     F                                     ignore(SDR01)
     F                                     ignore(SBC01)
     F                                     ignore(SCS01)
     F                                     ignore(SGL01)
     F                                     ignore(SP201)
     fwmpxl3    if   e           k disk    rename(ppx01:ppx03)
     f                                     usropn
      *** Beg Add ***** 02/27/24 ******************************
     fwdmpl     if   e           k disk    rename(pmp01:pmp0d)
     fwampl     if   e           k disk    rename(pmp01:pmp0a)
      *** End Add ***** 02/27/24 ******************************
     f*** Beg Add *** 03/04/24 ****************************************
     fwdell8    if   e           k disk    rename(pel01:pel08) prefix(E2:2)
     f*** End Add *** 03/04/24 ****************************************

     dintEffdt         s              7  0
     d intRrn#         s              9  0
     d intFctr         s              5  4
     D isLargeDedRcd   s              1n
     D@wmld            s             21    inz
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

     D MSG             S             60    DIM(33) CTDATA PERRCD(1)             INFORMATION EDITS
     D PL              S              1    DIM(3) CTDATA PERRCD(1)              Plan types, desc
     D PLD             S             40    DIM(3) ALT(PL)
     d tmp             s              1    Dim(22)
     d con             s             22    Dim(70) ctdata perrcd(1)
     d tmp_s           s             22
     d @2NPER          c                   CONST('2nd prd')
     d xxwk25          s             25
     d a               s              3  0
     d outtam          s             11  2
     d outtin          s             11  2
     d xxsubn          s             11  2
     d xxsubt          s             11  2
     d cltid           s              3    inz('000')
     d intCO#          s                   like(ELco#)
     d intFnd          s                   like(ELfnd)
     d intEmp#         s                   like(ELEmp#)
     d intDiv          s                   like(ELdiv)
     d intFyr          s                   like(ELfyr)
     d ldPFLG          s              1
     d rateStCode      s                   like(ELrtcd)
     d xtadld          s             11  2
     d xtoccd          s             11  2
     d xtnrat          s             11  2
     d xtadln          s             11  2
     d xxasp2          s              5  2
     d xxst            s              2
     d strMsg          s            256
     d strBiScCd       s             10
     d saveRate1       s                   like(scRate1   )
     d saveRate2       s                   like(scRate2   )
     d saveRate3       s                   like(scRate3   )
     d saveRateRec1    s                   like(scRateRec1)
     d saveRateRec2    s                   like(scRateRec2)
     d saveRateRec3    s                   like(scRateRec3)
     d charNormPrem    s             15
     d isContribution  s               n   inz(*off)
     d isAirCde        s               n   inz(*off)
     d xxDvnd          s             11  2
     d strpDiv         s                   like(ELdiv)
     d strCode         s              3
     d xxLic           s              1
     d*** Beg Add *** 10/25/23 *** WC2922 ****************
     d stRptDiv        s              5  0
     d*** Beg Add *** 10/25/23 *** WC2922 ****************
     d*** Begin Add *** 06/12/24 ****************************************
     d chrBsCd         s             10
     d chrBillSchedType...
     d                 s              1
     d*** End   Add *** 06/12/24 ****************************************
       //*** beg add ***** 02/23/22 ****************
       Dcl-S is3rdPartyBilling Ind;
       //*** end add ***** 02/23/22 ****************
       //*** Beg Add ***** 02/27/24 ****************
       Dcl-S ShowMinBalance Ind;
       //*** End Add ***** 02/27/24 ****************
       //*** Beg Add *** 03/04/24 *** *************************************
       Dcl-s intStRptDiv Like(ELdiv);
       Dcl-s svNcci char(1);
       Dcl-s isUpdNcci char(1);
       //*** End Add *** 03/04/24 *** ************************************
       //*** Beg Add *** 10/14/24 *** ************************************
     d xxTotal         s             11  2
       //*** End Add *** 10/14/24 *** ************************************
      //*** Beg Add *** 06/24/24 *****************************************
      //dcl-s xxOcol Like(EXoCol);
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
        dcl-s isUpdColl char(1);
      //*** End Add *** 06/24/24 *****************************************

     d                 ds
     d  hdg                    1   1100
     d                                     Dim(50)
     d  head01                 1     22
     d  head02                23     44
     d  head03                45     66
     d  head04                67     88
     d  head05                89    110
     d  head06               111    132
     d  head07               133    154
     d  head08               155    176
     d  head09               177    198
     d  head10               199    220
     d  head11               221    242
     d  head12               243    264
     d  head13               265    286
     d  head14               287    308
     d  head15               309    330
     d  head16               331    352
     d  head17               353    374
     d  head18               375    396
     d  head19               397    418
     d  head20               419    440
     d  head21               441    462
     d  head22               463    484
     d  head23               485    506
     d  head24               507    528
     d  head25               529    550
     d  head26               551    572
     d  head27               573    594
     d  head28               595    616
     d  head29               617    638
     d  head30               639    660
     d  head31               661    682
     d  head32               683    704
     d  head33               705    726
     d  head34               727    748
     d  head35               749    770
     d  head36               771    792
     d  head37               793    814
     d  head38               815    836
     d  head39               837    858
     d  head40               859    880
     d  head41               881    902
     d  head42               903    924
     d  head43               925    946
     d  head44               947    968
     d  head45               969    990
     d  head46               991   1012
     d  head47              1013   1034
     d  head48              1035   1056
     d  head49              1057   1078
     d  head50              1079   1100

     d                 ds
     d  fct                    1   1100
     d                                     Dim(50)
     d  fact01                 1     22
     d  fact02                23     44
     d  fact03                45     66
     d  fact04                67     88
     d  fact05                89    110
     d  fact06               111    132
     d  fact07               133    154
     d  fact08               155    176
     d  fact09               177    198
     d  fact10               199    220
     d  fact11               221    242
     d  fact12               243    264
     d  fact13               265    286
     d  fact14               287    308
     d  fact15               309    330
     d  fact16               331    352
     d  fact17               353    374
     d  fact18               375    396
     d  fact19               397    418
     d  fact20               419    440
     d  fact21               441    462
     d  fact22               463    484
     d  fact23               485    506
     d  fact24               507    528
     d  fact25               529    550
     d  fact26               551    572
     d  fact27               573    594
     d  fact28               595    616
     d  fact29               617    638
     d  fact30               639    660
     d  fact31               661    682
     d  fact32               683    704
     d  fact33               705    726
     d  fact34               727    748
     d  fact35               749    770
     d  fact36               771    792
     d  fact37               793    814
     d  fact38               815    836
     d  fact39               837    858
     d  fact40               859    880
     d  fact41               881    902
     d  fact42               903    924
     d  fact43               925    946
     d  fact44               947    968
     d  fact45               969    990
     d  fact46               991   1012
     d  fact47              1013   1034
     d  fact48              1035   1056
     d  fact49              1057   1078
     d  fact50              1079   1100

     d                 ds
     d  tam                    1    550  2
     d                                     Dim(50)
     d  tama                   1    550
     d  decTA01                1     11  2
     d  decTA02               12     22  2
     d  decTA03               23     33  2
     d  decTA04               34     44  2
     d  decTA05               45     55  2
     d  decTA06               56     66  2
     d  decTA07               67     77  2
     d  decTA08               78     88  2
     d  decTA09               89     99  2
     d  decTA10              100    110  2
     d  decTA11              111    121  2
     d  decTA12              122    132  2
     d  decTA13              133    143  2
     d  decTA14              144    154  2
     d  decTA15              155    165  2
     d  decTA16              166    176  2
     d  decTA17              177    187  2
     d  decTA18              188    198  2
     d  decTA19              199    209  2
     d  decTA20              210    220  2
     d  decTA21              221    231  2
     d  decTA22              232    242  2
     d  decTA23              243    253  2
     d  decTA24              254    264  2
     d  decTA25              265    275  2
     d  decTA26              276    286  2
     d  decTA27              287    297  2
     d  decTA28              298    308  2
     d  decTA29              309    319  2
     d  decTA30              320    330  2
     d  decTA31              331    341  2
     d  decTA32              342    352  2
     d  decTA33              353    363  2
     d  decTA34              364    374  2
     d  decTA35              375    385  2
     d  decTA36              386    396  2
     d  decTA37              397    407  2
     d  decTA38              408    418  2
     d  decTA39              419    429  2
     d  decTA40              430    440  2
     d  decTA41              441    451  2
     d  decTA42              452    462  2
     d  decTA43              463    473  2
     d  decTA44              474    484  2
     d  decTA45              485    495  2
     d  decTA46              496    506  2
     d  decTA47              507    517  2
     d  decTA48              518    528  2
     d  decTA49              529    539  2
     d  decTA50              540    550  2

     d                 ds
     d  tin                    1    550  2
     d                                     Dim(50)
     d  tina                   1    550
     d  decTI01                1     11  2
     d  decTI02               12     22  2
     d  decTI03               23     33  2
     d  decTI04               34     44  2
     d  decTI05               45     55  2
     d  decTI06               56     66  2
     d  decTI07               67     77  2
     d  decTI08               78     88  2
     d  decTI09               89     99  2
     d  decTI10              100    110  2
     d  decTI11              111    121  2
     d  decTI12              122    132  2
     d  decTI13              133    143  2
     d  decTI14              144    154  2
     d  decTI15              155    165  2
     d  decTI16              166    176  2
     d  decTI17              177    187  2
     d  decTI18              188    198  2
     d  decTI19              199    209  2
     d  decTI20              210    220  2
     d  decTI21              221    231  2
     d  decTI22              232    242  2
     d  decTI23              243    253  2
     d  decTI24              254    264  2
     d  decTI25              265    275  2
     d  decTI26              276    286  2
     d  decTI27              287    297  2
     d  decTI28              298    308  2
     d  decTI29              309    319  2
     d  decTI30              320    330  2
     d  decTI31              331    341  2
     d  decTI32              342    352  2
     d  decTI33              353    363  2
     d  decTI34              364    374  2
     d  decTI35              375    385  2
     d  decTI36              386    396  2
     d  decTI37              397    407  2
     d  decTI38              408    418  2
     d  decTI39              419    429  2
     d  decTI40              430    440  2
     d  decTI41              441    451  2
     d  decTI42              452    462  2
     d  decTI43              463    473  2
     d  decTI44              474    484  2
     d  decTI45              485    495  2
     d  decTI46              496    506  2
     d  decTI47              507    517  2
     d  decTI48              518    528  2
     d  decTI49              529    539  2
     d  decTI50              540    550  2

     d                 ds
     d  scHead                 1   1100
     d                                     Dim(50)
     d  scHeadings             1   1100
     d  scHead01               1     22
     d  scHead02              23     44
     d  scHead03              45     66
     d  scHead04              67     88
     d  scHead05              89    110
     d  scHead06             111    132
     d  scHead07             133    154
     d  scHead08             155    176
     d  scHead09             177    198
     d  scHead10             199    220
     d  scHead11             221    242
     d  scHead12             243    264
     d  scHead13             265    286
     d  scHead14             287    308
     d  scHead15             309    330
     d  scHead16             331    352
     d  scHead17             353    374
     d  scHead18             375    396
     d  scHead19             397    418
     d  scHead20             419    440
     d  scHead21             441    462
     d  scHead22             463    484
     d  scHead23             485    506
     d  scHead24             507    528
     d  scHead25             529    550
     d  scHead26             551    572
     d  scHead27             573    594
     d  scHead28             595    616
     d  scHead29             617    638
     d  scHead30             639    660
     d  scHead31             661    682
     d  scHead32             683    704
     d  scHead33             705    726
     d  scHead34             727    748
     d  scHead35             749    770
     d  scHead36             771    792
     d  scHead37             793    814
     d  scHead38             815    836
     d  scHead39             837    858
     d  scHead40             859    880
     d  scHead41             881    902
     d  scHead42             903    924
     d  scHead43             925    946
     d  scHead44             947    968
     d  scHead45             969    990
     d  scHead46             991   1012
     d  scHead47            1013   1034
     d  scHead48            1035   1056
     d  scHead49            1057   1078
     d  scHead50            1079   1100

     d                 ds
     d  scFactors              1   1100
     d  scfactor01             1     22
     d  scfactor02            23     44
     d  scfactor03            45     66
     d  scfactor04            67     88
     d  scfactor05            89    110
     d  scfactor06           111    132
     d  scfactor07           133    154
     d  scfactor08           155    176
     d  scfactor09           177    198
     d  scfactor10           199    220
     d  scfactor11           221    242
     d  scfactor12           243    264
     d  scfactor13           265    286
     d  scfactor14           287    308
     d  scfactor15           309    330
     d  scfactor16           331    352
     d  scfactor17           353    374
     d  scfactor18           375    396
     d  scfactor19           397    418
     d  scfactor20           419    440
     d  scfactor21           441    462
     d  scfactor22           463    484
     d  scfactor23           485    506
     d  scfactor24           507    528
     d  scfactor25           529    550
     d  scfactor26           551    572
     d  scfactor27           573    594
     d  scfactor28           595    616
     d  scfactor29           617    638
     d  scfactor30           639    660
     d  scfactor31           661    682
     d  scfactor32           683    704
     d  scfactor33           705    726
     d  scfactor34           727    748
     d  scfactor35           749    770
     d  scfactor36           771    792
     d  scfactor37           793    814
     d  scfactor38           815    836
     d  scfactor39           837    858
     d  scfactor40           859    880
     d  scfactor41           881    902
     d  scfactor42           903    924
     d  scfactor43           925    946
     d  scfactor44           947    968
     d  scfactor45           969    990
     d  scfactor46           991   1012
     d  scfactor47          1013   1034
     d  scfactor48          1035   1056
     d  scfactor49          1057   1078
     d  scfactor50          1079   1100

     d                 ds
     d  scWrtPremium           1    550
     d  scWrtPrm01             1     11  2
     d  scWrtPrm02            12     22  2
     d  scWrtPrm03            23     33  2
     d  scWrtPrm04            34     44  2
     d  scWrtPrm05            45     55  2
     d  scWrtPrm06            56     66  2
     d  scWrtPrm07            67     77  2
     d  scWrtPrm08            78     88  2
     d  scWrtPrm09            89     99  2
     d  scWrtPrm10           100    110  2
     d  scWrtPrm11           111    121  2
     d  scWrtPrm12           122    132  2
     d  scWrtPrm13           133    143  2
     d  scWrtPrm14           144    154  2
     d  scWrtPrm15           155    165  2
     d  scWrtPrm16           166    176  2
     d  scWrtPrm17           177    187  2
     d  scWrtPrm18           188    198  2
     d  scWrtPrm19           199    209  2
     d  scWrtPrm20           210    220  2
     d  scWrtPrm21           221    231  2
     d  scWrtPrm22           232    242  2
     d  scWrtPrm23           243    253  2
     d  scWrtPrm24           254    264  2
     d  scWrtPrm25           265    275  2
     d  scWrtPrm26           276    286  2
     d  scWrtPrm27           287    297  2
     d  scWrtPrm28           298    308  2
     d  scWrtPrm29           309    319  2
     d  scWrtPrm30           320    330  2
     d  scWrtPrm31           331    341  2
     d  scWrtPrm32           342    352  2
     d  scWrtPrm33           353    363  2
     d  scWrtPrm34           364    374  2
     d  scWrtPrm35           375    385  2
     d  scWrtPrm36           386    396  2
     d  scWrtPrm37           397    407  2
     d  scWrtPrm38           408    418  2
     d  scWrtPrm39           419    429  2
     d  scWrtPrm40           430    440  2
     d  scWrtPrm41           441    451  2
     d  scWrtPrm42           452    462  2
     d  scWrtPrm43           463    473  2
     d  scWrtPrm44           474    484  2
     d  scWrtPrm45           485    495  2
     d  scWrtPrm46           496    506  2
     d  scWrtPrm47           507    517  2
     d  scWrtPrm48           518    528  2
     d  scWrtPrm49           529    539  2
     d  scWrtPrm50           540    550  2

     d                 ds
     d  scInfPremium           1    550
     d  scInfPrm01             1     11  2
     d  scInfPrm02            12     22  2
     d  scInfPrm03            23     33  2
     d  scInfPrm04            34     44  2
     d  scInfPrm05            45     55  2
     d  scInfPrm06            56     66  2
     d  scInfPrm07            67     77  2
     d  scInfPrm08            78     88  2
     d  scInfPrm09            89     99  2
     d  scInfPrm10           100    110  2
     d  scInfPrm11           111    121  2
     d  scInfPrm12           122    132  2
     d  scInfPrm13           133    143  2
     d  scInfPrm14           144    154  2
     d  scInfPrm15           155    165  2
     d  scInfPrm16           166    176  2
     d  scInfPrm17           177    187  2
     d  scInfPrm18           188    198  2
     d  scInfPrm19           199    209  2
     d  scInfPrm20           210    220  2
     d  scInfPrm21           221    231  2
     d  scInfPrm22           232    242  2
     d  scInfPrm23           243    253  2
     d  scInfPrm24           254    264  2
     d  scInfPrm25           265    275  2
     d  scInfPrm26           276    286  2
     d  scInfPrm27           287    297  2
     d  scInfPrm28           298    308  2
     d  scInfPrm29           309    319  2
     d  scInfPrm30           320    330  2
     d  scInfPrm31           331    341  2
     d  scInfPrm32           342    352  2
     d  scInfPrm33           353    363  2
     d  scInfPrm34           364    374  2
     d  scInfPrm35           375    385  2
     d  scInfPrm36           386    396  2
     d  scInfPrm37           397    407  2
     d  scInfPrm38           408    418  2
     d  scInfPrm39           419    429  2
     d  scInfPrm40           430    440  2
     d  scInfPrm41           441    451  2
     d  scInfPrm42           452    462  2
     d  scInfPrm43           463    473  2
     d  scInfPrm44           474    484  2
     d  scInfPrm45           485    495  2
     d  scInfPrm46           496    506  2
     d  scInfPrm47           507    517  2
     d  scInfPrm48           518    528  2
     d  scInfPrm49           529    539  2
     d  scInfPrm50           540    550  2

     D clientID        s              3    inz('000')

     d* Proto-types

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d* Get the Territorial Differential Factor
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d  outTDF         s              5  4

     DAcceptAgg        pr             1n
     D inEffDt                             like(scLDEfd1) const
     D inAgg                               like(LDAMG1)   const
     D elRRN                          9  0 const options(*nopass)

     d callW1551C      pr                  extpgm('W1551C')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##QUE                       10
     d   ##QTE                        1

     d AddLoad         pr

     dSetCondition1    pr
     d inFlag                         1    const
     d outtam                        11  2
     d outtin                        11  2

     dSetCondition2    pr
     d inFlag                         1    const

     dSetCondition3    pr
     d inFlag                         1    const

     dSetCondition4    pr
     d inFlag                         1    const

     dSetCondition5    pr
     d inFlag                         1    const

     dSetCondition6    pr
     d inFlag                         1    const

     dSetCondition7    pr
     d inFlag                         1    const

     dSetCondition8    pr
     d inFlag                         1    const

     dSetCondition9    pr
     d inFlag                         1    const

     dSetCondition10   pr
     d inFlag                         1    const

     dSetCondition11   pr
     d inFlag                         1    const

     dSetCondition12   pr
     d inFlag                         1    const

     dSetCondition13   pr
     d inFlag                         1    const

     dSetCondition14   pr
     d inFlag                         1    const

     dSetCondition15   pr
     d inFlag                         1    const

     dSetCondition16   pr
     d inFlag                         1    const

     dSetCondition17   pr
     d inFlag                         1    const

     dSetCondition18   pr
     d inFlag                         1    const

     dSetCondition19   pr
     d inFlag                         1    const

     dSetCondition20   pr
     d inFlag                         1    const

     dSetCondition21   pr
     d inFlag                         1    const

     dSetCondition22   pr
     d inFlag                         1    const

     dSetCondition23   pr
     d inFlag                         1    const

     dSetCondition24   pr
     d inFlag                         1    const

     dSetCondition25   pr
     d inFlag                         1    const

     dSetCondition26   pr
     d inFlag                         1    const

     dSetCondition27   pr
     d inFlag                         1    const

     dSetCondition28   pr
     d inFlag                         1    const

     dSetCondition29   pr
     d inFlag                         1    const

     dSetCondition30   pr
     d inFlag                         1    const

     dSetCondition31   pr
     d inFlag                         1    const

     dSetCondition32   pr
     d inFlag                         1    const

     dSetCondition33   pr
     d inFlag                         1    const

     dSetCondition34   pr
     d inFlag                         1    const

     dSetCondition35   pr
     d inFlag                         1    const

     dSetCondition36   pr
     d inFlag                         1    const

     dSetCondition37   pr
     d inFlag                         1    const

     dSetCondition38   pr
     d inFlag                         1    const

     dSetCondition39   pr
     d inFlag                         1    const

     dSetCondition40   pr
     d inFlag                         1    const

     dSetCondition41   pr
     d inFlag                         1    const

     dSetCondition42   pr
     d inFlag                         1    const

     dSetCondition43   pr
     d inFlag                         1    const

     dSetCondition44   pr
     d inFlag                         1    const

     dSetCondition45   pr
     d inFlag                         1    const

     dSetCondition46   pr
     d inFlag                         1    const

     dSetCondition47   pr
     d inFlag                         1    const

     dSetCondition48   pr
     d inFlag                         1    const

     dSetCondition49   pr
     d inFlag                         1    const

     dSetCondition50   pr
     d inFlag                         1    const

     dSetCondition51   pr
     d inFlag                         1    const

     dSetCondition52   pr
     d inFlag                         1    const

     dSetCondition53   pr
     d inFlag                         1    const

     dSetCondition54   pr
     d inFlag                         1    const

     dSetCondition55   pr
     d inFlag                         1    const

     dSetCondition56   pr
     d inFlag                         1    const

     dSetCondition57   pr
     d inFlag                         1    const

     dSetCondition58   pr
     d inFlag                         1    const

     dSetCondition59   pr
     d inFlag                         1    const

     dSetCondition60   pr
     d inFlag                         1    const

     dSetCondition61   pr
     d inFlag                         1    const

     dSetCondition62   pr
     d inFlag                         1    const

     D*--------------------------------------------------
     D* Procedure name: SetCondition63
     D* Purpose:        Display Subject premium amounts
     D* Returns:
     D*--------------------------------------------------
     D SetCondition63  pr
     d inFlag                         1    const

     dSetCondition64   pr

     dGetOccdAndNrat   pr

     dSetCondition65   pr

     dSetCondition66   pr

1AA  dSetCondition67   pr
     d inFlag                         1    const

2AA  dSetCondition68   pr
     d inFlag                         1    const

3AA  dSetCondition69   pr
     d inFlag                         1    const

4AA  dSetCondition70   pr
     d inFlag                         1    const

5AA  dSetCondition71   pr
     d inFlag                         1    const

6AA  dSetCondition72   pr
     d inFlag                         1    const

7AA  dSetCondition73   pr
     d inFlag                         1    const

SF   dSetCondition74   pr
     d inFlag                         1    const

MF   dSetCondition75   pr
     d inFlag                         1    const

MF   dSetCondition76   pr
     d inFlag                         1    const

      *** Beg Add ***** 02/27/24 ************************************
     dSetCondition77   pr
     d inFlag                         1    const

     dSetCondition78   pr
      *** Beg Add ***** 11/21/25 ************************************
     d inFlag                         1    const
      *** End Add ***** 11/21/25 ************************************
      *** End Add ***** 02/27/24 ************************************
      *** Beg Add ***** 10/14/24 ************************************
     dSetCondition79   pr
      *** End Add ***** 10/14/24 ************************************

     d WritePolicyFee  pr
     d  inFlag                        1a   Const

     d* Effective Date
     d SrEffd          pr             7  0
     d inCo#                          3  0
     d inFnd                          3  0
     d inFyr                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0
     d inDate                         7  0
     d inType                         1    options(*nopass)
     d inClas                         5  0 options(*nopass)
     d GetDev1CrWst    pr                  extpgm('WA060R')
     d intRrn#                        9  0
     d intFctr                        5  4

     D/copy sPrDate
     D/copy sPrString
     D/copy sPrUsrPrms
     D/copy WprTable
     D/copy wPrRateSet
     D/copy wPrEmplr
     D/copy wPrAgent
     d/copy WPRPOLPOP
     d/copy WPRPOLERR
     d/copy sPrGenPop
     d/copy wPrDivPlan
     d/copy wPrGroup
     d/copy wPrProduce
     d/copy wPrEndorse
      //*** Beg Add *** 10/25/23 *** WC2922 ****************
     d/copy wPrPmtSvc
      //*** End Add *** 10/25/23 *** WC2922 ****************
      /Free
       Dcl-Proc AddWaiverToHdg;
       Dcl-Pi *N Char(22);
        inHdg Char(22);
       End-Pi;
       Dcl-S intLenWvVbg Zoned(1:0);
       Dcl-S intWvCnt Zoned(3:0);
       Dcl-S strWvVbg Char(7);
       If ELwvtp = 'B';
          Return inHdg;
       Endif;
       Select;
       When ELwvpc <> 0;
          %Subst(inHdg:20:3) = '(%)';
       When ELfchg <> 0;
          Setll (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pwv01;
          Dou %Eof(WMWVL1);
             Reade (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pwv01;
             If %Eof;
                Leave;
             Endif;
             intWvCnt += 1;
          Enddo;
          strWvVbg = '(' + %Trim(%EditC(intWvCnt:'Z')) + ')';
          intLenWvVbg = %Len(%Trim(strWvVbg));
          %Subst(inHdg:23 - intLenWvVbg:intLenWvVbg) = %Trim(strWvVbg);
       EndSl;
       Return inHdg;
       End-Proc;
      /End-Free
       //===================================================================
       // Payroll Edit Popup
       //===================================================================
     P PayrollEdit     B                   export
     D PayrollEdit     PI
     D epRRN                          9  0 const
     D inRTCD                         3    const
     D elRRN                          9  0 const options(*nopass)
     D inMode                         1    const options(*nopass)

     D scFnd           s                   like(ELFnd)
     D scEmp#          s                   like(ELEmp#)
     D xxSt            s              2
     D xxPxSt          s              2
     D xxType          s              1
     D xxKey           s                   like(BSKEY)
     D xxDate          s                   like(ELPFDT)
     D xxEfDt          s                   like(ELPFDT)
     D x2EfDt          s                   like(ELPFDT)
     D x3EfDt          s                   like(ELPFDT)
     D payError        s               n   inz(*off)
     D payAddMode      s               n   inz(*off)
     D deleteMode      s               n   inz(*off)
     D auditMode       s               n   inz(*off)
     D iOpen           s               n   inz(*off)

     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    EPCo#
     C                   Kfld                    EPFnd
     C                   Kfld                    EPEmp#
     C                   Kfld                    EPDiv
     C                   Kfld                    EPFyr
     C*
     C     key1WDELL     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scPayEDiv
     C                   Kfld                    scPayEFyr
     C*
     C* Employer Payroll File Key List
     C*
     C     keyWMEPL      Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scEmp#
     C                   Kfld                    scPayEDiv
     C                   Kfld                    scPayEFyr
     C*
     C     keyWMEPL1E    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scFnd
     C                   Kfld                    scPayEFyr
     C                   Kfld                    scEmp#
     C                   Kfld                    scPayEDiv
     C                   Kfld                    scPayEClas
     C                   Kfld                    scPayESpec
     C                   Kfld                    scPayEExt
     C                   Kfld                    xxEfDt
     C*
     C* Payroll Class File Key List
     C*
     C     keyWMPRL1     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EPClas
     C                   Kfld                    xxEfDt
     C     key1WMPRL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    EPClas
     C*
     C     key2WMPRL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    scPayECLAS
     C                   Kfld                    x2EfDt
     C     key3WMPRL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    ELRTCD
     C                   Kfld                    scPayECLAS
     C*
     C     key4WMPRL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    ELRTCD
     C*
     C* Payroll Description File Key List
     C*
     C     keyWMPXL1     Klist
     C                   Kfld                    scCo#
     C                   Kfld                    EPClas
     C                   Kfld                    EPExt
     C                   Kfld                    xxPxSt
     C                   Kfld                    EPEfDt
     C     key1WMPXL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    EPClas
     C                   Kfld                    EPExt
     C                   Kfld                    xxPxSt
     C*
     C     key2WMPXL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scPayECLAS
     C                   Kfld                    scPayEEXT
     C                   Kfld                    xxPxSt
     C                   Kfld                    x3EfDt
     C     key3WMPXL1    Klist
     C                   Kfld                    scCo#
     C                   Kfld                    scPayECLAS
     C                   Kfld                    scPayEEXT
     C                   Kfld                    xxPxSt
      /free

       Open WPIPOLPOP;
       If not %Open(wmepp);
         Open wmepp;
         iOpen = *On;
       Endif;

       GetClientID(clientID);

       payEditMod = *off;
       payAddMode = *off;

       If  epRRN <> 0;
          Chain(n) epRRN PEP0P;
          Chain keyWDELL PEL00;
          scELRRN = dbrrn;
          payEditMod = *on;
          scNextText = 'Save / Add New';
       Else;
          Chain(n) elRRN PEL0P;
          scELRRN = elRRN;
          dbrrn   = elRRN;
          payAddMode = *on;
          scNextText = 'Save / Add Next';
          EPclas = *zeros;
          EPext  = *blanks;
          EPspec = *blank;
          EPAapr = *zeros;
       Endif;
       //*** begin add ***  10/21/25  ***************************************
       Chain(n) (ELCo#:ELRtcd) prs00;
       //*** end add *****  10/21/25  ***************************************

       scCo#  = ELCo#;
       scFnd# = ELFnd;
       scFnd  = ELFnd;
       scEmp# = ELEmp#;
       scPayEDiv = ELDiv;
       scPayEFyr = ELFyr;

       payFyrMode = *on;
       payDivMode = *on;

       If %parms >= 4;
          // Add new class and allow selection of division
          If inMode = 'A';
             payDivMode = *off;
          Endif;
          // Add new class at audit (allow audit entry)
          If inMode = 'B';
             auditMode = *on;
          Endif;
          If inMode = 'D';
             deleteMode = *on;
             scDelTitle = 'Payroll Record Deletion';
             scDelLeft = 560;
             scDelTop = 325;
          Endif;
       Endif;

       If payAddMode;
          payTitle = 'Payroll Add';
       Else;
          payTitle = 'Payroll Edit';
       Endif;
       isContribution = GetPrmContflg(scCO#:scFND);
       If isContribution;
          flxDtlHd1 = 'Estimated Manual Contribution';
          flxDtlHd2 = 'Audited Manual Contribution';
       Else;
          flxDtlHd1 = 'Estimated Manual Premium';
          flxDtlHd2 = 'Audited Manual Premium';
       Endif;

       Exsr SetCriteria;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou payAddMode = *off;

       //* Populate the popup window with selected record.

          If payEditMod = *on;

             scPayEEFDT =  dtMDYY(EPEfDT);
             scPayECADT =  dtMDYY(EPCaDt);
             scPayECLAS =  EPClas;
             scPayESPEC =  EPSpec;
             scPayEEXT  =  EPExt;
             scPayESTAT =  EPStat;

             scClas     =  EPClas;

             If EPSAFL = 'A';
                scPayEPaSt = 'Audited';
                payEstimat = *off;
             Else;
                scPayEPaSt = 'Estimated';
                payEstimat = *on;
             Endif;
             scPayEARAT =  EPArat;
             scPayEEAPR =  EPEapr;
             scPayEEMPR =  EPEmpr;
             scPayEECNT =  EPEcnt;
             scPayEAAPR =  EPAapr;
             scPayEAMPR =  EPAmpr;
             scPayEACNT =  EPAcnt;
             scPayESAPR =  EPSapr;
             scPayEADLD =  EPAdld;
             isAirDesF = *On;
             intEffDt = dtCYMD(scPayEEFDT);
             SetGt (scCo#:scFnd#:scJRST:intEffDt) prr00;
             Readpe (scCo#:scFnd#:scJRST) prr00;
             If Not %EOF(wdrrl);
                If scPayECLAS = RRdccd;
                   isAirDesF  = *Off;
                   scAcDs = EPacds;
                Else;
                   isAirDesF  = *On;
                EndIf;
             EndIf;
             Exsr SetDescription;

          Else;
             scPayEEFDT =  dtMDYY(ELPFDT);
             scPayEStat = 'Active';
             scPayEPaSt = 'Estimated';
             payEstimat = *on;

             If auditMode;
                scPayEPaSt = 'Audited';
                payEstimat = *off;
             Endif;

             Clear scPayECADT;
             Clear scPayECLAS;
             Clear scPayESPEC;
             Clear scPayEEXT;
             Clear scPayEARAT;
             Clear scPayEEAPR;
             Clear scPayEEMPR;
             Clear scPayEECNT;
             Clear scPayEAAPR;
             Clear scPayEAMPR;
             Clear scPayEACNT;
             Clear scPayESAPR;
             Clear scPayEADLD;
             Clear scPayEDesc;
             Clear scPayEHZGR;
             Clear scAcDs;
             isAirDesf = *On;
          Endif;

       //* Clear errors and buttons.
          Exsr ClearErrors;
          Clear payEditSav;
          Clear payEditCnl;
          Clear payEditAdd;
          Clear deleteBtn;
          Clear cancelBtn;
          Clear divChange;
          Clear yearChange;

       //* Display popup window for Deletion mode.
          If deleteMode;

             write rcdPayEdt;

             Dou deleteBtn = *on or cancelBtn = *on;
                Exfmt rcdDelete;
             Enddo;

             If deleteBtn = *on;
                Chain epRRN PEP0P;
                If %found;
                   Delete PEP0P;
                Endif;
             Endif;

          Else;
       //* Display popup window for Add or Edit mode.

             Dou payEditSav = *on or payEditCnl = *on or payEditAdd = *on;

                Dou payError = *off or payEditCnl = *on;

                   Clear pxChoices;
                   If scPayEClas <> 0;
                      Exsr SetPXCriteria;
                      Exsr SetDescription;
                   Endif;
                   If scJRST = 'CA' And PayAddMode = *On;
                      isScrImg3 = *On;
                   Else;
                      isScrImg3 = *Off;
                   EndIf;

                     scPF1rrn = dbrrn;
                   Exfmt rcdPayEdt;

                   Exsr ClearErrors;

                   If divChange = *on or yearChange = *on;
                      Chain(n) key1WDELL PEL00;
                      If not %found;
                         erPayEDiv2 = *on;
                         msPayEDiv2 = 'This division does not exist +
                          for the policy year.';
                      Else;
                         Exsr SetCriteria;
                      Endif;
                      Clear divChange;
                      Clear yearChange;
                      payError = *on;
                   Else;
                      If payEditCnl = *off;
                         Exsr PayrollCheck;
                      Endif;
                   Endif;

                Enddo;

             Enddo;

             If payEditSav = *on or payEditAdd = *on;
                Exsr PayrollSave;
             Endif;

          Endif;

          If payEditAdd = *on and payAddMode = *off;
             scNextText = 'Save / Add Next';
             payTitle = 'Payroll Add';
             payAddMode = *on;
             payEditMod = *off;
          Endif;

          If payEditCnl = *on or payEditSav = *on
           or deleteBtn = *on or cancelBtn = *on;
             payAddMode = *off;
          Endif;

       Enddo;

       If %Open(wmepp) And iOpen;
         Close wmepp;
         iOpen = *Off;
       Endif;
       Close WPIPOLPOP;

       //===================================================================
       // Set dropdown/text box selection criteria
       //===================================================================
       BegSr SetCriteria;

       scExpDate = ELPTDT;
       scRTCD = ELRTCD;
       scJRST = GetRatingState(scCo#:scFnd:scEmp#:scPayEDiv:scPayEFyr);

       arate = *on;
       If ELStat = 'Y';
          arate = *off;
       Endif;

       xxSt = RatingSt(ELCO#:ELRTCD);

       Clear scPayECLAS;
       Clear scPayESPEC;
       Clear scPayEEXT;
       Clear scPayEARAT;
       Clear scPayEDesc;
       Clear scPayEHZGR;

       Clear divChoices;
       divChoices = 'EMCO# = ' + %char(ELCO#) + ' AND EMFND = '
        + %char(ELFND) + ' AND EMEMP# = ' + %char(ELEMP#);

       Clear fyrChoices;
       fyrChoices = 'ELCO# = ' + %char(ELCO#) + ' AND ELFND = '
        + %char(ELFND) + ' AND ELEMP# = ' + %char(ELEMP#)
        + ' AND ELDIV = ' + %char(ELDIV);

       Clear clsChoices;
       clsChoices = 'PRCO# = ' + %char(ELCO#) + ' AND PRRTCD = ''' + ELRTCD +
        ''' AND PREFDT = (SELECT MAX(A.PREFDT) FROM WMPRP A WHERE A.PRCO# = '
        + %char(ELCO#) + ' AND A.PRRTCD = ''' + ELRTCD + ''' AND A.PREFDT <= '
        + %char(ELPFDT) + ' AND PRSTAT = ''A'' AND A.PRCLAS = WMPRL1.PRCLAS)';

       Clear pxChoices;

       EndSr;

       //===================================================================
       // Set Extension dropdown selection criteria
       //===================================================================
       BegSr SetPXCriteria;

       pxChoices = 'PXCO# = ' + %char(ELCO#) + ' AND PXCLAS = ' +
        %char(scPayEClas) + ' AND (PXST = ''  '' OR PXST = ''' + scJRST +
       ''' )  AND PXEFDT <=  ' + %char(xxEfDt)  +
           ' AND ( PXEXDT = 0  OR  PXEXDT > ' + %char(xxEfDt) +')' ;

       EndSr;
       //===================================================================
       // Set Description
       //===================================================================
       BegSr SetDescription;

       Clear scPayEDesc;
       xxType = 'P';
       xxEfDt = SrEffd(ELCo#:ELFnd:ELFyr:ELEmp#:ELDiv:EPEfDt:xxType:
        EPClas);
       Setgt keyWMPRL1 PPR01;
       Readpe key1WMPRL1 PPR01;
       If not %eof;
          scPayEDesc = PRDesc;
          scPayEHZGR =  PRHzgr;
          arate = *on;
          If PRArfl = 'Y' and ELStat = 'Y';
             arate = *off;
          Endif;
          If EPExt <> *blanks;
             Clear xxPxSt;
             Setgt keyWMPXL1 PPX01;
             Readpe key1WMPXL1 PPX01;
             If %found;
                scPayEDesc = PXDesc;
             Else;
                xxPxSt = xxSt;
                Setgt keyWMPXL1 PPX01;
                Readpe key1WMPXL1 PPX01;
                If %found;
                   scPayEDesc = PXDesc;
                Endif;
             Endif;
          Endif;
       Endif;

       EndSr;
       //===================================================================

      /end-free

       //***************************************************************************//
       //===================================================================
       // Payroll Error Checking
       //===================================================================
     C     PayrollCheck  Begsr
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C     Dtcymdp       Plist
     C                   Parm                    Srdti
     C                   Parm      *BLANKS       Srffmt
     C                   Parm      '*CYMD'       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
     C*
     C     Dtmdyyp       Plist
     C                   Parm                    Srdti
     C                   Parm      '*CYMD'       Srffmt
     C                   Parm      *BLANKS       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
      *
     C                   Exsr      ClearErrors
      *
      * Validate policy year exists for division.
      *
     C                   If        payDivMode = *off
      *
     C     key1WDELL     Chain     PEL00
      *
     C                   If        not %found
      *
     C                   eval      payError = *on
     C                   eval      erPayEDiv2 = *on
     C                   eval      msPayEDiv2 = 'This division does not exist +
     C                              for the policy year.'
      *
     c                   Endif
      *
     c                   Endif
      *
     c                   If        erPayEDiv2 = *off
      *
      *  VALIDATE EFFECTIVE DATE
      *
     C                   Z-ADD     scPayEEFDT    SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scPayEEFDT
      *
     C                   CALL      'SRCVTDAT'    Dtcymdp
     C                   MOVEL     SRDTO         xxEfDt
      *
     C     SRERR         IFEQ      *ON
     C                   eval      payError = *on
     C                   eval      erPayEEFDT = *on
     C                   eval      msPayEEFDT = 'WCR0500'
      *
     C                   ELSE
      *
     C     xxEFDT        IFLT      ELPFDT
     C     xxEFDT        ORGT      ELPTDT
     C                   eval      payError = *on
     C                   eval      erPayEEFDT = *on
     C                   eval      msPayEEFDT = 'WCR0578'
     C                   ENDIF                                                  ELPFDT>XXEFDT>ELPTDT
      *
     C                   ENDIF                                                  SRERR
      *
      *  VALIDATE CLASS CODE
      *
     C                   Z-ADD     0             X2EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     scPayEEFDT    SRDTI
     C                   CALL      'SRCVTDAT'    dtcymdp
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   scPayECLAS    #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      #1DATE        X2EFDT
     C     key2WMPRL1    SETGT     PPR01
     C     key3WMPRL1    READPE    PPR01                                  60
      *
     C                   move      *on           arate
     C                   If        *in60 = *off and PRARFL = 'Y'
     C                             and ELSTAT = 'Y'
     C                   move      *off          arate
     c                   Endif
      *
     C     *IN60         IFEQ      *ON
     C                   eval      payError = *on
     C                   eval      erPayECLAS = *on
     C                   eval      msPayECLAS = 'WC30289'
     C                   ENDIF                                                  *IN60=*ON
      *
      * If have Payroll Class and it is inactive, is error.
     C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'I'
     C                   eval      payError = *on
     C                   eval      erPayECLAS = *on
     C                   eval      msPayECLAS = 'WC32391'
     C                   ENDIF                                                  *IN60=*ON
      *
      * Emp payroll eff dt < payroll expiration dt, in ADD mode
      *
     C                   If        payAddMode and *in60 = *off
      *
     C     PREXDT        IFNE      *ZERO
     C     X2EFDT        ANDGE     PREXDT
      *
     C                   Z-ADD     PREXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYYp
     C                   MOVEL     SRDTO         scEXDT            8 0
      *
     C                   eval      payError = *on
     C                   eval      erPayECls2 = *on
     C                   eval      msPayECls2 = 'This class code has expired +
     C                              as of ' + alphaUSA(cvttodate(scexdt)) +
     C                              '.'
     c                   If        prxfer <> 0
     c                   eval      msPayECls2 = %trimr(msPayECls2) +
     c                              '  The class code has been transferred +
     c                              to the new class code ' +
     c                              %char(prxfer) + '.'
     c                   Endif
      *
     c                   ELSE
      *
      *  CHECK TO BE SURE CLASS CODE HAS NOT BEEN USED PREVIOUSLY
      *
     C     keyWMEPL1E    CHAIN     PEP01E                             60
     C
     C                   If        *in60 = *off
     C                   eval      payError = *on
     C                   eval      erPayECLAS = *on
     C                   eval      msPayECLAS = 'WCR0697'
     C                   ENDIF                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  PREXDT IFNE
      *
     C                   ENDIF                                                  If payAddMode
      *
      *
      *  EXTENSION MUST BE VALID.
      *
     C     scPayEEXT     IFNE      *BLANKS
     C                   Z-ADD     0             X3EFDT
     C                   MOVE(P)   ELCO#         #1CO#             3
     C                   MOVE(P)   ELFND         #1FND             3
     C                   MOVE(P)   ELFYR         #1FYR             3
     C                   MOVE(P)   ELEMP#        #1EMP#            9
     C                   MOVE(P)   ELDIV         #1DIV             5
     C                   Z-ADD     scPayEEFDT    SRDTI
     C                   CALL      'SRCVTDAT'    dtcymdp
     C                   MOVEL(P)  SRDTO         #1DATE            7
     C                   MOVE(P)   'P'           #1TYPE            1
     C                   MOVE(P)   scPayECLAS    #1CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    #1CO#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1EMP#
     C                   PARM                    #1DIV
     C                   PARM                    #1DATE
     C                   PARM                    #1TYPE
     C                   PARM                    #1CLAS
      *
     C                   MOVE      *BLANKS       xxPxSt
     C                   MOVE      #1DATE        X3EFDT
     C     key2WMPXL1    SETGT     PPX01
     C     key3WMPXL1    READPE    PPX01                                  60
      *
     C                   If        *in60 = *on
     C                   MOVE      xxst          xxPxSt
     C     key2WMPXL1    SETGT     PPX01
     C     key3WMPXL1    READPE    PPX01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   eval      payError = *on
     C                   eval      erPayEEXT  = *on
     C                   eval      msPayEEXT  = 'WCR0513'
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     *IN60         IFEQ      *OFF
     C     PXSTAT        ANDEQ     'I'
     C                   eval      payError = *on
     C                   eval      erPayEEXT  = *on
     C                   eval      msPayEEXT  = 'WCR0690'
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDIF                                                  If *in60 = *on
     C*
     C                   ENDIF                                                  scPayEExt <> *blanks
      /Free
       If (xxSt = 'CA' Or  xxSt = 'WA') And PayAddMode = *On;

          Open wmpxl3;

          Setll (scCo#:scPayECLAS:xxSt) ppx03;
          If %Equal(wmpxl3);

             Chain (scCo#:scPayECLAS:xxSt:scPayEEXT) ppx03;
             If Not %Found(wmpxl3);
                payError = *On;
                erPayEEXT = *On;
                msPayEEXT = 'WCR4812';
             EndIf;

          EndIf;

          Close wmpxl3;
       Endif;
      /End-Free
      *
      *  ESTIMATED AMOUNT CANNOT BE LESS THAN ZERO
      *
     C     scPayEEAPR    IFLT      *ZERO
     C                   eval      payError = *on
     C                   eval      erPayEEAPR = *on
     C                   eval      msPayEEAPR = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  AUDITED AMOUNT CANNOT BE LESS THAN ZERO
      *
     C     scPayEAAPR    IFLT      *ZERO
     C                   eval      payError = *on
     C                   eval      erPayEAAPR = *on
     C                   eval      msPayEAAPR = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  ESTIMATED # OF EMPLOYEES CANNOT BE LESS THAN ZERO
      *
     C     scPayEECNT    IFLT      *ZERO
     C                   eval      payError = *on
     C                   eval      erPayEECNT = *on
     C                   eval      msPayEECNT = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  AUDITED # OF EMPLOYEES CANNOT BE LESS THAN ZERO
      *
     C     scPayEACNT    IFLT      *ZERO
     C                   eval      payError = *on
     C                   eval      erPayEACNT = *on
     C                   eval      msPayEACNT = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  ADDITIONAL LOAD CANNOT BE LESS THAN ZERO
      *
     C     scPayEADLD    IFLT      *ZERO
     C                   eval      payError = *on
     C                   eval      erPayEADLD = *on
     C                   eval      msPayEADLD = 'WCR0547'
     C                   ENDIF                                                  SCEAPR<*ZERO
      *
      *  VALIDATE CANCEL DATE, IF ENTERED
      *
     C                   Z-ADD     scPayECADT    SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         scPayECADT
      *
     C     SRERR         IFEQ      *ON
     C     scPayECADT    ANDNE     *ZEROS
     C                   eval      payError = *on
     C                   eval      erPayECADT = *on
     C                   eval      msPayECADT = 'WCR0500'
     C                   ENDIF                                                  SRERR=*ON
      *
     c                   Endif                                                  If erPayEDiv2 = *off
      *
      *  AirCraft Description
      /Free
       If payAddMode = *On and scPayECLAS <> *Zeros;
       isAirDesF = *On;
       intEffDt = dtCYMD(scPayEEFDT);
       SetGt (scCo#:scFnd#:scJRST:intEffDt) prr00;
       Readpe (scCo#:scFnd#:scJRST) prr00;
       If Not %EOF(wdrrl);
          If scPayECLAS = RRdccd;
             isAirDesF  = *Off;
          Else;
             isAirDesF  = *On;
          EndIf;
       EndIf;
       Endif;
       If isAirDesF = *Off and scAcDs = *Blanks;
          payError   = *on;
          erAirDsId  = *on;
          msAirDsId  = 'WCR0503';
       EndIf;

       If (clientId ='150' and PRpcf = 'C' and payError = *off) and
          (SCpayeclas <> EPclas or SCpayespec <> EPspec or
           SCpayeext <> EPext or scPayEEAPR <> EPEapr);
          strMsg = 'A per capita class code ' + %Editc(SCpayECLAS:'X') +
          ' ' + %Trim(SCpayESPEC) + ' ' + %Trim(SCpayEEXT) +
          ' has been added/edited. Please confirm that exposure ' +
          %Trim(%Editc(SCpayEEAPR:'J')) +
          ' was entered as employee count and not payroll. ' +
          'Press Cancel to return to payroll screen to edit amount.';

          If PopTwoBtn(strmsg:'Confirm Per Capita Class':
             'Cancel':'Save');
             payError = *on;
          Endif;
       Endif;
      /End-Free
      *
     C                   ENDSR                                                  PayrollCheck

       //===================================================================
       // Save Payroll Data.
       //===================================================================
     C     PayrollSave   Begsr
      /free

       If payEditMod = *on;
          Chain epRRN PEP0P;
       Else;
          Clear PEP0P;
          EPCO#  = ELCO#;
          EPFND  = ELFND;
          EPEMP# = ELEMP#;
          EPDIV  = ELDIV;
          EPFYR  = ELFYR;
          EPSTAT = 'A';
          EPSAFL = 'E';
          EPIPGM = Q1PGM;
          EPIUSR = Q1USER;
          EPITM  = Timeto6(SystemTime());
          EPIDT  = Dateto7(SystemDate());
       Endif;

       EPEFDT = dtCYMD(scPayEEFDT);
       EPCADT = dtCYMD(scPayECADT);
       EPCLAS = scPayECLAS;
       EPSPEC = scPayESPEC;
       EPEXT  = scPayEEXT;
       EPSTAT = scPayESTAT;

       If arate = *off;
             EPARAT = scPayEARAT;
       Else;
          xxType = 'P';
          xxEfDt = SrEffd(ELCo#:ELFnd:ELFyr:ELEmp#:ELDiv:EPEfDt:xxType:
           EPClas);
          Setgt keyWMPRL1 PPR01;
          Readpe key1WMPRL1 PPR01;
          If not %eof;
             Exsr srLCM;
             EPARAT = PRRT;
          Endif;
       Endif;

       EPEAPR = scPayEEAPR;
       EPECNT = scPayEECNT;
       EPAAPR = scPayEAAPR;
       EPACNT = scPayEACNT;
       EPADLD = scPayEADLD;
       If scAcDs <> *Blanks and isAirDesF = *Off;
          EPacds = scAcDs;
       EndIf;

       EPPGM  = Q1PGM;
       EPUSER = Q1USER;
       EPTIME = Timeto6(SystemTime());
       EPDT   = Dateto7(SystemDate());

       If payEditMod = *on;
          Update PEP0P;
       Else;
          Write PEP0P;
       Endif;

      /end-free
     C                   Endsr                                                  PayrollSave
     C**********************************************************************************
     C*
     C* Call SRLCM to update the payroll rate.
     C*
     C**********************************************************************************
     C     srLCM         Begsr
     C*
     C                   Z-ADD     1             XXLCM
     C*
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   EPCLAS        QQCLAS
     C                   MOVE(P)   xxEFDT        QQEFDT
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
       // Get the Territorial Differential Factor
       GetTDF(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:outTDF);
      /end-free
     C*** begin add ***  10/21/25  *******************************************
     C                   Z-add     0             x2arat            7 2
     C* Round/calculate payroll rate to 2 digits if not approved yet for 3.
     C                   If        RS3Date = 0 or xxEfdt < RS3Date
     C                   If        outTDF = 0
     C                   Eval(h)   x2arat = xxlcm * prrt
     C                   Else
     C                   Eval(h)   x2arat = outTDF * xxlcm * prrt
     C                   Endif
     C                   Z-add     x2arat        PRRT
     C                   Else
     C*** end add *****  10/21/25  *******************************************
     C                   If        outTDF = 0
     C                   Eval(h)   prrt = xxlcm * prrt
     C                   Else
     C                   Eval(h)   prrt = outTDF * xxlcm * prrt
     C                   Endif
     C*** begin add ***  10/21/25  *******************************************
     C                   Endif
     C*** end add *****  10/21/25  *******************************************
     C*
     C                   Endsr                                                  srLCM

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       payError = *off;

       erPayEDiv  = *off;
       msPayEDiv  = *blanks;
       erPayEDiv2 = *off;
       msPayEDiv2 = *blanks;
       erPayEEFDT = *off;
       msPayEEFDT = *blanks;
       erPayECADT = *off;
       msPayECADT = *blanks;
       erPayESTAT = *off;
       msPayESTAT = *blanks;
       erPayECLAS = *off;
       msPayECLAS = *blanks;
       erPayECls2 = *off;
       msPayECls2 = *blanks;
       erPayESPEC = *off;
       msPayESPEC = *blanks;
       erPayEEXT  = *off;
       msPayEEXT  = *blanks;
       erPayEARAT = *off;
       msPayEARAT = *blanks;
       erPayEEAPR = *off;
       msPayEEAPR = *blanks;
       erPayEECNT = *off;
       msPayEECNT = *blanks;
       erPayEAAPR = *off;
       msPayEAAPR = *blanks;
       erPayEACNT = *off;
       msPayEACNT = *blanks;
       erPayEADLD = *off;
       msPayEADLD = *blanks;

      /end-free
     C                   Endsr                                                  ClearErrors

     P PayrollEdit     E

       //***************************************************************************//
       //===================================================================
       // Excluded States Edit Popup
       //===================================================================
     P ExcludeEdit     B                   export
     D ExcludeEdit     PI
     D xsRRN                          9  0 const
     D elRRN                          9  0 const options(*nopass)

     D xxKey           s                   like(BSKEY)
     D exlError        s               n   inz(*off)

     C*
     C* Excluded States File Key List
     C*
     C     key1WDXSL     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELFYR
     C                   Kfld                    scExlState
     C*
     C* State File Key List
     C*
     C     keySTSTL3     Klist
     C                   Kfld                    xxKey
     C*
      /free

       Open WPIPOLPOP;

       GetClientID(clientID);

       exlDelMode = *off;
       exlAddMode = *off;

       If  xsRRN <> 0;
          Chain(n) xsRRN PXS0P;
          exlDelMode = *on;
       Else;
          Chain(n) elRRN PEL0P;
          exlAddMode = *on;
       Endif;

       //* Keep bringing up new popup windows in Add mode until cancel is hit.
       Dou exlAddMode = *off;

       //* Populate the popup window with selected record.

          If exlDelMode = *on;
             exlBtnDesc = 'Delete';
             scExlState =  XSST;
          Else;
             exlBtnDesc = 'Save';
             Clear scExlState;
          Endif;

          Exsr ClearErrors;
          Clear exlEditSav;
          Clear exlEditCnl;
          Clear exlEditAdd;

       //* Display popup window for the Excluded states record.

          Dou exlEditSav = *on or exlEditCnl = *on or exlEditAdd = *on;

             Dou exlError = *off or exlEditCnl = *on;

                //***** Beg Chg ****** 03/23/22 *************************
                  //***scPF1rrn = dbrrn;
                  If exlDelMode <> *on;
                     scPF1rrn = ELrrn;
                  Endif;
                //***** End Chg ****** 03/23/22 *************************
                Exfmt rcdExclude;

                If exlEditCnl = *off;
                   Exsr ExcludeCheck;
                Endif;

             Enddo;

          Enddo;

          If exlEditSav = *on or exlEditAdd = *on;
             Exsr ExcludeSave;
          Endif;

          If exlEditCnl = *on or exlEditSav = *on;
             exlAddMode = *off;
          Endif;

       Enddo;

       Close WPIPOLPOP;

      /end-free

       //***************************************************************************//
       //===================================================================
       // Excluded States Error Checking
       //===================================================================
     C     ExcludeCheck  Begsr
     C*
      /free

       Exsr ClearErrors;

       //* Verify state is valid.
       Clear xxKey;
       xxKey = scExlState;
       Chain keySTSTL3 PST03;
       If not %found;
          exlError = *on;
          erExlState = *on;
          msExlState = 'WCR0504';
       Else;

       //* Verify record does not exist yet in Add mode.
          If exlAddMode = *on;
             Chain key1WDXSL PXS00;
             If %found;
                exlError = *on;
                erExlState = *on;
                msExlState = 'WC30661';
             Endif;
          Endif;
       Endif;

      /end-free
     C                   ENDSR                                                  ExcludeCheck

       //===================================================================
       // Save Excluded State Data.
       //===================================================================
     C     ExcludeSave   Begsr
      /free

       If exlDelMode = *on;
          Chain XSrrn PXS0P;
       Else;
          Clear PXS0P;
          XSCO#  = ELCO#;
          XSFND  = ELFND;
          XSEMP# = ELEMP#;
          XSFYR  = ELFYR;
          XSIPGM = Q1PGM;
          XSIUSR = Q1USER;
          XSITIM = Timeto6(SystemTime());
          XSIDT  = Dateto7(SystemDate());
       Endif;

       XSST = scExlState;

       XSPGM  = Q1PGM;
       XSUSER = Q1USER;
       XSTIME = Timeto6(SystemTime());
       XSDT   = Dateto7(SystemDate());

       If exlDelMode = *on;
          Delete PXS0P;
       Else;
          Write PXS0P;
       Endif;

      /end-free
     C                   Endsr                                                  ExcludeSave

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       exlError = *off;

       erExlState = *off;
       msExlState = *blanks;


      /end-free
     C                   Endsr                                                  ClearErrors

     P ExcludeEdit     E

       //***************************************************************************//
       //===================================================================
       // Rating Edit Popup
       //===================================================================
     P RatingEdit      B                   export
     D RatingEdit      PI
     D elRRN                          9  0 const
     D chgMode                         n   options(*nopass)

     D xxSt            s              2
     D xxKey           s                   like(BSKEY)
     D xsKey           s              2
     D xxTBID          s                   like(GTTBID)
     D xxDDcd          s                   like(ELDCDE)
     D xxDate          s                   like(ELPFDT)
     D xxPfDt          s                   like(ELPFDT)
     D xxPtDt          s                   like(ELPFDT)
     D xxEfDt          s                   like(ELPFDT)
     D xxExDt          s                   like(ELPFDT)
     D scPfDt          s              8  0
     D scPtDt          s              8  0
     D error           s               n   inz(*off)
     D inEditData      s            500
     D xxEmrrn         s              9  0 inz
     DdeductInstalled  s              1n
     D flgRcdUpdated   s              1
     D xx#Div          s                   like(ELdiv)
     D saveDT          s                   like(ELdt)
     D saveTM          s                   like(ELtime)
     d chrWvrForm      s             15
     d chrPrtPgm       s             10
     d chrAddPgm       s             10
     d chrBlnkWdDf     s                   Like(ELblnkWD)
     d hasBlnkWdChg    s               n

     C*
     C* Group Master File Key List
     C*
     C     keyWMFNL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELFYR
     C*
     C*  Statistical Reporting/Rating File Key List
     C*
     C     keyWDRRL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDate
     C*
     C     key1WDRRL     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C*
     C*  Increased Limits File Key List
     C*
     C     keyWTLML1     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxKey
     C                   Kfld                    xxDate
     C*
     C     key1WTLML1    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxKey
     C*
     C     key2WTLML1    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C*
     C*  Deductible Discount File Key List
     C*
     C     keyWTDDL2     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDDcd
     C                   Kfld                    xxDate
     C*
     C     key1WTDDL2    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDDcd
     C*
     C     key2WTDDL2    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C*
     C*  Stock Discount File Key List
     C*
     C     keyWMDSL3     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C                   Kfld                    scDISC
     C                   Kfld                    xxDate
     C*
     C     key1WMDSL3    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C                   Kfld                    scDISC
     C*
     C* Generic Table File Key List
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C* MCO Group File Key List
     C*
     C     keyWMMGL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    scMcgp
     C                   Kfld                    xxDate
     C*
     C     key1WMMGL     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    scMcgp
     C*
     C     keyWMEML      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C*
     C     key1WMEML     Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    ELemp#
     C*
     C     key2WDELL     Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    ELemp#
     C                   Kfld                    EMdiv
     C                   Kfld                    ELfyr
     C*
     C     keyWDELL      Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    ELemp#
     C                   Kfld                    ELdiv
     C                   Kfld                    ELfyr
     C*
     C     keyWMRSL      KLIST
     C                   KFLD                    ELco#
     C                   KFLD                    ELrtcd
     C*
     C     key1WDELL1    Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    ELfyr
     C                   Kfld                    ELemp#
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C     Dtcymdp       Plist
     C                   Parm                    Srdti
     C                   Parm      *BLANKS       Srffmt
     C                   Parm      '*CYMD'       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
     C*
     C     Dtmdyyp       Plist
     C                   Parm                    Srdti
     C                   Parm      '*CYMD'       Srffmt
     C                   Parm      *BLANKS       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr

      /free

       Open WTPOLDTL;

       Chain xsKey SWC01;
       deductInstalled = WCFL06 = 'Y';

       GetClientID(clientID);

       Chain(n) elRRN PEL0P;
       saveDT = ELdt;
       saveTM = ELtime;

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;

       xx#Div = 0;
       Setll key1WMEML PEM00;
       Reade key1WMEML PEM00;

       Dow not %eof;
       // capture the state reporting division
          If EMSTRP = 'Y';
             strpDiv = EMDIV;
          Endif;
          xx#Div = xx#Div + 1;
          Reade key1WMEML PEM00;
       Enddo;

       Chain keyWMEML PEM00;

       //*** begin del ***  10/20/23  ******************************************
       //****If RRancs = 0;
       //****   isANCStat = *on;
       //****Endif;
       //*** end del *****  10/20/23  ******************************************
       //* Populate the popup window with selected record.
       Exsr LoadRating;

       Exsr ClearErrors;
       Clear ratSave;
       Clear ratCancel;

       //* Display window for rating fields (set indicator to enable show as window)

       ratPopup = *on;
       ratPop102 = *off;
       visPop150 = *off;
       If clientID = '150';
          visPop150 = *on;
       Endif;

       If clientID = '102';
          ratPop102 = *on;
       Endif;
       ratPop136 = *off;
       If clientId = '136';
          ratPop136 = *on;
       Endif;
       // Set Default Deviation #2 Factor
       isDefault  = *On;
       isStFc     = *Off;
       isCareWest = *Off;

       Select;
       // Use TASB Stabilisation Factor
       When clientId = '025';
          isStFc    = *On;
          isDefault = *Off;
       // Use CareWest Territory Rating
       When  clientId = '136';
          isCareWest = *On;
          isDefault  = *Off;
       EndSl;

       Dou ratSave = *on or ratCancel = *on;

          Dou error = *off or ratCancel = *on;
             Exfmt rcdRating2;

             If ratCancel = *off;
                Exsr RatingCheck;
             Endif;

          Enddo;

       Enddo;

       If ratSave = *on;
          Exsr Save;
       Endif;
       If xxUPDD = 'Y';
          Exsr SRUPDT;
       Endif;

       Close WTPOLDTL;

      /end-free

       //***************************************************************************//
       //===================================================================
       // Load Rating fields
       //===================================================================
     C     LoadRating    Begsr
     C*
      /free

       // Get the policy from date of the state reporting division

       Chain(n) (ELco#:ELfnd:ELemp#:strpDiv:ELfyr) PEL00;
       If %found();
          scPfdt3 = dtMDYY(ELPFDT);
       endif;

       // Now, effective date of state reporting div is captured, so set the
       // pointer of WDELP back to its original value.

       Chain(n) elRRN PEL0P;

       scDiv = ELDiv;
       scFyr = ELFyr;

       scPfdt =  dtMDYY(ELPFDT);
       scPfdt2 = dtMDYY(ELPFDT);
       scPtdt =  dtMDYY(ELPTDT);

       scPnlTitle = 'Rating Information - '+%Editc(ELdiv:'X')+' '+
        GetEmpName(ELemp#:ELdiv:xxEMrrn:ELfnd:ELco#);

       xxSt = RatingSt(ELCO#:ELRTCD);
       xxDate = ELPfdt;
       SetGt keyWDRRL PRR00;
       Readpe key1WDRRL PRR00;

       scFFL9 = ELFFL9;
       scCOFL = ELCOFL;
       //*** Begin Add *** 06/12/24 ****************************************
       // Get the bill schedule code type
       chrBsCd          = %EditC(ELbsCd:'X');
       chrBillSchedType = BillSchdType(ELco#:chrBsCd);

       // Set Detail S/R Assessment to group value and protect from change
       inqSRAs = *Off;

       If clientID = '146';
          Chain (ELco#:ELfnd:ELfyr) PF100;
          If %Found(wdf1l) and F1SRAs = 'Y';
             scSRAs  = 'Y';
             inqSRAs = *On;
          Else;
             scSRAs  = ELsrAs;
          EndIf;
       Else;
          scSRAs = ' ';
       EndIf;

       // Do show S/R Assessments if bill schedule code type is not S/R
       visSRAs = *On;
       If (chrBillSchedType <> 'S' And ELbsCd <> 0) Or
          (ELbsCd = 0) Or (clientID <> '146') Or (ratPopup = *Off);
          visSRAs = *Off;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
       scSRPR = ELSRPR;
       scManP = ELMANP;

       If clientID = '102';
          sc2500 =  ELFFL2;
       Endif;

       If clientId = '136';
          scExRat = ELffl2;
       Endif;

       Clear FlexHead11;
       FlexHead11 = 'Schedule Debit/Credit';
       If ClientId = '136';
          visibl136 = *on;
          If chgMode=' ' or chgMode <> ' ';
             intFctr = 0;
             intRrn# = elRRN;
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

       //Disable Increased Limits fields if no active codes are available.
       noILAvail = *off;
       Setll key2WTLML1 PLM01;
       Reade key2WTLML1 PLM01;
       Dow not %eof and LMSTAT = 'I';
       Reade key2WTLML1 PLM01;
       Enddo;

       If %eof;
          noILAvail = *on;
          Clear scILCD;
          Clear scINL2;
          Clear scIL1efDt;
          Clear scIL1exDt;
          Clear scIL2efDt;
          Clear scIL2exDt;
       Else;
          scILCD = ELILCD;
          scINL2 = ELINL2;
          // Increased limits 1 and 2
          scIL1efDt =  dtMDYY(ELILEF);
          scIL1exDt =  dtMDYY(ELILEX);
          scIL2efDt =  dtMDYY(ELI2EF);
          scIL2exDt =  dtMDYY(ELI2EX);
       Endif;

       // Deductible Discount 1 and 2
       scCxbl =  ELCXBL;

       //Disable Deductible Discount fields if no active codes are available.
       noDDAvail = *off;
       Setll key2WTDDL2 PDD02;
       Reade key2WTDDL2 PDD02;
       Dow not %eof and DDSTAT = 'I';
       Reade key2WTDDL2 PDD02;
       Enddo;

       If %eof;
          noDDAvail = *on;
          Clear scDCDE;
          Clear scDCD2;
          Clear scDD1efDt;
          Clear scDD1exDt;
          Clear scDD2efDt;
          Clear scDD2exDt;
          Clear scDD2exDt;
          Clear scCxBl;
       Else;
          scDCDE = ELDCDE;
          scDCD2 = ELDCD2;
          // Deductible Discount 1 and 2
          scDD1efDt =  dtMDYY(ELDCEF);
          scDD1exDt =  dtMDYY(ELDCEX);
          scDD2efDt =  dtMDYY(EL2DEF);
          scDD2exDt =  dtMDYY(EL2DEX);
       Endif;

       // Deviation 1
       deviation1 = *on;
       dev1Split  = *on;

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

       scMCCD = ELMCCD;
       scMCGP = ELMCGP;

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
       scSDefDt =  dtMDYY(ELSDEF);
       scSDexDt =  dtMDYY(ELSDEX);

       // Experience Modifiers
       scPROM = ELPROM;

       scNoExm1 = ELnoexm1;
       If scNoExm1 = 'Y';
          scExm1 = 0;
       Else;
          scExm1 = ELexm1;
       Endif;
       //*** Begin Add *** 07/30/23 ****************************************
       // Set exp mod 2 based upon if not rated
       scNoExm2 = ELnoexm2;
       If scNoExm2 = 'Y';
          scExm2 = 0;
       Else;
          scExm2 = ELexm2;
       Endif;
       // Set exp mod 3 based upon if not rated
       scNoExm3 = ELnoexm3;
       If scNoExm3 = 'Y';
          scExm3 = 0;
       Else;
          scExm3 = ELexm3;
       Endif;
       //*** End   Add *** 07/30/23 ****************************************
       scEM1Date =  dtMDYY(ELEMD1);
       //*** begin del *** 09/11/25 ****************************************
       //*****scEXM2 = ELEXM2;
       //*** end del ***** 09/11/25 ****************************************
       scEM2Date =  dtMDYY(ELEMD2);
       //*** begin del *** 09/11/25 ****************************************
       //*****scEXM3 = ELEXM3;
       //*** end del ***** 09/11/25 ****************************************
       scEM3Date =  dtMDYY(ELEMD3);
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
       scExrtSel = 'GTCO# = ' + %char(ELco#) +
        ' AND GTTBID = ''EXRT'' AND GTSTAT = ''A''';


       // Experience Rating
       If ELexrt = *Blanks;
          ELexrt = '3';
       Endif;

       If ClientID = '150' and Not ChgMode;
          ELexrt = '2';
       //*** begin add ***  08/05/24  ****************************************
          ELNcci = 'Y';
       //*** end add *****  08/05/24  ****************************************
       Endif;

       scExpRat  = ELexRt;

       // Report to NCCI
       scNcci = ELncci;
       svNcci = scNcci;

       // Get Current state and compare state reporting division and
       // if they are the same then enable "Report to NCCI" checkbox
       //*** Beg Del ***** 03/06/25 **************************
       //***If dspMod = *on;
       //***   disRptNcci = *on;
       //***Else;
       //*** End Del ***** 03/06/25 **************************
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
       //*** Beg Del ***** 03/06/25 **************************
       //***Endif;
       //*** End Del ***** 03/06/25 **************************
       //*** End Add *** 03/04/24 ********************************************

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

       scSFMN = ELSFMN;
       scSFPC = ELSFPC;

       isSftyPer  = *on;
       isSaftyAmt = *on;
       isSftyHdg  = *off;  // Safety heading grayed out.

       // Safety Fee Percentage.
       If RRsfAp <> ' ';
          isSftyPer = *off;
       EndIf;

       // Safety Fee minimum amount.
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
       Endif;

       // Populate various headings - missing from orig design
       isContribution = GetPrmContflg(scCO#:scFND);
       If isContribution = '1';
          flxExcMin2 = 'Exclude from $2,500 Minimum Contribution';
          flxPolInfo = 'Policy Contribution Information';
          flxLvlInfo = 'Contribution Level';
       Else;
          flxExcMin2 = 'Exclude from $2,500 Minimum Premium';
          flxPolInfo = 'Policy Premium Information';
          flxLvlInfo = 'Premium Level';
       Endif;

       scWVTP = ELWVTP;
       scWVPC = ELWVPC;
       scFCHG = ELFCHG;
       scWMIN = ELWMIN;
       scWMAX = ELWMAX;
       scWVFL = ELWVFL;
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

       // Load the search criteria for auto-complete

       Clear ilChoices;
       ilChoices = 'LMCO# = ' + %char(ELCO#) + ' AND LMFND = '
        + %char(ELFND) + ' AND LMST = ''' + xxSt
        + ''' AND LMEFDT <= ' + %char(ELPFDT);

       Clear ddChoices;
       ddChoices = 'DDCO# = ' + %char(ELCO#) + ' AND DDFND = '
        + %char(ELFND) + ' AND DDST = ''' + xxSt
        + ''' AND DDEFDT <= ' + %char(ELPFDT);

       Clear mgChoices;
       mgChoices = 'MGCO# = ' + %char(ELCO#) + ' AND MGFND = '
        + %char(ELFND) + ' AND MGEFDT <= ' + %char(ELPFDT);

       Clear dsChoices;
       dsChoices = 'DSCO# = ' + %char(ELCO#) + ' AND DSFND = '
        + %char(ELFND) + ' AND DSEFDT <= ' + %char(ELPFDT);

      /end-free
     C                   ENDSR                                                  LoadRating

       //***************************************************************************//
     C*===============================================================
     C* SRUPDT - SUBROUTINE TO UPDATE ALL DIVISIONS WITH NEW INCREASED
     C*          LIMITS OR DEDUCTIBLE DISCOUNT CODE
     C*===============================================================
     C*
     C     SRUPDT        BEGSR
      *
     C     key1WDELL1    CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  scILCD        ELILCD
     C                   MOVEL(P)  scINL2        ELINL2
     C                   MOVEL(P)  scDCDE        ELDCDE
     C                   MOVEL(P)  scDCD2        ELDCD2
      *
     C     scILCD        IFNE      *BLANKS
      *
     C     scIL1EfDt     IFNE      *ZERO
     C                   Z-ADD     scIL1EfDT     x8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EFDT            7 0
     C                   ENDIF
      *
     C     scIL1ExDt     IFNE      *ZERO
     C                   Z-ADD     scIL1ExDt     x8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     x7EFDT        ELILEF
     C                   Z-ADD     0             ELILEX
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C                   Z-ADD     x7EFDT        ELILEF
     C                   Z-ADD     x7EXDT        ELILEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     0             ELILEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDLT     ELPFDT
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
     C                   MOVE      *BLANKS       ELILCD
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     x7EXDT        ELILEX
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
      *
     C                   ENDIF                                                  SCILCD <> *BLANKS
      *
     C     scINL2        IFNE      *BLANKS
      *
     C     scIL2EfDt     IFNE      *ZERO
     C                   Z-ADD     scIL2EfDt     x8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EFDT            7 0
     C                   ENDIF
      *
     C     scIL2ExDt     IFNE      *ZERO
     C                   Z-ADD     scIL2ExDt     x8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     x7EFDT        ELI2EF
     C                   Z-ADD     0             ELI2EX
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     x7EFDT        ELI2EF
     C                   Z-ADD     x7EXDT        ELI2EX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   MOVE      *BLANKS       ELINL2
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      scINL2        ELILCD
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     0             ELILEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDLT     ELPFDT
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
     C                   MOVE      *BLANKS       ELILCD
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      *BLANKS       ELINL2
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDGT     ELPFDT
     C                   MOVE      scINL2        ELILCD
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     x7EXDT        ELILEX
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      *BLANKS       ELINL2
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
      *
     C                   ENDIF                                                  SCINL2 <> *BLANKS
      *
     C     scDCDE        IFNE      *BLANKS
      *
     C     scDD1EfDT     IFNE      *ZERO
     C                   Z-ADD     scDD1EfDt     x8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EFDT            7 0
     C                   ENDIF
      *
     C     scDD1ExDt     IFNE      *ZERO
     C                   Z-ADD     scDD1ExDt     x8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     x7EFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C                   Z-ADD     x7EFDT        ELDCEF
     C                   Z-ADD     x7EXDT        ELDCEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDLT     ELPFDT
     C                   MOVE      *BLANKS       ELDCDE
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     x7EXDT        ELDCEX
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
      *
     C                   ENDIF                                                  SCDCDE <> *BLANKS
      *
     C     scDCD2        IFNE      *BLANKS
      *
     C     scDD2EfDt     IFNE      *ZERO
     C                   Z-ADD     scDD2EfDt     x8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EFDT            7 0
     C                   ENDIF
      *
     C     scDD2ExDt     IFNE      *ZERO
     C                   Z-ADD     scDD2ExDt     x8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         x7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     x7EFDT        EL2DEF
     C                   Z-ADD     0             EL2DEX
     C     x7EFDT        WHENGT    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     x7EFDT        EL2DEF
     C                   Z-ADD     x7EXDT        EL2DEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDEQ     *ZERO
     C                   MOVE      *BLANKS       ELDCD2
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      SCINL2        ELDCDE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDLT     ELPFDT
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
     C                   MOVE      *BLANKS       ELDCDE
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      *BLANKS       ELDCD2
     C     x7EFDT        WHENLE    ELPFDT
     C     x7EXDT        ANDNE     *ZERO
     C     x7EXDT        ANDGT     ELPFDT
     C                   MOVE      scDCD2        ELDCDE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     x7EXDT        ELDCEX
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      *BLANKS       ELDCD2
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
      *
     C                   ENDIF                                                  SCDCD2 <> *BLANKS
      *
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Eval      ELTIME = Timeto6(SystemTime())
     C                   Eval      ELDT   = Dateto7(SystemDate())
      *
     C                   UPDATE    PEL01
      *
     C     key1WDELL1    READE     PEL01                                  60
     C                   ENDDO
      *
     C                   ENDSR
      *===============================================================
      * SRDCVT - DATE CONVERSION SUBROUTINE FOR SRUPDT
      *===============================================================
     C     SRDCVT        BEGSR
      *
     C                   Z-ADD     x8EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         x7EFDT
      *
     C                   Z-ADD     x8EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         x7EXDT
      *
     C                   ENDSR
       //===================================================================
       // Rating Error Checking
       //===================================================================
     C     RatingCheck   Begsr
     C*
     C                   Exsr      ClearErrors
     C*
      /free

       inEditData = dsRateEdit;

       ptrRating2 = %Addr(dsRating2);

       error = not errPolRating('R':' ':' ':ptrRating2:ELCo#:ELFnd:ELEmp#:
                    ELDiv:ELFyr:elRRN:inEditData);

       If error;
          strBiScCd = %char(ELbscd);
          Chain(n) (ELco#:strBiScCd) Pbs01;
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

      /end-free
     C*
     C                   If        error = *off
     C                   Exsr      UpdateDIL
     C                   Endif
     C                   If        not Error
     C                   Exsr      ManualPercent
     C                   Endif
     C*
     C                   If        error = *off
      /free
       // Check if WDELP updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WDELP':elrrn:'ELdt':'ELtime':
        'ELuser':saveDT:saveTM);
       Select;
       When flgRcdUpdated = '0'; // No error
          saveDT = ELdt;
          saveTM = ELtime;
       When flgRcdUpdated = '1'; // Error
          error = *on;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;
      /end-free
     C                   Endif
     C*
     C                   ENDSR                                                  RatingCheck

      *===============================================================
      * UpdateDIL - Deductible Discount/Increased Limits Popup
      *===============================================================
     C     UpdateDIL     BEGSR
      *
      * IF INCREASED LIMITS CODE OR DEDUCTIBLE DISCOUNT CODE HAS CHANGED, OFFER
      * OPTION TO UPDATE ALL DIVISIONS
      *
     C                   MOVE      ' '           scUDIAll
     C                   MOVE      ' '           xxUPDD            1
      *
     C     chgMode       IFEQ      *ON
     C     FNPEOF        ANDNE     *BLANK
     C     EMstrp        ANDEQ     'Y'
     C     xx#DIV        ANDGE     2
      *
     C     scILCD        IFNE      ELILCD
     C     scINL2        ORNE      ELINL2
     C     scDCDE        ORNE      ELDCDE
     C     scDCD2        ORNE      ELDCD2
      *
     C                   MOVE      *OFF          xxCHEK            1
     C                   move      *off          erUDIAll
     C                   move      *blanks       msUDIAll
     C                   MOVE      *ON           error
      *
     C     error         DOWEQ     *ON
      *
      * Display Update Deduc/Incr Limits popup window
      *
     C                   move      *off          udiCancel
     C                   move      *off          udiConfirm
     C*
     C                   Dou       udiCancel = *on or udiConfirm = *on
     C                   EXFMT     rcdUpdtDIL
     C                   ENDDO
     C*
     C                   MOVE      *OFF          error
     C                   move      *off          erUDIAll
     C                   move      *blanks       msUDIAll
     C*
     C     udiCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ELSE
      *
      *  ENSURE THAT INC LIMITS AND DED DISC CODES ARE VALID IN OTHER DIVS
      *
     C     scUDIAll      IFEQ      'Y'
      *
      * Display info message (once) if any divisions are cancelled.
      *
     C     key1WMEML     CHAIN     PEM00                              60
     C     *IN60         DOWEQ     *OFF
     C     xxCHEK        ANDEQ     *OFF
      *
     C                   If        getCxDate(emco#:emfnd:ememp#:emdiv) <> 0
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30345'     msUDIAll
     C                   MOVE      *ON           XXCHEK
     C                   MOVE      *ON           error
     C                   Leave
     C                   Endif
      *
     C     key1WMEML     READE     PEM00                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
     C     key1WDELL1    CHAIN(N)  PEL01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     keyWMRSL      CHAIN     PRS00                              60
     C                   MOVE      RSJRST        xxSt
     C                   Z-ADD     ELPFDT        xxDate
      *
     C     scILCD        IFNE      *BLANKS
     C                   MOVEL(P)  scILCD        xxKey
     C     keyWTLML1     SETGT     PLM01
     C     key1WTLML1    READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     key1WTLML1    READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *ON
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30346'     msUDIAll
     C                   MOVE      *ON           error
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCILCD <> *BLANKS
      *
     C     scINL2        IFNE      *BLANKS
     C                   MOVEL(P)  scINL2        xxKey
     C     keyWTLML1     SETGT     PLM01
     C     key1WTLML1    READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     key1WTLML1    READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *ON
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30346'     msUDIAll
     C                   MOVE      *ON           error
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCINL2 <> *BLANKS
      *
     C     scDCDE        IFNE      *BLANKS
     C                   MOVEL(P)  scDCDE        xxDdcd
     C     keyWTDDL2     SETGT     PDD02
     C     key1WTDDL2    READPE    PDD02                                  60
     C     *IN60         DOWEQ     '0'
     C     DDSTAT        ANDEQ     'I'
     C     key1WTDDL2    READPE    PDD02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *ON
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30347'     msUDIAll
     C                   MOVE      *ON           error
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDCDE <> *BLANKS
     C*
     C     scDCD2        IFNE      *BLANKS
     C                   MOVEL(P)  scDCD2        xxDdcd
     C     keyWTDDL2     SETGT     PDD02
     C     key1WTDDL2    READPE    PDD02                                  60
     C     *IN60         DOWEQ     '0'
     C     DDSTAT        ANDEQ     'I'
     C     key1WTDDL2    READPE    PDD02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *ON
     C                   Move      *on           erUDIAll
     C                   Movel(p)  'WC30347'     msUDIAll
     C                   MOVE      *ON           error
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  scDCD2 <> *BLANKS
     C*
     C     key1WDELL1    READE(N)  PEL01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  scUDIAll IFEQ
      *
     C                   ENDIF                                                  udiCancel ifeq
      *
     C                   ENDDO                                                  *IN90 = *ON
      *
     C     scUDIAll      IFEQ      'Y'
     C                   MOVE      'Y'           xxUPDD
     C                   ENDIF
      *
      *  RESET EMP MASTER AND EMP DETAIL INFO
      *
     C     keyWMEML      chain     pem00
     C     keyWDELL      chain(n)  pel00
     C     keyWMRSL      CHAIN     PRS00                              60
     C                   MOVE      RSJRST        xxSt
     C*
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxDate
      *
     C                   ENDIF                                                  scILCD <> ELILCD
      *
     C                   ENDIF                                                  if ChgMode
     c*
     c                   ENDSR
     C*====================================================================
    C* Display info window if % of manual premium value differs from PEO.
     C*====================================================================
     C     ManualPercent Begsr
     C*
      /free

          If clientID = '150' and (ELEmp# <> 1 or ELDiv <> 0);

             Chain(n) (ELco#:ELfnd:ELfyr) pf102;
             If %found and L2F1ManP <> scManP;

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
     c*
     c                   Endsr
       //===================================================================
       // Save Data.
       //===================================================================
     C     Save          Begsr
      /free

       Chain elRRN PEL0P;

       ELFFL9 =  scFFL9;
       ELSRPR =  scSRPR;
       ELCOFL =  scCOFL;
       //*** Begin Add *** 06/12/24 ****************************************
       ELsrAs =  scSRAs;
       //*** End   Add *** 06/12/24 ****************************************
       ELManP =  scManP;
       If ELEmp# = 1 and ELDiv = 0 and ELManP <> 0;
          Chain(n) (ELCo#:ELFnd:ELFyr) pf102;
          If %found and L2F1ManP <> ELManP;
             Chain (ELCo#:ELFnd:ELFyr) pf102;
             L2F1ManP = ELManP;
             Update pf102;
          Endif;
       Endif;

       If clientID = '102';
          ELFFL2 = sc2500;
       Endif;

       If clientId = '136';
          ELffl2 = scExRat;
          ELcmul = scComModM;
       Endif;
       // Increased limits 1 and 2
       ELILCD = scILCD;
       ELINL2 = scINL2;
       ELILEF =  dtCYMD(scIL1efDt);
       ELILEX =  dtCYMD(scIL1exDt);
       ELI2EF =  dtCYMD(scIL2efDt);
       ELI2EX =  dtCYMD(scIL2exDt);

       // Deductible Discount 1 and 2
       ELDCDE = scDCDE;
       ELDCD2 = scDCD2;
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
       ELMCCD = scMCCD;
       ELMCGP = scMCGP;
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

       Elnoexm1 = scNoExM1;
       If scNoExM1 = 'Y';
          ELexm1 = 1;
          ELemd1 = ELpfdt;
       Else;
          ELexm1 = scExM1;
          ELemd1 = dtCymd(scEm1Date);
       Endif;
       //*** Begin Chg *** 09/11/25 ****************************************
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
       //*** End   Chg *** 09/11/25 ****************************************
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

       //Blanket Waiver Description
       If scWvTp <> 'B';
          Clear scOvrWd;
       Endif;
       If scOvrWd <> 'Y';
          Clear scBlnkWd;
       Endif;
       If ELBlnkWd <> scBlnkWd;
          hasBlnkWdChg = *On;
       Else;
          hasBlnkWdChg = *Off;
       Endif;
       ELovrWd = scOvrWd;
       ELBlnkWd = scBlnkWd;

       // Facultative Reinsurance
       ELARCP = scARCP;
       ELARCA = scARCA;
       ELAREF = dtCYMD(scAREfDt);
       ELAREX = dtCYMD(scARExDt);

       // ARAP
       arap = *off;

       If xxSt = 'MA';
          arap = *on;
       Endif;

       ELARP1 = scARP1;
       ELARP2 = scARP2;
       ELARP3 = scARP3;
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

       ELVLCA = scVLCA;

       ELPGM  = Q1PGM;
       ELUSER = Q1USER;
       ELTIME = Timeto6(SystemTime());
       ELDT   = Dateto7(SystemDate());

       Update PEL0P;

       If hasBlnkWdChg = *On;
          chrWvrForm = *Blanks;
          If GetWaiverInfo(ELco#:ELfnd:xxSt:ELpfdt:chrWvrForm) = *On;
             UpdateBlkWvrDft(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:
              chrWvrForm:scBlnkWd);
          Endif;
       Endif;
       //*** Beg Add *** 03/04/24 *************************************
       If isUpdNcci = 'Y';
          Exsr SrUpdNcciAllDiv;
       Endif;
       //*** End Add *** 03/04/24 ************************************
      /end-free
     C                   Endsr                                                  Save

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       error = *off;

       erFFL9   = *off;
       msFFL9   = *blanks;
       erSRPR   = *off;
       msTSRPR   = *blanks;
       erMANP   = *off;
       msMANP   = *blanks;

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
       erOvrWd  = *Off;
       msOvrWd  = *blanks;
       erBlnkWd = *off;
       msBlnkWd = *blanks;
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
       msStFc = *blanks;
       erStFc = *off;
       msDev2F = *blanks;
       erDev2F = *off;
       //*** Beg Add *** 03/04/24 **************************************
       erExpRat = *off;
       msExpRat = *blanks;
       //*** End Add *** 03/04/24 **************************************
       //*** Beg Add *** 06/24/24 ***************************************
       //*** begin del * 06/19/25 ***************************************
       //* Collateral Infomation indicators
       //****errColwp = *Off;
       //****msColwp = *blanks;
       //****errColwa = *Off;
       //****msColwa = *blanks;
       //****errColrp = *Off;
       //****msColrp = *blanks;
       //****errCollid = *Off;
       //****msCollid = *blanks;
       //****errColled = *Off;
       //****msColled = *blanks;
       //****errColsid = *Off;
       //****msColsid = *blanks;
       //****errColsed = *Off;
       //****msColsed = *blanks;
       //*** end del *** 06/19/25 ***************************************
       //*** End Add *** 06/24/24 ***************************************

      /end-free
     C                   Endsr                                                  ClearErrors
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

     P RatingEdit      E

       //***************************************************************************//
       //===================================================================
       // Large Deductible Edit Popup
       //===================================================================
     P LargeDedEdit    B                   export
     D LargeDedEdit    PI
     D elRRN                          9  0 const
     D chgMode                         n   options(*nopass)

     d isUseDsDefault  s              1n
     D xxPfDt          s                   like(ELPFDT)
     D xxPtDt          s                   like(ELPFDT)
     D xxKey           s                   like(BSKEY)
     D xxTBID          s                   like(GTTBID)
     D xxArty          s              1
     D xtotalDedB      s             14  2
     D xxEmrrn         s              9  0 inz
     D error           s               n   inz(*off)
     D saveDT          s                   like(ELDT)
     D saveTM          s                   like(ELTIME)
     d saveRRN         s                   like(dbrrn8)
     D flgRcdUpdated   s              1
     D inEditData      s            500
     d poltypcde       s              1
     d dsWMLDL1        ds                  likerec(pld01:*input) inz
     C*
     C* Group Master File Key List
     C*
     C     keyWMFNL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELFYR
     C*
     C* Large Deductible File Key List
     C*
     C     keyWMLDL1     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR
     C*
     C* Deductible Detail File Key List
     C*
     C     keyWDDDL1     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR
     C*
     C* Table File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    xxKey
     C*
     C* Generic Table File Key List
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C* Reserve Code File Key List
     C*
     C     keyWTRTL1     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    xxKey
     C*
     C* A/R Detail File Key List
     C*
     C     keyWMEAL10    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELFYR
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    xxArty
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C     Dtcymdp       Plist
     C                   Parm                    Srdti
     C                   Parm      *BLANKS       Srffmt
     C                   Parm      '*CYMD'       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr
     C*
     C     Dtmdyyp       Plist
     C                   Parm                    Srdti
     C                   Parm      '*CYMD'       Srffmt
     C                   Parm      *BLANKS       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr

      /free

       Open WTPOLDTL;

       GetClientID(clientID);

       Chain(n) elRRN PEL0P;

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;

       //* Populate the popup window with selected record.
       Exsr LoadLargeDed;

       Exsr ClearErrors;
       Clear lrgSave;
       Clear lrgCancel;

       //* Display window for rating fields (set indicator to enable show as window)

       largeDeduc = *on;
       lrgPopup = *on;
       poltypcde = ' ';
       visbClmLib = *Off;
       visbClmAlw = *Off;
       VisibAggrP  = *off;
       VisibVarA   = *off;
       If ClientId = '150' and largeDeduc = *on;
          VisibAggrP = *on;
          VisibVarA = *on;
       Endif;
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


       Dou lrgSave = *on or lrgCancel = *on;

          Dou error = *off or lrgCancel = *on;

             Exfmt rcdLrgDed;

             If lrgCancel = *off;
                Exsr LargeDedCheck;
             Endif;

          Enddo;

       Enddo;

       If lrgSave = *on;
          Exsr Save;
       Endif;

       Close WTPOLDTL;

      /end-free

       //***************************************************************************//
       //===================================================================
       // Load Large Deductible fields
       //===================================================================
     C     LoadLargeDed  Begsr
     C*
      /free

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
       Clear scClmLib;
       Clear scClmAlw;
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

       saveRRN = dbrrn8;
       Reset dsWMLDL1;
       Select;
       When FNpeoF = *blank;
          Chain (ELco#:ELfnd:ELemp#:GetStRptDv(ELco#:ELfnd:ELemp#):ELfyr)
           pld01 dsWMLDL1;
       Other;
          Chain (ELco#:ELfnd:1:0:ELfyr) pld01 dsWMLDL1;
       Endsl;
       isUseDsDefault = %found(wmldl1);
       dbrrn8 = saveRRN;

       Chain(n) keyWMLDL1 PLD01;
       isLargeDedRcd = %found;
       If not %found and LDemp# = 0 and chgMode <> *on;
          If not %open(wmldl4);
             @wmld = %trim(RtvQuoteLib('*LIV')) + '/WMLDL4';
             Open wmldl4;
          Endif;
          Clear RSjrSt;
          chain (ELco#:ELrtCd) prs00;
          setgt (ELco#:ELfnd:RSjrSt:0:ELpfDt) pld04;
          readpe (ELco#:ELfnd:RSjrSt:0) pld04;
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
             Clear dbrrn8;
             If isUseDsDefault;
                LDamP1  = dsWMLDL1.LDamP1;
                LDamP2  = dsWMLDL1.LDamP2;
                LDamG1  = dsWMLDL1.LDamG1;
                LDamG2  = dsWMLDL1.LDamG2;
                LDvara1 = dsWMLDL1.LDvara1;
                LDvara2 = dsWMLDL1.LDvara2;
                LDadP1  = dsWMLDL1.LDadP1;
                LDadP2  = dsWMLDL1.LDadP2;
                LDdes1  = dsWMLDL1.LDdes1;
                LDdes2  = dsWMLDL1.LDdes2;
             Endif;
          Endif;
          If %open(wmldl4);
             Close wmldl4;
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
          scClmLib = LDlirt;
          scClmAlw = LDclal;
          scIncMdIn = LDincMdIn;
          If scIncMdIn = *blank;
             scIncMdIn = 'B';
          Endif;
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
          scADP2   = LDadp2;
          scVarA2  = LDvarA2;
          saveDT   = LDdt;
          saveTM   = LDtime;
       Else;
          dbrrn8 = 0;
       Endif;

       Clear ldPlnChoic;
       ldPlnChoic = 'GTCO# = ' + %char(ELCO#) + ' AND GTTBID = ''  DT'''
        + ' AND GTSTAT = ''A''';

       scPnlTitlD = 'Large Deductible Information - '+%Editc(ELdiv:'X')+' '+
        GetEmpName(ELemp#:ELdiv:xxEMrrn:ELfnd:ELco#);

      /end-free
     C                   ENDSR                                                  LoadLargeDed

       //***************************************************************************//
       //===================================================================
       // Large Deductible Error Checking
       //===================================================================
     C     LargeDedCheck Begsr
     C*
     C                   Exsr      ClearErrors
     C*
     C                   Z-ADD     ELPfdt        x5PFDT
     C                   Z-ADD     ELPtdt        x5PTDT
     c*
      /free

       inEditData = dsRateEdit;

       ptrLargeDed = %Addr(dsLargeDed);

       error = not errLargeDed('D':' ':' ':ptrLargeDed:ELCo#:ELFnd:ELEmp#:
                    ELDiv:ELFyr:elRRN:inEditData);

       dsRateEdit = inEditData;

      /end-free
     C*
     C                   If        not Error and chgMode = *on
     C                   EXSR      AggregateAmt
     C                   Endif
      *
     C                   If        error = *off
      /free
       // Check if WMLDP updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WMLDP':dbrrn8:'LDdt':'LDtime':
        'LDuser':saveDT:saveTM);
       Select;
       When flgRcdUpdated = '0'; // No error
          saveDT = LDdt;
          saveTM = LDtime;
       When flgRcdUpdated = '1'; // Error
          error = *on;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;
      /end-free
     C                   Endif
     C                   ENDSR                                                  LargeDedCheck

      *===============================================================
      * AggregateAmt - Large Deductible Aggregate Amount Popup
      *===============================================================
     C     AggregateAmt  BEGSR
     C*
     C* Notify user if aggregate entered differs from other divisions
     C*
     c                   if        not AcceptAgg(scLDEfD1:scLDAmg1:elrrn)
     c                             and erLDAmg1 <> *ON
     C                   move      *on           error
     C                   move      *on           erLDAMG1
     C                   movel(p)  'WC32434'     msLDAMG1
     c                   endif                                                  (scCFee <> *blanks)
     C*
     C* Notify user if aggregate entered differs from other divisions
     C*
     C                   if        not error
     C*
     c                   if        not AcceptAgg(scLDEfD2:scLDAmg2:elrrn)
     c                             and erLDAmg2 <> *ON
     C                   move      *on           error
     C                   move      *on           erLDAMG2
     C                   movel(p)  'WC32434'     msLDAMG2
     c                   endif                                                  (scCFee <> *blanks)
     C*
     c                   endif
     c*
     c                   ENDSR

       //===================================================================
       // Save Data.
       //===================================================================
     C     Save          Begsr
      /free

       Chain keyWMLDL1 PLD01;

       If not %Found;
          Clear PLD01;
          LDCo#  = ELCo#;
          LDFnd  = ELFnd;
          LDFyr  = ELFyr;
          LDEmp# = ELEmp#;
          LDDiv  = ELDiv;
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
       LDlirt = scClmLib;
       LDclal = scClmAlw;
       LDincMdIn = scIncMdIn;
       If LDincMdIn = 'B';
          LDincMdIn = *Blank;
       Endif;
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

       LDPGM  = Q1PGM;
       LDUSER = Q1USER;
       LDTIME = Timeto6(SystemTime());
       LDDT   = Dateto7(SystemDate());

       If %Found;
          Update PLD01;
       Else;
          Write PLD01;
       Endif;

      /end-free
     C                   Endsr                                                  Save

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       error = *off;

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

      /end-free
     C                   Endsr                                                  ClearErrors
     C*===============================================================
     C* DTERRS - SUBROUTINE FOR DETERMINING FROM/TO DATE ERRORS
     C*===============================================================
     C*
     C     DTERRS        BEGSR
     C*
     C                   MOVE      *off          *IN71
     C                   MOVE      *off          *IN72
     C                   MOVE      *off          *IN73
     C                   MOVE      *off          *IN74
     C                   MOVE      *off          *IN75
     C                   MOVE      *off          *IN76
     C*
     C     '0'           CHECK     xxKEY         xxZERO            3 0
     C                   SUB       1             xxZERO
     C     ' '           CHECKR    xxKEY         xxLEN             3 0
     C*
     C     xxZERO        IFEQ      xxLEN
     C                   MOVEL     *ZEROS        xxKEY
     C                   ENDIF                                                  XXZERO=XXLEN
     C*
     C*  IF ONE OF THE DATES IS ENTERED, CODE IS REQUIRED.
     C*
     C*
     C     xxKEY         IFEQ      *ZEROS
     C     xxCHAR        ANDEQ     *OFF
     C     xxKEY         OREQ      *BLANKS
     C*
     C     xxEF          IFNE      *ZERO
     C     xxEX          ORNE      *ZERO
     C                   SETON                                        71
     C                   ENDIF                                                  XXEF<>*ZERO<>XXEX
     C*
     C                   ENDIF                                                  *ZEROS=XXKEY=*BLANKS
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C     xxEF          IFNE      *ZERO
     C*
     C                   Z-ADD     xxEF          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         xxEF              8 0
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        72
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  EFFECTIVE DATE MUST BE BETWEEN POLICY FROM & TO DATES
     C*
     C                   Z-ADD     ELPFDT        xxPfdt
     C                   Z-ADD     ELPTDT        xxPtdt
     C*
     C                   Z-ADD     xxEF          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEFDT            7 0
     C*
     C     xxEFDT        IFLT      xxPFDT
     C     xxEFDT        ORGT      xxPTDT
     C                   SETON                                        73
     C                   ENDIF                                                  xxPF>xxEF>xxPT
     C*
     C                   ENDIF                                                  xxEF<>*ZERO
     C*
     C*  IF ENTERED, EXPIRATION DATE MUST BE A VALID DATE
     C*
     C     xxEX          IFNE      *ZERO
     C*
     C                   Z-ADD     xxEX          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         xxEX              8 0
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        75
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  EFFECTIVE DATE MUST BE LESS THAN EXPIRATION DATE
     C*
     C                   Z-ADD     xxEX          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxEXDT            7 0
     C*
     C     xxEXDT        IFLT      xxEFDT
     C                   SETON                                        74
     C                   ENDIF                                                  xxEXDT<xxEFDT
     C*
     C*  EXPIRATION DATE MUST BE BETWEEN POLICY FROM & TO DATES
     C*
     C     xxEXDT        IFLT      xxPFDT
     C     xxEXDT        ORGT      xxPTDT
     C                   SETON                                        76
     C                   ENDIF                                                  xxPFDT>xxEXDT>xxPTDT
     C*
     C                   ENDIF                                                  xxEXDT<>*ZERO
     C*
     C                   MOVE      *OFF          XXCHAR            1
     C                   ENDSR                                                  DTERRS

     P LargeDedEdit    E

       //***************************************************************************//
       //===================================================================
       // Retrospective Rating Edit Popup
       //===================================================================
     P RetroEdit       B                   export
     D RetroEdit       PI
     D elRRN                          9  0 const

     D xxKey           s                   like(BSKEY)
     D xxEmrrn         s              9  0 inz
     D error           s               n   inz(*off)
     D saveDT          s                   like(ELdt)
     D saveTM          s                   like(ELtime)
     D flgRcdUpdated   s              1
     D inEditData      s            500

      /free

       Open WTPOLDTL;

       GetClientID(clientID);

       Chain(n) elRRN PEL0P;
       saveDT = ELdt;
       saveTM = ELtime;

       //* Populate the popup window with selected record.
       Exsr LoadRetro;

       Exsr ClearErrors;
       Clear retSave;
       Clear retCancel;

       //* Display window for rating fields (set indicator to enable show as window)

       retro = *on;
       retPopup = *on;

       Dou retSave = *on or retCancel = *on;

          Dou error = *off or retCancel = *on;

             Exfmt rcdRetro;

             If retCancel = *off;
                Exsr RetroCheck;
             Endif;

          Enddo;

       Enddo;

       If retSave = *on;
          Exsr Save;
       Endif;

       Close WTPOLDTL;

      /end-free

       //***************************************************************************//
       //===================================================================
       // Load Retro fields
       //===================================================================
     C     LoadRetro     Begsr
     C*
      /free

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

       isContribution = GetPrmContflg(ELco#:Elfnd);
       If isContribution;
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

       scPnlTitlR = 'Retrospective Rating Information - '+%Editc(ELdiv:'X')+' '+
        GetEmpName(ELemp#:ELdiv:xxEMrrn:ELfnd:ELco#);

       If clientID = '114';
          scRatingL  = 'Retrospective/Retention Rating';
       Else;
          scRatingL  = 'Retrospective Rating Information';
       Endif;

      /end-free
     C                   ENDSR                                                  LoadRetro

       //***************************************************************************//
       //===================================================================
       // Retro Error Checking
       //===================================================================
     C     RetroCheck    Begsr
     C*
     C                   Exsr      ClearErrors
     C*
      /free

       inEditData = dsRateEdit;

       ptrRetroRate = %Addr(dsRetroRate);

       error = not errRetroRate('E':' ':' ':ptrRetroRate:ELCo#:ELFnd:ELEmp#:
                    ELDiv:ELFyr:elRRN:inEditData);

       dsRateEdit = inEditData;

      /end-free
     C*
     C                   If        error = *off
      /free
       // Check if WDELP updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WDELP':elrrn:'ELdt':'ELtime':
        'ELuser':saveDT:saveTM);
       Select;
       When flgRcdUpdated = '0'; // No error
          saveDT = ELdt;
          saveTM = ELtime;
       When flgRcdUpdated = '1'; // Error
          error = *on;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;
      /end-free
     C                   Endif
     C*
     C                   ENDSR                                                  RetroCheck

       //===================================================================
       // Save Data.
       //===================================================================
     C     Save          Begsr
      /free

       Chain elRRN PEL0P;

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

       Update PEL0P;

      /end-free
     C                   Endsr                                                  Save

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       error = *off;

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

      /end-free
     C                   Endsr                                                  ClearErrors

     P RetroEdit       E

       //***************************************************************************//
       //===================================================================
       // Additional Info Edit Popup
       //===================================================================
     P AddInfoEdit     B                   export
     D AddInfoEdit     PI
     D elRRN                          9  0 const
     D quoteMode                      1    const
     D chgMode                         n   options(*nopass) const
     D inDefaultMode                   n   options(*nopass) const

     D xxKey           s                   like(BSKEY)
     D xxDate          s                   like(ELPFDT)
     D xxTBID          s                   like(GTTBID)
     d xsRRN           s              9  0
     d xxUPDA          s              1
     D xx#Div          s                   like(ELdiv)
     d svRlFl          s                   like(ELrlfl)
     d s2RlFl          s                   like(ELrlfl)
     D error           s               n   inz(*off)
     D saveDT          s                   like(ELdt)
     D saveTM          s                   like(ELtime)
     D saveDT1         s                   like(ELdt)
     D saveTM1         s                   like(ELtime)
     D*** begin add ***  06/19/25  *************************************
     D numDivs         s              5  0 inz(0)
     D*** end add *****  06/19/25  *************************************
     D flgRcdUpdated   s              1
     D inEditData      s            500
     D isDefaultMode   s              1n   inz(*off)

     D xxEmrrn         s              9  0 inz
     C*
     C* Generic Table File Key List
     C*
     C     keyWTGTL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    xxTbid
     C                   Kfld                    xxKey
     C*
     C     keyWTGIL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    xxTbid
     C*
     C* State File Key List
     C*
     C     keySTSTL3     Klist
     C                   Kfld                    xxKey
     C*
     C* Admin/Darts File Key List
     C*
     C     keyWMEXL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C*
     C* Group Carrier Information File Key List
     C*
     C     keyWMGCL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C*
     C*  CARRIER/STATE REPORTING MASTER FILE
     C*
     C     keyWMRPL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR
     C*
     C     keyWMRPL3     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    scRPCAP#
     C*
     C* Group Printing Text File Key List
     C*
     C     keyWDF4L      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxKey
     C                   Kfld                    xxDate
     C*
     C     key1WDF4L     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxKey
     C*
     C* Excluded States File Key List
     C*
     C     keyWDXSL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELFYR
     C*
     C* Flexible Heading File Key List
     C*
     C     keyWDFHL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C*
     C     key1WMEML     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C*
     C     keyWMEML      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C*
     C     key2WDELL     Klist
     C                   Kfld                    ELco#
     C                   Kfld                    ELfnd
     C                   Kfld                    ELemp#
     C                   Kfld                    EMdiv
     C                   Kfld                    ELfyr
     C*
     C     Datchk        Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      *BLANKS       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C     Dtcymdp       Plist
     C                   Parm                    Srdti
     C                   Parm      *BLANKS       Srffmt
     C                   Parm      '*CYMD'       Srtfmt
     C                   Parm                    Srdto
     C                   Parm                    Srerr

      /free

       Open WTPOLDTL;

       GetClientID(clientID);

       quote = *off;

       If quoteMode = 'Q';
          quote = *on;
       Endif;
       If %parms >= 4;
          isDefaultMode = inDefaultMode;
       Endif;

       Chain(n) elRRN PEL0P;
       // Do not allow a Dividend Plan to be visible unless client is eligible.
       If ValidDivClient = *on;
          visDivPlan = *on;
          btnAddCxTp = 840;
          btnAddSvTp = 840;
       else;
          visDivPlan = *off;
          btnAddCxtp = 635;
          btnAddSvTp = 635;
       endif;
       // Do not allow a Dividend Plan to be selected if a bill hasn't been run. If the renewal
       // has rolled with a new plan, let it be edited before running a bill.
       If ELfaf =  *blank and ELdvcd = *blank;
          dsbDivdPln = *On;
       Endif;
       If clientID = '111';
          dspDivInfo = *off;
          btnAddCxTp = 745;
          btnAddSvTp = 745;
       Else;
          dspDivInfo = *on;
       Endif;

       //*** begin add ***  10/20/23  ******************************************
       xxSt = RatingSt(ELCO#:ELRTCD);
       xxDate = ELPfdt;
       SetGt (ELCo#:ELFnd:xxSt:xxDate) PRR00;
       Readpe (ELCo#:ELFnd:xxSt) PRR00;

       isANCStat = *off;
       If RRancs = 0;
          isANCStat = *on;
       Endif;
       //*** end add *****  10/20/23  ******************************************
       //*** Beg Add ***** 06/24/24 ********************************************
       If ClientID = '146';
          visCollate = *On;
          If dspDivInfo = *On and visDivPlan = *Off;
             scContTop = 690;
             btnAddCxTp = 890;
             btnAddSvTp = 890;
          Else;
             scContTop = 890;
             btnAddCxTp = 1105;
             btnAddSvTp = 1105;
          Endif;
       Else;
          visCollate = *Off;
       Endif;
       //*** End Add ***** 06/24/24 ********************************************
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

       saveDt = ELdt;
       saveTM = ELtime;
       xx#Div = 0;
       Setll key1WMEML PEM00;
       Reade key1WMEML PEM00;

       Dow not %eof;
          xx#Div = xx#Div + 1;
          Reade key1WMEML PEM00;
       Enddo;

       Chain keyWMEML PEM00;

       //* Populate the popup window with selected record.
       Exsr LoadAddInfo;
       If isDefaultMode;
          Exsr Save;
          Close WTPOLDTL;
          Return;
       Endif;

       Exsr ClearErrors;
       Clear addSave;
       Clear addCancel;

       //* Display window for rating fields (set indicator to enable show as window)

       addPopup = *on;
       //*** Beg Add *** 10/16/23 *** 153WAO ***************************
       // Only Display Prior Policy # for Clinet ID = 153
       If clientId = '153';
          ShowPPol = *On;
          btnAddCxTp = 645;
          btnAddSvTp = 645;
       Else;
          ShowPPol = *Off;
       EndIf;
       //*** End Add *** 10/16/23 *** 153WAO ***************************

       Dou addSave = *on or addCancel = *on;

          Dou error = *off or addCancel = *on;

             If scFFLA = 'Y';
                disbPP3C = *off;
             Else;
                disbPP3C = *on;
             Endif;

             Exfmt rcdAddInfo;

             Exsr ClearErrors;

             Select;

             //*  Add new excluded state record.
                When addExclude = *on;
                   xsRRN = 0;
                   ExcludeEdit(xsRRN:ELrrn);
                   addExclude = *off;
                   Exsr LoadExcludeGrd;

             //*  Perform error check (from Enter or Save).
                When addSave = *on;
                   Exsr AddInfoCheck;
                   If error = *on;
                      addSave = *off;
                   Endif;

                Other;

                //*  Launch Popup for Excluded States delete.
                   Readc ExcludeSFL;
                   If not %EOF();
                      If excludeDel = *on;
                         xsRRN = scXSrrn;
                         ExcludeEdit(xsRRN);
                         excludeDel = *off;
                         Exsr LoadExcludeGrd;
                      Endif;
                   Endif;

             Endsl;

          Enddo;

       Enddo;

       If addSave = *on;
          Exsr Save;
       Endif;
       If xxUPDA = *on;
          Exsr updAll;
       Endif;

       Close WTPOLDTL;

      /end-free

     C*===============================================================
     C* Update all divisions
     C*===============================================================
     C     UPDALL        BEGSR
     C*
     C     *LIKE         DEFINE    *IN60         SVIN60
     C     *LIKE         DEFINE    *IN60         SVIN61
     C     *LIKE         DEFINE    ELUSER        SVUSER
     C     *LIKE         DEFINE    ELPGM         SVPGM
     C     *LIKE         DEFINE    ELDT          SVDT
     C     *LIKE         DEFINE    ELTIME        SVTIME
     C*
     C                   MOVEL     ELRLFL        SVRLFL
     C                   MOVEL     ELUSER        SVUSER
     C                   MOVEL     ELPGM         SVPGM
     C                   Z-ADD     ELDT          SVDT
     C     1             ADD       ELTIME        SVTIME
     C*
     C                   MOVE      *IN60         SVIN60
     C                   MOVE      *IN61         SVIN61
     C*
     C     key1WMEML     SETLL     PEM00                              60
     C*
     C     *IN60         DOUEQ     *ON
     C*
     C     key1WMEML     READE     PEM00                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     key2WDELL     CHAIN     PEL00                              61
     C*
     C     *IN61         IFEQ      *OFF
     C*
     C     dbrrn         Chain     PEL0P                              61
     C*
     C                   MOVEL     SVRLFL        ELRLFL
     C                   MOVEL     SVUSER        ELUSER
     C                   MOVEL     SVPGM         ELPGM
     C                   Z-ADD     SVDT          ELDT
     C                   Z-ADD     SVTIME        ELTIME
     C*
     C                   UPDATE    PEL0P
     C*
     C                   ADD       1             SVTIME
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   MOVE      *OFF          xxUPDA
     C*
     C                   MOVE      SVIN60        *IN60
     C                   MOVE      SVIN61        *IN61
     C*
     C                   ENDSR                                                  UPDALL
       //===================================================================
       // Call WA200R to default the carrier policy information.
       //===================================================================
     C     DefaultCarr   Begsr
      *
      *  Create new Carrier #
      *
     C     quote         IFNE      *on
      *
     C                   CALL      'WA200R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM      'W0051R02'    #1PGM            10
      *
     C                   ENDIF
      *
     C                   Endsr                                                  DefaultCarr Subr

       //***************************************************************************//
       //===================================================================
       // Load Additional Info Fields
       //===================================================================
     C     LoadAddInfo   Begsr
     C*
      /free

       xxDate = ELPFDT;

       dspExcCtl  = *on;

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
       //*** Beg Add *** 10/16/23 *** 153WAO ***************************
       Clear scRPPPOL;
       //*** End Add *** 10/16/23 *** 153WAO ***************************
       //*** Beg Add ***** 06/24/24 ************************************
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
       //*** End Add ***** 06/24/24 ************************************

       Chain(n) keyWMRPL PRP00;

       If not %found;
          Exsr DefaultCarr;
       Endif;

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
          Else;
          Clear dbrrn9;
       Endif;

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

       // Excluded States
       Exsr LoadExcludeGrd;

       scPnlTitlA = 'Additional Information - '+%Editc(ELdiv:'X')+' '+
        GetEmpName(ELemp#:ELdiv:xxEMrrn:ELfnd:ELco#);
       isContribution = GetPrmContflg(ELco#:Elfnd);
       If isContribution;
          flxPrmAcrs = 'Accumulate Contribution Across Divisions';
       Else;
          flxPrmAcrs = 'Accumulate Premium Across Divisions';
       Endif;

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

       // Other Policy
       scFFD1 = ELFFD1;
       scFFD2 = ELFFD2;
       scFFD3 = ELFFD3;
       scFFD4 = ELFFD4;
       scFFD5 = ELFFD5;
       //*** Beg Add ***** 06/24/24 ***************************
        // Collateral Information
        If clientID = '146';

           Chain(n) keyWMEXL pex00;

           If ExoCol = 'Y';                                                     // If EXocol = 'Y'
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
           Else;
              disableCol = *On;
              scCoTlTip = 'Collateral has not been set up for this group.';
           Endif;                                                               //Endif EXocol = 'Y'
        Endif;
       //*** End Add ***** 06/24/24 ***************************

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

       Chain keyWMEXL PEX00;

       If EXEHD1 <> *blanks;
          scFlexHdg1 = EXEHD1;
          flex1 = *on;
          If EXTBD1 <> *blanks;
             flex1Choic = 'GTTBID = ''' + EXTBD1 + '''';
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
             flex2Choic = 'GTTBID = ''' + EXTBD2 + '''';
             flexDrop2 = *on;
             scFFDD2 = ELFFD2;
          Else;
             flexBox2 = *on;
             scFFD2 = ELFFD2;
          Endif;
       Endif;

       Chain keyWDFHL PFH00;

       If %found;

          flex = 'Y';

          If FHFH#1 <> *blanks;
             scFlexHdg1 = FHFH#1;
             flex1 = *on;
             If FHF1TB <> *blanks;
                flex1Choic = 'GTTBID = ''' + FHF1TB + '''';
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
                flex2Choic = 'GTTBID = ''' + FHF2TB + '''';
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
                flex3Choic = 'GTTBID = ''' + FHF3TB + '''';
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

       Endif;

      /end-free
      *
      * Setup for Flex fields editing.
      *
     C                   select
     C                   when      quote = *on
     C                   if        flex = 'N'
     C                   eval      xxRQD1 = EXERQ1
     C                   eval      xxRQD2 = EXERQ2
     C                   endif
     C*
     C                   if        flex = 'Y'
     C                   eval      xxRQD1 = FHFH1Q
     C                   eval      xxRQD2 = FHFH2Q
     C                   eval      xxRQD3 = FHFH3Q
     C                   eval      xxRQD4 = FHFH4Q
     C                   eval      xxRQD5 = FHFH5Q
     C                   eval      xxRQD6 = FHFH6Q
     C                   endif                                                  flex = 'Y'
     C                   other
     C                   if        flex = 'N'
     C                   eval      xxRQD1 = EXERW1
     C                   eval      xxRQD2 = EXERW2
     C                   endif
     C*
     C                   if        flex = 'Y'
     C                   eval      xxRQD1 = FHFH1W
     C                   eval      xxRQD2 = FHFH2W
     C                   eval      xxRQD3 = FHFH3W
     C                   eval      xxRQD4 = FHFH4W
     C                   eval      xxRQD5 = FHFH5W
     C                   eval      xxRQD6 = FHFH6W
     C                   endif                                                  flex ='Y'
     C                   endsl
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

      /end-free
     C                   ENDSR                                                  LoadRetro
       //***************************************************************************//
       //===================================================================
       // Load Excluded States Grid
       //===================================================================
     C     LoadExcludeGrdBEGSR
      /free

       // Clear the subfile.
       clrExclude = *On;
       dspExcSfl  = *off;
       dspExcCtl  = *off;
       Write rcdAddInfo;
       clrExclude = *Off;
       dspExcSfl  = *on;
       dspExcCtl  = *on;
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

       //* Position exclude grid at top at all times.
       rrnExclude = 1;

      /end-free
     C                   ENDSR                                                  LoadExcludeGrd

       //***************************************************************************//
       //===================================================================
       // Additional Info Error Checking
       //===================================================================
     C     AddInfoCheck  Begsr
     C*
      /free

       inEditData = dsRateEdit;

       ptrAddInfo = %Addr(dsAddInfo);

       error = not errAddInfo('A':' ':quoteMode:ptrAddInfo:ELCo#:ELFnd:ELEmp#:
                    ELDiv:ELFyr:elRRN:inEditData);

       dsRateEdit = inEditData;

      /end-free
     C*
     C                   If        not Error
     C                   EXSR      RenewFlag
     C                   Endif
       //*** begin add ***  06/19/25  ***********************************
       If not error and numDivs > 1;
          Exsr CheckCollateral;
       Endif;
       //*** end add *****  06/19/25  ***********************************

      /free
       If not Error;
          // Check if WDELP updated since screen displayed
          flgRcdUpdated = RcdUpdatedErr('WDELP':elrrn:'ELdt':'ELtime':
           'ELuser':saveDT:saveTM);
          Select;
          When flgRcdUpdated = '0'; // No error
             // Check if WMRPP updated since screen displayed
             flgRcdUpdated = RcdUpdatedErr('WMRPP':dbrrn9:'RPdt':'RPtime':
              'RPuser':saveDT1:saveTM1);
             Select;
             When flgRcdUpdated = '0'; // No error
                saveDt = ELdt;
                saveTM = ELtime;
                saveDt1 = RPdt;
                saveTM1 = RPtime;
             When flgRcdUpdated = '1'; // Error
                error = *on;
             When flgRcdUpdated = '2'; // Error with refresh
             Endsl;
          When flgRcdUpdated = '1'; // Error
             error = *on;
          When flgRcdUpdated = '2'; // Error with refresh
          Endsl;
       Endif;
      /end-free
     C                   ENDSR                                                  AddInfoCheck
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
             Endif;

          Endif;

       Endsr;
       //===================================================================
       //*** end add *****  06/19/25  ***********************************

      *===============================================================
      * RenewFlag - Do Not Renew Flag Popup
      *===============================================================
     C     RenewFlag     BEGSR
      *
      * Verify if user wants to update the Do Not Renew flag on all
      * divisions.
      *
     C                   MOVE      *OFF          xxUPDA
      *
     C                   If        scRLFL <> svRLFL
     C                             and scRLFL <> s2RLFL
      *
     C                   If        ChgMode and xx#Div >= 2
      *
     C     keyWMEXL      CHAIN     PEX00                              60
      *
     C     *IN60         IFEQ      *OFF
     C     EXUPDA        ANDNE     'Y'
      *
      * Display Update Renew flag popup window (If update all functionality is not active)
      *
     C                   MOVE      *BLANK        scURFAll
     C*
     C                   move      *off          urfCancel
     C                   move      *off          urfConfirm
     C*
     C                   Dou       urfCancel = *on or urfConfirm = *on
     C                   EXFMT     rcdRenewF
     C                   ENDDO
     C*
     C     urfCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ELSE
      *
     C     scURFAll      Ifeq      'Y'
     C                   MOVE      *ON           xxUPDA
     C                   Endif                                                  scURFAll ifeq
      *
     C                   ENDIF                                                  urfCancel ifeq
      *
     C                   ENDIF                                                  *IN60 ifeq
      *
     C                   ENDIF                                                  if ChgMode
     c*
     C                   move      scRLFL        s2RLFL
      *
     C                   ENDIF                                                  if scRLFL <>
     c*
     c                   ENDSR
       //===================================================================
       // Save Data.
       //===================================================================
     C     Save          Begsr
      /free

       Chain elRRN PEL0P;

       Chain keyWMRPL PRP00;

       If not %found;
          Clear PRP00;
          RPCO#  = ELCO#;
          RPFND  = ELFND;
          RPEMP# = ELEMP#;
          RPDIV  = ELDIV;
          RPFYR  = ELFYR;

          RPIPGM = Q1PGM;
          RPIUSR = Q1USER;
          RPITM  = Timeto6(SystemTime());
          RPIDT  = Dateto7(SystemDate());
       Endif;

       RPCARR = scRPCARR;
       RPCAP# = scRPCAP#;
       RPISDT = dtCYMD(scRPISDT);
       RPEFDT = dtCYMD(scRPEFDT);
       RPEXDT = dtCYMD(scRPEXDT);
       RPCLT# = scRPCLT#;
       RPTPA# = scRPTPA#;
       RPBRCH = scRPBRCH;
       RPCNTR = scRPCNTR;
       //*** Beg Add *** 10/16/23 *** 153WAO ***************************
       RPPPOL = scRPPPOL;
       //*** End Add *** 10/16/23 *** 153WAO ***************************

       RPPGM  = Q1PGM;
       RPUSER = Q1USER;
       RPTIME = Timeto6(SystemTime());
       RPDT   = Dateto7(SystemDate());

       If %found;
          Update PRP00;
       Else;
          Write PRP00;
          Feod WMRPL;
       Endif;

       // Policy Page 3C
       ELFFLA = scFFLA;
       ELPP3C = scPP3C;
       ELPPC3 = scPPC3;
       If ELFFLA <> 'Y';
          Clear ELPP3C;
          Clear ELPPC3;
       Endif;

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
            Select;
            When ELARSM = ' ';
               RemoveInvoices(ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr:'A');
            When ELARSM = 'Y';
               // Get The state reporting division
               stRptDiv = GetStRptDv(ELco#:Elfnd:Elemp#);
               RemoveInvoices(ELCo#:ELFnd:ELEmp#:stRptDiv:ELFyr:'A');
            EndSl;
         EndIf;

         If scCTST <> 'Y' and ELCTST = 'Y' ;
            Select;
            When ELARSM = ' ';
               ResendInvoices(ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr:'A');
            When ELARSM = 'Y';
               // Get The state reporting division
               stRptDiv = GetStRptDv(ELco#:Elfnd:Elemp#);
               ResendInvoices(ELCo#:ELFnd:ELEmp#:stRptDiv:ELFyr:'A');
            EndSl;
         EndIf;

       //*** Beg Add *** 10/25/23 *** WC2922 ***************************

       // Audits
       ELCTST = scCTST;
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

       ELPGM  = Q1PGM;
       ELUSER = Q1USER;
       ELTIME = Timeto6(SystemTime());
       ELDT   = Dateto7(SystemDate());

       Update PEL0P;

       If clientID = '111';
          Chain (ELco#:ELfnd:ELfyr:ELemp#:ELDiv) pdv00;
          DVamt  = scDivAmt;
          DVpgm  = q1Pgm;
          DVuser = q1User;
          DVdt   = Dateto7(SystemDate());
          DVtime = Timeto6(SystemTime());

          If %found;
             Update pdv00;
          Else;
             DVco#  = ELco#;
             DVfnd  = ELfnd;
             DVfyr  = ELfyr;
             DVemp# = ELemp#;
             DVdiv  = ELdiv;
             DVipgm = q1Pgm;
             DViusr = q1User;
             DVidt  = Dateto7(SystemDate());
             DVtime = Timeto6(SystemTime());

             Write pdv00;
             Feod WMDVL;
          Endif;
       Endif;
       //*** Beg Add *** 06/24/24 *************************************
       If isUpdColl = 'Y';
          Exsr SrUpdCollAllDiv;
       ENDIF;
       //*** End Add *** 06/24/24 *************************************
      /end-free
     C                   Endsr                                                  Save

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       error = *off;

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
       Clear erDivdPlan;
       Clear msDivdPlan;
       Clear erFlatPrct;
       Clear msFlatPrct;
       Clear erFlatDef;
       Clear msFlatDef;
       Clear erDivAmt;
       Clear mgDivAmt;
       //*** Beg Add *** 06/19/25 ***************************************
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
       //*** End Add *** 06/19/25 ***************************************

      /end-free
     C                   Endsr                                                  ClearErrors
       //*** Beg Add *** 06/24/24 **************************************
       Begsr SrUpdCollAllDiv;
         Setll (ELco#:ELfnd:ELfyr:ELemp#) pel01;
         Reade (ELco#:ELfnd:ELfyr:ELemp#) pel01;
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
            Reade (ELco#:ELfnd:ELfyr:ELemp#) pel01;
         Enddo;
       Endsr;
       //*** End Add *** 06/24/24 **************************************

     P AddInfoEdit     E

       //***************************************************************************//
       //===================================================================
       // Agent History Edit Popup
       //===================================================================
     P AgentEdit       B                   export
     D AgentEdit       PI
     D ahRRNin                        9  0 const
     D quoteMode                      1    const
     D elRRNin                        9  0 const options(*nopass)
     D inMode                         1    const options(*nopass)

     D xxKey           s                   like(BSKEY)
     D xxStat          s                   like(AHSTAT)
     D xxQTyp          s                   like(AHSTAT)
     D xxRqfl          s                   like(AHSTAT)
     D xxEfdt          s                   like(AHEFDT)
     D x7Efdt          s                   like(AHEFDT)
     D x7Date          s                   like(AHEFDT)
     D xxEffDate       s                   like(AHEFDT)
     D xxRate          s                   like(AHRT#)
     D xxPlan          s                   like(ARPLAN)
     D xxAgt#          s                   like(AHAGT#)
     D xxProd          s                   like(AHPROD)
     D oldPL           s                   like(ARPLAN)
     D ##Fyr           s                   like(AHFyr)
     D ##Agt#          s                   like(AHAGT#)
     D xxAgt1          s                   like(AHAGT#)
     D xxAgt2          s                   like(AHAGT#)
     D xxDiv1          s                   like(AHDIV)
     D xxStt1          s                   like(AHSTAT)
     D xxFlg           s                   like(AHSTAT)
     D agtError        s               n   inz(*off)
     D agtAddMode      s               n   inz(*off)
     D xxMode          s              1    inz(' ')
     D emRRN           s              9  0
     D elRRN           s              9  0
     D ahRRN           s              9  0
     D message         s            100
     D message2        s            300
     D prComm          s              1
     D p               s              1  0
     D sysDate         s              8P 0
     D ACCDIR          C                   CONST('Account Current to -
     D                                     Direct Bill.')
     D DIRACC          C                   CONST('Direct Bill to -
     D                                     Account Current.')
     D svStat          s                   like(AHSTAT)
     D svAgent1        s                   like(AHAGT#)
     D svAgent2        s                   like(AHAGT#)
     D svAgent3        s                   like(AHAGT#)
     D svProd1         s                   like(AHPROD)
     D svProd2         s                   like(AHPROD)
     D svProd3         s                   like(AHPROD)
     D s1Agent2        s                   like(AHAGT#)
     D s1Rate1         s                   like(AHRT#)
     D s1Rate2         s                   like(AHRT#)
     D s1Prod1         s                   like(AHPROD)
     D s1Prod2         s                   like(AHPROD)
     D sameAG          S              1    inz('0')
     D sameEF          S              1    inz('0')
     D NOTDSP          S              1    inz('0')
     D NOTDSQ          S              1    inz('0')
     D xxDiv           s                   like(AHDIV)
     D xxANam          s             40    inz(' ')
     D xxAHRrn         s              9  0 inz(0)
     D xxAARrn         s              9  0 inz(0)
     D xxAGRrn         s              9  0 inz(0)
     D CHG             S              1    DIM(3)
     D                 DS
     D  SVPL                   1      3
     D                                     DIM(3)
     D  SVPLAN                 1      1
     D  SVPLN2                 2      2
     D  SVPLN3                 3      3
     D                 DS
     D  X8EFDT                 1      8  0
     D  X8DAY                  3      4  0
     D                 DS
     D  XXPODT                 1      7  0
     D  XYYYMM                 1      5  0
     D                 DS
     D  SYSDT                  1      7  0
     D  SYYYMM                 1      5  0

     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    AHCo#
     C                   Kfld                    AHFnd
     C                   Kfld                    AHEmp#
     C                   Kfld                    AHDiv
     C                   Kfld                    AHFyr
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C*
     C     key1WMEML     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C*
     C* Employer A/R File Key List
     C*
     C     keyWMEAL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     key1WMEAL     KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    X7EFDT
     C*
     C* Admin/Darts Control File Key List
     C*
     C     keyWMEXL      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C*
     C* Agent History File Key List
     C*
     C     keyWMAHL      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C                   Kfld                    ELFyr
     C*
     C     key1WMAHL     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C                   Kfld                    ELFyr
     C                   Kfld                    xxEffDate
     C*
     C* Agent Rate File Key List
     C*
     C     keyWMARL      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    xxRate
     C                   Kfld                    xxEfdt
     C*
     C     key1WMARL     Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    xxRate
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     keyWMAAL1     KLIST
     C                   KFLD                    ELCo#
     C                   KFLD                    ELFnd
     C                   KFLD                    xxAGT#
     C*
     C*  AGENT PRODUCER FILE
     C*
     C     keyWMAPL      KLIST
     C                   KFLD                    ELCo#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    xxPROD
      *
      *  Producer State Info
      *
     C     keyWMPSL      KLIST
     C                   KFLD                    APCO#
     C                   KFLD                    APFEIN
     C                   KFLD                    APMOD
     C                   KFLD                    APPROD
     C*
     C* State File Key List
     C*
     C     keySTSTL3     Klist
     C                   Kfld                    xxKey
     C*
     C*  Agent Commission file
     C*
     C     key3WMACL     KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHFYR
     C                   KFLD                    AHEMP#
     C                   KFLD                    AHDIV
     C*
     C*  Rate Set Code file
     C*
     C     keyWMRSL      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C*
     C* Billing Request File Key List
     C*
     C     keyWABLL      Klist
     C                   Kfld                    ELCo#
     C                   Kfld                    ELFnd
     C                   Kfld                    ELFyr
     C                   Kfld                    ELEmp#
     C                   Kfld                    ELDiv
     C*
     C*  Quote Status FILE
     C*
     C     keyWAQTL7     KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    xxSTAT
     C*
     C     key1WAQTL7    KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    xxSTAT
     C                   KFLD                    scAgent1
     C*
     C     key2WAQTL7    KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    xxSTAT
     C                   KFLD                    xxAGT1
     C*
     C     key3WAQTL7    KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    xxDIV1
     C                   KFLD                    xxSTT1
     C                   KFLD                    ##AGT#
     C*
     C     key4WAQTL7    KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    xxDIV1
     C                   KFLD                    xxSTT1
     C                   KFLD                    ##AGT#
     C                   KFLD                    x7DATE
     C*
      /free

       Open WPIPOLPOP;

       GetClientID(clientID);

       agtAddMode = *off;
       agtDivMode = *on;
       agtFyrMode = *on;
       dsbAgent1 = *off;

       If  ahRRNin <> 0;
          Chain(n) ahRRNin PAH0P;
          ahRRN = ahRRNin;
          Chain keyWDELL PEL00;
          elRRN = dbrrn;
          agtEdit = *on;
       Else;
          Chain(n) elRRNin PEL0P;
          elRRN = elRRNin;
          agtAddMode = *on;
          agtEdit = *off;
       Endif;

       If quoteMode = 'Q';
          xxStat = 'Q';
       Else;
          xxStat = 'W';
       Endif;

       If %parms >= 4;
          xxMode = inMode;
       Endif;

       Exsr QuoteType;

       Chain keyWMEML PEM00;
       emRRN = dbRrn4;

       xxRqfl = EMRQFL;

       Chain keyWMEXL PEX00;

       // Load security levels
       Exsr Compapi;

       //* Populate the popup window with selected record.

       Exsr LoadAgt;

       Exsr ClearErrors;
       Clear agtSave;
       Clear agtCancel;

       //* Display popup window for the agent history record.

       Dou agtSave = *on or agtCancel = *on;

          Dou agtError = *off or agtCancel = *on;

             Exsr SetCriteria;

                 scPF1rrn = elrrn;
             Exfmt rcdAgtEdit;

             If agtCancel = *off;
                Exsr AgentCheck;
             Endif;

          Enddo;

       Enddo;

       If agtSave = *on;
          Exsr AgentSave;
       Endif;

       Close WPIPOLPOP;

      /end-free

       //***************************************************************************//
       //===================================================================
       // Company API for security
       //===================================================================
     C     Compapi       Begsr
     C*
     C                   move      elco#         ##co#
     C                   move      elfnd         ##fnd
     C*
     C                   CALL      'WA001R'      COMPL                  99
     C*
     C                   Endsr


       //***************************************************************************//
       //===================================================================
       // QuoteType - State reporting division quote type
       //===================================================================
     C     QuoteType     Begsr
     C*
      /free
       xxQtyp = ' ';
       xxFlg = ' ';
       xxDiv1 = 0;
       xxStt1 = 'Q';

       Setll key1WMEML PEM00;
       Reade key1WMEML PEM00;
       Dow not %eof;
          If EMSTRP = 'Y';
             xxDiv1 = EMDIV;
             xxFlg = '1';
             Leave;
          Endif;
          Reade key1WMEML PEM00;
       Enddo;

      /end-free
     C*
     C     XXFLG         IFEQ      '1'
     C*
     C                   CALL      'WA002R'                             99
     C                   PARM                    ELCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    XXDIV1
     C                   PARM                    ELFYR
     C                   PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C*
     C                   eval      x7Date = Dateto7(SystemDate())
     C*
     C     key4WAQTL7    SETGT     PQT07
     C     key3WAQTL7    READPE(N) PQT07                                  56
     C     *IN56         IFEQ      *OFF
     C                   MOVE      QTTYPE        xxQTYP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Endsr                                                  QuoteType Subr

       //***************************************************************************//
       //===================================================================
       // Load Agent History window
       //===================================================================
     C     LoadAgt       Begsr
     C*
      /free

       scCo#      = ELCo#;
       scFnd      = ELFnd;
       scDivision = ELDiv;
       scYear     = ELFyr;

       xxAHRRN = 0;

       If xxMode = 'B';
          xxDiv = getStRptDv(ELCO#:ELFND:ELEMP#);
          GetEmpAgt(ELCo#:ELFND:ELEMP#:xxDiv:ELFYR:ELPFDT:xxANam:xxAHRrn:
           xxAARrn:xxAGRrn);
          If xxAHRrn <> 0;
             Chain(n) xxAHRRN PAH0P;
             If not %found;
                xxAHRRN = 0;
             Endif;
          Endif;
       Endif;

       If not agtAddMode or xxAHRrn <> 0;

          scEffDate  = dtMDYY(AHEfdt);
          scStatus   = AHStat;
          svStat = AHStat;

          scAgent1 = AHAGT#;
          scAgt1   = AHAGT#;
          scProd1  = AHPROD;
          scRate1  = AHRT#;

          xxRate = AHRT#;
          xxEfdt = AHEfdt;
          Clear svPlan;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             scPlan1 = GetPlanTypeDesc(ARplan);
             svPlan = ARPlan;
          Endif;

          scAgent2 = AHSAgt;
          scAgt2   = AHSAgt;
          scProd2  = AHSPrd;
          scRate2  = AHSRt#;

          xxRate = AHSRt#;
          xxEfdt = AHEfdt;
          Clear svPln2;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             scPlan2 = GetPlanTypeDesc(ARplan);
             svPln2 = ARPlan;
          Endif;

          scAgent3 = AH3Agt;
          scAgt3   = AH3Agt;
          scProd3  = AH3Prd;
          scRate3  = AH3Rt#;

          xxRate = AH3Rt#;
          xxEfdt = AHEfdt;
          Clear svPln3;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             scPlan3 = GetPlanTypeDesc(ARplan);
             svPln3 = ARPlan;
          Endif;

          scAgtRec1 = AHAGRC;
          scAgtR1   = AHAGRC;
          scProdRec1  = AHPRRC;
          scRateRec1  = AHRTRC;

          xxRate = AHRTRC;
          xxEfdt = AHEfdt;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             scPlanRec1 = GetPlanTypeDesc(ARplan);
          Endif;

          scAgtRec2 = AH2ARC;
          scAgtR2   = AH2ARC;
          scProdRec2  = AH2PRC;
          scRateRec2  = AH2RRC;

          xxRate = AH2RRC;
          xxEfdt = AHEfdt;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             scPlanRec2 = GetPlanTypeDesc(ARplan);
          Endif;

          scAgtRec3 = AH3ARC;
          scAgtR3   = AH3ARC;
          scProdRec3  = AH3PRC;
          scRateRec3  = AH3RRC;

          xxRate = AH3RRC;
          xxEfdt = AHEfdt;
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             scPlanRec3 = GetPlanTypeDesc(ARplan);
          Endif;

          Select;
             When AHCaf1 = 'Y';
                scControl1 = 1;
             When AHCaf2 = 'Y';
                scControl2 = 2;
          Endsl;

          Select;
             When AHCrf1 = 'Y';
                scContRec1 = 1;
             When AHCrf2 = 'Y';
                scContRec2 = 2;
          Endsl;

          If xxStat  = 'W';
             dsbAgent1 = *on;
          Endif;
       Else;
          scEffDate  = 0;
          scStatus   = 'A';
          svStat     = scStatus;
          scAgent1   = 0;
          scAgent2   = 0;
          scAgent3   = 0;
          scAgtRec1  = 0;
          scAgtRec2  = 0;
          scAgtRec3  = 0;
          scProd1    = 0;
          scProd2    = 0;
          scProd3    = 0;
          scProdRec1 = 0;
          scProdRec2 = 0;
          scProdRec3 = 0;
          scRate1    = 0;
          scRate2    = 0;
          scRate3    = 0;
          scRateRec1 = 0;
          scRateRec2 = 0;
          scRateRec3 = 0;
          Clear scControl1;
          Clear scContRec1;
          Clear scControl2;
          Clear scContRec2;

       Endif;

       // Save rate values
       saveRate1    = scRate1;
       saveRate2    = scRate2;
       saveRate3    = scRate3;
       saveRateRec1 = scRateRec1;
       saveRateRec2 = scRateRec2;
       saveRateRec3 = scRateRec3;

       Clear divChoices;
       divChoices = 'EMCO# = ' + %char(ELCO#) + ' AND EMFND = '
        + %char(ELFND) + ' AND EMEMP# = ' + %char(ELEMP#);

       Clear fyrChoices;
       fyrChoices = 'ELCO# = ' + %char(ELCO#) + ' AND ELFND = '
        + %char(ELFND) + ' AND ELEMP# = ' + %char(ELEMP#)
        + ' AND ELDIV = ' + %char(ELDIV);

       Clear agChoices;
       agChoices = 'AACO# = ' + %char(ELCO#) + ' AND AAFND = '
        + %char(ELFND) + ' AND AASTAT = ''A''';

       Clear arChoices;
       arChoices = 'ARCO# = ' + %char(ELCO#) + ' AND ARFND = '
        + %char(ELFND) + ' AND ARSTAT = ''A''';

       s1Agent2 = scAgent2;
       s1Rate1  = scRate1;
       s1Rate2  = scRate2;
       s1Prod1  = scProd1;
       s1Prod2  = scProd2;

       svAgent1 = scAgent1;
       svAgent2 = scAgent2;
       svAgent3 = scAgent3;
       svProd1  = scProd1;
       svProd2  = scProd2;
       svProd3  = scProd3;

       Exsr PriorComm;

      /end-free
     C*
     C                   Endsr                                                  LoadAgt subr

       //===================================================================
       // SetCriteria - Load the search criteria fields
       //===================================================================
     C     SetCriteria   Begsr
      /free

       pr1Choice = ' ';

       If scAgent1 = 0;
          pr1Disabl = *on;
       Else;
          xxAgt# = scAgent1;
          Chain keyWMAAL1 PAA01;
          If %found;
             pr1Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN = '
              + %char(AAFEIN) + ' AND APMOD = ' + %char(AAMOD)
              + ' AND APSTAT = ''A''';
          Endif;
          pr1Disabl = *off;
       Endif;

       pr2Choice = ' ';

       If scAgent2 = 0;
          pr2Disabl = *on;
       Else;
          xxAgt# = scAgent2;
          Chain keyWMAAL1 PAA01;
          If %found;
             pr2Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN = '
              + %char(AAFEIN) + ' AND APMOD = ' + %char(AAMOD)
              + ' AND APSTAT = ''A''';
          Endif;
          pr2Disabl = *off;
       Endif;

       pr3Choice = ' ';

       If scAgent3 = 0;
          pr3Disabl = *on;
       Else;
          xxAgt# = scAgent3;
          Chain keyWMAAL1 PAA01;
          If %found;
             pr3Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN = '
              + %char(AAFEIN) + ' AND APMOD = ' + %char(AAMOD)
              + ' AND APSTAT = ''A''';
          Endif;
          pr3Disabl = *off;
       Endif;

       prR1Choice = ' ';

       If scAgtRec1 = 0;
          prR1Disabl = *on;
       Else;
          xxAgt# = scAgtRec1;
          Chain keyWMAAL1 PAA01;
          If %found;
             prR1Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN = '
              + %char(AAFEIN) + ' AND APMOD = ' + %char(AAMOD)
              + ' AND APSTAT = ''A''';
          Endif;
          prR1Disabl = *off;
       Endif;

       prR2Choice = ' ';

       If scAgtRec2 = 0;
          prR2Disabl = *on;
       Else;
          xxAgt# = scAgtRec2;
          Chain keyWMAAL1 PAA01;
          If %found;
             prR2Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN = '
              + %char(AAFEIN) + ' AND APMOD = ' + %char(AAMOD)
              + ' AND APSTAT = ''A''';
          Endif;
          prR2Disabl = *off;
       Endif;

       prR3Choice = ' ';

       If scAgtRec3 = 0;
          prR3Disabl = *on;
       Else;
          xxAgt# = scAgtRec3;
          Chain keyWMAAL1 PAA01;
          If %found;
             prR3Choice = 'APCO# = ' + %char(scCO#) + ' AND APFEIN = '
              + %char(AAFEIN) + ' AND APMOD = ' + %char(AAMOD)
              + ' AND APSTAT = ''A''';
          Endif;
          prR3Disabl = *off;
       Endif;

      /end-free
     C                   Endsr                                                  SetCriteria Subr


       //***************************************************************************//
       //===================================================================
       // Agent History Error Checking
       //===================================================================
     C     AgentCheck    Begsr
     C*
      /free

       Exsr ClearErrors;

       // If prior commission exists, then do not allow for agent/producer/rate
       // change and reset the fields to starting values.

       If prComm = 'Y';

          If scRate1 <> s1Rate1 or
           scProd1 <> s1Prod1 or
           scAgent2 <> s1Agent2 or
           scRate2 <> s1Rate2 or
           scProd2 <> s1Prod2;

             e2Prod1   = *on;
             m2Prod1   = %trimr(msg(7)) + ' ' +
                         %trimr(msg(8)) + ' ' +
                         %trimr(msg(9)) + ' ' +
                         %trimr(msg(10)) + ' ' +
                         %trimr(msg(11));
             agtError = *on;

             scAgent2 = s1Agent2;
             scProd1  = s1Prod1;
             scProd2  = s1Prod2;
             scRate1  = s1Rate1;
             scRate2  = s1Rate2;

          Endif;

       Endif;

       // Validate effective date (for Add Mode)

       If agtAddMode;
          erEffDate = ValidDate(scEffDate:message);
          If erEffDate = *on;
             msEffDate = message;
             agtError   = *on;
          Endif;
       Endif;


       // Validate 1st Agent #, Producer, and Rate

       If scAgent1 <> 0;

          erAgent1 = ValidAgent(ELCo#:ELFnd:scAgent1:message);
          If erAgent1 = *on;
             msAgent1 = message;
             agtError = *on;
          Endif;

          If erProd1 = *off and e2Prod1 = *off;
             erProd1 = ValidProducer(ELCo#:ELFnd:scAgent1:scProd1:message);
             If erProd1 = *on;
                msProd1 = message;
                agtError = *on;
             Endif;
          Endif;

          If scRate1 <> saveRate1;
             erRate1 = ValidAgtRate(ELCo#:ELFnd:scRate1:scEffDate:message);
             If erRate1 = *on;
                msRate1 = message;
                agtError = *on;
             Else;
                saveRate1 = scRate1;
             Endif;
          Endif;

          If erAgent1 = *off;
             erAgent1 = PolAgtDate(ELCo#:ELFnd:scAgent1:scEffDate:message);
             If erAgent1 = *on;
                msAgent1 = message;
                agtError = *on;
             Endif;
          Endif;

          If erProd1 = *off;
             erProd1 = PolProdDate(ELCo#:ELFnd:scAgent1:scProd1:scEffDate:
              message);
             If erProd1 = *on;
                msProd1 = message;
                agtError = *on;
             Endif;
          Endif;
       Endif;

       // Validate 2nd Agent #, Producer, and Rate

       If scAgent2 <> 0;

          erAgent2 = ValidAgent(ELCo#:ELFnd:scAgent2:message);
          If erAgent2 = *on;
             msAgent2 = message;
             agtError = *on;
          Endif;

          erProd2 = ValidProducer(ELCo#:ELFnd:scAgent2:scProd2:message);
          If erProd2 = *on;
             msProd2 = message;
             agtError = *on;
          Endif;

          If scRate2 <> saveRate2;
             erRate2 = ValidAgtRate(ELCo#:ELFnd:scRate2:scEffDate:message);
             If erRate2 = *on;
                msRate2 = message;
                agtError = *on;
             Else;
                saveRate2 = scRate2;
             Endif;
          Endif;

          If erAgent2 = *off;
             erAgent2 = PolAgtDate(ELCo#:ELFnd:scAgent2:scEffDate:message);
             If erAgent2 = *on;
                msAgent2 = message;
                agtError = *on;
             Endif;
          Endif;

          If erProd2 = *off;
             erProd2 = PolProdDate(ELCo#:ELFnd:scAgent2:scProd2:scEffDate:
              message);
             If erProd2 = *on;
                msProd2 = message;
                agtError = *on;
             Endif;
          Endif;
       Endif;

       // Validate 3rd Agent #, Producer, and Rate

       If scAgent3 <> 0;

          erAgent3 = ValidAgent(ELCo#:ELFnd:scAgent3:message);
          If erAgent3 = *on;
             msAgent3 = message;
             agtError = *on;
          Endif;

          erProd3 = ValidProducer(ELCo#:ELFnd:scAgent3:scProd3:message);
          If erProd3 = *on;
             msProd3 = message;
             agtError = *on;
          Endif;

          If scRate3 <> saveRate3;
             erRate3 = ValidAgtRate(ELCo#:ELFnd:scRate3:scEffDate:message);
             If erRate3 = *on;
                msRate3 = message;
                agtError = *on;
             Else;
                saveRate3 = scRate3;
             Endif;
          Endif;

          If erAgent3 = *off;
             erAgent3 = PolAgtDate(ELCo#:ELFnd:scAgent3:scEffDate:message);
             If erAgent3 = *on;
                msAgent3 = message;
                agtError = *on;
             Endif;
          Endif;

          If erProd3 = *off;
             erProd3 = PolProdDate(ELCo#:ELFnd:scAgent3:scProd3:scEffDate:
              message);
             If erProd3 = *on;
                msProd3 = message;
                agtError = *on;
             Endif;
          Endif;
       Endif;

       // Validate 1st Agent of Record, Producer, and Rate

       sysdate = Dateto8(SystemDate());
       If scAgtRec1 <> 0;

          erAgtRec1 = ValidAgent(ELCo#:ELFnd:scAgtRec1:message);
          If erAgtRec1 = *on;
             msAgtRec1 = message;
             agtError  = *on;
          Endif;

          erProdRec1 = ValidProducer(ELCo#:ELFnd:scAgtRec1:scProdRec1:message);
          If erProdRec1 = *on;
             msProdRec1 = message;
             agtError   = *on;
          Endif;

          If scRateRec1 <> saveRateRec1;
             erRateRec1 = ValidAgtRate(ELCo#:ELFnd:scRateRec1:sysDate:message);
             If erRateRec1 = *on;
                msRateRec1 = message;
                agtError   = *on;
             Else;
                saveRateRec1 = scRateRec1;
             Endif;
          Endif;

          If erAgtRec1 = *off;
             erAgtRec1 = PolAgtDate(ELCo#:ELFnd:scAgtRec1:sysDate:message);
             If erAgtRec1 = *on;
                msAgtRec1 = message;
                agtError  = *on;
             Endif;
          Endif;

          If erProdRec1 = *off;
             erProdRec1 = PolProdDate(ELCo#:ELFnd:scAgtRec1:scProdRec1:
              sysDate:message);
             If erProdRec1 = *on;
                msProdRec1 = message;
                agtError = *on;
             Endif;
          Endif;
       Endif;

       // Validate 2nd Agent of Record, Producer, and Rate

       If scAgtRec2 <> 0;

          erAgtRec2 = ValidAgent(ELCo#:ELFnd:scAgtRec2:message);
          If erAgtRec2 = *on;
             msAgtRec2 = message;
             agtError  = *on;
          Endif;

          erProdRec2 = ValidProducer(ELCo#:ELFnd:scAgtRec2:scProdRec2:message);
          If erProdRec2 = *on;
             msProdRec2 = message;
             agtError   = *on;
          Endif;

          If scRateRec2 <> saveRateRec2;
             erRateRec2 = ValidAgtRate(ELCo#:ELFnd:scRateRec2:sysDate:message);
             If erRateRec2 = *on;
                msRateRec2 = message;
                agtError   = *on;
             Else;
                saveRateRec2 = scRateRec2;
             Endif;
          Endif;

          If erAgtRec2 = *off;
             erAgtRec2 = PolAgtDate(ELCo#:ELFnd:scAgtRec2:sysDate:message);
             If erAgtRec2 = *on;
                msAgtRec2 = message;
                agtError  = *on;
             Endif;
          Endif;

          If erProdRec2 = *off;
             erProdRec2 = PolProdDate(ELCo#:ELFnd:scAgtRec2:scProdRec2:
              sysDate:message);
             If erProdRec2 = *on;
                msProdRec2 = message;
                agtError = *on;
             Endif;
          Endif;
       Endif;

       // Validate 3rd Agent of Record, Producer, and Rate

       If scAgtRec3 <> 0;

          erAgtRec3 = ValidAgent(ELCo#:ELFnd:scAgtRec3:message);
          If erAgtRec3 = *on;
             msAgtRec3 = message;
             agtError  = *on;
          Endif;

          erProdRec3 = ValidProducer(ELCo#:ELFnd:scAgtRec3:scProdRec3:message);
          If erProdRec3 = *on;
             msProdRec3 = message;
             agtError  = *on;
          Endif;

          If scRateRec3 <> saveRateRec3;
             erRateRec3 = ValidAgtRate(ELCo#:ELFnd:scRateRec3:sysDate:message);
             If erRateRec3 = *on;
                msRateRec3 = message;
                agtError   = *on;
             Else;
                saveRateRec3 = scRateRec3;
             Endif;
          Endif;

          If erAgtRec3 = *off;
             erAgtRec3 = PolAgtDate(ELCo#:ELFnd:scAgtRec3:sysDate:message);
             If erAgtRec3 = *on;
                msAgtRec3 = message;
                agtError  = *on;
             Endif;
          Endif;

          If erProdRec3 = *off;
             erProdRec3 = PolProdDate(ELCo#:ELFnd:scAgtRec3:scProdRec3:
              sysDate:message);
             If erProdRec3 = *on;
                msProdRec3 = message;
                agtError = *on;
             Endif;
          Endif;
       Endif;

       // 1st and 2nd agents cannot be the same.
       If scAgent1 = scAgent2 and scProd1 = scProd2;
          If erAgent1 = *off and e2Agent1 = *off;
             e2Agent1 = *on;
             m2Agent1 = 'Primary agency/producer is the same as '+
              'Secondary agency/producer.';
             agtError = *on;
          Endif;
       Endif;

       // Do not allow producer change if comission exists.
       If not agtAddMode;
       If erProd1 = *off and e2Prod1 = *off;
          e2Prod1 = PolProdChg(scProd1:svProd1:emRRN:scYear:scAgent1:message);
          If e2Prod1 = *on;
             m2Prod1 = message;
             agtError = *on;
          Endif;
       Endif;
       Endif;

       // Do not allow an inactive record to be activated.
       If scStatus = 'A' and svStat = 'I' and erStatus = *off
        and e2Status = *off;
          agtError = *on;
          e2Status = *on;
          m2Status = 'Activation is not allowed, add a new agent +
           history record instead.';
       Endif;

       // Do not allow a record to be inactivated if there is no replacement.
       If scStatus = 'I' and svStat = 'A' and erStatus = *off
        and e2Status = *off;
          erStatus = PolAgtCount(elRRN:xxStat:scEffDate:message);
          If erStatus = *on;
             msStatus = message;
             agtError = *on;
          Endif;
       Endif;

       // check for controlling agency
       If scControl1 > 0 and scControl2 > 0;
          erControl1 = *on;
          erControl2 = *on;
          agtError = *on;
       Endif;

       If scContRec1 > 0 and scContRec2 > 0;
          erContRec1 = *on;
          erContRec2 = *on;
          agtError = *on;
       Endif;
       //******************************************************************************
       // Edit for Popup window messages from this point on.                          *
       //******************************************************************************

       // Display popup window if rate change causes plan change and prior info.
       // 1st Rate
       If agtError = *off;
          xxRate = scRate1;
          xxEfdt = dtCYMD(scEffDate);
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             p = 1;
             Exsr ChkPln;
          Endif;
       Endif;

       // 2nd Rate
       If agtError = *off;
          xxRate = scRate2;
          xxEfdt = dtCYMD(scEffDate);
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             p = 2;
             Exsr ChkPln;
          Endif;
       Endif;

       // 3rd Rate
       If agtError = *off;
          xxRate = scRate3;
          xxEfdt = dtCYMD(scEffDate);
          Setgt keyWMARL PAR00;
          Readpe key1WMARL PAR00;
          If not %eof;
             p = 3;
             Exsr ChkPln;
          Endif;
       Endif;


       // Display popup if record is to be inactivated and prior commissions exist.
       If scStatus = 'I' and svStat = 'A' and agtError = *off;
          Exsr CheckComm;
          If commission = *on;
             Clear agtMsgCnl;
             Clear agtMsgCon;
             Dou agtMsgCnl = *on or agtMsgCon = *on;
                Exfmt rcdAgtMsg;
             Enddo;

             If agtMsgCnl = *on;
                agtError = *on;
             Endif;
          Endif;
       Endif;

       // Display popup window if primary agent matches another record.
       If notDSP = *off and agtError = *off;
          Exsr SamAgt;
          If sameAG = *on;
             Clear agtMsgCnl;
             Clear agtMsgCon;
             Dou agtMsgCnl = *on or agtMsgCon = *on;
                Exfmt rcdAgtMsg;
             Enddo;

             If agtMsgCnl = *on;
                agtError = *on;
             Endif;

             If agtMsgCon = *on;
                notDSP = *on;
             Endif;
          Endif;
       Endif;

       // Display popup window if another record with same eff date.
       If notDSQ = *off and agtError = *off and agtAddMode;
          Exsr SamEff;
          If sameEF = *on;
             Clear agtMsgCnl;
             Clear agtMsgCon;
             Dou agtMsgCnl = *on or agtMsgCon = *on;
                Exfmt rcdAgtMsg;
             Enddo;

             If agtMsgCnl = *on;
                agtError = *on;
             Endif;

             If agtMsgCon = *on;
                notDSQ = *on;
             Endif;
          Endif;
       Endif;

       // Verify producer is licensed in state.
       If scProd1 <> 0 and agtError = *off;
          xxAgt# = scAgent1;
          xxProd = scProd1;

          Exsr PRLIC;

          If xxLIC = 'N';
             msProd1 = 'WC31403';
             erProd1 = *on;
             agtError = *on;
          Endif;

       Endif;

       // Verify producer is licensed in state.
       If scProd2 <> 0 and agtError = *off;
          xxAgt# = scAgent2;
          xxProd = scProd2;

          Exsr PRLIC;

          If xxLIC = 'N';
             msProd2 = 'WC31403';
             erProd2 = *on;
             agtError = *on;
          Endif;

       Endif;

       // Verify producer is licensed in state.
       If scProd3 <> 0 and agtError = *off;
          xxAgt# = scAgent3;
          xxProd = scProd3;

          Exsr PRLIC;

          If xxLIC = 'N';
             msProd3 = 'WC31403';
             erProd3 = *on;
             agtError = *on;
          Endif;

       Endif;

       // Verify producer is licensed in state.
       If scProdRec1 <> 0 and agtError = *off;
          xxAgt# = scAgtRec1;
          xxProd = scProdRec1;

          Exsr PRLIC;

          If xxLIC = 'N';
             msProdRec1 = 'WC31403';
             erProdRec1 = *on;
             agtError = *on;
          Endif;

       Endif;

       // Verify producer is licensed in state.
       If scProdRec2 <> 0 and agtError = *off;
          xxAgt# = scAgtRec2;
          xxProd = scProdRec2;

          Exsr PRLIC;

          If xxLIC = 'N';
             msProdRec2 = 'WC31403';
             erProdRec2 = *on;
             agtError = *on;
          Endif;

       Endif;

       // Verify producer is licensed in state.
       If scProdRec3 <> 0 and agtError = *off;
          xxAgt# = scAgtRec3;
          xxProd = scProdRec3;

          Exsr PRLIC;

          If xxLIC = 'N';
             msProdRec3 = 'WC31403';
             erProdRec3 = *on;
             agtError = *on;
          Endif;

       Endif;





      /end-free
     C                   ENDSR                                                  AgentCheck
     C*
     C*================================================================
     C* CHKAR - Check for A/R in a prior month for year
     C*================================================================
     C*
     C     CHKAR         BEGSR
     C*
     C                   MOVE      *OFF          PRIOR             1
     C*
     C                   Z-ADD     scEffDate     X8EFDT
     C                   Z-ADD     1             X8DAY
     C                   eval      X7EFDT = dtCYMD(X8EFDT)
     C*
     C     key1WMEAL     SETGT     PEA00
     C     keyWMEAL      READPE    PEA00                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EATRDT        ANDEQ     X7EFDT
     C     keyWMEAL      READPE    PEA00                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           PRIOR
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHKAR
     C*
     C*================================================================
     C* CheckComm - Check for prior commission for inactivation.
     C*================================================================
     C*
     C     CheckComm     BEGSR
     C*
     C                   MOVE      *OFF          commission        1
     C                   CLEAR                   WDMSG1
     C                   CLEAR                   WDMSG2
     C                   CLEAR                   WDMSG3
     C                   CLEAR                   WDMSG4
     C                   CLEAR                   WDMSG5
     C*
     C                   EVAL      sysDt = Dateto7(SystemDate())
      *
     C     key3WMACL     SETLL     PAC00
     C     key3WMACL     READE     PAC00                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-add     ACPODT        xxPODT
     C                   If        (ACAgt# = AHAgt# and ACProd = AHProd) or
     C                             (ACAgt# = AHSAgt and ACProd = AHSPrd) or
     C                             (ACAgt# = AH3Agt and ACProd = AH3Prd)
      *
     C     XYYYMM        IFLT      SYYYMM
     C                   MOVE      'Y'           commission
     C                   MOVEL(P)  MSG(31)       WDMSG2
     C                   MOVEL(P)  MSG(32)       WDMSG3
     C                   MOVEL(P)  MSG(33)       WDMSG4
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     key3WMACL     READE     PAC00                                  60
     C                   ENDDO
     C*
     C                   ENDSR                                                  CheckComm Subr
     C*
     C*================================================================
     C* CHKLST - Check for last agent history record for year
     C*================================================================
     C*
     C     CHKLST        BEGSR
     C*
     C                   MOVE      *OFF          OLDPLN            1
     C*
     C     keyWMAHL      SETGT     PAH00
     C     keyWMAHL      READPE    PAH00                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     dbrrn4        ANDNE     ahRRN
     C*
     C                   SELECT
     C     P             WHENEQ    1
     C     A2AHRT#       ANDNE     *ZERO
     C                   Z-ADD     A2AHRT#       XXRate
     C                   EXSR      RATES
     C                   MOVE      *ON           OLDPLN
     C     P             WHENEQ    2
     C     A2AHSRT#      ANDNE     *ZERO
     C                   Z-ADD     A2AHSRT#      XXRate
     C                   EXSR      RATES
     C                   MOVE      *ON           OLDPLN
     C                   ENDSL
     C*
     C                   MOVEL(P)  XXPLAN        OLDPL
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  CHKLST
     C*
     C*================================================================
     C* CHKLVL - Check levls
     C*================================================================
     C*
     C     CHKLVL        BEGSR
     C*
     C     LVLPL         PLIST
     C                   PARM                    ##UTYP            1            User Type
     C                   PARM                    ##ULVL            3            User Level
     C                   PARM                    ##CTYP            1            Client Type
     C                   PARM                    ##CLVL            3            Client Level
     C                   PARM                    ##EXEC            1            Execution Type
     C                   PARM                    ##RES             1            Result Flag
     C*
     C                   MOVE      ##TYPE        ##UTYP
     C                   MOVE      ##LVL         ##ULVL
     C*_______________________________________________________________
     C*
     C*  OVERRIDE PLAN CHANGE
     C*
     C                   MOVE      EXPLNT        ##CTYP
     C                   MOVE      EXPLNL        ##CLVL
     C                   MOVE      '1'           ##EXEC
     C*
     C                   CALL      'SRCCSC'      LVLPL                  99
     C*
     C                   SELECT
     C     ##RES         WHENEQ    *OFF
     C                   MOVE      *ON           PLNOVR            1
     C                   OTHER
     C                   MOVE      *OFF          PLNOVR            1
     C                   ENDSL
     C*_______________________________________________________________
     C*
     C                   ENDSR                                                  CHKLVL
     C*
     C*================================================================
     C* CHKPLN - Check for plan changes
     C*================================================================
     C*
     C     CHKPLN        BEGSR
     C*
     C                   If        agtAddMode
     C                   EXSR      CHKLST
     C                   Endif
     C*
     C     ARPLAN        IFNE      SVPL(P)
     C     agtAddMode    ANDEQ     *OFF
     C     ARPLAN        ORNE      OLDPL
     C     OLDPLN        ANDEQ     *ON
     C*
     C                   EXSR      CHKAR
     C*
     C     PRIOR         IFEQ      *ON
     C*
     C                   SELECT
     C     ARPLAN        WHENEQ    'A'
     C     SVPL(P)       ANDEQ     *BLANK
     C                   MOVEL(P)  DIRACC        SCCHG            60
     C     ARPLAN        WHENEQ    *BLANK
     C     SVPL(P)       ANDEQ     'A'
     C                   MOVEL(P)  ACCDIR        SCCHG
     C                   OTHER
     C                   CLEAR                   SCCHG
     C                   ENDSL
     C*
     C     SCCHG         IFNE      *BLANKS
     C*
      /free

       Clear scErrHead;
       Clear scRtChange;

       scErrHead = 'Agent Rate Error';

       scRtChange = 'The rate change you have requested will change ' +
        'the agent commission plan from ' + scChg +
        '  This could potentially impact both the Account Current ' +
        'file and the A/R file and would necessitate manual changes ' +
        'to one and/or the other files.';

       Clear scOverride;
       Clear agtOverCnl;
       Clear agtOverCon;

       If PLNOVR = *ON;

          Dou agtOverCnl = *on or agtOverCon = *on;
             Exfmt rcdAgtOver;
          Enddo;

          If agtOverCnl = *on;
             agtError = *on;
          Endif;

       Endif;

       Select;
          When scOverride = 'Y';
             SVPL(P) = ARPlan;
             CHG(P) = *ON;
          When scOverride <> 'Y';

             message = 'The rate you are entering will change the plan '+
              'type of the commission process.  The plan can not be changed '+
              'mid-term if A/R records have been recognized for a previous '+
              'month for the year.  This can be overridden by a user '+
              'with the proper authority.';

             Select;
                When p = 1;
                   e2Rate1 = *on;
                   m2Rate1 = message;
                   agtError = *on;
                When p = 2;
                   e2Rate2 = *on;
                   m2Rate2 = message;
                   agtError = *on;
                When p = 3;
                   e2Rate3 = *on;
                   m2Rate3 = message;
                   agtError = *on;
             Endsl;

          Endsl;

      /end-free
     C*
     C                   ENDIF                                                  SCCHG<>*BLANKS
     C*
     C                   ENDIF                                                  PRIOR=*ON
     C*
     C                   ENDIF                                                  ARPLAN<>SVPL,P
     C*
     C                   ENDSR                                                  CHKPLN

       //===================================================================
       // Check for prior commission in case user tries to change agent.
       //===================================================================
     C     PriorComm     Begsr
     C*
     C                   EVAL      sysDt = Dateto7(SystemDate())
      *
      * DO NOT ALLOW UPDATE IF COMMISSION RECORD EXISTS IN PRIOR MONTH OR
      * IF COMMISSION HAS BEEN PAID IN THE CURRENT MONTH. ANY CHANGES
      * TO AGENT HISTORY WILL HAVE TO BE ACCOMPLISHED IN INACTIVATING
      * THIS RECORD AND WRITING ANOTHER
      *
     C                   MOVE      'N'           prComm
      *
     C                   If        agtEdit = *on
      *
     C     key3WMACL     SETLL     PAC00
     C     key3WMACL     READE     PAC00                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   Z-add     ACPODT        xxPODT
      *
     C     XYYYMM        IFLT      SYYYMM
     C                   MOVE      'Y'           prComm
     C                   LEAVE
     C                   ENDIF
      *
     C     ACCOTP        IFEQ      'P'
     C     XYYYMM        ANDEQ     SYYYMM
     C                   MOVE      'Y'           prComm
     C                   LEAVE
     C                   ENDIF
      *
     C     key3WMACL     READE     PAC00                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   Endsr
      *
      *================================================================
      * PRLIC - Determine if Agt Producer is licensed for emp rating st
      *================================================================
     C     PRLIC         BEGSR
      *
      /Free
       strCode = ELrtCd;
       If ValidStateForProducer(ELco#:ELfnd:xxAgt#:xxProd:strCode:'RateSet');
          xxLic = 'Y';
          Leavesr;
       Else;
          xxLic = 'N';
       Endif;
      /End-free
      *
      *  Check for user override
      *
     C     xxLIC         IFEQ      'N'
      *
     C                   CALL      'SA006R'
     C                   PARM      ##LVL         #1ULVL            3 0          USER LEVEL
     C                   PARM      ##TYPE        #1UTYP            1            USER TYPE
     C                   PARM      EXPRSL        #1FLVL            3 0          FILE LEVEL
     C                   PARM      EXPRST        #1FTYP            1            FILE TYPE
     C                   PARM      *BLANK        #1RTCD            1            RETURN CODE
      *
     C     #1RTCD        IFNE      'F'
      /free

       Clear scErrHead;
       Clear scRtChange;

       scErrHead = 'Producer Error';

       scRtChange = 'Producer ' + %editc(xxProd : 'X') +
        ' is NOT LICENSED for employer rating state ' + %trim(strCode) + '.';

       Clear scOverride;
       Clear agtOverCnl;
       Clear agtOverCon;

       Dou agtOverCnl = *on or agtOverCon = *on;
          Exfmt rcdAgtOver;
       Enddo;

       If agtOverCnl = *on;
          agtError = *on;
       Endif;


      /end-free
      *
     C     scOverride    IFEQ      'Y'
     C                   MOVE      scOverride    xxLIC
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  XXLIC = N
      *
      *
     C                   ENDSR                                                  PRLIC
     C*
     C*================================================================
     C* RATES - Get new and renew rates for rate number
     C*================================================================
     C*
     C     RATES         BEGSR
     C*
     C                   EVAL      xxEfdt = Dateto7(SystemDate())
     C*
     C                   CLEAR                   XXPLAN
     C*
     C     keyWMARL      SETGT     PAR00
     C     key1WMARL     READPE    PAR00                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     key1WMARL     READPE    PAR00                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ARPLAN        XXPLAN
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SAMAGT - Verify the Agent is alread Active
     C*================================================================
     C     SAMAGT        BEGSR
      *
     C                   CLEAR                   SAMEAG
     C                   CLEAR                   WDMSG1
     C                   CLEAR                   WDMSG2
     C                   CLEAR                   WDMSG3
     C                   CLEAR                   WDMSG4
     C                   CLEAR                   WDMSG5
      *
     C     keyWMAHL      CHAIN     PAH00                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     A2AHSTAT      IFEQ      'A'
      *
     C                   SELECT
      *
     C     scAgent1      WHENEQ    A2AHSAGT
     C     scProd1       ANDEQ     A2AHSPRD
     C                   MOVE      *on           SAMEAG
     C                   MOVEL(P)  MSG(12)       WDMSG3
     C                   MOVEL(P)  MSG(13)       WDMSG4
      *
     C     scAgent1      WHENEQ    A2AH3AGT
     C     scProd1       ANDEQ     A2AH3PRD
     C                   MOVE      *on           SAMEAG
     C                   MOVEL(P)  MSG(14)       WDMSG3
     C                   MOVEL(P)  MSG(15)       WDMSG4
      *
     C     scAgent2      WHENEQ    A2AHAGT#
     C     scProd2       ANDEQ     A2AHPROD
      *
     C     scAgent2      IFNE      0
     C                   MOVE      *on           SAMEAG
     C                   MOVEL(P)  MSG(16)       WDMSG3
     C                   MOVEL(P)  MSG(17)       WDMSG4
     C                   ENDIF
      *
     C     scAgent2      WHENEQ    A2AH3AGT
     C     scProd2       ANDEQ     A2AH3PRD
      *
     C     scAgent2      IFNE      0
     C                   MOVE      *on           SAMEAG
     C                   MOVEL(P)  MSG(18)       WDMSG3
     C                   MOVEL(P)  MSG(19)       WDMSG4
     C                   ENDIF
      *
     C     scAgent3      WHENEQ    A2AHAGT#
     C     scProd3       ANDEQ     A2AHPROD
      *
     C     scAgent3      IFNE      0
     C                   MOVE      *on           SAMEAG
     C                   MOVEL(P)  MSG(20)       WDMSG3
     C                   MOVEL(P)  MSG(21)       WDMSG4
     C                   ENDIF
      *
     C     scAgent3      WHENEQ    A2AHSAGT
     C     scProd3       ANDEQ     A2AHSPRD
      *
     C     scAgent3      IFNE      0
     C                   MOVE      *on           SAMEAG
     C                   MOVEL(P)  MSG(22)       WDMSG3
     C                   MOVEL(P)  MSG(23)       WDMSG4
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
     C     keyWMAHL      READPE    PAH00                                  60
     C                   ENDDO
     c*
     C                   ENDSR
     C*
     C*================================================================
     C* SAMEFF - Verify if another record with same effective date exists.
     C*================================================================
     C     SAMEFF        BEGSR
      *
     C                   CLEAR                   SAMEEF
     C                   CLEAR                   WDMSG1
     C                   CLEAR                   WDMSG2
     C                   CLEAR                   WDMSG3
     C                   CLEAR                   WDMSG4
     C                   CLEAR                   WDMSG5
     C                   eval      xxEffDate = dtCYMD(scEffDate)
     C                   z-add     0             efdcnt            1 0
     c*
     C     key1WMAHL     CHAIN     PAH00                              60
     C     *IN60         DOWEQ     *OFF
     c*
     C     A2AHSTAT      IFEQ      'A'
     C                   Add       1             efdcnt
     C                   Leave
     C                   ENDIF
     c*
     C     key1WMAHL     READPE    PAH00                                  60
     C                   ENDDO
     c*
     c                   If        efdcnt <> 0
     C                   MOVE      '1'           SAMEEF
     C                   MOVEL(P)  MSG(5)        WDMSG3
     C                   MOVEL(P)  MSG(6)        WDMSG4
     c                   Endif
     c*
     C                   ENDSR
     C*================================================================

       //===================================================================
       // Save Agent History Data.
       //===================================================================
     C     AgentSave     Begsr
      /free

       // Save Current Agent # before change in status

       If scStatus <> svStat;
          Exsr GETAGT;
          xxAgt1 = ##AGT#;
       Endif;

       If not agtAddMode;
          Chain ahRRN PAH0P;
       Else;
          Clear PAH0P;
          AHco#  = ELCO#;
          AHfnd  = ELFND;
          AHemp# = ELEMP#;
          AHdiv  = ELDIV;
          AHfyr  = ELFYR;
          AHefdt = dtCYMD(scEffDate);
          AHipgm = Q1PGM;
          AHiusr = Q1USER;
          AHitm  = Timeto6(SystemTime());
          AHidt  = Dateto7(SystemDate());
       Endif;

       //1st Agent
       AHAgt# = scAgent1;
       AHProd = scProd1;

       If AHAgt# <> 0;
          AHRt# = scRate1;
       Else;
          AHRt# = 0;
       Endif;


       //2nd Agent
       AHSAgt = scAgent2;
       AHSPrd = scProd2;

       If AHSAgt <> 0;
          AHSRt# = scRate2;
       Else;
          AHSRt# = 0;
       Endif;


       //3rd Agent
       AH3Agt = scAgent3;
       AH3Prd = scProd3;

       If AH3Agt <> 0;
          AH3Rt# = scRate3;
       Else;
          AH3Rt# = 0;
       Endif;


       //1st Agent of Record
       AHAGRC = scAgtRec1;
       AHPRRC = scProdRec1;

       If AHAGRC <> 0;
          AHRTRC = scRateRec1;
       Else;
          AHRTRC = 0;
       Endif;


       //2nd Agent of Record
       AH2ARC = scAgtRec2;
       AH2PRC = scProdRec2;

       If AH2ARC <> 0;
          AH2RRC = scRateRec2;
       Else;
          AH2RRC = 0;
       Endif;


       //3rd Agent of Record
       AH3ARC = scAgtRec3;
       AH3PRC = scProdRec3;

       If AH3ARC <> 0;
          AH3RRC = scRateRec3;
       Else;
          AH3RRC = 0;
       Endif;

       //Controlling Agent flag
       AHCAF1 = *blank;
       AHCAF2 = *blank;
       AHCRF1 = *blank;
       AHCRF2 = *blank;
       Select;
          When scControl1 = 1;
             AHCAF1 = 'Y';
          When scControl2 = 2;
             AHCAF2 = 'Y';
       Endsl;

       Select;
          When scContRec1 = 1;
             AHCRF1 = 'Y';
          When scContRec2 = 2;
             AHCRF2 = 'Y';
       Endsl;

       AHStat = scStatus;

       AHPGM  = Q1PGM;
       AHUSER = Q1USER;
       AHTIME = Timeto6(SystemTime());
       AHDT   = Dateto7(SystemDate());

       If not agtAddMode;
          Update PAH0P;
       Else;
          Exsr GETAGT;
          xxAgt1 = ##AGT#;
          Write PAH0P;
       Endif;

       //* Update Quote Status file
       If AHStat <> svStat;
          Exsr UpdSts;
       Else;
          Exsr Quote;
       Endif;

       Exsr Billng;

      /end-free
     C                   Endsr                                                  AgentSave
     C*
     C*================================================================
     C* GETAGT - Get the latest agent for employer
     C*================================================================
     C*
     C     GETAGT        BEGSR
     C*
     C                   CALL      'WA002R'                             99
     C                   PARM                    ELCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    ELDIV
     C                   PARM      *ZERO         ##FYR
     C                   PARM      *ZERO         ##AGT#
     C                   PARM      *BLANK        ##ANAM           60
     C*
     C                   ENDSR                                                  GETAGT
     C*
     C*================================================================
     C* EMPAGT - Check to see if this emp already has an agent assigned
     C*================================================================
     C*
     C     EMPAGT        BEGSR
     C*
     C                   MOVE      'N'           OLDAGT            1
     C*
     C*  IF RECORDS ARE MANUALLY DELETED, IT COULD PRODUCE UNEXPECTED
     C*   RESULTS.
     C*
     C     agtAddMode    IFEQ      *ON
     C     key2WAQTL7    SETGT     PQT07
     C     keyWAQTL7     READPE(n) PQT07                                  60
     C                   ELSE
     C     keyWAQTL7     CHAIN(n)  PQT07                              60
     C                   ENDIF                                                  agtAddMode
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     quoteMode     IFEQ      'Q'
     C*
     C     agtAddMode    IFEQ      *ON
     C     key2WAQTL7    SETGT     PQT07
     C     keyWAQTL7     READPE    PQT07                                  60
     C                   ELSE
     C     keyWAQTL7     CHAIN     PQT07                              60
     C                   ENDIF                                                  agtAddMode
     C*
     C                   Z-ADD     scAgent1      QTAGT#
     C*
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1PGM         QTPGM
      /free
          QTTIME = Timeto6(SystemTime());
          QTDT   = Dateto7(SystemDate());
      /end-free
      *
     C                   UPDATE    PQT07
     C                   ELSE
     c*
     C                   SELECT
     C     QTTYPE        WHENEQ    'C'
     C     QTTYPE        OREQ      'R'
     C                   MOVE      QTTYPE        QTXFER
     C                   OTHER
     C                   MOVE      *BLANKS       QTXFER
     C                   ENDSL
     C*
     C                   MOVE      'X'           QTTYPE
     C*
     C                   EXSR      WRITEQ
     C*
     C                   Z-ADD     scAgent1      QTAGT#
     C                   MOVE      'T'           QTTYPE
     C*
     C                   EXSR      WRITEQ
     C*
     C                   CLEAR                   QTXFER
     C*
     C                   ENDIF                                                  quoteMode
     C*
     C                   MOVE      'Y'           OLDAGT
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  EMPAGT
      *
      *===============================================================
      * UPDSTS - Update Quote Status records (if necessary)
      *===============================================================
     C     UPDSTS        BEGSR
     C*
     C                   EXSR      GETAGT
      *
      * Write Quote Status transfer records if change in current agent
      * as a result of change in status of agent history record.
      *
     C     XXAGT1        IFNE      0
     C     ##AGT#        ANDNE     0
     C     XXAGT1        ANDNE     ##AGT#
     C     key2WAQTL7    SETGT     PQT07
     C     key2WAQTL7    READPE(N) PQT07                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     QTTYPE        IFNE      'X'
     C     QTTYPE        ANDNE     'T'
     C                   MOVE(P)   QTTYPE        QTXFER
     C                   ENDIF                                                  QTTYPE
      *
     C                   MOVE(P)   'X'           QTTYPE
     C                   EXSR      WRITEQ
     C                   MOVE(P)   'T'           QTTYPE
     C                   Z-ADD     ##AGT#        QTAGT#
     C                   EXSR      WRITEQ
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  X1AGT#
      *
     C                   ENDSR                                                  UPDSTS
     C*
     C*================================================================
     C* WRITEQ - Write records to the WAQTP file
     C*================================================================
     C*
     C     WRITEQ        BEGSR
      /free
          QTTIME = Timeto6(SystemTime());
          QTITM  = Timeto6(SystemTime());
          QTDT   = Dateto7(SystemDate());
          QTIDT  = Dateto7(SystemDate());
          QTWKDT = Dateto7(SystemDate());
      /end-free
     C*
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   MOVEL(P)  Q1PGM         QTIPGM
      *
     C                   WRITE     PQT07
     C*
     C                   ENDSR                                                  WRITEQ
     C*
     C*
     C*================================================================
     C* QUOTE - Add record to quote file if it does not exist
     C*================================================================
     C*
     C     QUOTE         BEGSR
     C*
     C*  IF IT DOESN'T ALREADY EXIST CREATE AN AGENT QUOTE RECORD
     C*
     C     key1WAQTL7    CHAIN(N)  PQT07                              60        CHAIN W/ AGT# TOO
     C*
     C                   EXSR      GETAGT
     C                   Z-ADD     ##AGT#        XXAGT2
     C*
     C   60scAgent1      IFNE      XXAGT2
     C                   MOVE      *OFF          *IN60
     C                   ENDIF                                                  scAgent1<>XXAGT2
     C*
     C     XXAGT1        IFNE      XXAGT2
     C     scAgent1      ANDEQ     XXAGT2
     C     agtAddMode    ANDEQ     *ON                                          ADD MODE
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  XXAGT1<>XXAGT2=SCAGT
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   EXSR      EMPAGT
     C*
     C     OLDAGT        IFEQ      'N'
     C*
     C                   CLEAR                   PQT07
     C*
     C                   Z-ADD     EMCO#         QTCO#
     C                   Z-ADD     EMFND         QTFND
     C                   Z-ADD     EMEMP#        QTEMP#
     C                   Z-ADD     EMDIV         QTDIV
     C                   Z-ADD     scAgent1      QTAGT#
     C*
     C                   Z-ADD     EMEFD1        QTEFDT
     C                   Z-ADD     EMCHAP        QTCHAP
     C                   MOVEL     EMNAME        QTNAME
     C*
     C     quoteMode     IFEQ      'Q'
     C                   SELECT
     C     XXRQFL        WHENEQ    'Y'
     C                   MOVE      'p'           QTTYPE
     C                   OTHER
     C     XXQTYP        IFNE      ' '
     C                   MOVE      XXQTYP        QTTYPE
     C                   ELSE
     C                   MOVE      'P'           QTTYPE
     C                   ENDIF
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      'W'           QTTYPE
     C                   eval      QTNBDT = Dateto7(SystemDate())
     C                   ENDIF                                                  quoteMode
     C*
     C                   Z-ADD     *ZEROS        QTPREM
     C                   MOVE      *BLANKS       QTCARR
     C                   MOVE      *BLANKS       QTCOMM
     C*
     C     quoteMode     IFEQ      'Q'
     C                   MOVE      'Q'           QTSTAT
     C                   ELSE
     C                   MOVE      'W'           QTSTAT
     C                   ENDIF                                                   quoteMode
     C*
     C*
     C                   MOVEL(P)  Q1USER        QTUSER
     C                   MOVEL(P)  Q1USER        QTIUSR
     C                   MOVEL(P)  Q1PGM         QTPGM
     C                   MOVEL(P)  Q1PGM         QTIPGM
      /free
          QTTIME = Timeto6(SystemTime());
          QTITM  = Timeto6(SystemTime());
          QTDT   = Dateto7(SystemDate());
          QTIDT  = Dateto7(SystemDate());
          QTWKDT = Dateto7(SystemDate());
          QTORDT = Dateto7(SystemDate());
      /end-free
      *
     C                   WRITE     PQT07
      /free
          UpdFQLockSts(QTco#:QTfnd:QTemp#:QTdiv:QTtype);
      /end-free
     C*
     C                   ENDIF                                                  OLDAGT=N
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDSR                                                  QUOTE
     C*
     C*================================================================
     C* BILLNG - Update the Bill Transaction File
     C*================================================================
     C*
     C     BILLNG        BEGSR
     C*
     C     keyWABLL      CHAIN     PBL00                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'WA002R'                             99
     C                   PARM                    ELCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    ELDIV
     C                   PARM                    ELFYR
     C                   PARM                    ##AGT#
     C                   PARM                    ##ANAM           60
     C*
     C                   Z-ADD     ##AGT#        BLAGT#
     C                   MOVEL     Q1USER        BLUSER
     C                   MOVEL     Q1PGM         BLPGM
     C*
      /free
          BLTIME = Timeto6(SystemTime());
          BLDT   = Dateto7(SystemDate());
      /end-free
     C*
     C                   UPDATE    PBL00
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  BILLNG
     C*
      *

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       agtError = *off;

       msDivision = *blanks;
       erDivision = *off;
       msDivisio2 = *blanks;
       erDivisio2 = *off;
       msYear     = *blanks;
       erYear     = *off;
       msEffDate  = *blanks;
       erEffDate  = *off;
       msStatus   = *blanks;
       erStatus   = *off;
       msAgent1   = *blanks;
       erAgent1   = *off;
       m2Agent1   = *blanks;
       e2Agent1   = *off;
       msProd1    = *blanks;
       erProd1    = *off;
       m2Prod1    = *blanks;
       e2Prod1    = *off;
       msRate1    = *blanks;
       erRate1    = *off;
       m2Rate1    = *blanks;
       e2Rate1    = *off;
       erControl1 = *off;
       msAgent2   = *blanks;
       erAgent2   = *off;
       msProd2    = *blanks;
       erProd2    = *off;
       msRate2    = *blanks;
       erRate2    = *off;
       m2Rate2    = *blanks;
       e2Rate2    = *off;
       erControl2 = *off;
       msAgent3   = *blanks;
       erAgent3   = *off;
       msProd3    = *blanks;
       erProd3    = *off;
       msRate3    = *blanks;
       erRate3    = *off;
       msAgtRec1  = *blanks;
       erAgtRec1  = *off;
       msProdRec1 = *blanks;
       erProdRec1 = *off;
       msRateRec1 = *blanks;
       erRateRec1 = *off;
       erContRec1 = *off;
       msAgtRec2  = *blanks;
       erAgtRec2  = *off;
       msProdRec2 = *blanks;
       erProdRec2 = *off;
       msRateRec2 = *blanks;
       erRateRec2 = *off;
       erContRec2 = *off;
       msAgtRec3  = *blanks;
       erAgtRec3  = *off;
       msProdRec3 = *blanks;
       erProdRec3 = *off;
       msRateRec3 = *blanks;
       erRateRec3 = *off;
       e2Status = *off;

      /end-free
     C                   Endsr                                                  ClearErrors

     P AgentEdit       E

       //***************************************************************************//
       //===================================================================
       // Billing Auxiliary Popup
       //===================================================================
     P BillAuxiliary   B                   export
     D BillAuxiliary   PI             3  0
     D type                           1    const
     D begDate                        8  0 const
     D endDate                        8  0 const
     D fnRRN                          9  0 const

     DxxBSCD           s                   like(ELBSCD)
     D                 DS
     D  XXDT                   1      7  0
     D  XXYY                   1      3  0
     D  XXMM                   4      5  0
     D  XXDD                   6      7  0
     D  XXMDD                  4      7  0
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1

      *
      *  GET THE NUMBER OF POSSIBLE MONTHLY INSTALLMENTS FOR THE POLICY WHICH
      *  CURRENTLY BILLS THE FIRST OF THE MONTH. SUBTRACT THIS NUMBER FROM
      *  12 TO GET THE MONTH VALUE FOR THE BILLING AUXILIARY EFFECTIVE DATE.
      *  USE THE DAY VALUE OF THE BSDT OR PFDT FOR THE DAY VALUE. CALL W2157R
      *  WITH THE QQPFDT PARM EQUAL TO MMDD0000 WHERE MM IS THE RELATIVE NUMBER
      *  OF MTHS FOR THE WMBXP KEY.  DD EQUALS THE EFFECTIVE DAY TO BE USED
      *  FILTER THE BILLING SCHEDULE CODE RECORD SELECTION IN W2157R.
      *
     C                   CLEAR                   QQPFDT
      *
     C                   Z-ADD     begDate       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0
      *
     C                   Z-ADD     endDate       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT            7 0
      *
     C                   Z-ADD     XXBGDT        XXDT
     C                   Z-ADD     1             XXDD
     C                   Z-ADD     XXDT          XXSTDT            7 0
     C                   Z-ADD     0             XXINCT            2 0
     C                   Z-ADD     XXPTDT        SVPTDT            7 0
     C                   MOVE      '01'          SVPTDT
     C     XXSTDT        IFLT      XXBGDT
     C     XXPTDT        ANDEQ     SVPTDT
     C                   ADD       1             XXINCT
     C                   ENDIF
     C     XXDT          DOWLE     XXPTDT
     C     XXDT          IFGE      XXBGDT
     C     XXDT          ANDLT     XXPTDT
     C                   ADD       1             XXINCT
     C                   ENDIF
     C                   ADD       1             XXMM
     C     XXMM          IFEQ      13
     C                   Z-ADD     1             XXMM
     C                   ADD       1             XXYY
     C                   ENDIF
     C                   ENDDO
     C                   MOVE      XXBGDT        XXDD
     C     12            SUB       XXINCT        XXMM
     C                   ADD       1             XXMM
      *
      * Concatenate the efeective month/day (01/16) with the number of
      * installments which fit on the effective date range
     C                   MOVE      *ZEROS        QQPFDT
     C                   MOVEL     XXMDD         XXMDI1            6
     C                   MOVE      XXINCT        XXMDI1
     C                   MOVEL     XXMDI1        QQPFDT            8
      *
     C                   Z-ADD     *ZEROS        ##RRN
     C                   MOVE      type          xxType
     C*
     C                   CALL      'W2157R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      fnRRN         ##RRN2            9 0
     C                   PARM                    xxType            1
     C                   PARM                    QQPFDT
     C*
     C                   CLEAR                   XXBSCD
     C                   If        *INKC = *off and *INKL = *off
     C     ##RRN         CHAIN     WMBXP                              60
     C  N60              MOVEL(P)  BXBSCD        XXBSCD
     C                   Endif
     c*
     C                   RETURN    XXBSCD

     P BillAuxiliary   E

      **********************************************************************************************
       //===================================================================
       // DemoPopup - City Demographics Selection Popup
       //===================================================================
     P DemoPopup       B                   export
     D DemoPopup       PI             9  0
     D co#                            3  0 const
     D fnd                            3  0 const
     D state                          2    const
     D city                          25    const
     D*
     D demoRRN         s              9  0
     D count           s              1  0
     D found           s              1n
     D xxKey           s             10
     D svState         s              2
     D svCity          s             25
     D workSt          s              2
     D workSt2         s              2
     D workCity        s             25
     D workCity2       s             25
     C*
     C* City Demographics Key List
     C*
     C     keyWMCYL1     Klist
     C                   Kfld                    co#
     C                   Kfld                    scDemFSt
     C*
     C     keyWMCYL2     Klist
     C                   Kfld                    co#
     C*
     C* Region Key List
     C*
     C     keyWMREL      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    CYRgn
     C*
     C* Chapter Key List
     C*
     C     keyWMCPL      Klist
     C                   Kfld                    co#
     C                   Kfld                    fnd
     C                   Kfld                    CYChap
      /free

       //Determine if single record matches city/state and return.
       Exsr SingleMatch;

       Open WPIPOLPOP;

       scDemFCity = city;
       scDemFSt   = state;

       If scDemFCity = *blanks and scDemFSt = *blanks;
          showDemClr = *off;
       Else;
          showDemClr = *on;
       Endif;

       Exsr LoadGrid;

       demoCancel = *off;
       demoRRN = 0;

       Dou demoCancel = *on or demoRRN <> 0;

          //*  Process Demographics Popup
          Exfmt rcdDemoPop;

          Select;

          //*  Filter button.
          When demoFilter = *on;
             Exsr DemoFilterPop;
             demoFilter = *off;

          //*  Filter Payroll button.
          When demoClear = *on;
             showDemClr = *off;
             scDemFCity = *blanks;
             scDemFSt = *blanks;
             Exsr LoadGrid;
             demoClear = *off;

          Other;
             //*  Return RRN if record selected.
             Readc DemoLosSFL;
             If not %EOF();
                If demoSelect = *on;
                   demoRRN = scDemoRRN;
                Endif;
             Endif;
          Endsl;

       Enddo;

       Close WPIPOLPOP;

       Return demoRRN;

      /end-free

       //===================================================================
       // SingleMatch - Return RRN if one record matches city/state.
       //===================================================================
     C     SingleMatch   Begsr
      /free

       demoRRN = 0;
       count = 0;

       workSt   = Upper(state);
       workCity = Upper(city);

       Setll keyWMCYL2 PCY02;
       Reade keyWMCYL2 PCY02;
       Dow not %eof;
          workSt2   = Upper(CYSt);
          workCity2 = Upper(CYCty);

          If workSt = workSt2 and workCity = workCity2;
             count = count +1;
             demoRRN = dbrrn7;
          Endif;
          If count > 1;
             Leave;
          Endif;
          Reade keyWMCYL2 PCY02;
       Enddo;

       //If one record matches city/state, then return it.
       If count = 1;
          Return demoRRN;
       Endif;

      /end-free
     C                   Endsr                                                  SingleMatch Subr

       //===================================================================
       // LoadGrid - Load the city demographics grid
       //===================================================================
     C     LoadGrid      Begsr
      /free

       // Clear the subfile.
       clrDemoSel = *On;
       dspDemoSFL = *off;
       Write rcdDemoPop;
       dspDemoSFL = *on;
       clrDemoSel = *Off;
       rrnDemoSel = 0;

       // Populate the subfile
       If scDemFSt <> '  ';
          Setll keyWMCYL1 PCY01;
          Reade keyWMCYL1 PCY01;
       Else;
          Setll keyWMCYL2 PCY02;
          Reade keyWMCYL2 PCY02;
       Endif;

       Dow not %eof;

          found = *on;
          If scDemFCity <> ' ';
             If not StringFound(%trim(CYCty):%trim(scDemFCity):'1');
                found = *off;
             Endif;
          Endif;

          If found;
             scDemoSt   =  CYSt;
             scDemoCity =  CYCty;
             Clear scDemoRegn;
             If CYRgn <> 0;
                Chain keyWMREL PRE00;
                If %found;
                   scDemoRegn =  REDESC;
                Endif;
             Endif;
             Clear scDemoChpt;
             If CYChap <> 0;
                Chain keyWMCPL PCP00;
                If %found;
                   scDemoChpt =  CPDESC;
                Endif;
             Endif;
             xxKey = CYSt + CYCnty;
             scDemoCnty = %Trim(CountyCdDs(co#:xxKey));
             scDemoLRep =  GetUserName(GetUserId(CYLRep));
             If scDemFSt <> '  ';
                scDemoRRN  =  dbrrn6;
             Else;
                scDemoRRN  =  dbrrn7;
             Endif;

             rrnDemoSel =  rrnDemoSel + 1;
             Write DemoLosSFL;
          Endif;

          If scDemFSt <> '  ';
             Reade keyWMCYL1 PCY01;
          Else;
             Reade keyWMCYL2 PCY02;
          Endif;
       Enddo;

       //* Position grid at selected record.
       rrnDemoSel = 1;

      /end-free
     C                   Endsr                                                  LoadGrid Subr

       //===================================================================
       // DemoFilterPop - Demographics Filter Popup
       //===================================================================
     C     DemoFilterPop Begsr
      /free

       svCity  = scDemFCity;
       svState = scDemFSt;

       //* Display popup window for the demographics filter.

       Dou demoFilApl = *on or demoFilCnl = *on;
          Exfmt rcdDemoFil;
       Enddo;

       If scDemFCity = *blanks and scDemFSt = *blanks;
          showDemClr = *off;
       Else;
          showDemClr = *on;
       Endif;

       If scDemFCity <> svCity or scDemFSt <> svState;
          Exsr LoadGrid;
       Endif;

      /end-free
     C                   Endsr                                                  DemoFilterPop Subr

     P DemoPopup       E
      **********************************************************************************************

       //***************************************************************************//
       //===================================================================
       // Scheduled Rating Edit Popup
       //===================================================================
     P SchdRatingEdit  B                   export
     D SchdRatingEdit  PI
     D elRRN                          9  0 const
     D quoteMode                      1    const

     D xxEmrrn         s              9  0 inz
     D error           s               n   inz(*off)
     D message         s            100
     D inEditData      s            500
     D TOTPCC          s                   like(RFMSCA)                         CREDIT APPLIED
     D TOTPCD          s                   like(RFMSCA)                         DEBIT APPLIED
     D TOTALC          s                   like(SCDBCR)                         TOT CRDT APPLIED
     D TOTALD          s                   like(SCDBCR)                         TOT DEBIT APPLIED
     D TOTPCT          s                   like(SCDBCR)                         CATEGORY DB/CR
     d ##CO#           s              3
     d ##FND           s              3
     d ##FYR           s              3
     d ##EMP#          s              9
     d ##DIV           s              5
     d ##Que           s             10
     d ##Qte           s              1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     keyWMUPL      Klist
     C                   Kfld                    Q1USER
     C*
     C* Scheduled Rating File Key List
     C*
     C     keyWMRFL      Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR
     C*

      /free

       Open WTPOLDTL;

       GetClientID(clientID);

       Chain(n) keyWMUPL PUP00;

       Chain(n) elRRN PEL0P;

       //* Populate the popup window with selected record.
       Exsr LoadSchdRating;

       Exsr ClearErrors;
       Clear schdSave;
       Clear schdCancel;

       //* Display window for rating fields (set indicator to enable show as window)

       schdRating = *on;
       schdRteInq = *on;
       schdPopup = *on;

       Dou schdSave = *on or schdCancel = *on;

          Dou error = *off or schdCancel = *on;

             Exfmt rcdSched;

             Select;

             //* Print Scheduled Rating Form
             When schdPrint = *on;
                Exsr ScheduleCheck;
                If not Error;
                   Exsr Save;
                   ##Co# = %editc(ELCO#: 'X');
                   ##Fnd = %editc(ELFND: 'X');
                   ##Fyr = %editc(ELFYR: 'X');
                   ##Emp# = %editc(ELEMP#: 'X');
                   ##Div = %editc(ELDIV: 'X');
                   ##Que = UPOUTQ;
                   ##Qte = quoteMode;
                   callW1551C(##Co#:##Fnd:##Fyr:##Emp#:##Div:##Que:##Qte);
                   scSchdPnl = 'Print Scheduled Rating Form';
                   scSchdMsg = 'Job has been submitted.';
                   Exfmt rcdSchdErr;
                Endif;
                schdPrint = *off;

             When schdCancel = *off;
                Exsr ScheduleCheck;

             Endsl;

          Enddo;

       Enddo;

       If schdSave = *on;
          Exsr Save;
       Endif;

       Close WTPOLDTL;

      /end-free

       //***************************************************************************//
       //===================================================================
       // Load Large Deductible fields
       //===================================================================
     C     LoadSchdRatingBegsr
     C*
      /free

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

       scDBCR2 = ELDBCR;

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

       Endif;

       scPnlTitlS = 'Scheduled Rating Information - '+%Editc(ELdiv:'X')+' '+
        GetEmpName(ELemp#:ELdiv:xxEMrrn:ELfnd:ELco#);

      /end-free
     C                   ENDSR                                                  LoadSchdRating

       //***************************************************************************//
       //===================================================================
       // Scheduled Rating Error Checking
       //===================================================================
     C     ScheduleCheck Begsr
     C*
     C                   Exsr      ClearErrors
     C*
      /free

       inEditData = dsRateEdit;

       ptrSchedRate = %Addr(dsSchedRate);

       error = not errSchedRate('S':' ':' ':ptrSchedRate:ELCo#:ELFnd:ELEmp#:
                    ELDiv:ELFyr:elRRN:inEditData);

       dsRateEdit = inEditData;

      /end-free
     C*
      *
      * Add up percentages to compare to Rate factor entered.
      *
     C                   if        not Error
     c*
     C                   CLEAR                   TOTPCC
     C                   CLEAR                   TOTPCD
     C                   CLEAR                   TOTALC
     C                   CLEAR                   TOTALD
     C                   CLEAR                   TOTPCT
      *
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
      *
     C                   EVAL      TOTALC = (TOTPCC * -1) / 100
     C                   EVAL      TOTALD = (TOTPCD / 100)
      *
     C                   IF        scDBCR2 <> *ZEROS
     C                   EVAL      TOTPCT = 1.000 + TOTALC + TOTALD
     C                   ELSE
     C                   EVAL      TOTPCT = TOTALC + TOTALD
     C                   ENDIF                                                  SCDBCR2 = *ZEROS
      *
      * Check TOTPCT = SCDBCR2, if not is Error
      *
     C                   If        TOTPCT <> SCDBCR2
     C                   move      *on           error
     C                   Clear                   scSchdMsg
     C                   Clear                   scSchdPnl
      /free
       scSchdPnl = 'Scheduled Rating Information Error';

       scSchdMsg = 'The sum of the DB/CR percentages of the rating form ' +
        'does not equal the Schedule DB/CR Factor.  The sum of the category ' +
        'percentages is ' + %editc(totpct : '3') + ' and the Schedule DB/CR ' +
        'Factor is ' + %editc(scdbcr2 : '3') + '.';
      /end-free
     C                   Exfmt     rcdSchdErr
     c*
     C                   ENDIF                                                  TOTPCT<>SCDBCR2
     c*
     C                   Endif
     C*
     C                   ENDSR                                                  ScheduleCheck

       //===================================================================
       // Save Data.
       //===================================================================
     C     Save          Begsr
      /free

       Chain keyWMRFL RFP00;

       If not %Found;
          Clear RFP00;
          RFCo#  = ELCo#;
          RFFnd  = ELFnd;
          RFFyr  = ELFyr;
          RFEmp# = ELEmp#;
          RFDiv  = ELDiv;
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
          Feod WMRFL;
       Endif;

      /end-free
     C                   Endsr                                                  Save

       //===================================================================
       // Clear all Errors
       //===================================================================
     C     ClearErrors   Begsr
      /free

       error = *off;

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

      /end-free
     C                   Endsr                                                  ClearErrors

     P SchdRatingEdit  E

       //***************************************************************************//
       //===================================================================
       // Effective Date API Call.
       //===================================================================
     p SrEffd          b
     d SrEffd          pi             7  0
     d   inCO#                        3  0
     d   inFND                        3  0
     d   inFYR                        3  0
     d   inEMP#                       9  0
     d   inDIV                        5  0
     d   inDate                       7  0
     d   inType                       1    options(*nopass)
     d   inClas                       5  0 options(*nopass)
     d*
     d* SReffd API prototype
     d SReffdAPI       pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7
     d   ##Type                       1
     d   ##Clas                       5
     d*
     d   ##CO#         s              3
     d   ##FND         s              3
     d   ##FYR         s              3
     d   ##EMP#        s              9
     d   ##DIV         s              5
     d   ##Date        s              7
     d   ##Type        s              1
     d   ##Clas        s              5
     d   outDate       s              7  0
     d*

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
     P*================================================================
     PAcceptAgg        b
     DAcceptAgg        pi             1n
     D inEffDt                             like(scLDEfD1) const
     D inAgg                               like(LDAMG1)   const
     D elRRN                          9  0 const options(*nopass)
     D*
     D effDt7          s                   like(LDEFD1)
     D compareAgg      s                   like(LDAMG1)
     D accepted        s              1n   inz('0')

     C     keyWMLDL2     klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELFYR
     C                   kfld                    ELEMP#

     C     Dtcymdp       Plist
     C                   Parm                    Srdti             8 0
     C                   Parm      *BLANKS       Srffmt            8
     C                   Parm      '*CYMD'       Srtfmt            8
     C                   Parm                    Srdto            10
     C                   Parm                    Srerr             1
     C*
     C                   z-add     inEffDt       srDtI
     C                   call      'SRCVTDAT'    DtCYMDp
     C                   movel     srDtO         effDt7
     C     elRRN         Chain(n)  PEL0P

     c     keyWMLDL2     Setll     wmldl2
     C*
     c                   Dou       %eof
     c     keyWMLDL2     Reade     wmldl2
     C*
     c                   If        not %eof
     C*
     c                   If        LDDIV = ELDIV
     c                   Iter
     c                   Endif
     c                   Exsr      GetAggregate
     C*
     c                   If        inAgg <> compareAgg
     c                   eval      scLAAamt1 = inAgg
     c                   eval      scLAAamt2 = compareAgg
     c                   eval      scLAAdiv2 = LDDIV
     C*
     C                   Dou       laaCancel = *on or laaConfirm = *on
     C                   EXFMT     rcdAggAmt
     C                   ENDDO
     C*
     C     laaConfirm    IFEQ      *ON
     C                   eval      accepted = *on
     C                   ENDIF
     C*
     c                   Leave
     C                   Endif
     C*
     C                   Endif
     C*
     C                   Enddo

     c                   If        %eof
     C                   eval      accepted = *on
     c                   Endif

     C                   Return    accepted
    C*================================================================
    C* Determine comparison aggregate
    C*================================================================
     C     getAggregate  begsr

     c                   select
     c                   when      effDt7 >= LDEFD1
     c                              and (effDt7 <= LDEXP1 or LDEXP1 = *zero)
     c                   eval      compareAgg = LDAMG1
     c                   when      effDt7 >= LDEFD2
     c                              and (effDt7 <= LDEXP2 or LDEXP2 = *zero)
     c                   eval      compareAgg = LDAMG2
     c                   other
     c                   clear                   compareAgg
     c                   endsl

     C                   endsr

     PAcceptAgg        e

     C*====================================================================
      ******** beg   add ***  06/14/13  ************************************************
     P*================================================================
     P DisplayPremium  b                   export
     D DisplayPremium  pi
     D inRRN                          9  0 const
     D inFLAG                         1    const

     d outHeadings     s           1100
     d outFactors      s           1100
     d outAmounts      s            550
     d outInfPrem      s            550
     d index           s              3  0

      /free

       If not %Open(WPIPOLPOP);
          Open WPIPOLPOP;
       Endif;

       GetClientID(clientID);
       Select;
       When inFLAG = 'D';
            scPrmTitle = 'Premium Information';
       When inFLAG = 'U';
            scPrmTitle = 'Unbilled Premium Information';
       When inFLAG = 'H';
            scPrmTitle = 'Premium History';
       When inFLAG = 'A';
            scPrmTitle = 'Revised Premium Information';
       Endsl;
       //* Populate the popup window with selected record.
       Clear scHeadings;
       Clear scFactors;
       Clear scWrtPremium;
       Clear scInfPremium;
       PremiumInfo(inRRN:inFLAG:outHeadings:outFactors:outAmounts:outInfPrem);
       scHeadings = outHeadings;
       scFactors  = outFactors;
       scWrtPremium = outAmounts;
       scInfPremium = outInfPrem;
       isContribution = GetPrmContflg(intCO#:intFND);
       If isContribution;
          flexhead1 = 'Written Contribution';
          flexhead2 = 'Inforce Contribution';
       Else;
          flexhead1 = 'Written Premium';
          flexhead2 = 'Inforce Premium';
       Endif;
       Select;
       When inFLAG = 'D' and isContribution;
            scPrmTitle = 'Contribution Information';
       When inFLAG = 'U' and isContribution;
            scPrmTitle = 'Unbilled Contribution Information';
       When inFLAG = 'H' and isContribution;
            scPrmTitle = 'Contribution History';
       When inFLAG = 'A';
            scPrmTitle = 'Revised Contribution Information';
       Endsl;
       //* Set height of window based on last premium element shown.
       index = 50;
       Dow index > 0;
          If scHead(index) <> ' ';
             Leave;
          Endif;
          index = index - 1;
       Enddo;

       intUnBlWin = 115 + (index * 20);
       intUnBlBtn = 215 + (index * 20);

       //* Do not show inforce premium column for revised bill.
       visInforce = *on;
       If inFlag = 'A';
          visInforce = *off;
       Endif;
       Dou btnPremOK = *on;
             Exfmt rcdUnBilld;
             If btnPremOK = *off;
             Endif;
       Enddo;

       If %open(WPIPOLPOP);
          Close WPIPOLPOP;
       endif;
      /end-free

     PDisplayPremium   e
     P*================================================================
     PPremiumInfo      b                   export
     DPremiumInfo      pi
     D inRRN                          9  0 const
     D incFlag                        1    const
     D outHeadings                 1100
     D outFactors                  1100
     D outAmounts                   550
     D outInfPrem                   550
     d*
     d numEdtCde       c                   const('N')

     d cltid           s              3    inz('000')
     d bilSchCode      s                   like(ELbscd)
     d effdate         s                   like(ELpfdt)
     d inFlag          s              1

      /Free
       inFlag = incFlag;
       GetClientID(CLTID);
       Select;
       When inFLAG = 'D';
          Chain(n) inRRN pel0p;
          intCO# = ELco#;
          intFND = ELfnd;
          intEMP# =Elemp#;
          intDIV = ELdiv;
          intFYR = ELfyr;
          rateStCode = ELrtcd;
          bilSchCode = ELbscd;
          effDate = ELpfdt;
          //*** Beg Add ***** 02/27/24 *************************
          Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pmp0d;
          //*** End Add ***** 02/27/24 *************************
       When inFLAG = 'U';
          Chain(n) inRRN pxelp;
          intCO# = ELco#;
          intFND = ELfnd;
          intEMP# =Elemp#;
          intDIV = ELdiv;
          intFYR = ELfyr;
          rateStCode = ELrtcd;
          bilSchCode = ELbscd;
          effDate = ELpfdt;
          //*** Beg Add ***** 02/27/24 *************************
          Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pmp0d;
          //*** End Add ***** 02/27/24 *************************
       When inFLAG = 'H';
          Chain(n) inRRN peh0p;
          intCO# = EHco#;
          intFND = EHfnd;
          intEMP# =EHemp#;
          intDIV = EHdiv;
          intFYR = EHfyr;
          Chain (intCo#:intFnd:intEmp#:intDiv:intFyr) pel00;
          rateStCode = EHrtcd;
          effDate = EHpfdt;
          bilSchCode = EHbscd;
       When inFLAG = 'A';
          Chain(n) inRRN pel0pa;
          intCO# = ELco#;
          intFND = ELfnd;
          intEMP# =Elemp#;
          intDIV = ELdiv;
          intFYR = ELfyr;
          rateStCode = ELrtcd;
          bilSchCode = ELbscd;
          effDate = ELpfdt;
          inFLAG = 'D';
          //*** Beg Add ***** 02/27/24 *************************
          Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pmp0a;
          //*** End Add ***** 02/27/24 *************************
       Endsl;

       Chain (intCo#:intFnd) pex00;
       is3rdPartyBilling = EX3rdB = 'Y';

       //*** Beg Add ***** 02/27/24 ****************************
       If EHWASTD = *Zeros and EHWSPR <> *Zeros and EHWMNP = 'Y';
          ShowMinBalance = *On;
       Endif;
       //*** End Add ***** 02/27/24 ****************************

       Exsr LoadPrem;

       Return;
      /End-Free
     c*===============================================================
     c*  LoadPrem - Load premium information.
     c*===============================================================
     c*
     c     LoadPrem      BegSr

     c*  Rate Set Code File
     c     rsky          klist
     c                   kfld                    intCO#
     c                   kfld                    xxrtcd            3

     c     rrky          klist
     c                   kfld                    intCO#
     c                   kfld                    intfnd
     c                   kfld                    xxSt
     c                   kfld                    x1EfDt
     c*
     c     rrky2         klist
     c                   kfld                    intCO#
     c                   kfld                    intFND
     c                   kfld                    xxSt

     c*  Incr Limit table File
     c     lmky          klist
     c                   kfld                    intCO#
     c                   kfld                    intFND
     c                   kfld                    xxSt              2
     c                   kfld                    xxKey
     c                   kfld                    xxPfDt            7 0
     c*
     c     lmky2         klist
     c                   kfld                    intCO#
     c                   kfld                    intFND
     c                   kfld                    xxSt              2
     c                   kfld                    xxKey
     c*
     c*   Key List for the System Control file
     c     syky          klist
     c                   kfld                    xxSyK             2

      *  Table file (WTBSP)
     c     tbkey         klist
     c                   kfld                    intCO#
     c                   kfld                    xxKey1
      *
      * Key list of the large Deductible file (WMLDP)
     c     ldky          klist
     c                   kfld                    intCO#
     c                   KFLD                    intFND
     c                   kfld                    intEMP#
     c                   kfld                    intDIV
     c                   kfld                    intFYR

      * Key list of the Admin/Darts Control Defination File(WMEXP)
     c     exky          klist
     c                   kfld                    intCO#
     c                   KFLD                    intFND

     c                   movel     'WC'          xxSyK
     c     syky          Chain     SWC01                              60
     c     WCfl06        CabNe     'Y'                                1313

     c                   move      'N'           ldpflg
     c     WCfl43        ifeq      'Y'
     c                   movel     ELbscd        xxKey1           10
     c     tbkey         Chain(n)  PBS01                              60
     c     *in60         Ifeq      *Off
     c     BSlpgm        Ifeq      'L'
      /Free
          VisibAggrP = *off;
          VisibVarA   = *off;
          If ClientId = '150';
             VisibAggrP = *on;
             VisibVarA = *on;
          Endif;
      /End-Free
      *
     c     ldky          Chain(n)  PLD01                              60
     c     *in60         Ifeq      *Off
      *
     c                   move      'Y'           ldpflg
      *
     c                   Endif
     c                   Endif
     c                   Endif
     c                   Endif
     c*
     c     exky          Chain(n)  PEX00                              60
     c     exky          Chain(n)  PFN00                              60

     c                   move      rateStCode    xxRtcd
     c*
     c     rsky          Chain     PRS00                              60
     c*
     c                   move      RSjrst        xxSt              2
     c                   move      effDate       x1EfDt            7 0
     c*
     c     rrky          Setgt     PRR00
     c     rrky2         Readpe    PRR00                                  60
     c*
      /free
       isContribution = GetPrmContflg(intCO#:intFND);
      /end-free
      /free
       xxDvnd = 0;
       GetClientID(clientID);
       Chain(n) (intCo#:intFnd:intFyr:intEmp#:intDiv) pdv00;
       If %found and clientID = '111';
          xxDvnd = DVamt;
       Endif;
      /end-free
     c*
     c*  Intialize Screen Arrays
     c*
     c                   Clear                   hdg
     c                   Clear                   tam
     c                   Clear                   tin
     c                   Clear                   fct
     c                   Clear                   xxAsp2
     c*
     c*  Load Screen Arrays
     c*
     c                   Z-add     1             a                              MANUAL PREM


     c*  Manual Premium/Manual contribution
     c                   If        not isContribution
     c                   MoveL     con(1)        hdg(a)
     c                   Else
     c                   MoveL     con(30)       hdg(a)
     c                   EndIf
     c                   Callp     SetCondition1(inflag:outtam:outtin)
     c                   Z-Add     outtam        tam(a)
     c                   Z-Add     outtin        tin(a)
     c                   Add       1             a


     C*  Load Short Rate Penalty if Applied to Manual Premium
     c                   Callp     SetCondition2(inflag)


     C*  Load Deviation if Applied to Manual Premium
     c                   Callp     SetCondition3(inflag)


     C*  Load Deviation 1 2nd Period if applied to ManulL Premium
     c                   Callp     SetCondition4(inflag)


     C*  Load 2nd Deviation if Applied to Manual Premium
     c                   Callp     SetCondition5(inflag)


     C*  Load 2nd Deviation 2nd Period if applied to Manual Premium
     c                   Callp     SetCondition6(inflag)

      // Load Schedule DB/CR if Applied to Manual Premium
      //*** Beg Add ***** 04/18/24 ***************************************
       If RRDcap = 'M';
          If inFlag = 'D' or inFlag = 'U';
             Exsr CALDBRDU;
          Else;
             Exsr CALDBRH;
          Endif;
       Endif;
      //*** End Add ***** 04/18/24 ***************************************

     c*  Waiver of Subrogation
     c     ELWVFL        IFEQ      'M'
     c     RRWVHD        IFNE      *BLANKS
     c                   MOVEL     RRWVHD        HDG(A)
     c                   ELSE
     c                   If        not isContribution
     c                   MOVEL     CON(1)        HDG(A)
     c                   ELSE
     c                   MOVEL     CON(30)       HDG(A)
     c                   ENDIF
     c                   ENDIF
     c                   Callp     SetCondition7(inflag)
     C                   ADD       1             A
     C                   Endif                                                  RRWVFL IFEQ


     C*  Increased Limits
     C                   Move      *Blanks       xxKEY            10
     c                   Select
     c                   When      inFLAG = 'D' or inflag = 'U'
     C                   MOVEL     ELilcd        xxKEY
     C                   MOVE      ELilef        xxPFDT
     c                   When      inFLAG = 'H'
     C                   MOVEL     EHilcd        xxKEY
     C                   MOVE      EHilef        xxPFDT
     c                   Endsl
     C     LMKY          SETGT     PLM01                              60
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C   60              Z-ADD     *ZERO         LMPCT
     C                   MOVEA     CON(2)        TMP
     C     LMPCT         IFNE      *ZERO
     C                   eval      fct(a) = %editc(LMpct:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     c                   Callp     SetCondition8(inflag)
      //*** Beg Add ***** 08/14/24 **********************************
       Select;
       When inflag = 'D' or inflag = 'U';
          If elfaf = 'A';
             If MPABIL <> 0;
                A += 1;
                Hdg(A) = con(65);
                TAM(A) = MPABIL;
             Endif;
          Else;
             If MPEBIL <> 0;
                A += 1;
                Hdg(A) = con(65);
                TAM(A) = MPEBIL;
             Endif;
          Endif;
       Other;                              //Employer History Mode 'H'
          If EHWBIL <> 0 or EHIBIL <> 0;   //Written/Inforce Bal to Increased Limit Minimum
             A += 1;
             Hdg(A) = con(65);
             TAM(A) = EHWBIL;
             TIN(A) = EHIBIL;
          Endif;
       Endsl;
      //*** End Add ***** 08/14/24 **********************************
     C                   ADD       1             A



     C*  2ND INCREASED LIMITS
     C                   MOVE      *BLANKS       XXKEY            10
     c                   Select
     c                   When      inFLAG = 'D' or inflag = 'U'
     C                   MOVEL     ELinl2        xxKEY
     C                   MOVE      ELi2ef        xxPFDT
     c                   When      inFLAG = 'H'
     C                   MOVEL     EHi2cd        xxKEY
     C                   MOVE      EHi2ef        xxPFDT
     c                   Endsl
     C     LMKY          SETGT     PLM01                              60
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C   60              Z-ADD     *ZERO         LMPCT
     C                   MOVEA     CON(17)       TMP
     C     LMPCT         IFNE      *ZERO
     C                   eval      fct(a) = %editc(LMpct:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   Clear                   TMP
     c                   Callp     SetCondition9(inflag)
      //*** Beg Add ***** 08/14/24 **********************************
       Select;
       When inflag = 'D' or inflag = 'U';
          If elfaf = 'A';
             If MPABIL2 <> 0;
                A += 1;
                Hdg(A) = con(66);
                TAM(A) = MPABIL2;
             Endif;
          Else;
             If MPEBIL2 <> 0;
                A += 1;
                Hdg(A) = con(66);
                TAM(A) = MPEBIL2;
             Endif;
          Endif;
       Other;                                //Employer History Mode 'H'
          If EHWBIL2 <> 0 or EHIBIL2 <> 0;   //Written/Inforce Bal to Increased Limit Minimum
             A += 1;
             Hdg(A) = con(66);
             TAM(A) = EHWBIL2;
             TIN(A) = EHIBIL2;
          Endif;
       Endsl;
      //*** End Add ***** 08/14/24 **********************************
     c                   Add       1             a


     C*  VOLUNTARY LABOR CHARGE
     c                   Callp     SetCondition10(inflag)


     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
     c                   Callp     SetCondition11(inflag)


     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
     c                   Callp     SetCondition12(inflag)


     c*  HCN Credit
     c                   Callp     SetCondition13(inflag)

     c*  Subject Premium amounts
     c                   Callp     SetCondition63(inflag)
     c                   Move      Con(49)       Hdg(a)
     c                   ADD       1             A

     c*  MANAGED CARE CREDIT
     c     RR1AHD        IFNE      *BLANKS
     c                   Callp     SetCondition14(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF


     c*  SAFETY CREDIT
     c     RR1BHD        IFNE      *BLANKS
     c                   Callp     SetCondition15(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF


     c*  LOAD DEVIATION IF APPLIED TO MANUAL PREMIUM
     c     RRDV1L        IFEQ      'J'
     c                   Callp     SetCondition16(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF


     c*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     c     RRDV1L        IFEQ      'J'
     c                   Callp     SetCondition17(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   Endif


     c*  LOAD 2ND DEVIATION IF APPLIED TO MANUAL PREMIUM
     c     RRDV2L        IFEQ      'J'
     c                   Callp     SetCondition18(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF


     c*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     c     RRDV2L        IFEQ      'J'
     c                   Callp     SetCondition19(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF


     c* WAIVER OF SUBROGATION
     c     ELWVFL        IFEQ      'J'
     c     RRWVHD        IFNE      *BLANKS
     c                   MOVEL     RRWVHD        HDG(A)
     c                   ELSE
     c                   If        not isContribution
     c                   MOVEL     CON(1)        HDG(A)
     c                   ELSE
     c                   MOVEL     CON(30)       HDG(A)
     c                   ENDIF
     c                   ENDIF
     c                   Callp     SetCondition20(inflag)
     c                   ADD       1             A
     c                   ENDIF                                                  RRWVFL IFEQ

     c* ADDITIONAL LOAD
     c     RRASFL        IFEQ      'J'
     c                   Callp     SetCondition21(inflag)
     c                   Endif                                                  RRWVFL IFEQ

      /Free

       SetCondition65();

       SetCondition66();

      /End-Free

     c*  LOAD SHORT RATE PENALTY IF APPLIED TO SUBJECT PREMIUM
     c     RRSRAF        IFEQ      'J'
     c                   Callp     SetCondition22(inflag)
     c                   MOVEL     CON(36)       HDG(A)
     c                   ADD       1             A
     c                   ENDIF                                                  RRSRAF IFEQ

     c*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO SUBJECT PREMIUM
     c                   Callp     SetCondition23(inflag)


     C*  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
     c                   Callp     SetCondition24(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A

     C*  CONTRACTOR CREDIT
     c                   Callp     SetCondition25(inflag)
     c                   MoveA     TMP           HDG(A)
     c                   Clear                   tmp
     c                   Add       1             a


     C*  SCHEDULE DB/CR
     C     RRDCAP        IFEQ      ' '
     C                   If        inFLAG = 'D' or inFLAG = 'U'
     C                   EXSR      CALDBRDU
     c                   Else
     C                   EXSR      CALDBRH
     C                   ENDIF
     C                   ENDIF                                                  ELDBCR IFNE


     C*  LOAD DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV1L        IFEQ      'E'
     c                   Callp     SetCondition26(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   Add       1             A
     c                   Endif


     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     c     RRDV1L        IFEQ      'E'
     c                   Callp     SetCondition27(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   Endif


     C*  LOAD 2ND DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV2L        IFEQ      'E'
     c                   Callp     SetCondition28(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF


     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV2L        IFEQ      'E'
     c                   Callp     SetCondition29(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   END
     c     RRDCAP        IFEQ      'Y'
     C                   If        inFLAG = 'D' or inFLAG = 'U'
     C                   EXSR      CALDBRDU
     c                   Else
     C                   EXSR      CALDBRH
     C                   ENDIF                                                  ELDBCR IFNE
     c                   ENDIF


     C* WAIVER OF SUBROGATION
     C     ELWVFL        IFEQ      'E'
     C     RRWVHD        IFNE      *BLANKS
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ELSE
     c                   If        not isContribution
     C                   MOVEL     CON(1)        HDG(A)
     C                   ELSE
     C                   MOVEL     CON(30)       HDG(A)
     C                   ENDIF
     C                   ENDIF
     c                   Callp     SetCondition30(inflag)
     C                   ADD       1             A
     C                   ENDIF                                                  RRWVFL IFEQ


     C* ADDITIONAL LOAD
     c                   Callp     SetCondition31(inflag)

     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO EXPERIENCE MODIFIER PRM
     C     LDPFLG        IFEQ      'Y'
     c                   Callp     SetCondition32(inflag)
     C                   ENDIF                                                  LDPFLG = 'Y'


     C*  ADDITIONAL LOAD
     C*
     C*
     C*  CONTRACTOR CREDIT
     C*

     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
     C     RRRDPR        IFEQ      'S'
     C     LDPFLG        ANDNE     'Y'
     c                   Callp     SetCondition33(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF


     C* Loss Constant
     c                   Callp     SetCondition34(inflag)

      *** Beg Add ***** 02/27/24 ************************************
     C*  Balance to minimum line
     c                   Callp     SetCondition77(inflag)
      *** End Add ***** 02/27/24 ************************************


     C*  LOAD SHORT RATE PENALTY IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRSRAF        IFEQ      'S'
     c                   Callp     SetCondition35(inflag)
     C                   ENDIF                                                  RRSRAF IFEQ


     C*  STANDARD PREMIUM/Standard Contribution
     c                   If        not isContribution
     C                   MOVEL     CON(11)       HDG(A)                         STD PREM
     C                   ELSE
     C                   MOVEL     CON(27)       HDG(A)
     C                   ENDIF
     c                   Callp     SetCondition36(inflag)
     C                   ADD       1             A

      /free
       // Earned premium credit
       If clientID = '111' and xxDvnd > 0;
          SetCondition64();
       Endif;
      /end-free

     C*  LOAD DEVIATION IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRDV1L        IFEQ      'S'
     c                   Callp     SetCondition37(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF


     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     c     RRDV1L        IFEQ      'S'
     c                   Callp     SetCondition38(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   END


     C*  LOAD 2ND DEVIATION IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRDV2L        IFEQ      'S'
     c                   Callp     SetCondition39(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     C     RRDV2L        IFEQ      'S'
     c                   Callp     SetCondition40(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF


     c* WAIVER OF SUBROGATION
     c     ELWVFL        IFEQ      'S'
     c     RRWVHD        IFNE      *BLANKS
     c                   MOVEL     RRWVHD        HDG(A)
     c                   ELSE
     c                   If        not isContribution
     c                   MOVEL     CON(1)        HDG(A)
     c                   ELSE
     c                   MOVEL     CON(30)       HDG(A)
     c                   ENDIF
     c                   ENDIF
     c                   Callp     SetCondition41(inflag)
     c                   ADD       1             A
     c                   ENDIF                                                  RRWVFL IFEQ


     c* ADDITIONAL LOAD
     c                   Callp     SetCondition42(inflag)


     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
     c     LDPFLG        IFEQ      'Y'
     c                   Callp     SetCondition43(inflag)
     c                   ENDIF


     c*  DISCOUNT
     c                   Callp     SetCondition44(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A


     C*  LOAD DEVIATION IF APPLIED TO NORMAL PREMIUM
     C     RRDV1L        IFEQ      'D'
     c                   Callp     SetCondition45(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF


     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO NORMAL PREMIUM
     C     RRDV1L        IFEQ      'D'
     c                   Callp     SetCondition46(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   END


     C*  LOAD 2ND DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
     C     RRDV2L        IFEQ      'D'
     c                   Callp     SetCondition47(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF


     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO DISCOUNTED PREMIUM
     C     RRDV2L        IFEQ      'D'
     c                   Callp     SetCondition48(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF


     C* WAIVER OF SUBROGATION
     C     ELWVFL        IFEQ      'D'
     C     RRWVHD        IFNE      *BLANKS
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ELSE
     c                   If        not isContribution
     C                   MOVEL     CON(1)        HDG(A)
     c                   ELSE
     c                   MOVEL     CON(30)       HDG(A)
     c                   Endif
     c                   Endif
     c                   Callp     SetCondition49(inflag)
     C                   ADD       1             A
     C                   ENDIF                                                  RRWVFL IFEQ


     c* ADDITIONAL LOAD
     c                   Callp     SetCondition50(inflag)


     c*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO DISCOUNTED PREMIUM
     c     LDPFLG        IFEQ      'Y'
     c                   Callp     SetCondition51(inflag)
     C                   ENDIF

      *** Beg Add *** 11/11/24 ***********************************
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO DISCOUNTED PREMIUMS
     C     RRRDPR        IFEQ      'D'
     C     LDPFLG        ANDNE     'Y'
     c                   Callp     SetCondition33(inflag)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
      *** End Add *** 11/11/24 ***********************************

     C*  MASSACHUSETTS ARAP SURCHARGE
     c                   Callp     SetCondition52(inflag)


     C*  NORMAL PREMIUM
     c                   Callp     SetCondition53(inflag)
     C                   MOVEL     CON(13)       HDG(A)
     C                   ADD       1             A


     c*  Harbor Exposure
     c                   Callp     SetCondition54(inflag)


     c* Reinsurance charge
     c                   Callp     SetCondition55(inflag)


     C*  EXPENSE CONSTANT
     c                   Callp     SetCondition56(inflag)
     C                   MOVEL     CON(14)       HDG(A)                         EXP CONSTANT
     c                   ADD       1             A

      // Policy Fee
      /Free
       WritePolicyFee(inFlag);
       a += 1;
      /End-Free

     c* UNEARNED PREMIUM SURCHARGE AMOUNT
     c                   Callp     SetCondition57(inflag)


     c* TERRORISM RISK ACT
     c                   Callp     SetCondition58(inflag)


     c* Domestic TERRORISM RISK ACT
     c                   Callp     SetCondition59(inflag)

     c*  ANNUAL PREMIUM/Annual Contribution
     c                   If        not isContribution
     c                   MOVEL     CON(15)       HDG(A)                         EST PREM
     c                   ELSE
     c                   MOVEL     CON(28)       HDG(A)
     c                   ENDIF
     c                   Callp     SetCondition60(inflag)
     c                   ADD       1             A


     c*  LOAD PREMIUM INTO ARRAY IF NOT EQUAL 1
     c                   Callp     SetCondition61(inflag)
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A


     c* RETROSPECTIVE PREMIUM.
     c                   Callp     SetCondition62(inflag)

      *** Beg Chg ***** 10/14/24 ***********************************
      * moving this block after retrospective Premium
      *  Assessments, Safety Fees and Membership Fees
     c                   Callp     SetCondition67(inflag)
     c                   Callp     SetCondition68(inflag)
     c                   Callp     SetCondition69(inflag)
     c                   Callp     SetCondition70(inflag)
     c                   Callp     SetCondition71(inflag)
     c                   Callp     SetCondition72(inflag)
     c                   Callp     SetCondition73(inflag)
     c                   Callp     SetCondition74(inflag)
     c                   Callp     SetCondition75(inflag)
     c                   Callp     SetCondition76(inflag)
      *
      *** End Chg ***** 10/14/24 ***********************************
      *** Beg Add ***** 10/14/24 ***********************************
     c* Total
     c                   Callp     SetCondition79()

      *** End Add ***** 10/14/24 ***********************************
      *** Beg Add ***** 02/27/24 ***********************************
     c* *Minimum has invoked.
      *** Beg Chg ***** 11/21/25 **********************************
     c***                Callp     SetCondition78()
     c                   Callp     SetCondition78(inflag)
      *** End Chg ***** 11/21/25 **********************************

       If inflag = 'H';
          If showMinBalance = *On and EHwmnp = 'Y';
             HDG(50) = 'MINIMUMMSG';
          Endif;
       Endif;
      *** End Add ***** 02/27/24 ***********************************

     c                   MoveA     HDG           outheadings
     c                   MoveA     FCT           outFactors
     c                   Move      TAMA          outAmounts
     c                   Move      TINA          outInfPrem

     c                   Endsr                                                  LoadPrem
     C**************************************************************************************
     C*
     C*  SCHEDULE DB/CR
     C*
     C     CALDBRDU      BEGSR
     C*
     C*
     C                   If        elfaf = 'A'
     C                   Z-ADD     ELAFRS        TAM(A)
     c                   Else
     C                   Z-ADD     ELEFRS        TAM(A)
     c                   Endif
     C*
     C                   Z-ADD     ELEDC         TAM(A)
     C                   MOVEA     CON(19)       TMP
     C     ELDBCR        IFLT      1
     C     RRCRHD        IFNE      *BLANKS
     C                   MOVEA(P)  RRCRHD        TMP
     C                   ENDIF
     C                   ELSE
     C     RRDBHD        IFNE      *BLANKS
     C                   MOVEA(P)  RRDBHD        TMP
     C                   ENDIF
     C                   ENDIF                                                  ELDBCR IFLT
     C     CLTID         IFEQ      '136'
     C                   MOVEA     CON(26)       TMP
     C                   ENDIF
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C     ELDBCR        MULT(H)   ELCMUL        XXWK54            5 4
     c                   eval      fct(a) = %editc(XXWK54:'K')
     C                   OTHER
     c                   eval      fct(a) = %editc(ELdbcr:'K')
     C                   ENDSL
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDSR

     C     CALDBRH       BEGSR
     C*  SCHEDULE DB/CR
     C*
     C                   Z-ADD     EHWFRS        TAM(A)
     C                   Z-ADD     EHIFRS        TIN(A)
     C                   Z-SUB     TAM(A)        XTADLD
     C                   Z-SUB     TIN(A)        XTADLN
     C*** Beg Chg *****  07/21/24  **************************************
     C*****              Z-ADD     EHWDC         TAM(A)
     C                   Z-ADD     EHWADC        TAM(A)
     C*** End Chg *****  07/21/24  **************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC = *zeros
     c                   Z-ADD     EHWDC         TAM(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     *** Beg Chg ***** 09/24/24 *********************************
     C***                Z-ADD     EHIDC         TIN(A)
     c                   Z-ADD     EHIADC        TIN(A)
     c                   If        EHIADC = *zeros
     c                   Z-ADD     EHIDC         TIN(A)
     c                   Endif
     *** End Chg ***** 09/24/24 *********************************
     C                   MOVEA     CON(19)       TMP
     C     EHDBCR        IFLT      1
     C     RRCRHD        IFNE      *BLANKS
     C                   MOVEA(P)  RRCRHD        TMP
     C                   ENDIF
     C                   ELSE
     C     RRDBHD        IFNE      *BLANKS
     c                   MOVEA(P)  RRDBHD        TMP
     c                   ENDIF
     c                   ENDIF                                                  EHDBCR IFLT
     c     CLTID         IFEQ      '136'
     c                   MOVEA     CON(26)       TMP
     c                   ENDIF
     c                   SELECT
     c     CLTID         WHENEQ    '136'
     c     EHDBCR        MULT(H)   EHCMUL        XXWK54            5 4
     c                   eval      fct(a) = %editc(XXWK54:'K')
     c                   OTHER
     c                   eval      fct(a) = %editc(EHdbcr:'K')
     c                   ENDSL
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c*
     c                   ENDSR

     PPremiumInfo      e


     C**************************************************************************************
     p AddLoad         b
     d AddLoad         pi

      *  Employer Payroll File
     C     epky1         KLIST
     C                   KFLD                    intCO#
     C                   KFLD                    intFND
     C                   KFLD                    intEMP#
     C                   KFLD                    intDIV
     C                   KFLD                    intFYR
     C*
     C*  Payroll Class File
     C     PRKY          KLIST
     C                   KFLD                    intCO#
     C                   KFLD                    rateStCode
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    intCO#
     C                   KFLD                    rateStCode
     C                   KFLD                    EPCLAS
      *
      *  Description Extension File
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XPST              2
     C                   Z-ADD     *ZERO         XTADLD
      *
     C     EPKY1         CHAIN     pep0E                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EPSTAT        IFNE      'I'
      *
     C     PRKY          SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      PROCCD        XXOCCD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXOCCD
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     PXCODE        IFEQ      *ZERO
     C     XXOCCD        ANDEQ     *BLANK
     C                   ADD       EPADLD        XTADLD
     C                   ENDIF                                                  PXCODE IFEQ
      *
     C                   ENDIF
     C     EPKY1         READE     pep0E                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     P AddLoad         e


     c**************************************************************************************
     c*  Manual Premium
     c**************************************************************************************
     p SetCondition1   b
     d SetCondition1   pi
     d inflag                         1    Const
     d outtam                        11  2
     d outtin                        11  2

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 *********************************
     c***                Z-Add     ELAmpr        outtam
     c                   If        inflag = 'D'
     c                   Z-Add     MPAmpr        outtam
     c                   Else
     c                   Z-Add     ELAmpr        outtam
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     C                   Else
      *** Beg Chg ***** 02/27/24 *********************************
     c***                Z-Add     ELMapr        outtam
     c                   If        inflag = 'D'
     c                   Z-Add     MPMapr        outtam
     c                   Else
     c                   Z-Add     ELMapr        outtam
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Endif
     c                   If        elfaf = 'A'
     c     ELAmnp        Ifeq      'Y'
     c                   SUB       ELain1        outtam
     c                   SUB       ELain2        outtam
     c                   SUB       ELawav        outtam
     c                   Endif
     c                   Else
     c     ELEmnp        Ifeq      'Y'
     c                   SUB       ELein1        outtam
     c                   SUB       ELein2        outtam
     c                   SUB       ELewav        outtam
     c                   Endif

     c                   Endif

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 *********************************
     c***                Z-Add     EHwmpr        outtam
     c***                Z-Add     EHimpr        outtin
     c                   Z-Add     EHWAMAN       outtam
     c                   Z-Add     EHIAMAN       outtin
      *** End Chg ***** 02/27/24 *********************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAMAN = *zeros
     c                   Z-Add     EHwmpr        outtam
     c                   Endif
     c                   If        EHIAMAN = *zeros
     c                   Z-Add     EHimpr        outtin
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Del ***** 11/04/24 *********************************
     c***                Select
     c***  EHwmnp        WhenEq    'Y'
     c***                SUB       EHwin1        outtam
     c***                SUB       EHwwav        outtam
     c***  EHimnp        WhenEq    'Y'
     c***                Sub       EHiin1        outtin
     c***                Sub       EHiwav        outtin
     c***                Endsl
      *** End Del ***** 11/04/24 *********************************
     c                   Z-Add     outtam        xxSubT
     c                   Z-Add     outtin        xxSubN
     c                   Endif
     P SetCondition1   e


     c**************************************************************************************
     c*  Load Short Rate Penalty if Applied to Manual Premium
     c**************************************************************************************
     p SetCondition2   b
     d SetCondition2   pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
     c     RRsraf        Ifeq      'M'
     c     ELsram        AndNe     *Zeros
     c     RRsraf        Oreq      ' '
     c     ELsram        Andne     *ZERO
      *** Beg Chg ***** 02/27/24 *********************************
     c***                Z-ADD     ELsram        TAM(A)
      *** Beg Chg ***** 09/24/24 ***********************************
     c***                Z-ADD     MPsram        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-ADD     MPsram        TAM(A)
     c                   Else
     c                   Z-ADD     ELsram        TAM(A)
     c                   Endif
      *** End Chg ***** 09/24/24 ***********************************
      *** End Chg ***** 02/27/24 *********************************
     c                   MoveL     CON(36)       HDG(A)
     c                   Add       1             A
     c                   Endif                                                  RRsraf = 'M'
     c                   Endif                                                  elfaf = 'A'

     c**** When flag is 'H'
     c                   Else

     c     RRsraf        Ifeq      'M'
     c     EHwsra        ANDNE     *Zeros
     c     RRsraf        Oreq      ' '
     c     EHwsra        Andne     *ZERO
     c                   Z-ADD     EHWSRA        TAM(A)
     c                   Z-ADD     0             TIN(A)
     c                   MOVEL     CON(36)       HDG(A)
     c                   Add       1             A
     c                   Endif                                                  RRSRAF IFEQ

     c                   Endif

     P SetCondition2   e


     c**************************************************************************************
     c*  Load Deviation if Applied to Manual Premium
     c**************************************************************************************
     p SetCondition3   b
     d SetCondition3   pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c     RRDV1L        IFEQ      'M'
     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 *********************************
     c***                Z-SUB     ELADC2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPADC2        TAM(A)
     c                   Else
     c                   Z-SUB     ELADC2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Else
      *** Beg Chg ***** 02/27/24 *********************************
     c***                Z-SUB     ELEDC2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPEDC2        TAM(A)
     c                   Else
     c                   Z-SUB     ELEDC2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Endif
     c     RRDV1H        IFEQ      *BLANKS
     c                   movea     con(6)        TMP
     c                   else
     c                   MOVEA     RRDV1H        TMP
     c                   ENDIF                                                  RRDV1H = ' '
     c     ELASP2        IFNE      *ZERO
     c     ELASP2        IFLT      0
     c     ELASP2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELASP2        XXASP2
     c                   ENDIF                                                  ELASP2 LT 0
     c                   ENDIF                                                  ELASP2 NE 0
     c                   If        xxAsp2 <> 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   Else
     c                   eval      fct(a) = ' '
     c                   Endif
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF                                                  RRDV1L = 'M'

     c**** When flag is 'H'

     c                   Else
     C     RRdv1l        IFEQ      'M'
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     EHWDC2        TAM(A)
     C                   Z-SUB     EHWADC2       TAM(A)
      *** End Chg ***** 02/27/24 *********************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC2 = *zeros
     c                   Z-SUB     EHWDC2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     EHIDC2        TIN(A)
     C                   Z-SUB     EHIADC2       TIN(A)
      *** End Chg ***** 02/27/24 *********************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADC2 = *zeros
     c                   Z-SUB     EHIDC2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C     RRdv1h        IFEQ      *BLANKS
     C                   movea     con(6)        TMP
     C                   else
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF                                                  RRDV1H = ' '
     c     EHASP2        IFNE      *ZERO
     c     EHASP2        IFLT      0
     c     EHASP2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      EHASP2        XXASP2
     c                   ENDIF                                                  EHASP2 LT 0
     c                   ENDIF                                                  EHASP2 NE 0
     c                   If        xxAsp2 <> 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   Else
     c                   eval      fct(a) = ' '
     c                   Endif
     c                   MOVEA     TMP           HDG(A)
     c                   Clear                   TMP
     c                   Add       1             A
     c                   Endif                                                  RRDV1L = 'M'

     c                   Endif

     P SetCondition3   e


     c**************************************************************************************
     c*  Load Deviation 1 2nd Period if applied to ManulL Premium
     c**************************************************************************************
     p SetCondition4   b
     d SetCondition4   pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c     RRDV1L        IFEQ      'M'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     ELD1A2        TAM(A)
     c                   If        inflag = 'D'
     C                   Z-SUB     MPD1A2        TAM(A)
     c                   Else
     C                   Z-SUB     ELD1A2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Else
      *** Beg Chg ***** 02/27/24 *********************************
     c***                Z-SUB     ELD1E2        TAM(A)
     c                   If        inflag = 'D'
     C                   Z-SUB     MPD1E2        TAM(A)
     c                   Else
     c                   Z-SUB     ELD1E2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Endif
     c                   MOVE      *BLANKS       XXWK25
     c     RRDV1H        IFEQ      *BLANKS
     c     @2NPER        CAT       CON(6):1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ELSE
     c     @2NPER        CAT       RRDV1H:1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ENDIF
     c                   Clear                   XXASP2
     c     ELD1S2        IFNE      *ZERO
     c     ELD1S2        IFLT      0
     c     ELD1S2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELD1S2        XXASP2
     c                   ENDIF                                                  ELD1S2 LT 0
     c                   ENDIF                                                  ELD1S2 NE 0
     c                   If        xxAsp2 <> 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   Else
     c                   eval      fct(a) = ' '
     c                   Endif
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   Endif                                                  RRDV1L = 'M'

     c**** When flag is 'H'
     c                   Else

     C     RRDV1L        IFEQ      'M'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHD1W2        TAM(A)
     c                   Z-SUB     EHWAD12       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWAD12 = *zeros
     c                   Z-SUB     EHD1W2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     c                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHD1I2        TIN(A)
     c                   Z-SUB     EHIAD12       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHIAD12 = *zeros
     c                   Z-SUB     EHD1I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     c                   ADD       TIN(A)        XXSUBN
     c                   MOVE      *BLANKS       XXWK25
     c     RRDV1H        IFEQ      *BLANKS
     c     @2NPER        CAT       CON(6):1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ELSE
     c     @2NPER        CAT       RRDV1H:1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ENDIF
     c                   Clear                   XXASP2
     c     EHD1S2        IFNE      *ZERO
     c     EHD1S2        IFLT      0
     c     EHD1S2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      EHD1S2        XXASP2
     c                   ENDIF                                                  EHD1S2 LT 0
     c                   ENDIF                                                  EHD1S2 NE 0
     c                   If        xxAsp2 <> 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   Else
     c                   eval      fct(a) = ' '
     c                   Endif
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF                                                  RRDV1L = 'M'

     c                   Endif

     P SetCondition4   e


     c**************************************************************************************
     c*  Load 2nd Deviation if Applied to Manual Premium
     c**************************************************************************************
     p SetCondition5   b
     d SetCondition5   pi
     d inflag                         1    Const

     C                   Callp     GetClientID(CLTID)
     c                   If        inflag = 'D' or inflag = 'U'

     C     RRDV2L        IFEQ      'M'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     ELADV2        TAM(A)
     c                   If        inflag = 'D'
     C                   Z-SUB     MPADV2        TAM(A)
     c                   Else
     c                   Z-SUB     ELADV2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Else
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     ELEDV2        TAM(A)
     c                   If        inflag = 'D'
     C                   Z-SUB     MPEDV2        TAM(A)
     c                   Else
     C                   Z-SUB     ELEDV2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Endif
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF                                                  RRDV2L= 'M'

     c**** When flag is 'H'
     c                   Else

     C     RRDV2L        IFEQ      'M'
     C*
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     EHWDV2        TAM(A)
     C                   Z-SUB     EHWADV2       TAM(A)
      *** End Chg ***** 02/27/24 *********************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADV2 = *zeros
     c                   Z-SUB     EHWDV2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     EHIDV2        TIN(A)
     C                   Z-SUB     EHIADV2       TIN(A)
      *** End Chg ***** 02/27/24 *********************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADV2 = *zeros
     c                   Z-SUB     EHIDV2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   Endif                                                  RRDV2L= 'M'

     c                   Endif

     P SetCondition5   e


     C**************************************************************************************
     C*  Load 2nd Deviation 2nd Period if applied to Manual Premium
     C**************************************************************************************
     p SetCondition6   b
     d SetCondition6   pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C     RRDV2L        IFEQ      'M'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     ELD2A2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPD2A2        TAM(A)
     C                   Else
     C                   Z-SUB     ELD2A2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Else
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     ELD2E2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPD2E2        TAM(A)
     C                   Else
     C                   Z-SUB     ELD2E2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 *********************************
     c                   Endif
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     ELD2S2        IFNE      *ZERO
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2
     C                   ENDIF                                                  ELD2S2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELD2S2 NE 0
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     C     ELSTFC        IFNE      *ZERO
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      ELSTFC        XXASN8
     C                   ENDIF                                                  ELSTFC LT 0
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else

     C     RRDV2L        IFEQ      'M'
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     EHD2W2        TAM(A)
     C                   Z-SUB     EHWAD22       TAM(A)
      *** End Chg ***** 02/27/24 *********************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAD22 = *zeros
     c                   Z-SUB     EHD2W2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 *********************************
     C***                Z-SUB     EHD2I2        TIN(A)
     C                   Z-SUB     EHIAD22       TIN(A)
      *** End Chg ***** 02/27/24 *********************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIAD22 = *zeros
     c                   Z-SUB     EHD2I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C*
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C*
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     EHD2S2        IFNE      *ZERO
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP2
     C                   ENDIF                                                  EHD2S2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  EHD2S2 NE 0
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     C     EHSTFC        IFNE      *ZERO
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      EHSTFC        XXASN8
     C                   ENDIF                                                  EHSTFC LT 0
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  EHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c                   Endif

     P SetCondition6   e


     C**************************************************************************************
     C* WAIVER OF SUBROGATION
     C**************************************************************************************
     p SetCondition7   b
     d SetCondition7   pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELEWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPEWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWWAV        TAM(A)
     C***                Z-ADD     EHIWAV        TIN(A)
     C                   Z-ADD     EHWAWAV       TAM(A)
     C                   Z-ADD     EHIAWAV       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAWAV = *zeros
     c                   Z-ADD     EHWWAV        TAM(A)
     c                   Endif
     c                   If        EHIAWAV = *zeros
     c                   Z-ADD     EHIWAV        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************

     c                   Endif

      /Free
       If tam(a) <> 0;
          hdg(a) = AddWaiverToHdg(hdg(a));
       Endif;
      /End-Free

     P SetCondition7   e


     C**************************************************************************************
     C*  INCREASED LIMITS
     C**************************************************************************************
     p SetCondition8   b
     d SetCondition8   pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

      //*** Beg Chg ***** 08/14/24 **********************************
     c***                If        elfaf = 'A'
     C***                Z-ADD     ELAIN1        TAM(A)
     c***                Else
     C***                Z-ADD     ELEIN1        TAM(A)
     c***                Endif
       If elfaf = 'A';
          TAM(A) = MPAIN1;
       Else;
          TAM(A) = MPEIN1;
       Endif;
      //*** End Chg ***** 08/14/24 **********************************

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-ADD     EHwin1        TAM(A)
     c                   Z-ADD     EHWAIL1       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
      *** Beg Del ***** 11/21/25 *********************************
     c***                If        EHWAIL1 = *zeros
     c***                Z-ADD     EHwin1        TAM(A)
     c***                Endif
      *** End Del ***** 11/21/25 *********************************
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TAM(A)        XXSUBT           11 2
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-ADD     EHiin1        TIN(A)
     c                   Z-ADD     EHIAIL1       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
      *** Beg Del ***** 11/21/25 *********************************
     c***                If        EHIAIL1 = *zeros
     c***                Z-ADD     EHiin1        TIN(A)
     c***                Endif
      *** End Del ***** 11/21/25 *********************************
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TIN(A)        XXSUBN

     c                   Endif

     P SetCondition8   e


     C**************************************************************************************
     C*  2ND INCREASED LIMITS
     C**************************************************************************************
     p SetCondition9   b
     d SetCondition9   pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

      //*** Beg Chg ***** 08/14/24 **********************************
     C***                If        elfaf = 'A'
     C***                Z-ADD     ELAIN2        TAM(A)
     c***                Else
     C***                Z-ADD     ELEIN2        TAM(A)
     c***                Endif
       If elfaf = 'A';
          TAM(A) = MPAIN2;
       Else;
          TAM(A) = MPEIN2;
       Endif;
      //*** End Chg ***** 08/14/24 **********************************

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-ADD     EHWIN2        TAM(A)
     c                   Z-ADD     EHWAIL2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
      *** Beg Del ***** 11/21/25 *********************************
     c***                If        EHWAIL2 = *zeros
     c***                Z-ADD     EHWIN2        TAM(A)
     c***                Endif
      *** End Del ***** 11/21/25 *********************************
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TAM(A)        XXSUBT           11 2
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-ADD     EHIIN2        TIN(A)
     c                   Z-ADD     EHIAIL2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
      *** Beg Del ***** 11/21/25 *********************************
     c***                If        EHIAIL2 = *zeros
     c***                Z-ADD     EHIIN2        TIN(A)
     c***                Endif
      *** End Del ***** 11/21/25 *********************************
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TIN(A)        XXSUBN

     c                   Endif

     P SetCondition9   e


     C**************************************************************************************
     C*  VOLUNTARY LABOR CHARGE
     C**************************************************************************************
     p SetCondition10  b
     d SetCondition10  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C     ELEMNP        IFNE      'Y'
     C     ELVLCA        ANDNE     *ZERO
     C     RRVLCH        IFNE      *BLANKS
     C                   MOVEL     RRVLCH        HDG(A)
     C                   ELSE
     C                   MOVEL     CON(25)       HDG(A)
     C                   ENDIF
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELVLCA        TAM(A)
     C                   If        inflag = 'D'
     C                   If        elfaf = 'A'
     C                   Z-ADD     MPAVLC        TAM(A)
     C                   Else
     C                   Z-ADD     MPEVLC        TAM(A)
     C                   Endif
     C                   Else
     C                   Z-ADD     ELVLCA        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     C                   ADD       1             A
     C                   ENDIF                                                  ELEMNP IFEQ

     c**** When flag is 'H'
     c                   Else

     C     EHWMNP        IFNE      'Y'
     C     EHIMNP        ANDNE     'Y'
     C     EHWVLC        ANDNE     *ZERO
     C     RRVLCH        IFNE      *BLANKS
     C                   MOVEL     RRVLCH        HDG(A)
     C                   ELSE
     C                   MOVEL     CON(25)       HDG(A)
     C                   ENDIF
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWVLC        TAM(A)
     C***                Z-ADD     EHIVLC        TIN(A)
     C                   Z-ADD     EHWAVLC       TAM(A)
     C                   Z-ADD     EHIAVLC       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWAVLC = *zeros
     c                   Z-ADD     EHWVLC        TAM(A)
     c                   Endif
     c                   If        EHIAVLC = *zeros
     c                   Z-ADD     EHIVLC        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     C                   ADD       1             A
     C                   Endif                                                  EHWMNP IFEQ

     c                   Endif

     P SetCondition10  e


     C**************************************************************************************
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
     C**************************************************************************************
     p SetCondition11  b
     d SetCondition11  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c     RRRDPR        IFEQ      'M'
     c     LDPFLG        ANDNE     'Y'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELADCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(3)        TMP
     C     ELDDCT        IFNE      *ZERO
     C                   eval      fct(a) = %editc(ELddct:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   If        elfaf = 'A'
     C                   Z-SUB     ELADD2        TAM(A)
     c                   Else
     C                   Z-SUB     ELEDD2        TAM(A)
     c                   Endif
     C                   MOVEA     CON(31)       TMP
     C     ELDD2P        IFNE      *ZERO
     C                   eval      fct(a) = %editc(ELDD2P:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else

     C     RRRDPR        IFEQ      'M'
     C     LDPFLG        ANDNE     'Y'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDCT        TAM(A)
     C                   Z-SUB     EHWADCT       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCT = *zeros
     c                   Z-SUB     EHWDCT        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHIDCT        TIN(A)
     C                   Z-SUB     EHIADCT       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 **********************************
     c                   If        EHIADCT = *zeros
     c                   Z-SUB     EHIDCT        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 **********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(3)        TMP
     C     EHDDCT        IFNE      *ZERO
     C                   eval      fct(a) = %editc(EHddct:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   Z-SUB     EHWDD2        TAM(A)
     C                   ADD       TAM(A)        XXSUBT
     C                   Z-SUB     EHIDD2        TIN(A)
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(31)       TMP
     C     EHDD2P        IFNE      *ZERO
     C                   eval      fct(a) = %editc(EHDD2P:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition11  e


     C**************************************************************************************
     C*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
     C**************************************************************************************
     p SetCondition12  b
     d SetCondition12  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c     LDPFLG        IFEQ      'Y'
     c     LDPLV1        IFEQ      'M'
     c     LDPCT1        IFNE      *ZEROS
     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADCT        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPADCT        TAM(A)
     C                   Else
     c                   Z-SUB     ELADCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELEDCT        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPEDCT        TAM(A)
     C                   Else
     c                   Z-SUB     ELEDCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(LDpct1:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF
     c                   ENDIF                                                  LDPLV1 = 'M'
     c     LDPLV2        IFEQ      'M'
     c     LDPCT2        ANDNE     *ZEROS
     c                   If        elfaf = 'A'
     c                   Z-SUB     ELADD2        TAM(A)
     c                   Else
     c                   Z-SUB     ELEDD2        TAM(A)
     c                   Endif
     c                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(LDPCT2:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF
     c                   ENDIF                                                  LDPFLG = 'Y'

     c**** When flag is 'H'
     c                   Else

     c     LDPFLG        IFEQ      'Y'
     c     LDPLV1        IFEQ      'M'
     c     EHPCT1        IFNE      *ZEROS
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHWDCT        TAM(A)
     c                   Z-SUB     EHWADCT       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCT = *zeros
     c                   Z-SUB     EHWDCT        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHIDCT        TIN(A)
     c                   Z-SUB     EHIADCT       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADCT = *zeros
     c                   Z-SUB     EHIDCT        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TIN(A)        XXSUBN
     c                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(EHpct1:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF
     c                   ENDIF                                                  LDPLV1 = 'M'
     c     LDPLV2        IFEQ      'M'
     c     EHPCT2        ANDNE     *ZEROS
     c                   Z-SUB     EHWDD2        TAM(A)
     c                   ADD       TAM(A)        XXSUBT
     c                   Z-SUB     EHIDD2        TIN(A)
     c                   ADD       TIN(A)        XXSUBN
     c                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(EHPCT2:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF
     c                   ENDIF                                                  LDPFLG = 'Y'

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition12  e


     C**************************************************************************************
     c*  HCN Credit
     C**************************************************************************************
     p SetCondition13  b
     d SetCondition13  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c     ELHCN         IFNE      *ZERO
     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELAHCN        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPAHCN        TAM(A)
     C                   Else
     c                   Z-SUB     ELAHCN        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELEHCN        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPEHCN        TAM(A)
     C                   Else
     c                   Z-SUB     ELEHCN        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c                   MOVEA     CON(38)       TMP
     c                   eval      fct(a) = %editc(ELHCN:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     C                   ENDIF                                                  ELHCN IFNE

     c**** When flag is 'H'
     c                   Else

     c     EHHCN         IFNE      *ZERO
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHWHCN        TAM(A)
     c                   Z-SUB     EHWAHCN       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWAHCN = *zeros
     c                   Z-SUB     EHWHCN        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     c                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHIHCN        TIN(A)
     c                   Z-SUB     EHIAHCN       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHIAHCN = *zeros
     c                   Z-SUB     EHIHCN        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     c                   ADD       TIN(A)        XXSUBN
     c                   MOVEA     CON(38)       TMP
     c                   eval      fct(a) = %editc(EHHCN:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF                                                  EHHCN IFNE

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition13  e


     C**************************************************************************************
     c*  MANAGED CARE CREDIT
     C**************************************************************************************
     p SetCondition14  b
     d SetCondition14  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADCM        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPADCM        TAM(A)
     C                   Else
     c                   Z-SUB     ELADCM        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDCM        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDCM        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDCM        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C*
     C                   MOVEA     RR1AHD        TMP
     C     ELASPM        IFNE      *ZERO
     C                   eval      fct(a) = %editc(ELASPM:'K')  + ' ' + '%'
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDCM        TAM(A)
     C                   Z-SUB     EHWADCM       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCM = *zeros
     c                   Z-SUB     EHWDCM        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHIDCM        TIN(A)
     C                   Z-SUB     EHIADCM       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADCM = *zeros
     c                   Z-SUB     EHIDCM        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     RR1AHD        TMP
     C     EHASPM        IFNE      *ZERO
     C                   eval      fct(a) = %editc(EHASPM:'K')  + ' ' + '%'
     C                   ENDIF

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition14  e


     C**************************************************************************************
     C*  SAFETY CREDIT
     C**************************************************************************************
     p SetCondition15  b
     d SetCondition15  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADCS        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPADCS        TAM(A)
     C                   Else
     c                   Z-SUB     ELADCS        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELEDCS        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPEDCS        TAM(A)
     C                   Else
     c                   Z-SUB     ELEDCS        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c                   MOVEA     RR1BHD        TMP
     c     ELASPS        IFNE      *ZERO
     c                   eval      fct(a) = %editc(ELASPS:'K')  + ' ' + '%'
     c                   ENDIF

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHWDCS        TAM(A)
     c                   Z-SUB     EHWADCS       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 **********************************
     c                   If        EHWADCS = *zeros
     c                   Z-SUB     EHWDCS        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 **********************************
     c                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHIDCS        TIN(A)
     c                   Z-SUB     EHIADCS       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 **********************************
     c                   If        EHIADCS = *zeros
     c                   Z-SUB     EHIDCS        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 **********************************
     c                   ADD       TIN(A)        XXSUBN
     c                   MOVEA     RR1BHD        TMP
     c     EHASPS        IFNE      *ZERO
     c                   eval      fct(a) = %editc(EHASPS:'K')  + ' ' + '%'
     c                   Endif

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition15  e


     C**************************************************************************************
     C*  LOAD DEVIATION IF APPLIED TO MANUAL PREMIUM
     C**************************************************************************************
     p SetCondition16  b
     d SetCondition16  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADC2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPADC2        TAM(A)
     C                   Else
     c                   Z-SUB     ELADC2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDC2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDC2        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDC2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF                                                  RRDV1H = ' '
     C     ELASP2        IFNE      *ZERO
     C     ELASP2        IFLT      0
     C     ELASP2        MULT      -1            XXASP2
     C                   eval      fct(a) = %editc(xxASP2:'K')  + ' ' + '%'
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2
     C                   eval      fct(a) = %editc(xxASP2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2
     C                   ENDIF                                                  ELASP2

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHWDC2        TAM(A)
     c                   Z-SUB     EHWADC2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC2 = *zeros
     c                   Z-SUB     EHWDC2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHIDC2        TIN(A)
     c                   Z-SUB     EHIADC2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADC2 = *zeros
     c                   Z-SUB     EHIDC2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TIN(A)        XXSUBN
     c     RRDV1H        IFEQ      *BLANKS
     c                   MOVEA     CON(6)        TMP
     c                   ELSE
     c                   MOVEA     RRDV1H        TMP
     c                   ENDIF                                                  RRDV1H = ' '
     c     EHASP2        IFNE      *ZERO
     c     EHASP2        IFLT      0
     c     EHASP2        MULT      -1            XXASP2
     c                   eval      fct(a) = %editc(xxASP2:'K')  + ' ' + '%'
     c                   ELSE
     c                   MOVE      EHASP2        XXASP2
     c                   eval      fct(a) = %editc(xxASP2:'K')  + ' ' + '%'
     c                   ENDIF                                                  EHASP2
     c                   ENDIF                                                  EHASP2

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition16  e


     C**************************************************************************************
     c*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C**************************************************************************************
     p SetCondition17  b
     d SetCondition17  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD1A2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPD1A2        TAM(A)
     C                   Else
     c                   Z-SUB     ELD1A2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD1E2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPD1E2        TAM(A)
     C                   Else
     c                   Z-SUB     ELD1E2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c                   MOVE      *BLANKS       XXWK25
     c     RRDV1H        IFEQ      *BLANKS
     c     @2NPER        CAT       CON(6):1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ELSE
     c     @2NPER        CAT       RRDV1H:1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ENDIF
     c     ELD1S2        IFNE      *ZERO
     c     ELD1S2        IFLT      0
     c     ELD1S2        MULT      -1            XXASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ELSE
     c                   MOVE      ELD1S2        XXASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF                                                  ELD1S2
     c                   ENDIF

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD1W2        TAM(A)
     C                   Z-SUB     EHWAD12       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWAD12 = *zeros
     c                   Z-SUB     EHD1W2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD1I2        TIN(A)
     C                   Z-SUB     EHIAD12       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHIAD12 = *zeros
     c                   Z-SUB     EHD1I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     EHD1S2        IFNE      *ZERO
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF                                                  EHD1S2
     c                   ENDIF

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition17  e


     C**************************************************************************************
     C*  LOAD 2ND DEVIATION IF APPLIED TO MANUAL PREMIUM
     C**************************************************************************************
     p SetCondition18  b
     d SetCondition18  pi
     d inflag                         1    Const

     C                   Callp     GetClientID(CLTID)
     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADV2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPADV2        TAM(A)
     C                   Else
     c                   Z-SUB     ELADV2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELEDV2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPEDV2        TAM(A)
     C                   Else
     c                   Z-SUB     ELEDV2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHWDV2        TAM(A)
     c                   Z-SUB     EHWADV2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADV2 = *zeros
     c                   Z-SUB     EHWDV2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHIDV2        TIN(A)
     c                   Z-SUB     EHIADV2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADV2 = *zeros
     c                   Z-SUB     EHIDV2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TIN(A)        XXSUBN
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition18  e


     C**************************************************************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C**************************************************************************************
     p SetCondition19  b
     d SetCondition19  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD2A2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPD2A2        TAM(A)
     C                   Else
     c                   Z-SUB     ELD2A2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD2E2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPD2E2        TAM(A)
     C                   Else
     c                   Z-SUB     ELD2E2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c     CLTID         IFNE      '025'
     c                   MOVE      *BLANKS       XXWK25
     c     RRDV2H        IFEQ      *BLANKS
     c     @2NPER        CAT       CON(18):1     XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     ELD2S2        IFNE      *ZERO
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2
     C                   ENDIF                                                  ELD2S2
     c                   eval      fct(a) = %editc(XXASP2:'K')  + ' ' + '%'
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C*
     C     ELSTFC        IFNE      *ZERO
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      ELSTFC        XXASN8
     C                   ENDIF                                                  ELSTFC
     c                   eval      fct(a) = %editc(XXASN8:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2W2        TAM(A)
     C                   Z-SUB     EHWAD22       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAD22 = *zeros
     c                   Z-SUB     EHD2W2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2I2        TIN(A)
     C                   Z-SUB     EHIAD22       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIAD22 = *zeros
     c                   Z-SUB     EHD2I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     EHD2S2        IFNE      *ZERO
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP2
     C                   ENDIF                                                  EHD2S2
     c                   eval      fct(a) = %editc(XXASP2:'K')  + ' ' + '%'
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE

     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      EHSTFC        XXASN8
     C                   ENDIF                                                  EHSTFC
     c                   eval      fct(a) = %editc(XXASN8:'K')  + ' ' + '%'
     C                   ENDIF                                                  EHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition19  e


     C**************************************************************************************
     C* WAIVER OF SUBROGATION
     C**************************************************************************************
     p SetCondition20  b
     d SetCondition20  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELEWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPEWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWWAV        TAM(A)
     C***                Z-ADD     EHIWAV        TIN(A)
     C                   Z-ADD     EHWAWAV       TAM(A)
     C                   Z-ADD     EHIAWAV       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAWAV = *zeros
     c                   Z-ADD     EHWWAV        TAM(A)
     c                   Endif
     c                   If        EHIAWAV = *zeros
     c                   Z-ADD     EHIWAV        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************

     c                   Endif                                                  inflag = 'D' or 'H'

      /Free
       If tam(a) <> 0;
          hdg(a) = AddWaiverToHdg(hdg(a));
       Endif;
      /End-Free
     P SetCondition20  e


     C**************************************************************************************
     C* ADDITIONAL LOAD
     C**************************************************************************************
     p SetCondition21  b
     d SetCondition21  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c     ELAMNP        IFNE      'Y'
     c     ELFAF         ANDEQ     'A'
     c     ELEMNP        ORNE      'Y'
     c                   callp     AddLoad
     C                   If        XTADLD <> 0
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFNE
     C                   ENDIF                                                  ELAMNP IFNE

     c**** When flag is 'H'
     c                   Else

     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C                   ADD       1             A

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition21  e


     c**************************************************************************************
     c*  LOAD SHORT RATE PENALTY IF APPLIED TO SUBJECT PREMIUM
     c**************************************************************************************
     p SetCondition22  b
     d SetCondition22  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

      *** Beg Chg ***** 02/27/24 ************************************
     c***  ELSRAM        Ifne      *ZERO
     C***                Z-ADD     ELSRAM        TAM(A)
     c                   If        inflag = 'D'
     c     MPSRAM        Ifne      *ZERO
     C                   Z-ADD     MPSRAM        TAM(A)
     c                   Endif
     c                   Else
     c     ELSRAM        Ifne      *ZERO
     C                   Z-ADD     ELSRAM        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif

     c**** When flag is 'H'
     c                   Else

     C     EHWSRA        IfNe      *ZERO
     C                   Z-ADD     EHWSRA        TAM(A)
     C                   Z-ADD     0             TIN(A)
     c                   Endif

     c                   Endif                                                  inflag = 'D' or 'H'

     P SetCondition22  e


     c**************************************************************************************
     c*  LARGE DEDUCTIBLE DISCOUNT - IF APPLIED TO SUBJECT PREMIUM
     c**************************************************************************************
     p SetCondition23  b
     d SetCondition23  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c     LDPFLG        IFEQ      'Y'
     c     LDPLV1        IFEQ      'J'
     c     LDPCT1        IFNE      *ZEROS
     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADCT        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPADCT        TAM(A)
     C                   Else
     c                   Z-SUB     ELADCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELEDCT        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPEDCT        TAM(A)
     C                   Else
     c                   Z-SUB     ELEDCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(LDpct1:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF                                                  LDPCT1 <> 0
     c                   ENDIF                                                  LDPLV1 = 'J'
     c     LDPLV2        IFEQ      'J'
     c     LDPCT2        ANDNE     *ZEROS
     c                   If        elfaf = 'A'
     c                   Z-SUB     ELADD2        TAM(A)
     c                   Else
     c                   Z-SUB     ELEDD2        TAM(A)
     c                   Endif
     c                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(LDpct2:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF                                                  LDPLV2 = 'J'
     C                   ENDIF                                                  LDPFLG = 'Y'

     c**** When flag is 'H'
     c                   Else

     c     LDPFLG        IFEQ      'Y'
     c     LDPLV1        IFEQ      'J'
     c     EHPCT1        IFNE      *ZEROS
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHWDCT        TAM(A)
     c                   Z-SUB     EHWADCT       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCT = *zeros
     c                   Z-SUB     EHWDCT        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHIDCT        TIN(A)
     c                   Z-SUB     EHIADCT       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADCT = *zeros
     c                   Z-SUB     EHIDCT        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   ADD       TIN(A)        XXSUBN
     c                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(EHpct1:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF                                                  EHPCT1 <> 0
     c                   ENDIF                                                  LDPLV1 = 'J'
     c     LDPLV2        IFEQ      'J'
     c     EHPCT2        ANDNE     *ZEROS
     c                   Z-SUB     EHWDD2        TAM(A)
     c                   ADD       TAM(A)        XXSUBT
     c                   Z-SUB     EHIDD2        TIN(A)
     c                   ADD       TIN(A)        XXSUBN
     c                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(EHpct2:'K')  + ' ' + '%'
     c                   MOVEA     TMP           HDG(A)
     c                   CLEAR                   TMP
     c                   ADD       1             A
     c                   ENDIF                                                  LDPLV2 = 'J'
     c                   ENDIF                                                  LDPFLG = 'Y'

     c                   Endif

     P SetCondition23  e


     c**************************************************************************************
     C*  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
     c**************************************************************************************
     p SetCondition24  b
     d SetCondition24  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***  ELAXP1        ADD       ELAXP2        TAM(A)
     c***                ADD       ELAXP3        TAM(A)
     C                   If        inflag = 'D'
     c     MPAXP1        ADD       MPAXP2        TAM(A)
     c                   ADD       MPAXP3        TAM(A)
     C                   Else
     c     ELAXP1        ADD       ELAXP2        TAM(A)
     c                   ADD       ELAXP3        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***  ELEXP1        ADD       ELEXP2        TAM(A)
     c***                ADD       ELEXP3        TAM(A)
     C                   If        inflag = 'D'
     c     MPEXP1        ADD       MPEXP2        TAM(A)
     c                   ADD       MPEXP3        TAM(A)
     C                   Else
     c     ELEXP1        ADD       ELEXP2        TAM(A)
     c                   ADD       ELEXP3        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c     ELEMNP        IFEQ      'Y'
     c     ELWVFL        ANDNE     'M'
     c     ELWVFL        ANDNE     'J'
     c     TAM(A)        ANDNE     0
     c                   SUB       ELEWAV        TAM(A)
     c                   ENDIF
     c                   MOVEA     CON(8)        TMP
      /free
       Select;
       When ELexm3 <> 0 and ELnoexm1 = 'Y';
          fct(a) = %editc(ELEXM2:'K')  +  '*';
       When ELexm2 <> 0 and ELnoexm1 = 'Y';
          fct(a) = %editc(ELEXM2:'K');
       When ELexm2 <> 0;
          fct(a) = %editc(ELEXM1:'K')  +  '*';
       When ELexm1 <> 0;
          fct(a) = %editc(ELEXM1:'K');
      *** Beg Add ***** 01/28/22 **************************
       When ELexm1 = 0;
          fct(a) = '*';
      *** End Add ***** 01/28/22 **************************
       Endsl;
      /end-free

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***  EHWXP1        ADD       EHWXP2        TAM(A)
     c***                ADD       EHWXP3        TAM(A)
      *** Beg Chg ***** 09/24/24 ************************************
     c***  EHWAXP1       ADD       EHIAXP1       TAM(A)
     c***                ADD       EHWAXP3       TAM(A)
     c                   If        EHWAXP1 <> *zeros or EHWAXP2 <> *zeros
     c                             or EHWAXP3 <> *zeros
     c     EHWAXP1       ADD       EHWAXP2       TAM(A)
     c                   ADD       EHWAXP3       TAM(A)
     c                   Else
     c     EHWXP1        ADD       EHWXP2        TAM(A)
     c                   ADD       EHWXP3        TAM(A)
     c                   Endif
      *** End Chg ***** 09/24/24 ************************************
      *** End Chg ***** 02/27/24 ************************************
     c                   Z-SUB     TAM(A)        XTADLD
     c     EHWMNP        IFEQ      'Y'
     c     EHWVFL        ANDNE     'M'
     c     EHWVFL        ANDNE     'J'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                SUB       EHWWAV        TAM(A)
     c                   SUB       EHWAWAV       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAWAV = *zeros
     c                   SUB       EHWWAV        TAM(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     c                   ENDIF
      *** Beg Chg ***** 02/27/24 ************************************
     c***  EHIXP1        ADD       EHIXP2        TIN(A)
     c***                ADD       EHIXP3        TIN(A)
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHIAXP1 <> *zeros or EHIAXP2 <> *zeros
     c                             or EHIAXP3 <> *zeros
      *** End Add ***** 09/24/24 ************************************
     c     EHIAXP1       ADD       EHIAXP2       TIN(A)
     c                   ADD       EHIAXP3       TIN(A)
      *** Beg Add ***** 09/24/24 ************************************
     c                   Else
     c     EHIXP1        ADD       EHIXP2        TIN(A)
     c                   ADD       EHIXP3        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
      *** End Chg ***** 02/27/24 ************************************
     c                   Z-SUB     TIN(A)        XTADLN
     c     EHIMNP        IFEQ      'Y'
     c     EHWVFL        ANDNE     'M'
     c     EHWVFL        ANDNE     'J'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                SUB       EHIWAV        TIN(A)
     c                   SUB       EHIAWAV       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHIAWAV = *zeros
     c                   SUB       EHIWAV        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     c                   ENDIF
     c                   MOVEA     CON(8)        TMP
      /free
       Select;
       When EHexm3 <> 0 and EHnoexm1 = 'Y';
          fct(a) = %editc(EHEXM2:'K')  +  '*';
       When EHexm2 <> 0 and EHnoexm1 = 'Y';
          fct(a) = %editc(EHEXM2:'K');
       When EHexm2 <> 0;
          fct(a) = %editc(EHEXM1:'K')  +  '*';
       When EHexm1 <> 0;
          fct(a) = %editc(EHEXM1:'K');
      *** Beg Add ***** 01/28/22 **************************
       When EHexm1 = 0;
          fct(a) = '*';
      *** End Add ***** 01/28/22 **************************
       Endsl;
      /end-free

     c                   Endif

     P SetCondition24  e


     c**************************************************************************************
     C*  CONTRACTOR CREDIT
     c**************************************************************************************
     p SetCondition25  b
     d SetCondition25  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADC1        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADC1        TAM(A)
     C                   Else
     C                   Z-SUB     ELADC1        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDC1        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDC1        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDC1        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVE      '0'           *IN58
     C     RRCCHD        IFNE      *BLANKS
     C                   MOVEL     RRCCHD        CON(10)
     C                   MOVEA     CON(10)       TMP
     C                   ELSE
     C                   MOVEA     CON(10)       TMP
     C                   ENDIF                                                  RRCCHD  = ' '
     C     ELASP1        IFNE      *ZERO
     c                   eval      fct(a) = %editc(ELasp1:'K')  + ' ' + '%'
     C                   Endif                                                  ELASP1 <> 0

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDC1        TAM(A)
     C***                Z-SUB     EHIDC1        TIN(A)
     C                   Z-SUB     EHWADC1       TAM(A)
     C                   Z-SUB     EHIADC1       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC1 = *zeros
     c                   Z-SUB     EHWDC1        TAM(A)
     c                   Endif
     c                   If        EHIADC1 = *zeros
     c                   Z-SUB     EHIDC1        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   MOVE      '0'           *IN58
     C     RRCCHD        IFNE      *BLANKS
     C                   MOVEL     RRCCHD        CON(10)
     C                   MOVEA     CON(10)       TMP
     C                   ELSE
     C                   MOVEA     CON(10)       TMP
     C                   ENDIF                                                  RRCCHD  = ' '
     C     EHASP1        IFNE      *ZERO
     c                   eval      fct(a) = %editc(EHasp1:'K')  + ' ' + '%'
     C                   Endif                                                  EHASP1 <> 0

     c                   Endif

     P SetCondition25  e


     c**************************************************************************************
     C*  LOAD DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     c**************************************************************************************
     p SetCondition26  b
     d SetCondition26  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADC2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPADC2        TAM(A)
     C                   Else
     c                   Z-SUB     ELADC2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELEDC2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPEDC2        TAM(A)
     C                   Else
     c                   Z-SUB     ELEDC2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     c     RRDV1H        IFEQ      *BLANKS
     c                   MOVEA     CON(6)        TMP
     c                   ELSE
     c                   MOVEA     RRDV1H        TMP
     c                   ENDIF                                                  RRDV1H = ' '
     c     ELASP2        IFNE      *ZERO
     c     ELASP2        IFLT      0
     c     ELASP2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELASP2        XXASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF                                                  ELASP2  < 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF                                                  ELASP2  <> 0

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHWDC2        TAM(A)
     c***                ADD       EHWDC2        XTADLD
     c***                Z-SUB     EHIDC2        TIN(A)
     c***                ADD       EHIDC2        XTADLN
     c                   Z-SUB     EHWADC2       TAM(A)
     c                   ADD       EHWADC2       XTADLD
     c                   Z-SUB     EHIADC2       TIN(A)
     c                   ADD       EHIADC2       XTADLN
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC2 = *zeros
     c                   Z-SUB     EHWDC2        TAM(A)
     c                   ADD       EHWDC2        XTADLD
     c                   Endif
     c                   If        EHIADC2 = *zeros
     c                   Z-SUB     EHIDC2        TIN(A)
     c                   ADD       EHIDC2        XTADLN
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c     RRDV1H        IFEQ      *BLANKS
     c                   MOVEA     CON(6)        TMP
     c                   ELSE
     c                   MOVEA     RRDV1H        TMP
     c                   ENDIF                                                  RRDV1H = ' '
     c     EHASP2        IFNE      *ZERO
     c     EHASP2        IFLT      0
     c     EHASP2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      EHASP2        XXASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF                                                  EHASP2  < 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF                                                  EHASP2  <> 0

     c                   Endif

     P SetCondition26  e


     c**************************************************************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     c**************************************************************************************
     p SetCondition27  b
     d SetCondition27  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD1A2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPD1A2        TAM(A)
     C                   Else
     c                   Z-SUB     ELD1A2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELD1E2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPD1E2        TAM(A)
     C                   Else
     C                   Z-SUB     ELD1E2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     ELD1S2        IFNE      *ZERO
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2
     c                   ENDIF                                                  ELD1S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     EHD1W2        TAM(A)
     c***                ADD       EHD1W2        XTADLD
     c***                Z-SUB     EHD1I2        TIN(A)
     c***                ADD       EHIDC2        XTADLN
     c                   Z-SUB     EHWAD12       TAM(A)
     c                   ADD       EHWAD12       XTADLD
     c                   Z-SUB     EHIAD12       TIN(A)
      *** Beg Chg ***** 09/24/24 *********************************
     c***                ADD       EHIADC2       XTADLN
     c                   ADD       EHIAD12       XTADLN
      *** End Chg ***** 09/24/24 *********************************
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAD12 = *zeros
     c                   Z-SUB     EHD1W2        TAM(A)
     c                   ADD       EHD1W2        XTADLD
     c                   Endif
     c                   If        EHIAD12 = *zeros
     c                   Z-SUB     EHD1I2        TIN(A)
     c                   ADD       EHD1I2        XTADLN
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     c                   MOVE      *BLANKS       XXWK25
     c     RRDV1H        IFEQ      *BLANKS
     c     @2NPER        CAT       CON(6):1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ELSE
     c     @2NPER        CAT       RRDV1H:1      XXWK25
     c                   MOVEA     XXWK25        TMP
     c                   ENDIF
     c     EHD1S2        IFNE      *ZERO
     c     EHD1S2        IFLT      0
     c     EHD1S2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      EHD1S2        XXASP2
     c                   ENDIF                                                  EHD1S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF

     c                   Endif

     P SetCondition27  e


     c**************************************************************************************
     C*  LOAD 2ND DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     c**************************************************************************************
     p SetCondition28  b
     d SetCondition28  pi
     d inflag                         1    Const

     C                   Callp     GetClientID(CLTID)
     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADV2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADV2        TAM(A)
     C                   Else
     C                   Z-SUB     ELADV2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDV2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDV2        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDV2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDV2        TAM(A)
     C***                ADD       EHWDV2        XTADLD
     C                   Z-SUB     EHWADV2       TAM(A)
     C                   ADD       EHWADV2       XTADLD
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADV2 = *zeros
     c                   Z-SUB     EHWDV2        TAM(A)
     c                   ADD       EHWDV2        XTADLD
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHIDV2        TIN(A)
     C***                ADD       EHIDV2        XTADLN
     C                   Z-SUB     EHIADV2       TIN(A)
     C                   ADD       EHIADV2       XTADLN
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADV2 = *zeros
     c                   Z-SUB     EHIDV2        TIN(A)
     c                   ADD       EHIDV2        XTADLN
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c                   Endif

     P SetCondition28  e


     c**************************************************************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     c**************************************************************************************
     p SetCondition29  b
     d SetCondition29  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD2A2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPD2A2        TAM(A)
     C                   Else
     c                   Z-SUB     ELD2A2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD2E2        TAM(A)
     C                   If        inflag = 'D'
     c                   Z-SUB     MPD2E2        TAM(A)
     C                   Else
     c                   Z-SUB     ELD2E2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV2H  = ' '
     C     ELD2S2        IFNE      *ZERO
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASP2
     C                   MOVE      ELD2S2        XXASP2
     C                   ENDIF
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      ELSTFC        XXASN8
     C                   ENDIF                                                  ELSTFC
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2W2        TAM(A)
     C***                ADD       EHD2W2        XTADLD
     C                   Z-SUB     EHWAD22       TAM(A)
     C                   ADD       EHWAD22       XTADLD
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAD22 = *zeros
     c                   Z-SUB     EHD2W2        TAM(A)
     c                   ADD       EHD2W2        XTADLD
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2I2        TIN(A)
     C***                ADD       EHIDV2        XTADLN
     C                   Z-SUB     EHIAD22       TIN(A)
      *** Beg Chg ***** 09/24/24 *********************************
     C***                ADD       EHIADV2       XTADLN
     C                   ADD       EHIAD22       XTADLN
      *** End Chg ***** 09/24/24 *********************************
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIAD22 = *zeros
     c                   Z-SUB     EHD2I2        TIN(A)
     c                   ADD       EHD2I2        XTADLN
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF                                                  RRDV2H  = ' '
     C     EHD2S2        IFNE      *ZERO
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2
     C                   MOVE      EHD2S2        XXASP2
     C                   ENDIF
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      EHSTFC        XXASN8
     C                   ENDIF                                                  EHSTFC
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'

     c                   Endif

     P SetCondition29  e


     c**************************************************************************************
     C* WAIVER OF SUBROGATION
     c**************************************************************************************
     p SetCondition30  b
     d SetCondition30  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELEWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPEWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWWAV        TAM(A)
     C***                Z-ADD     EHIWAV        TIN(A)
     C                   Z-ADD     EHWAWAV       TAM(A)
     C                   Z-ADD     EHIAWAV       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAWAV = *zeros
     c                   Z-ADD     EHWWAV        TAM(A)
     c                   Endif
     c                   If        EHIAWAV = *zeros
     c                   Z-ADD     EHIWAV        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************

     c                   Endif

      /Free
       If tam(a) <> 0;
          hdg(a) = AddWaiverToHdg(hdg(a));
       Endif;
      /End-Free

     P SetCondition30  e


     c**************************************************************************************
     C* ADDITIONAL LOAD
     c**************************************************************************************
     p SetCondition31  b
     d SetCondition31  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C     RRASFL        IFEQ      'E'
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   callp     AddLoad
     C                   If        XTADLD <> 0
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFNE
     C                   ENDIF                                                  ELAMNP IFNE
     C                   ENDIF                                                  RRASFL IFEQ

     c**** When flag is 'H'
     c                   Else

     C     RRASFL        IFEQ      'E'
     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRWVFL IFEQ

     c                   Endif

     P SetCondition31  e


     c**************************************************************************************
     c*  Large Deductible Discount - If Applied to Experience Modifier Prm
     c**************************************************************************************
     p SetCondition32  b
     d SetCondition32  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C     LDPLV1        IFEQ      'E'
     C     LDPCT1        IFNE      *ZEROS
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELADCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(LDpct1:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF                                                  LDPLV1 = 'E'
     C*
     C     LDPLV2        IFEQ      'E'
     C     LDPCT2        ANDNE     *ZEROS
     C                   If        elfaf = 'A'
     C                   Z-SUB     ELADD2        TAM(A)
     c                   Else
     C                   Z-SUB     ELEDD2        TAM(A)
     c                   Endif
     C                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(LDpct2:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else

     C     LDPLV1        IFEQ      'E'
     C     EHPCT1        IFNE      *ZEROS
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDCT        TAM(A)
     C                   Z-SUB     EHWADCT       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCT = *zeros
     c                   Z-SUB     EHWDCT        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHIDCT        TIN(A)
     C                   Z-SUB     EHIADCT       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADCT = *zeros
     c                   Z-SUB     EHIDCT        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(EHpct1:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF                                                  LDPLV1 = 'E'
     C*
     C     LDPLV2        IFEQ      'E'
     C     EHPCT2        ANDNE     *ZEROS
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDD2        TAM(A)
     C                   Z-SUB     EHWADD2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADD2 = *zeros
     c                   Z-SUB     EHWDD2        TAM(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHIDD2        TIN(A)
     C                   Z-SUB     EHIADD2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADD2 = *zeros
     c                   Z-SUB     EHIDD2        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(EHpct2:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c                   Endif

     P SetCondition32  e


     c**************************************************************************************
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
     c**************************************************************************************
     p SetCondition33  b
     d SetCondition33  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELADCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(3)        TMP
     C     ELDDCT        IFNE      *ZERO
     c                   eval      fct(a) = %editc(ELddct:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADD2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADD2        TAM(A)
     C                   Else
     C                   Z-SUB     ELADD2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDD2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDD2        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(31)       TMP
     C     ELDD2P        IFNE      *ZERO
     c                   eval      fct(a) = %editc(ELdd2p:'K')  + ' ' + '%'
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDCT        TAM(A)
     C***                Z-SUB     EHIDCT        TIN(A)
     C                   Z-SUB     EHWADCT       TAM(A)
     C                   Z-SUB     EHIADCT       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCT = *zeros
     c                   Z-SUB     EHWDCT        TAM(A)
     c                   Endif
     c                   If        EHIADCT = *zeros
     c                   Z-SUB     EHIDCT        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   MOVEA     CON(3)        TMP
     C     EHDDCT        IFNE      *ZERO
     c                   eval      fct(a) = %editc(EHddct:'K')  + ' ' + '%'
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDD2        TAM(A)
     C***                Z-SUB     EHIDD2        TIN(A)
     C                   Z-SUB     EHWADD2       TAM(A)
     C                   Z-SUB     EHIADD2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADD2 = *zeros
     c                   Z-SUB     EHWDD2        TAM(A)
     c                   Endif
     c                   If        EHIADD2 = *zeros
     c                   Z-SUB     EHIDD2        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     C                   MOVEA     CON(31)       TMP
     C     EHDD2P        IFNE      *ZERO
     c                   eval      fct(a) = %editc(EHdd2p:'K')  + ' ' + '%'
     C                   ENDIF

     c                   Endif

     P SetCondition33  e


     c**************************************************************************************
     C* Loss Constant
     c**************************************************************************************
     p SetCondition34  b
     d SetCondition34  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C     ELLOSS        IFNE      *ZERO
     C                   MOVEL     CON(21)       HDG(A)
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELLOSS        TAM(A)
     C                   If        inflag = 'D'
     C                   If        elfaf = 'A'
     C                   Z-ADD     MPALOS        TAM(A)
     C                   Else
     C                   Z-ADD     MPELOS        TAM(A)
     C                   Endif
     C                   Else
     C                   Z-ADD     ELLOSS        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else

     C                   MOVEL     CON(21)       HDG(A)
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWLOS        TAM(A)
     C***                Z-ADD     EHILOS        TIN(A)
     C                   Z-ADD     EHWALOS       TAM(A)
     C                   Z-ADD     EHIALOS       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWALOS <> *zeros
     c                   Z-ADD     EHWLOS        TAM(A)
     c                   Endif
     c                   If        EHIALOS <> *zeros
     c                   Z-ADD     EHILOS        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       1             A

     c                   Endif

     P SetCondition34  e


     c**************************************************************************************
     C*  LOAD SHORT RATE PENALTY IF APPLIED TO STANDARD PREMIUM
     c**************************************************************************************
     p SetCondition35  b
     d SetCondition35  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

      *** Beg Chg ***** 02/27/24 *************************************
     C***  ELSRAM        IFNE      *ZERO
     C***                Z-ADD     ELSRAM        TAM(A)
     C***                MOVEL     CON(36)       HDG(A)
     C***                ADD       1             A
     c***                ENDIF
     C                   If        inflag = 'D'
     C     MPSRAM        IFNE      *Zero
     C                   Z-ADD     MPSRAM        TAM(A)
     C                   MOVEL     CON(36)       HDG(A)
     C                   ADD       1             A
     C                   Endif
     C                   Else
     C     ELSRAM        IFNE      *Zero
     C                   Z-ADD     ELSRAM        TAM(A)
     C                   MOVEL     CON(36)       HDG(A)
     C                   ADD       1             A
     C                   Endif
     C                   Endif
      *** End Chg ***** 02/27/24 *************************************

     c**** When flag is 'H'
     c                   Else

     C     EHWSRA        IFNE      *ZERO
     C                   Z-ADD     EHWSRA        TAM(A)
     C                   Z-ADD     0             TIN(A)
     C                   MOVEL     CON(36)       HDG(A)
     C                   ADD       1             A
     c                   ENDIF

     c                   Endif

     P SetCondition35  e


     c**************************************************************************************
     C*  STANDARD PREMIUM
     c**************************************************************************************
     p SetCondition36  b
     d SetCondition36  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-ADD     ELASPR        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-ADD     MPASPR        TAM(A)
     c                   Else
     c                   Z-ADD     ELASPR        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c     ELAMNP        IFEQ      'Y'
     c     ELWVFL        ANDNE     'M'
     c     ELWVFL        ANDNE     'J'
     c     ELWVFL        ANDNE     'E'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                SUB       ELAWAV        TAM(A)
     c                   If        inflag = 'D'
     c                   SUB       MPAWAV        TAM(A)
     c                   Else
     c                   SUB       ELAWAV        TAM(A)
     c                   End
      *** End Chg ***** 02/27/24 ************************************
     c                   ENDIF
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELSTPR        TAM(A)
     c                   If        inflag = 'D'
     C                   Z-ADD     MPSTPR        TAM(A)
     c                   Else
     C                   Z-ADD     ELSTPR        TAM(A)
     c                   End
      *** End Chg ***** 02/27/24 ************************************
     C     ELEMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C     ELWVFL        ANDNE     'J'
     C     ELWVFL        ANDNE     'E'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                SUB       ELEWAV        TAM(A)
     c                   If        inflag = 'D'
     C                   SUB       MPEWAV        TAM(A)
     c                   Else
     C                   SUB       ELEWAV        TAM(A)
     c                   End
      *** End Chg ***** 02/27/24 ************************************
     C                   ENDIF
     c                   Endif

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-ADD     EHWSPR        TAM(A)
     c***                Z-ADD     EHISPR        TIN(A)
     c                   Z-ADD     EHWASTD       TAM(A)
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWASTD = *zeros
     c                   Z-ADD     EHWSPR        TAM(A)
      *** Beg Add ***** 10/14/24 ************************************
       Else;
       //*** Beg Chg ***** 02/06/25 ******************************
       //*** xxSt = RatingSt(ELCO#:ELRTCD);
       //*** If ELEMNP = 'Y' and xxSt ='NJ';
       //***    If ELPror <> 0 and ELPror <> 1;
       //***       Eval TAM(A) += %Int((EHwbal / ELPror) + 0.5);
       //***    Else;
       //***       Eval TAM(A) += EHwbal;
       //***    Endif;
       //*** Else;
       //***    Eval TAM(A) += EHwbal;
       //*** Endif;
          If EHPror <> 0 and EHPror <> 1;
             Eval TAM(A) += %Int((EHwbal / EHpror) + 0.5);
          Else;
             Eval TAM(A) += EHwbal;
          Endif;
       //*** End Chg ***** 02/06/25 ******************************
      *** End Add ***** 10/14/24 ************************************
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Add ***** 08/27/24 ************************************
      *** Beg Del ***** 10/14/24 ************************************
     c***                add       EHWBAL        TAM(A)
      *** End Del ***** 10/14/24 ************************************
      *** End Add ***** 08/27/24 ************************************
     c                   Z-ADD     EHIASTD       TIN(A)
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIASTD = *zeros
     c                   Z-ADD     EHISPR        TIN(A)
      *** Beg Add ***** 10/14/24 ************************************
       Else;
       //*** Beg Chg ***** 02/06/25 ******************************
       //*** xxSt = RatingSt(ELCO#:ELRTCD);
       //*** If ELEMNP = 'Y' and xxSt ='NJ';
       //***    If ELPror <> 0 and ELPror <> 1;
       //***       Eval TIN(A) += %Int((EHibal / ELPror) + 0.5);
       //***    Else;
       //***       Eval TIN(A) += EHibal;
       //***    Endif;
       //*** Else;
       //***    Eval TIN(A) += EHibal;
       //*** Endif;
      * inforce premium is an annualized figure so no annualizing is needed.
     c                   add       EHIBAL        TIN(A)
       //*** End Chg ***** 02/06/25 ******************************
      *** End Add ***** 10/14/24 ************************************
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Add ***** 08/27/24 ************************************
      *** Beg Del ***** 10/14/24 ************************************
     c***                add       EHIBAL        TIN(A)
      *** End Del ***** 10/14/24 ************************************
      *** End Add ***** 08/27/24 ************************************
      *** End Chg ***** 02/27/24 ************************************
     c     EHWMNP        IFEQ      'Y'
     c     EHWVFL        ANDNE     'M'
     c     EHWVFL        ANDNE     'J'
     c     EHWVFL        ANDNE     'E'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                SUB       EHWWAV        TAM(A)
     c                   SUB       EHWAWAV       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAWAV = *zeros
     c                   SUB       EHWWAV        TAM(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     c                   ENDIF
     c     EHIMNP        IFEQ      'Y'
     c     EHWVFL        ANDNE     'M'
     c     EHWVFL        ANDNE     'J'
     c     EHWVFL        ANDNE     'E'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                SUB       EHIWAV        TIN(A)
     c                   SUB       EHIAWAV       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHIAWAV = *zeros
     c                   SUB       EHIWAV        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     c                   ENDIF

     c                   Endif

     P SetCondition36  e


     c**************************************************************************************
     C*  LOAD DEVIATION IF APPLIED TO STANDARD PREMIUM
     c**************************************************************************************
     p SetCondition37  b
     d SetCondition37  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADC2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPADC2        TAM(A)
     c                   Else
     c                   Z-SUB     ELADC2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDC2        TAM(A)
     c                   If        inflag = 'D'
     C                   Z-SUB     MPEDC2        TAM(A)
     c                   Else
     C                   Z-SUB     ELEDC2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     ELASP2        IFNE      *ZERO
     C     ELASP2        IFLT      0
     C     ELASP2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2
     C                   ENDIF                                                  ELASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 IFLT

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDC2        TAM(A)
     C***                Z-SUB     EHIDC2        TIN(A)
     C                   Z-SUB     EHWADC2       TAM(A)
     C                   Z-SUB     EHIADC2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC2 = *zeros
     c                   Z-SUB     EHWDC2        TAM(A)
     c                   Endif
     c                   If        EHIADC2 = *zeros
     c                   Z-SUB     EHIDC2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     EHASP2        IFNE      *ZERO
     C     EHASP2        IFLT      0
     C     EHASP2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHASP2        XXASP2
     C                   ENDIF                                                  EHASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  EHASP2 IFLT

     c                   Endif

     P SetCondition37  e


     c**************************************************************************************
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     c**************************************************************************************
     p SetCondition38  b
     d SetCondition38  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELD1A2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPD1A2        TAM(A)
     C                   Else
     C                   Z-SUB     ELD1A2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELD1E2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPD1E2        TAM(A)
     C                   Else
     C                   Z-SUB     ELD1E2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     ELD1S2        IFNE      *ZERO
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2
     C                   ENDIF                                                  ELD1S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD1W2        TAM(A)
     C***                Z-SUB     EHD1I2        TIN(A)
     C                   Z-SUB     EHWAD12       TAM(A)
     C                   Z-SUB     EHIAD12       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWAD12 = *zeros
     c                   Z-SUB     EHD1W2        TAM(A)
     c                   Endif
     c                   If        EHIAD12 = *zeros
     c                   Z-SUB     EHD1I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     EHD1S2        IFNE      *ZERO
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP2
     C                   ENDIF                                                  EHD1S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF

     c                   Endif

     P SetCondition38  e


     c**************************************************************************************
     C*  LOAD 2ND DEVIATION IF APPLIED TO STANDARD PREMIUM
     c**************************************************************************************
     p SetCondition39  b
     d SetCondition39  pi
     d inflag                         1    Const

     C                   Callp     GetClientID(CLTID)
     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADV2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADV2        TAM(A)
     C                   Else
     C                   Z-SUB     ELADV2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDV2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDV2        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDV2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c**** When flag is 'H'
     c                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDV2        TAM(A)
     C                   Z-SUB     EHWADV2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADV2 = *zeros
     c                   Z-SUB     EHWDV2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHIDV2        TIN(A)
     C                   Z-SUB     EHIADV2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADV2 = *zeros
     c                   Z-SUB     EHIDV2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c                   Endif

     P SetCondition39  e


     c**************************************************************************************
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     c**************************************************************************************
     p SetCondition40  b
     d SetCondition40  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELD2A2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPD2A2        TAM(A)
     C                   Else
     C                   Z-SUB     ELD2A2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELD2E2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPD2E2        TAM(A)
     C                   Else
     C                   Z-SUB     ELD2E2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     ELD2S2        IFNE      *ZERO
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2
     C                   ENDIF                                                  ELD2S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELD2S2 IFLT
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      ELSTFC        XXASN8
     C                   ENDIF                                                  ELSTFC
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELSTFC IFLT
     C                   ENDIF                                                  CLTID<> '025'

     c**** When flag is 'H'
     C                   ELSE

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2W2        TAM(A)
     C                   Z-SUB     EHWAD22       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAD22 = *zeros
     c                   Z-SUB     EHD2W2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2I2        TIN(A)
     C                   Z-SUB     EHIAD22       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIAD22 = *zeros
     c                   Z-SUB     EHD2I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     EHD2S2        IFNE      *ZERO
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP2
     C                   ENDIF                                                  EHD2S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  EHD2S2 IFLT
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      EHSTFC        XXASN8
     C                   ENDIF                                                  EHSTFC
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  EHSTFC IFLT
     C                   ENDIF                                                  CLTID<> '025'

     c                   Endif

     P SetCondition40  e


     c**************************************************************************************
     c* WAIVER OF SUBROGATION
     c**************************************************************************************
     p SetCondition41  b
     d SetCondition41  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELEWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPEWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif

     c**** When flag is 'H'
     C                   Else

      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWWAV        TAM(A)
     C***                Z-ADD     EHIWAV        TIN(A)
     C                   Z-ADD     EHWAWAV       TAM(A)
     C                   Z-ADD     EHIAWAV       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAWAV = *zeros
     c                   Z-ADD     EHWWAV        TAM(A)
     c                   Endif
     c                   If        EHIAWAV = *zeros
     c                   Z-ADD     EHIWAV        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************

     c                   Endif

      /Free
       If tam(a) <> 0;
          hdg(a) = AddWaiverToHdg(hdg(a));
       Endif;
      /End-Free

     P SetCondition41  e


     c**************************************************************************************
     c* ADDITIONAL LOAD
     c**************************************************************************************
     p SetCondition42  b
     d SetCondition42  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'

     C     RRASFL        IFEQ      'S'
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   callp     AddLoad
     C                   If        XTADLD <> 0
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFNE
     C                   ENDIF                                                  ELAMNP IFNE
     C                   ENDIF                                                  RRASFL IFEQ

     c**** When flag is 'H'
     c                   Else

     C     RRASFL        IFEQ      'S'
     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRWVFL IFEQ

     c                   Endif

     P SetCondition42  e


     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition43  b
     d SetCondition43  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C     LDPLV1        IFEQ      'S'
     C     LDPCT1        IFNE      *ZEROS
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELADCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(LDpct1:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF                                                  LDPLV1 = 'S'
     C*
     C     LDPLV2        IFEQ      'S'
     C     LDPCT2        ANDNE     *ZEROS
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADD2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADD2        TAM(A)
     C                   Else
     C                   Z-SUB     ELADD2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDD2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDD2        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(LDpct2:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
     C     LDPLV1        IFEQ      'S'
     C     EHPCT1        IFNE      *ZEROS
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDCT        TAM(A)
     C***                Z-SUB     EHIDCT        TIN(A)
     C                   Z-SUB     EHWADCT       TAM(A)
     C                   Z-SUB     EHIADCT       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCT = *zeros
     c                   Z-SUB     EHWDCT        TAM(A)
     c                   Endif
     c                   If        EHIADCT = *zeros
     c                   Z-SUB     EHIDCT        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(EHpct1:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF                                                  LDPLV1 = 'S'
     C*
     C     LDPLV2        IFEQ      'S'
     C     EHPCT2        ANDNE     *ZEROS
     C*
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDD2        TAM(A)
     C***                Z-SUB     EHIDD2        TIN(A)
     C                   Z-SUB     EHWADD2       TAM(A)
     C                   Z-SUB     EHIADD2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADD2 = *zeros
     c                   Z-SUB     EHWDD2        TAM(A)
     c                   Endif
     c                   If        EHIADD2 = *zeros
     c                   Z-SUB     EHIDD2        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     C                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(EHpct2:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c                   Endif

     P SetCondition43  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition44  b
     d SetCondition44  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADC3        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADC3        TAM(A)
     C                   Else
     C                   Z-SUB     ELADC3        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDC3        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDC3        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDC3        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     RRDSHD        IFEQ      *BLANKS
     C                   MOVEA     CON(12)       TMP
     C                   ELSE
     C                   MOVEA     RRDSHD        TMP
     C                   ENDIF                                                  RRDSHD IFEQ
     C     ELASP3        IFNE      *ZERO
     c                   eval      fct(a) = %editc(ELASP3:'K')  + ' ' + '%'
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDC3        TAM(A)
     C***                Z-SUB     EHIDC3        TIN(A)
     C                   Z-SUB     EHWADC3       TAM(A)
     C                   Z-SUB     EHIADC3       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC3 = *zeros
     c                   Z-SUB     EHWDC3        TAM(A)
     c                   Endif
     c                   If        EHIADC3 = *zeros
     c                   Z-SUB     EHIDC3        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C     RRDSHD        IFEQ      *BLANKS
     C                   MOVEA     CON(12)       TMP
     C                   ELSE
     C                   MOVEA     RRDSHD        TMP
     C                   ENDIF                                                  RRDSHD IFEQ
     C     EHASP3        IFNE      *ZERO
     c                   eval      fct(a) = %editc(EHASP3:'K')  + ' ' + '%'
     C                   ENDIF

     c                   Endif

     P SetCondition44  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition45  b
     d SetCondition45  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADC2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADC2        TAM(A)
     C                   Else
     C                   Z-SUB     ELADC2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDC2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDC2        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDC2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     ELASP2        IFNE      *ZERO
     C     ELASP2        IFLT      0
     C     ELASP2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2
     C                   ENDIF                                                  ELASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDC2        TAM(A)
     C***                Z-SUB     EHIDC2        TIN(A)
     C                   Z-SUB     EHWADC2       TAM(A)
     C                   Z-SUB     EHIADC2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC2 = *zeros
     c                   Z-SUB     EHWDC2        TAM(A)
     c                   Endif
     c                   If        EHIADC2 = *zeros
     c                   Z-SUB     EHIDC2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   ELSE
     C                   MOVEA     RRDV1H        TMP
     C                   ENDIF
     C     EHASP2        IFNE      *ZERO
     C     EHASP2        IFLT      0
     C     EHASP2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHASP2        XXASP2
     C                   ENDIF                                                  EHASP2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     c                   ENDIF

     c                   Endif

     P SetCondition45  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition46  b
     d SetCondition46  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD1A2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPD1A2        TAM(A)
     c                   Else
     c                   Z-SUB     ELD1A2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD1E2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPD1E2        TAM(A)
     c                   Else
     c                   Z-SUB     ELD1E2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     ELD1S2        IFNE      *ZERO
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2
     C                   ENDIF                                                  ELD1S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD1W2        TAM(A)
     C***                Z-SUB     EHD1I2        TIN(A)
     C                   Z-SUB     EHWAD12       TAM(A)
     C                   Z-SUB     EHIAD12       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWAD12 = *zeros
     c                   Z-SUB     EHD1W2        TAM(A)
     c                   Endif
     c                   If        EHIAD12 = *zeros
     c                   Z-SUB     EHD1I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(6):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV1H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     EHD1S2        IFNE      *ZERO
     C     EHD1S2        IFLT      0
     C     EHD1S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHD1S2        XXASP2
     C                   ENDIF                                                  EHD1S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF

     c                   Endif

     P SetCondition46  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition47  b
     d SetCondition47  pi
     d inflag                         1    Const

     C                   Callp     GetClientID(CLTID)
     c                   If        inflag = 'D' or inflag = 'U'
     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELADV2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPADV2        TAM(A)
     c                   Else
     c                   Z-SUB     ELADV2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELEDV2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPEDV2        TAM(A)
     c                   Else
     c                   Z-SUB     ELEDV2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDV2        TAM(A)
     C                   Z-SUB     EHWADV2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADV2 = *zeros
     c                   Z-SUB     EHWDV2        TAM(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHIDV2        TIN(A)
     C                   Z-SUB     EHIADV2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIADV2 = *zeros
     c                   Z-SUB     EHIDV2        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(37)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELstFc        IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELstFc:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   If        RRdv2H = *Blanks
     C                   MoveA     con(63)       tmp
     C                   Else
     C                   MoveA     RRdv2H        tmp
     C                   EndIf
     C*
     C     ELdev2F       IfNE      *Zeros
     c                   eval      fct(a) = %editc(ELdev2F:'K')
     C                   Endif                                                  ELSTFC  NE 0
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(18)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF                                                  RRDV2H = ' '
     c     ELDEV2        IFNE      *ZERO
     c     ELDEV2        IFLT      0
     c     ELDEV2        MULT      -1            XXASP2
     c                   ELSE
     c                   MOVE      ELDEV2        XXASP2
     C                   ENDIF                                                  ELASP2 LT 0
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELASP2 NE 0
     C                   EndSl

     c                   Endif

     P SetCondition47  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition48  b
     d SetCondition48  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     c                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD2A2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPD2A2        TAM(A)
     c                   Else
     c                   Z-SUB     ELD2A2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     c***                Z-SUB     ELD2E2        TAM(A)
     c                   If        inflag = 'D'
     c                   Z-SUB     MPD2E2        TAM(A)
     c                   Else
     c                   Z-SUB     ELD2E2        TAM(A)
     c                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     ELD2S2        IFNE      *ZERO
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2
     C                   ENDIF                                                  ELD2S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     ELSTFC        IFNE      *ZERO
     C     ELSTFC        IFLT      0
     C     ELSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      ELSTFC        XXASN8
     C                   ENDIF                                                  ELSTFC
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  ELSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2W2        TAM(A)
     C                   Z-SUB     EHWAD22       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAD22 = *zeros
     c                   Z-SUB     EHD2W2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TAM(A)        XXSUBT
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHD2I2        TIN(A)
     C                   Z-SUB     EHIAD22       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIAD22 = *zeros
     c                   Z-SUB     EHD2I2        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       TIN(A)        XXSUBN
     C     CLTID         IFNE      '025'
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(18):1     XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C     @2NPER        CAT       RRDV2H:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C     EHD2S2        IFNE      *ZERO
     C     EHD2S2        IFLT      0
     C     EHD2S2        MULT      -1            XXASP2
     C                   ELSE
     C                   MOVE      EHD2S2        XXASP2
     C                   ENDIF                                                  EHD2S2
     c                   eval      fct(a) = %editc(xxAsp2:'K')  + ' ' + '%'
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(37)       TMP
     C                   ELSE
     C                   MOVEA     RRDV2H        TMP
     C                   ENDIF
     C     EHSTFC        IFNE      *ZERO
     C     EHSTFC        IFLT      0
     C     EHSTFC        MULT      -1            XXASN8            8 6
     C                   ELSE
     C                   MOVE      EHSTFC        XXASN8
     C                   ENDIF                                                  EHSTFC
     c                   eval      fct(a) = %editc(xxAsn8:'K')  + ' ' + '%'
     C                   ENDIF                                                  RHSTFC <> 0
     C                   ENDIF                                                  CLTID<> '025'

     c                   Endif

     P SetCondition48  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition49  b
     d SetCondition49  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELEWAV        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPEWAV        TAM(A)
     C                   Else
     C                   Z-ADD     ELEWAV        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWWAV        TAM(A)
     C***                Z-ADD     EHIWAV        TIN(A)
     C                   Z-ADD     EHWAWAV       TAM(A)
     C                   Z-ADD     EHIAWAV       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWAWAV = *zeros
     c                   Z-ADD     EHWWAV        TAM(A)
     c                   Endif
     c                   If        EHIAWAV = *zeros
     c                   Z-ADD     EHIWAV        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 ************************************

     c                   Endif

      /Free
       If tam(a) <> 0;
          hdg(a) = AddWaiverToHdg(hdg(a));
       Endif;
      /End-Free

     P SetCondition49  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition50  b
     d SetCondition50  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C     RRASFL        IFEQ      'D'
     C     ELAMNP        IFNE      'Y'
     C     ELFAF         ANDEQ     'A'
     C     ELEMNP        ORNE      'Y'
     C                   callp     AddLoad
     C                   If        XTADLD <> 0
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVEL     CON(9)        HDG(A)
     C                   ADD       1             A
     C                   ENDIF                                                  XTADLD IFNE
     C                   ENDIF                                                  ELAMNP IFNE
     C                   ENDIF                                                  RRASFL IFEQ

     c**** When flag is 'H'
     c                   Else
     C     RRASFL        IFEQ      'D'
     C                   MOVEL     CON(9)        HDG(A)
     C                   Z-ADD     EHWADL        TAM(A)
     C                   Z-ADD     EHIADL        TIN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRWVFL IFEQ

     c                   Endif

     P SetCondition50  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition51  b
     d SetCondition51  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C     LDPLV1        IFEQ      'D'
     C     LDPCT1        IFNE      *ZEROS
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELADCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDCT        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDCT        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDCT        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(LDpct1:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF                                                  LDPLV1 = 'D'
     C*
     C     LDPLV2        IFEQ      'D'
     C     LDPCT2        ANDNE     *ZEROS
     C*
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELADD2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPADD2        TAM(A)
     C                   Else
     C                   Z-SUB     ELADD2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     ELEDD2        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-SUB     MPEDD2        TAM(A)
     C                   Else
     C                   Z-SUB     ELEDD2        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(LDpct2:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
     C     LDPLV1        IFEQ      'D'
     C     EHPCT1        IFNE      *ZEROS
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDCT        TAM(A)
     C***                Z-SUB     EHIDCT        TIN(A)
     C                   Z-SUB     EHWADCT       TAM(A)
     C                   Z-SUB     EHIADCT       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADCT = *zeros
     c                   Z-SUB     EHWDCT        TAM(A)
     c                   Endif
     c                   If        EHIADCT = *zeros
     c                   Z-SUB     EHIDCT        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   MOVEA     CON(3)        TMP
     c                   eval      fct(a) = %editc(EHpct1:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF                                                  LDPLV1 = 'D'
     C*
     C     LDPLV2        IFEQ      'D'
     C     EHPCT2        ANDNE     *ZEROS
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-SUB     EHWDD2        TAM(A)
     C***                Z-SUB     EHIDD2        TIN(A)
     C                   Z-SUB     EHWADD2       TAM(A)
     C                   Z-SUB     EHIADD2       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADD2 = *zeros
     c                   Z-SUB     EHWDD2        TAM(A)
     c                   Endif
     c                   If        EHIADD2 = *zeros
     c                   Z-SUB     EHIDD2        TIN(A)
     c                   Endif
     *** End Add ***** 09/24/24 *********************************
     C                   MOVEA     CON(31)       TMP
     c                   eval      fct(a) = %editc(Ehpct2:'K')  + ' ' + '%'
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF

     c                   Endif

     P SetCondition51  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition52  b
     d SetCondition52  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***  ELRPA1        ADD       ELRPA2        XXWRP            13 2
     C***                ADD       ELRPA3        XXWRP
     C                   If        inflag = 'D'
     C     MPRPA1        ADD       MPRPA2        XXWRP            13 2
     C                   ADD       MPRPA3        XXWRP
     C                   Else
     C     ELRPA1        ADD       ELRPA2        XXWRP            13 2
     C                   ADD       ELRPA3        XXWRP
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***  ELRPE1        ADD       ELRPE2        XXWRP            13 2
     C***                ADD       ELRPE3        XXWRP
     C                   If        inflag = 'D'
     C     MPRPE1        ADD       MPRPE2        XXWRP            13 2
     C                   ADD       MPRPE3        XXWRP
     C                   Else
     C     ELRPE1        ADD       ELRPE2        XXWRP            13 2
     C                   ADD       ELRPE3        XXWRP
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     XXWRP         IFNE      *ZERO
     C                   MOVEL     CON(23)       HDG(A)
     C                   Z-ADD     XXWRP         TAM(A)
      /free
       Select;
       When ELARP2 <> 0;
          fct(a) = %editc(ELARP1:'K')  +  '*';
       When ELARP1 <> 0;
          fct(a) = %editc(ELARP1:'K');
       Endsl;
      /end-free
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***  EHWRP1        ADD       EHWRP2        XXWRP            13 2
     C***                ADD       EHWRP3        XXWRP
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHWARP1 <> *zeros or EHWARP2 <> *zeros
     c                             or EHWARP3 <> *zeros
      *** End Add ***** 09/24/24 ************************************
     C     EHWARP1       ADD       EHWARP2       XXWRP            13 2
     C                   ADD       EHWARP3       XXWRP
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   Else
     c     EHWRP1        ADD       EHWRP2        XXWRP            13 2
     c                   ADD       EHWRP3        XXWRP
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     C     XXWRP         IFNE      *ZERO
      *** Beg Chg ***** 02/27/24 ************************************
     C***  EHIRP1        ADD       EHIRP2        XXIRP            13 2
     C***                ADD       EHIRP3        XXIRP
      *** Beg Add ***** 09/24/24 ************************************
     c                   If        EHIARP1 <> *zeros or EHIARP2 <> *zeros
     c                             or EHIARP3 <> *zeros
      *** End Add ***** 09/24/24 ************************************
     C     EHIARP1       ADD       EHIARP2       XXIRP            13 2
     C                   ADD       EHIARP3       XXIRP
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c                   Else
     C     EHIRP1        ADD       EHIRP2        XXIRP            13 2
     C                   ADD       EHIRP3        XXIRP
     c                   Endif
      *** End Add ***** 09/24/24 ************************************
     C                   MOVEL     CON(23)       HDG(A)
     C                   Z-ADD     XXWRP         TAM(A)
     C                   Z-ADD     XXIRP         TIN(A)
      /free
       Select;
       When EHARP2 <> 0;
          fct(a) = %editc(EHARP1:'K')  +  '*';
       When EHARP1 <> 0;
          fct(a) = %editc(EHARP1:'K');
       Endsl;
      /end-free
     C                   ADD       1             A
     C                   ENDIF

     c                   Endif

     P SetCondition52  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition53  b
     d SetCondition53  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELANMP        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPANMP        TAM(A)
     C                   Else
     C                   Z-ADD     ELANMP        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELENMP        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPENMP        TAM(A)
     C                   Else
     C                   Z-ADD     ELENMP        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWNMP        TAM(A)
     C***                Z-ADD     EHINMP        TIN(A)
     C                   Z-ADD     EHWANORM      TAM(A)
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWANORM = *zeros
     c                   Z-ADD     EHWNMP        TAM(A)
      *** Beg Add ***** 10/14/24 ************************************
       Else;
       //*** Beg Chg ***** 02/06/25 ******************************
       //*** xxSt = RatingSt(ELCO#:ELRTCD);
       //*** If ELEMNP = 'Y' and xxSt ='NJ';
       //***    If ELPror <> 0 and ELPror <> 1;
       //***       Eval TAM(A) += %Int((EHwbal / ELPror) + 0.5);
       //***    Else;
       //***       Eval TAM(A) += EHwbal;
       //***    Endif;
       //*** Else;
       //***    Eval TAM(A) += EHwbal;
       //*** Endif;
          If EHPror <> 0 and EHPror <> 1;
             Eval TAM(A) += %Int((EHwbal / EHpror) + 0.5);
          Else;
             Eval TAM(A) += EHwbal;
          Endif;
       //*** End Chg ***** 02/06/25 ******************************
      *** End Add ***** 10/14/24 ************************************
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Add ***** 08/27/24 ************************************
      *** Beg Del ***** 10/14/24 ************************************
     c***                add       EHWBAL        TAM(A)
      *** End Del ***** 10/14/24 ************************************
      *** End Add ***** 08/27/24 ************************************
     C                   Z-ADD     EHIANORM      TIN(A)
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHIANORM = *zeros
     c                   Z-ADD     EHINMP        TIN(A)
      *** Beg Add ***** 10/14/24 ************************************
       Else;
       //*** Beg Chg ***** 02/06/25 ******************************
       //*** xxSt = RatingSt(ELCO#:ELRTCD);
       //*** If ELEMNP = 'Y' and xxSt ='NJ';
       //***    If ELPror <> 0 and ELPror <> 1;
       //***       Eval TIN(A) += %Int((EHibal / ELPror) + 0.5);
       //***    Else;
       //***       Eval TIN(A) += EHibal;
       //***    Endif;
       //*** Else;
       //***    Eval TIN(A) += EHibal;
       //*** Endif;
      * inforce premium is an annualized figure so no annualizing is needed.
     c                   add       EHIBAL        TIN(A)
       //*** End Chg ***** 02/06/25 ******************************
      *** End Add ***** 10/14/24 ************************************
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Add ***** 08/27/24 ************************************
      *** Beg Del ***** 10/14/24 ************************************
     c***                add       EHIBAL        TIN(A)
      *** End Del ***** 10/14/24 ************************************
      *** End Add ***** 08/27/24 ************************************
      *** End Chg ***** 02/27/24 ************************************

     c                   Endif
      /free
       If clientID = '111';
          TAM(A) = TAM(A) - xxDvnd;
          TIN(A) = TIN(A) - xxDvnd;
       Endif;
      /end-free

     P SetCondition53  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition54  b
     d SetCondition54  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   Z-ADD     ELHXMN        TAM(A)
     C     TAM(A)        IFNE      *ZEROS
     C                   MOVEL     CON(35)       HDG(A)
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
     C                   MOVEL     CON(35)       HDG(A)
     C                   Z-ADD     EHWHXM        TAM(A)
     C                   Z-ADD     EHIHXM        TIN(A)
     C                   ADD       1             A

     c                   Endif

     P SetCondition54  e


     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition55  b
     d SetCondition55  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAARC        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAARC        TAM(A)
     C                   Else
     C                   Z-ADD     ELAARC        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELEARC        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPEARC        TAM(A)
     C                   Else
     C                   Z-ADD     ELEARC        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C     TAM(A)        IFNE      *ZEROS
     C                   MOVEL     CON(34)       HDG(A)
     C                   ADD       1             A
     C                   ENDIF

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***  EHWARC        IFNE      *ZEROS
     C***                MOVEL     CON(34)       HDG(A)
     C***                Z-ADD     EHWARC        TAM(A)
     C***                Z-ADD     EHIARC        TIN(A)
     C     EHWAARC       IFNE      *ZEROS
     C                   MOVEL     CON(34)       HDG(A)
     C                   Z-ADD     EHWAARC       TAM(A)
     C                   Z-ADD     EHIAARC       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
     C                   ADD       1             A
     C                   ENDIF
      *** Beg Add ***** 09/24/24 *********************************
     c     EHWARC        IFNE      *ZEROS
     c     EHWAARC       ANDEQ     *ZEROS
     c                   MOVEL     CON(34)       HDG(A)
     c                   Z-ADD     EHWARC        TAM(A)
     c                   Z-ADD     EHIARC        TIN(A)
     C                   ADD       1             A
     C                   ENDIF
      *** End Add ***** 09/24/24 *********************************

     c                   Endif

     P SetCondition55  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition56  b
     d SetCondition56  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C     ELFAF         IFNE      'A'
     C                   Z-ADD     ELPRTF        ELPRTD
     C                   ENDIF                                                  ELFAF IFNE
     C                   Z-ADD     ELPRTD        TAM(A)

     c**** When flag is 'H'
     c                   Else
     C     EHFAF         IFNE      'A'
     C                   Z-ADD     EHPRTF        EHPRTD
     C                   ENDIF                                                  EHFAF IFNE
      *** Beg Chg ***** 02/27/24 ************************************
     C***  EHWAPR        IFNE      *ZERO
     C     EHWAANN       IFNE      *ZERO
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
     c     EHWAPR        ORNE      *ZERO
      *** End Add ***** 09/24/24 ************************************
     C                   Z-ADD     EHPRTD        TAM(A)
     C                   ENDIF
      *** Beg Chg ***** 02/27/24 ************************************
     C***  EHIAPR        IFNE      *ZERO
     C     EHIAANN       IFNE      *ZERO
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c     EHIAPR        ORNE      *ZERO
      *** End Add ***** 09/24/24 *********************************
     C                   Z-ADD     EHPRTD        TIN(A)
     C                   ENDIF

     c                   Endif

     P SetCondition56  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition57  b
     d SetCondition57  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C     ELUPSA        IFNE      *ZEROS
     C                   Z-ADD     ELUPSA        TAM(A)
     c                   If        not isContribution
     c                   MoveL     CON(43)       HDG(A)
     c                   Else
     c                   MoveL     CON(48)       HDG(A)
     c                   Endif
     C                   ADD       1             A
     C                   ENDIF                                                  ELAUSA

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***  EHWUSA        IFNE      *ZERO
     C     EHWAUSA       IFNE      *ZERO
      *** End Chg ***** 02/27/24 ************************************
     c                   If        not isContribution
     c                   MoveL     CON(43)       HDG(A)
     c                   Else
     c                   MoveL     CON(48)       HDG(A)
     c                   Endif
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWUSA        TAM(A)
     C                   Z-ADD     EHWAUSA       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
     C                   ADD       1             A
     C                   ENDIF                                                  EHWUSA
      *** Beg Add ***** 09/24/24 *********************************
     c     EHWUSA        IFNE      *ZERO
     c     EHWAUSA       ANDEQ     *ZERO
     c                   If        not isContribution
     c                   MoveL     CON(43)       HDG(A)
     c                   Else
     c                   MoveL     CON(48)       HDG(A)
     c                   Endif
     C                   Z-ADD     EHWUSA        TAM(A)
     C                   ADD       1             A
     C                   ENDIF                                                  EHWUSA
      *** End Add ***** 09/24/24 *********************************

     c                   Endif

     P SetCondition57  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition58  b
     d SetCondition58  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
     C                   Z-ADD     ELATER        TAM(A)
     c                   Else
     C                   Z-ADD     ELETER        TAM(A)
     c                   Endif
     C     TAM(A)        IFNE      *ZERO
     C*
     C* Set the terrorism heading fields to optional or default heading.
     C                   MOVEL     CON(22)       HDG(A)
     C     RRTERH        IFNE      *BLANKS
     C                   MOVEL     RRTERH        HDG(A)
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF                                                  TAM(A) IFNE

     c**** When flag is 'H'
     c                   Else
     C     EHWTER        IFNE      *ZERO
     C*
     C                   MOVEL     CON(22)       HDG(A)

     C* Set the terrorism heading fields to optional or default heading.
     C     RRTERH        IFNE      *BLANKS
     C                   MOVEL     RRTERH        HDG(A)
     C                   ENDIF
     C                   Z-ADD     EHWTER        TAM(A)
     C                   Z-ADD     EHITER        TIN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  EHWTER IFNE

     c                   Endif

     P SetCondition58  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition59  b
     d SetCondition59  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
     C                   Z-ADD     ELADTR        TAM(A)
     c                   Else
     C                   Z-ADD     ELEDTR        TAM(A)
     c                   Endif
     C     TAM(A)        IFNE      *ZERO
     C*
     C* Set the terrorism heading fields to optional or default heading.
     C                   MOVEL     CON(25)       HDG(A)
     C     RRDTRH        IFNE      *BLANKS
     C                   MOVEL     RRDTRH        HDG(A)
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF                                                  TAM(A) IFNE

     c**** When flag is 'H'
     c                   Else
     C     EHWDTR        IFNE      *ZERO
     C                   MOVEL     CON(25)       HDG(A)
     C* Set the terrorism heading fields to optional or default heading.
     C     RRDTRH        IFNE      *BLANKS
     C                   MOVEL     RRDTRH        HDG(A)
     C                   ENDIF
     C                   Z-ADD     EHWDTR        TAM(A)
     C                   Z-ADD     EHIDTR        TIN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  EHWTER IFNE

     c                   Endif

     P SetCondition59  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition60  b
     d SetCondition60  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C     ELFAF         IFEQ      'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAAPR        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAAPR        TAM(A)
     C                   Else
     C                   Endif
     C                   Z-ADD     ELAAPR        TAM(A)
      *** End Chg ***** 02/27/24 ************************************
     C                   ELSE
     C     ELEMNP        IFEQ      'Y'
     C     ELPRTD        OREQ      *ZERO
     C                   Z-ADD     ELENMP        TAM(A)
     C                   ADD       ELETER        TAM(A)
     C                   ADD       ELEDTR        TAM(A)
     C                   ADD       ELEARC        TAM(A)
     C     ELEMNP        IFEQ      'Y'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELANPR        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPANPR        TAM(A)
     C                   Else
     C                   Z-ADD     ELANPR        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     C                   ENDIF
     C                   ELSE
     C     ELENMP        ADD       ELPRTF        TAM(A)
     C                   ADD       ELETER        TAM(A)
     C                   ADD       ELEDTR        TAM(A)
     C                   ADD       ELEARC        TAM(A)
     C                   ADD       ELEPF         TAM(A)
     C                   ENDIF                                                  ELEMNP=Y
     C                   ENDIF                                                  ELFAF=A
      /free
       TAM(A) = TAM(A) + EL1AAM + EL2AAM + EL3AAM + EL4AAM + EL5AAM
        + EL6AAM + EL7AAM + ELSFAM + ELMFAM;
      /end-free

      /free
       TAM(A) = TAM(A) + ELanca;
      /end-free
     c**** When flag is 'H'
     c                   Else
     C     EHWMNP        IFEQ      'Y'
     C     EXBEXP        ANDNE     'Y'
     C     EHPRTD        OREQ      0
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWNMP        TAM(A)
     C                   Z-ADD     EHWANORM      TAM(A)
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWANORM = *zeros
     c                   Z-ADD     EHWNMP        TAM(A)
      *** Beg Add ***** 10/14/24 ************************************
       Else;
       //*** Beg Chg ***** 02/06/25 ******************************
       //*** xxSt = RatingSt(ELCO#:ELRTCD);
       //*** If ELEMNP = 'Y' and xxSt ='NJ';
       //***    If ELPror <> 0 and ELPror <> 1;
       //***       Eval TAM(A) += %Int((EHwbal / ELPror) + 0.5);
       //***    Else;
       //***       Eval TAM(A) += EHwbal;
       //***    Endif;
       //*** Else;
       //***    Eval TAM(A) += EHwbal;
       //*** Endif;
          If EHPror <> 0 and EHPror <> 1;
             Eval TAM(A) += %Int((EHwbal / EHpror) + 0.5);
          Else;
             Eval TAM(A) += EHwbal;
          Endif;
       //*** End Chg ***** 02/06/25 ******************************
      *** End Add ***** 10/14/24 ************************************
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Add ***** 08/27/24 ************************************
      *** Beg Del ***** 10/14/24 ************************************
     c***                add       EHWBAL        TAM(A)
      *** End Del ***** 10/14/24 ************************************
      *** End Add ***** 08/27/24 ************************************
      *** End Chg ***** 02/27/24 ************************************
     C     RRDV1L        IFEQ      'D'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                SUB       EHWDC2        TAM(A)
     C                   SUB       EHWADC2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC2 = *zeros
     c                   SUB       EHWDC2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ENDIF
     C                   ELSE
      *** Beg Chg ***** 02/27/24 ************************************
     C***  EHWNMP        ADD       EHPRTD        TAM(A)
     C     EHWANORM      ADD       EHPRTD        TAM(A)
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWANORM = *zeros
     c     EHWNMP        ADD       EHPRTD        TAM(A)
      *** Beg Add ***** 10/14/24 ************************************
       Else;
       //*** Beg Chg ***** 02/06/25 ******************************
       //*** xxSt = RatingSt(ELCO#:ELRTCD);
       //*** If ELEMNP = 'Y' and xxSt ='NJ';
       //***    If ELPror <> 0 and ELPror <> 1;
       //***       Eval TAM(A) += %Int((EHwbal / ELPror) + 0.5);
       //***    Else;
       //***       Eval TAM(A) += EHwbal;
       //***    Endif;
       //*** Else;
       //***    Eval TAM(A) += EHwbal;
       //*** Endif;
          If EHPror <> 0 and EHPror <> 1;
             Eval TAM(A) += %Int((EHwbal / EHpror) + 0.5);
          Else;
             Eval TAM(A) += EHwbal;
          Endif;
       //*** End Chg ***** 02/06/25 ******************************
      *** End Add ***** 10/14/24 ************************************
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Add ***** 08/27/24 ************************************
      *** Beg Del ***** 10/14/24 ************************************
     c***                add       EHWBAL        TAM(A)
      *** End Del ***** 10/14/24 ************************************
      *** End Add ***** 08/27/24 ************************************
      *** End Chg ***** 02/27/24 ************************************
     C     RRDV1L        IFEQ      'D'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                SUB       EHWDC2        TAM(A)
     C                   SUB       EHWADC2       TAM(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWADC2 = *zeros
     c                   SUB       EHWDC2        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ENDIF
     C                   ENDIF
     C                   ADD       EHWTER        TAM(A)
     C                   ADD       EHWDTR        TAM(A)
     C                   ADD       EHWARC        TAM(A)
     C                   ADD       EHWHXM        TAM(A)
      *** Beg Chg ***** 02/27/24 ************************************
     C***                ADD       EHWUSA        TAM(A)
     C***                Z-ADD     EHIAPR        TIN(A)
     C                   ADD       EHWAUSA       TAM(A)
     C                   Z-ADD     EHIAANN       TIN(A)
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAUSA = *zeros
     c                   ADD       EHWUSA        TAM(A)
     c                   Endif
     c                   If        EHIAANN = *zeros
     c                   Z-ADD     EHIAPR        TIN(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   ADD       EHWPF         TAM(A)
      /free
      *** Beg Del ***** 10/14/24 *********************************
      ***TAM(A) = TAM(A) + EHW1AS + EHW2AS + EHW3AS + EHW4AS + EHW5AS
      *** + EHW6AS + EHW7AS + EHWSF  + EHWMF ;
      ***TIN(A) = TIN(A) + EHI1AS + EHI2AS + EHI3AS + EHI4AS + EHI5AS
      *** + EHI6AS + EHI7AS + EHISF  + EHIMF ;
      /end-free

      /free
      ***TAM(A) = TAM(A) + EHwanc;
      ***TIN(A) = TIN(A) + EHianc;
      *** End Del ***** 10/14/24 *********************************
      /end-free

     c                   Endif
      /free
       If clientID = '111';
          TAM(A) = TAM(A) - xxDvnd;
          TIN(A) = TIN(A) - xxDvnd;
       Endif;
      /end-free

     P SetCondition60  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition61  b
     d SetCondition61  pi
     d inflag                         1    Const

     c                   If        inflag = 'D' or inflag = 'U'
     C                   If        elfaf = 'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELAAPR        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPAAPR        TAM(A)
     C                   Else
     C                   Z-ADD     ELAAPR        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     ELANPR        TAM(A)
     C                   If        inflag = 'D'
     C                   Z-ADD     MPANPR        TAM(A)
     C                   Else
     C                   Z-ADD     ELANPR        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     c                   Endif
     C                   MOVEA     CON(16)       TMP
     c                   If        ELpror <> 0
     c                   eval      fct(a) = %editc(ELpror:'K')  + ' ' + '%'
     c                   Else
     c                   eval      fct(a) = ' '
     c                   Endif
      /free
       TAM(A) = TAM(A) + EL1AAM + EL2AAM + EL3AAM + EL4AAM + EL5AAM
        + EL6AAM + EL7AAM + ELSFAM + ELMFAM;
      /end-free

      /free
       TAM(A) = TAM(A) + ELanca;
      /end-free

     c**** When flag is 'H'
     c                   Else
      *** Beg Chg ***** 02/27/24 ************************************
     C***                Z-ADD     EHWAPR        TAM(A)
     C                   Z-ADD     EHWAANN       TAM(A)
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAANN = *zeros
     c                   Z-ADD     EHWAPR        TAM(A)
      *** Beg Add ***** 10/14/24 *********************************
     c                   Else
     c                   add       EHWBAL        TAM(A)
      *** Beg Add ***** 10/14/24 ************************************
       //*** Beg Chg ***** 02/06/25 ******************************
       //*** If ELPror <> 0 and ELPror <> 1;
       //***    Eval TAM(A) += %Int((EHWTER * ELPror) + 0.5);
       //***    Eval TAM(A) += %Int((EHWDTR * ELPror) + 0.5);
       If EHpror <> 0 and EHpror <> 1;
          Eval TAM(A) += %Int((EHWTER * EHpror) + 0.5);
          Eval TAM(A) += %Int((EHWDTR * EHpror) + 0.5);
       //*** End Chg ***** 02/06/25 ******************************
       Else;
          Eval TAM(A) += EHWTER;
          Eval TAM(A) += EHWDTR;
       Endif;
      *** End Add ***** 10/14/24 *********************************
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
      *** Beg Add ***** 08/27/24 ************************************
      *** Beg Del ***** 10/14/24 *********************************
     c***                add       EHWBAL        TAM(A)
      *** End Del ***** 10/14/24 *********************************
      *** End Add ***** 08/27/24 ************************************
      *** End Chg ***** 02/27/24 ************************************
     C                   Z-ADD     *ZERO         TIN(A)
     C                   MOVEA     CON(16)       TMP
     c                   If        Ehpror <> 0
     c                   eval      fct(a) = %editc(Ehpror:'K')  + ' ' + '%'
     c                   Else
     c                   eval      fct(a) = ' '
     c                   Endif
      /free
      *** Beg Del ***** 10/14/24 *********************************
       //***TAM(A) = TAM(A) + EHW1AS + EHW2AS + EHW3AS + EHW4AS + EHW5AS
       //*** + EHW6AS + EHW7AS + EHWSF  + EHWMF ;
      /end-free

      /free
       //***TAM(A) = TAM(A) + EHwanc;
      *** Beg Del ***** 10/14/24 *********************************

      /end-free
     c                   Endif
      /free
       If clientID = '111';
          TAM(A) = TAM(A) - xxDvnd;
       Endif;
       //*** Beg Add ***** 10/14/24 **************************************
       xxTotal = TAM(A);
       //*** End Add ***** 10/14/24 **************************************
      /end-free

     P SetCondition61  e
     c**************************************************************************************
     c*
     c**************************************************************************************
     p SetCondition62  b
     d SetCondition62  pi
     d inflag                         1    Const
       //*** beg add ***** 02/23/22 ******************
       Dcl-S decTotalRetro Packed(11:2);
       //*** end add ***** 02/23/22 ******************

     c                   If        inflag = 'D' or inflag = 'U'
     C     ELSTAM        IFNE      *zero
     C                   MOVEL     CON(32)       HDG(A)
     C     ELFAF         IFEQ      'A'
      *** Beg Chg ***** 02/27/24 ************************************
     C***  ELSTAM        SUB       ELAAPR        TAM(A)
     C                   If        inflag = 'D'
     C     ELSTAM        SUB       MPAAPR        TAM(A)
     C                   Else
     C     ELSTAM        SUB       ELAAPR        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     C                   ELSE
      *** Beg Chg ***** 02/27/24 ************************************
     C***  ELSTAM        SUB       ELANPR        TAM(A)
     C                   If        inflag = 'D'
     C     ELSTAM        SUB       MPANPR        TAM(A)
     C                   Else
     C     ELSTAM        SUB       ELANPR        TAM(A)
     C                   Endif
      *** End Chg ***** 02/27/24 ************************************
     C                   ENDIF                                                  ELFAF IFEQ
     C                   ADD       1             A
     c                   If        not isContribution
     C                   MOVEL     CON(33)       HDG(A)
     C                   ELSE
     C                   MOVEL     CON(29)       HDG(A)
     C                   ENDIF
     C                   Z-ADD     ELSTAM        TAM(A)
     C                   ADD       1             A
     C                   ENDIF                                                  ELSTAM IFNE

     c**** When flag is 'H'
     c                   Else
     C     EHFAF         IFEQ      'X'
     C*
     C                   MOVEL     CON(32)       HDG(A)
      *** Beg Chg ***** 02/27/24 *****************************
     C***  EHWRPR        SUB       EHWAPR        TAM(A)
     C     EHWRPR        SUB       EHWAANN       TAM(A)
      *** End Chg ***** 02/27/24 *****************************
      *** Beg Add ***** 09/24/24 *********************************
     c                   If        EHWAANN = *zeros
     c     EHWRPR        SUB       EHWAPR        TAM(A)
     c                   Endif
      *** End Add ***** 09/24/24 *********************************
     C                   Z-ADD     0             TIN(A)
     C                   ADD       1             A
     c                   If        not isContribution
     C                   MOVEL     CON(33)       HDG(A)
     C                   ELSE
     C                   MOVEL     CON(29)       HDG(A)
     C                   ENDIF
     C                   Z-ADD     EHWRPR        TAM(A)
     C                   Z-ADD     0             TIN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  EHFAF IFEQ

       //*** beg add ***** 02/23/22 ******************
       If is3rdPartyBilling = *On;
         // Total retro billings
          Setll (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:'B') pea10;
          Dou %Eof(WMEAL10);
              Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv:'B') pea10;
              If %Eof;
                 Leave;
              Endif;
             // Only include billings as of the prem history transaction date
              If EActyp <> 'C' And EApodt > EHtrdt;
                 Iter;
              Endif;
              decTotalRetro += EAamt;
          Enddo;
         // Display if retro billings exist
          If decTotalRetro <> 0;
            // Display retro adjust in annual prem spot
             hdg(a) = con(32);
             tam(a) = decTotalRetro;
             tin(a) = 0;
             a += 1;
            // Display settlement amount (retro + annual prem)
             If Not(isContribution);
                hdg(a) = con(33);
             Else;
                hdg(a) = con(29);
             Endif;
             tam(a) = decTotalRetro + EHwapr;
             tin(a) = 0;
             a += 1;
          Endif;
          Return;
       Endif;
       //*** end add ***** 02/23/22 ******************

     C     CLTID         IFEQ      '071'
     C     FN3PPE        ANDEQ     'Y'
     C     EHFAF         IFEQ      'E'
     C     EHFAF         OREQ      'R'
     c                   If        isContribution
     c                   MOVEL     CON(44)       HDG(A)
     c                   Else
     c                   MOVEL     CON(39)       HDG(A)
     c                   Endif
     C                   ENDIF
     C     EHFAF         IFEQ      'A'
     c                   If        isContribution
     c                   MoveL     CON(45)       HDG(A)
     c                   Else
     c                   MoveL     CON(40)       HDG(A)
     c                   Endif
     C                   ENDIF
     C                   Z-ADD     EHNPRM        TAM(A)
     C                   ADD       1             A
     C     EHFAF         IFEQ      'E'
     C     EHFAF         OREQ      'R'
     c                   If        isContribution
     c                   MoveL     CON(46)       HDG(A)
     c                   Else
     c                   MOVEL     CON(41)       HDG(A)
     c                   Endif
     C                   ENDIF
     C     EHFAF         IFEQ      'A'
     c                   If        isContribution
     c                   MoveL     CON(47)       HDG(A)
     c                   Else
     c                   MOVEL     CON(42)       HDG(A)
     c                   Endif
     C                   ENDIF
     C                   Z-ADD     EHPRM         TAM(A)
     C                   ADD       1             A
     C                   ENDIF

     c                   Endif

     P SetCondition62  e

     p*--------------------------------------------------
     p* Procedure name: SetCondition63
     p* Purpose:        Load Subject premium amounts
     p* Returns:
     p*--------------------------------------------------
     p SetCondition63  B
     d SetCondition63  PI
     d inflag                         1    Const

      /Free

       If inflag = 'D' or inflag = 'U';
       //*** Beg Del ***** 09/24/24 ************************************
          //*** if  ELfaf = 'A';
      *** Beg Chg ***** 02/27/24 ************************************
       //*** tam(a) = ELasbj;
             //*** tam(a) = MPasbj;
      *** End Chg ***** 02/27/24 ************************************
          //*** Else;
      *** Beg Chg ***** 02/27/24 ************************************
       //*** tam(a) = ELesbj;
             //*** tam(a) = MPesbj;
      *** End Chg ***** 02/27/24 ************************************
          //*** Endif;
       //*** End Del ***** 09/24/24 ************************************
          //*** Beg Add ***** 09/24/24 ************************************
          If  ELfaf = 'A';
             If inflag = 'D';
                tam(a) = MPasbj;
             Else;
                tam(a) = ELasbj;
             Endif;
          Else;
             If inflag = 'D';
                tam(a) = MPesbj;
             Else;
                tam(a) = ELesbj;
             Endif;
          Endif;
          //*** End Add ***** 09/24/24 ************************************
       Else;
      *** Beg Chg ***** 02/27/24 ************************************
      //* tam(a) = EHwsbj;
      //* tin(a) = EHisbj;
          tam(a) = EHWASBJ;
          tin(a) = EHIASBJ;
      *** End Chg ***** 02/27/24 ************************************
      *** Beg Add ***** 09/24/24 ************************************
          If EHWASBJ = *zeros;
             tam(a) = EHwsbj;
          Endif;
          If EHIASBJ = *zeros;
             tin(a) = EHisbj;
          Endif;
      *** End Add ***** 09/24/24 ************************************
       Endif;

      /End-free
     p SetCondition63  E
     p SetCondition64  B
     d SetCondition64  PI

      /free

       TAM(A) = xxDvnd * (-1);
       TIN(A) = xxDvnd * (-1);
       HDG(A) = Con(50);
       A = A + 1;

      /End-free
     p SetCondition64  E


       //** Beg Add * 10/20/15
     p GetOccdAndNrat  b
     d GetOccdAndNrat  pi
     d xxOccd          s              1A
     d xxNrat          s              1A
     d xpst            s              2A

      /Free
       xtAdld = *zeros;

       Setll (IntCo#:IntFnd:IntEmp#:IntDiv:IntFyr) wmeple;
       Reade (IntCo#:IntFnd:IntEmp#:IntDiv:IntFyr) wmeple;
       Clear xxOccd;
       Clear xxNrat;

       Dow Not %EOF(wmeple);

          IF EPstat <> 'I';

             Setgt (intCo#:rateStCode:EPclas:EPefdt) wmprl1;
             Reade (intCo#:rateStCode:EPclas) wmprl1;
             If not %EOF(wmprl1);
                xxOccd = PRoccd;
                xxNrat = PRnrat;
             Endif;

             xpst = *Blanks;
             PXcode = *Zeros;

             Chain (EPco#:EPclas:EPext:xpst) wmpxl1;
             If Not %Found(wmpxl1);
                xpst = xxst;
                Chain (EPco#:EPclas:EPext:xpst) wmpxl1;
             Endif;

             If PXcode = *Zero and xxOccd = 'Y';
                xtOccd = xtOccd + EPadld;
             Endif;

             If PXcode = *Zero and xxNrat = 'Y';
                xtNrat = xtNrat + EPadld;
             Endif;

          Endif;
          Reade (IntCo#:IntFnd:IntEmp#:IntDiv:IntFyr) wmeple;
       Enddo;

      /End-free
     P GetOccdAndNrat  e

     C**************************************************************************************
     C* Occupational Desease
     C**************************************************************************************
     p SetCondition65  b
     d SetCondition65  pi

      /Free

       GetOccdAndNrat();
       If xtOccd <> *zeros;
          Tam(a) = xtOccd;
          Hdg(A) = Con(51);
          A = A + 1;
       Endif;
      /End-Free

     P SetCondition65  e

     C**************************************************************************************
     C* Non-Ratable codes
     C**************************************************************************************
     p SetCondition66  b
     d SetCondition66  pi

      /Free

       GetOccdAndNrat();
       If xtNrat <> *zeros;
          Tam(a) = xtNrat;
          Hdg(A) = Con(52);
          A = A + 1;
       Endif;
      /End-Free

     P SetCondition66  e

     C**************************************************************************************
     C* 1st Assessment
     C**************************************************************************************
     p SetCondition67  b
     d SetCondition67  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = EL1AAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHW1AS;
          TIN(A) = EHI1AS;
       Endif;

       If TAM(A) <> *zeros;
          If RR1STH <> *blanks;
             TMP_s = RR1STH;
          Else;
             TMP_s = CON(53);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition67  e
     C**************************************************************************************
     C* 2nd Assessment
     C**************************************************************************************
     p SetCondition68  b
     d SetCondition68  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = EL2AAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHW2AS;
          TIN(A) = EHI2AS;
       Endif;

       If TAM(A) <> *zeros;
          If RR2NDH <> *blanks;
             TMP_s = RR2NDH;
          Else;
             TMP_s = CON(54);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition68  e
     C**************************************************************************************
     C* 3rd Assessment
     C**************************************************************************************
     p SetCondition69  b
     d SetCondition69  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = EL3AAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHW3AS;
          TIN(A) = EHI3AS;
       Endif;

       If TAM(A) <> *zeros;
          If RR3RDH <> *blanks;
             TMP_s = RR3RDH;
          Else;
             TMP_s = CON(55);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition69  e
     C**************************************************************************************
     C* 4th Assessment
     C**************************************************************************************
     p SetCondition70  b
     d SetCondition70  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = EL4AAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHW4AS;
          TIN(A) = EHI4AS;
       Endif;

       If TAM(A) <> *zeros;
          If RR4THH <> *blanks;
             TMP_s = RR4THH;
          Else;
             TMP_s = CON(56);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition70  e
     C**************************************************************************************
     C* 5th Assessment
     C**************************************************************************************
     p SetCondition71  b
     d SetCondition71  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = EL5AAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHW5AS;
          TIN(A) = EHI5AS;
       Endif;

       If TAM(A) <> *zeros;
          If RR5THH <> *blanks;
             TMP_s = RR5THH;
          Else;
             TMP_s = CON(57);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition71  e
     C**************************************************************************************
     C* 6th Assessment
     C**************************************************************************************
     p SetCondition72  b
     d SetCondition72  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = EL6AAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHW6AS;
          TIN(A) = EHI6AS;
       Endif;

       If TAM(A) <> *zeros;
          If RR6THH <> *blanks;
             TMP_s = RR6THH;
          Else;
             TMP_s = CON(58);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition72  e
     C**************************************************************************************
     C* 7th Assessment
     C**************************************************************************************
     p SetCondition73  b
     d SetCondition73  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = EL7AAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHW7AS;
          TIN(A) = EHI7AS;
       Endif;

       If TAM(A) <> *zeros;
          If RR7THH <> *blanks;
             TMP_s = RR7THH;
          Else;
             TMP_s = CON(59);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition73  e
     C**************************************************************************************
     C* Safety Fees
     C**************************************************************************************
     p SetCondition74  b
     d SetCondition74  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = ELSFAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHWSF ;
          TIN(A) = EHISF ;
       Endif;

       If TAM(A) <> *zeros;
          If RRSFHD <> *blanks;
             TMP_s = RRSFHD;
          Else;
             TMP_s = CON(60);
          Endif;
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition74  e
     C**************************************************************************************
     C* Membership Fees
     C**************************************************************************************
     p SetCondition75  b
     d SetCondition75  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = ELMFAM;
       Else; //***  When flag is 'H'
          TAM(A) = EHWMF ;
          TIN(A) = EHIMF ;
       Endif;

       If TAM(A) <> *zeros;
          TMP_s = CON(61);
          HDG(A) = TMP_s;
          A = A + 1;
       Endif;
      /end-free

     P SetCondition75  e

     C**************************************************************************************
     C* ANC Charge
     C**************************************************************************************
     p SetCondition76  b
     d SetCondition76  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          TAM(A) = ELanca;
       Else; //***  When flag is 'H'
          TAM(A) = EHwanc;
          TIN(A) = EHianc;
       Endif;

       If TAM(A) <> *zeros;
          HDG(A) = CON(62);
          A = A + 1;
       Endif;
      /end-free

     P SetCondition76  e

      *** Beg Add ***** 02/27/24 ********************************
     C***********************************************************
     C* Balance to Minimum                                      *
     C***********************************************************
     p SetCondition77  b
     d SetCondition77  pi
     d inflag                         1    Const
      /Free
       If inflag = 'D' or inflag = 'U';
          If ELfaf = 'A';
             If ELAMNP = 'Y';
                TAM(A) = MPABAL;
                //*** Beg Add ***** 08/14/24 ******************************
                //*** Beg Chg ***** 09/24/24 ******************************
                //*** A += 1;
                //*** Hdg(A) = con(67);
                Hdg(A) = con(67);
                A += 1;
                //*** End Chg ***** 09/24/24 ******************************
                //*** End Add ***** 08/14/24 ******************************
             Endif;
          Else;
             If ELEMNP = 'Y';
                TAM(A) = MPEBAL;
                //*** Beg Add ***** 08/14/24 ******************************
                //*** Beg Chg ***** 09/24/24 ******************************
                //*** A += 1;
                //*** Hdg(A) = con(67);
                Hdg(A) = con(67);
                A += 1;
                //*** End Chg ***** 09/24/24 ******************************
                //*** End Add ***** 08/14/24 ******************************
             Endif;
          Endif;
       Else; //***  When flag is 'H'
          If EHWMNP = 'Y' or EHIMNP = 'Y';
             TAM(A) = EHwbal;
             TIN(A) = EHibal;
             //*** Beg Add ***** 02/06/25 ************************************
             If EHPror <> 0 and EHPror <> 1;
                Eval TAM(A) = %Int((EHwbal / EHpror) + 0.5);
             Endif;
             //*** End Add ***** 02/06/25 ************************************
          //*** Beg Add ***** 10/14/24 ************************************
          //*** Beg Del ***** 02/06/25 ******************************
          //*** Else;
          //***    xxSt = RatingSt(ELCO#:ELRTCD);
          //***    If ELEMNP = 'Y' and xxSt ='NJ';
          //***       If ELPror <> 0 and ELPror <> 1;
          //***          Eval TAM(A) = %Int((EHwbal / ELPror) + 0.5);
          //***          Eval TIN(A) = %Int((EHibal / ELPror) + 0.5);
          //***       Endif;
          //***    Endif;
          //*** End Del ***** 02/06/25 ******************************
          //*** End Add ***** 10/14/24 ************************************
             //*** Beg Add ***** 08/14/24 ******************************
             //*** Beg Chg ***** 09/24/24 ******************************
             //*** A += 1;
             //*** Hdg(A) = con(67);
             Hdg(A) = con(67);
             A += 1;
             //*** End Chg ***** 09/24/24 ******************************
             //*** End Add ***** 08/14/24 ******************************
          Endif;
       Endif;


       //*** Beg Del ***** 08/14/24 **********************************
       //***HDG(A) = 'Balance to Minimum*';
       //***A += 1;
       //*** End Del ***** 08/14/24 **********************************

      /end-free

     P SetCondition77  e

     C***********************************************************
     C* Minimum has invoked info.                               *
     C***********************************************************
     p SetCondition78  b
     d SetCondition78  pi
      /Free
       //*** Beg Chg ***** 11/21/25 *************************
       //***If ELEMNP = 'Y' or EHWMNP = 'Y';
       //***   A += 1;
       //***   HDG(A) = '*Mimimum has invoked.';
       //***Endif;
     d inflag                         1    Const

       If inflag = 'D' or inflag = 'U';
          If (ELfaf = 'A' and ELamnp = 'Y') or (ELfaf <> 'A' and ELemnp = 'Y');
             A += 1;
             HDG(A) = '*Mimimum has invoked.';
          Endif;
       Else; //***  When flag is 'H'
          If EHwmnp = 'Y';
             A += 1;
             HDG(A) = '*Mimimum has invoked.';
          Endif;
       Endif;
       //*** End Chg ***** 11/21/25 *************************
      /end-free

     P SetCondition78  e

      *** End Add ***** 02/27/24 ********************************
      *** Beg Add ***** 10/14/24 ********************************
     C***********************************************************
     C* Total                                                   *
     C***********************************************************
     p SetCondition79  b
     d SetCondition79  pi
      /Free

       TAM(A) = xxTotal + EHW1AS + EHW2AS + EHW3AS + EHW4AS + EHW5AS
         + EHW6AS + EHW7AS + EHWSF  + EHWMF + EHwanc;
       HDG(A) = 'Total';
       A += 1;

      /end-free

     P SetCondition79  e

      *** End Add ***** 10/14/24 ********************************

      *--------------------------------------------------
      * Procedure name: WritePolicyFee
      * Purpose:
      * Returns:
      * Parameter:      inFlag
      *--------------------------------------------------
     p WritePolicyFee  b
     d WritePolicyFee  pi
     d  inFlag                        1a   Const
      /Free
       // Get policy fee amount
       Select;
       When inFlag = 'D' or inFlag = 'U';
          If ELfaf = 'A';
             tam(a) = ELapf;
          Else;
             tam(a) = ELepf;
          Endif;
       When inFlag = 'H';
          tam(a) = EHwpf;
          tin(a) = EHipf;
       Endsl;
       Select;
       When tam(a) = *Zero;
       When RRpfhd <> *Blanks;
          hdg(a) = RRpfhd;
       Other;
          hdg(a) = con(64);
       Endsl;
      /End-Free
     p WritePolicyFee  e
**
The primary agent and producer are the secondary
agent and producer in another active record or the
secondary agent and producer are the primary agent
and producer on another active record.
An active record with the same effective date
already exists for this policy.
Commission records exist on the policy in a
prior month. The primary and secondary agent, rate,
and producer information cannot be changed. If
change is needed, inactivate this record and add
a new one with the desired information.
The Primary agent and producer are the secondary
agent and producer for another active record.
The Primary agent and producer are the Teriary
agent and producer for another active record.
The Secondary agent and producer are the primary
agent and producer for another active record.
The Secondary agent and producer are the Tertiary
agent and producer for another active record.
The Tertiary  agent and producer are the primary
agent and producer for another active record.
The Tertiary agent and producer are the Secondary
agent and producer for another active record.
Primary Agent information changed.
Secondary Agent information changed.
Tertiary Agent information is changed.
Earnings have been posted to the agent being inactivated.
Use the Agent Commission Maintenance process to
post any needed adjustments.
**
AAgency bill
DDirect bill (pre-paid)
PBased on A/R payments
**  Premium Headings
Manual Premium
Increased Limit
Deductible
MC Credit
Safety Credit
Deviation
Subtotal
Experience Modifier
Additional Load
Contractors Credit
Standard Premium
Discount
Total Normal
Expense Constant
Annual Premium
Prorata Factor
2nd Increased Limit
2nd Deviation
Schedule DR/CR
Waiver of Subrogation
Loss Constant
Foreign Terrorism
Mass ARAP Surcharge
Voluntary Labor Charge
Domestic Terrorism Charge
Company Factor
Standard Contribution
Annual Contribution
Retrospective Contribution
Manual contribution
2nd Deductible
Retro Adjustment
Retrospective premium
Facultative Reinsurance
Harbor Exposure Fee
Short Rate Penalty
Stbl Factor
HCN Credit
Estimated Non-Premium
Audited Non-Premium
Estimated Premium
Audited Premium
Unearn Premium Surcharge
Estimated Non-Contribution
Audited Non-Contribution
Estimated Contribution
Audited Contribution
Unearn Contribution Surcharge
Subject Premium
Earned Prem Credit
Occupational Desease
Non-Ratable Class Code
1st Assessment
2nd Assessment
3rd Assessment
4th Assessment
5th Assessment
6th Assessment
7th Assessment
Safety Fees
Membership Fees
ANC Charge
Territory Rating
Policy Fee
Balance to I.L. Minimum                                                      65 *** 08/14/24
Balance to 2nd I.L. Minimum                                                  66 *** 08/14/24
Balance to Minimum*                                                          67 *** 08/14/24
Open (not used)
Open (not used)
Open (not used)
