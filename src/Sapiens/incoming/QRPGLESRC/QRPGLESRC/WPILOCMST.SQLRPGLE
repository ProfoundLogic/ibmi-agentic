     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WPILOCMST  - Policy Location Master
     h option(*nodebugio: *srcstmt)
     h nomain
     h/define WPILOCMST
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/25/12                                         *
      *  Programmer: C:191                                            *
      *  Project   : WC2116                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 11/11/25 C:62  Support: Phw#120075(099)                       *
      *                Don't send name change to IVC if not at IVC.   *
      *                                                               *
      * 10/16/25 C:65  Support: Phw#119562 (Amwins)                   *
      *                Populate transaction date/time when deletion   *
      *                occurs on an additional location or named ins. *
      *                                                               *
      * 08/04/25 C:079 Support: 119946 (Sapiens)                      *
      *                Set the "top' property for message about       *
      *                defaults from state reporting division         *
      *                                                               *
      * 06/02/25 C:333 Project: WC3009                                *
      *                Clms, Policy, Agncy contacts: Maint Info       *
      *                - Add/Update staff details in WMECP            *
      *                - Delete staff details from WMECP/WMECPCRT     *
      *                                                               *
      * 04/24/25 C:079 Support: 118909 (Vital)                        *
      *                Override to WMP3P member in case override was  *
      *                set to the ARCHIVE member                      *
      *                                                               *
      * 03/28/25 C:333 Support: 118734 (Vital)                        *
      *                Experience Rating not being Set in GO on Quotes*
      *                from Connect                                   *
      *                                                               *
      * 03/10/25 C:344 Support: 118576 (Coverisk)                     *
      *                Change the sort order for Location Year to     *
      *                descending order.                              *
      *                                                               *
      * 02/27/25 C:333 Support: 118572                                *
      *                Quote Entry - Default of Audit Cancellation    *
      *                Calculation Checkbox is not happening for Add  *
      *                of Division.                                   *
      *                                                               *
      * 11/28/24 C:300 Support: 118070 (AmWINS)                       *
      *                Inception Date is not showing same on Policy   *
      *                Summary Page.                                  *
      *                On policy/quote Entry user enters a date on the*
      *                clearance screen, but changes it on subsequent *
      *                Billing Division screen. Then also update the  *
      *                Inception Date (WMEMP/EMEFD1)                  *
      *                                                               *
      * 10/18/24 C:65  Support: Phw#117783 (Vital)                    *
      *                Apply billing plan disabling to bill on eff    *
      *                day checkbox.                                  *
      *                                                               *
      * 09/25/24 C:291 Support: 117660                                *
      *                Clear record format pel00 instead of pel0p     *
      *                before creating WDELP shell record.            *
      *                                                               *
      * 08/08/24 C:65  Support: Phw#117209/WC2965 (Sunz)              *
      *                Change the defaulting of the email address to  *
      *                only happen during an add.  There is also no   *
      *                need to check state reporting division's email *
      *                address since we're not using it and it gets   *
      *                copied over by the inital record setup.        *
      *                Unrelated fix - initialize addMode and chgMode *
      *                within LoadBillDiv because those variables do  *
      *                not get set when called from WPILOCMAIN for    *
      *                showing the division from Location Master scr. *
      *                                                               *
      * 07/23/24 C:333 Support: 117026                                *
      *                Mid-term End Not Showing iDARTS and When Chkd  *
      *                Not Defaulting Remark.                         *
      *                                                               *
      * 07/11/24 C:333 Support: 115909 (UBIC)                         *
      *                Error Processing Phy Address Endorsement.      *
      *                - DSPF Changes                                 *
      *                  Changed disabled indicator for SelCounty     *
      *                - Changes in runtime js:                       *
      *                  runtime-989390279d.js                        *
      *                                                               *
      * 06/24/24 C:300 Project: 146WKR (Prescient)                    *
      *                Collateral Processing - Recompile              *
      *                                                               *
      * 06/12/24 C:333 Support: 115734 (Vital)                        *
      *                Adding Locations, County is not auto-populating*
      *                Changes:                                       *
      *                -RcdLocWdw - 1) Adding OutEMrrn.               *
      *                 2) Adding onblur event for TxbDMZip & TxbDPZip*
      *                -runtime-989390279d.js changes to add county to*
      *                 SlbDCounty in StateChange() & ZipChange().    *
      *                                                               *
      * 06/06/24 C:291 Project: WC2968                                *
      *                Name and Address change for Invoice Cloud.     *
      *                                                               *
      * 04/29/24 C:340 Project: WC2965                                *
      *                Email Address Required configuration.          *
      *                                                               *
      * 04/29/24 C:340 Project: WC2965                                *
      *                Email Address Required configuration.          *
      *                                                               *
      * 03/19/24 C:333 Support: PHW114843                             *
      *                Mid Term Endorsement shows different state     *
      *                than entered                                   *
      *                                                               *
      * 03/04/24 C:079 Project: WC2951                                *
      *                Remove Exp Rating. Change Report to NCCI       *
      *                                                               *
      * 08/03/23 C:300 Support: 113211 (AmWINS)                       *
      *                PEO Naming Convention field ovelap on Mid-term *
      *                Add Divison screen.                            *
      *                                                               *
      * 05/03/23 C:342 Support: 107604 (AmWINS)                       *
      *                FEIN error handling when Admin/DARTS is set to *
      *                indicate FEIN is required.                     *
      *                                                               *
      * 04/25/23 C:291 Support: 100129                                *
      *                Do not copy all the cancellation detail/history*
      *                records from SRD to New added division when div*
      *                is added to cancelled policy. Only create CX   *
      *                Detail/History for current active CX record.   *
      *                When Div is added to cancelled policy, update  *
      *                new division WMEMP/EMCAD1 with CX date.        *
      *                                                               *
      * 04/12/23 C:65  Support: 107397 (UBIC)                         *
      *                Update employer master RRN in 3rd party grid.  *
      *                                                               *
      * 04/12/23 C:65  Support: 107397                                *
      *                Restrict MPN write/update to client 132.       *
      *                                                               *
      * 04/11/23 C:65  Support: 107397 (Prescient)                    *
      *                Create shell WMEMP and WDELP records at start  *
      *                of division add process - to ensure records    *
      *                save/hold the new division # and are there     *
      *                to support EFT and third party pay stuff. If   *
      *                add process is cancelled delete any WMEMP,     *
      *                WDELP, WMEFP, or WMP3P records created. Add    *
      *                popup to request division # when it is not     *
      *                auto-assigned prior to displaying main window. *
      *                                                               *
      * 04/10/23 C:65  Support: 101820                                *
      *                Get policy year for newly added division based *
      *                on existing policy year on employer.           *
      *                                                               *
      * 04/05/23 C:65  Support: 107397 (Prescient)                    *
      *                Use newly added division # for Eft Add.        *
      *                                                               *
      * 03/28/23 C:342 Support: 107744 (Sunz)                         *
      *                Corrected the width for PEO Naming Convention. *
      *                Corrected the left position for Deposit Cap.   *
      *                                                               *
      * 03/13/23 C:291 Project: WC2896                                *
      *                When an Additional Named insured Expiration    *
      *                date is cleared to re-activate it, remove CX   *
      *                Reason code from the record. If no more Named  *
      *                insured written in WI are cancelled then       *
      *                remove the WC480323 endorsement from WMMUP.    *
      *                                                               *
      * 03/08/23 C:65  Support: 107507 (Sunz)                         *
      *                Shrink location add window when no 3rd pay.    *
      *                                                               *
      * 03/02/23 C:65  Support: 102977 (Amwins)                       *
      *                When processing an addition of division and    *
      *                user clicks Add for EFT, ensure a duplicate    *
      *                employer master record is not created when     *
      *                the Save button is pressed.                    *
      *                                                               *
      * 01/19/23 C:291 Support: 94030                                 *
      *                Default Billing Plan from the State Reporting  *
      *                division regardless of effective date.         *
      *                                                               *
      * 11/16/22 C:342 Support: 101820                                *
      *                Get policy year from WDF1L file.               *
      *                                                               *
      * 09/29/22 C:344 Support: PHW103722                             *
      *                Corrected the top and left screen position     *
      *                values for the PEO Naming Convention and       *
      *                and Deposit Cap labels in the Display File     *
      *                when the input parameter inCaller = 'DIV'.     *
      *                                                               *
      * 09/23/22 C:65  Support: 103391 (Amwins)                       *
      *                Reduce size of labels beside personal check    *
      *                boxes so they do not overlap/cover them.       *
      *                Display file only change.                      *
      *                                                               *
      * 09/22/22 C:342 Support: 101820                                *
      *                Pass correct policy year to errPolRating       *
      *                procedure                                      *
      *                                                               *
      * 03/11/22 C:340 Project: WC2843                                *
      *                Start using new Logical - WMTML1               *
      *                                                               *
      * 02/26/22 C:65  Project: Phw#100820 (Amwins)                   *
      *                Set on the visibility property for the Report  *
      *                as PEO checkbox when processing a mid-term     *
      *                term endorsement on a master policy - this     *
      *                will be a addition of division and checkbox    *
      *                should be available.                           *
      *                                                               *
      * 02/25/22 C:136 Support: 98291                                 *
      *                Don't override cx issue date with current      *
      *                date when copying cx records from the SRD      *
      *                on division add.                               *
      *                                                               *
      * 02/22/22 C:062 Project: 150WFN                                *
      *                Expand the usage of PEO Naming Convention      *
      *                                                               *
      * 02/25/22 C:136 Support: 98291                                 *
      *                Don't override cx issue date with current      *
      *                date when copying cx records from the SRD      *
      *                on division add.                               *
      *                                                               *
      * 01/25/22 C:079 Project: 133WSP                                *
      *                Allow Staff Assignement Update in inq mode     *
      *                                                               *
      * 01/22/22 C:300 Support: 98388 (In-House)                      *
      *                Display Third Party Information Grid when adding
      *                division in Mid-term Endorsement               *
      *                                                               *
      * 12/22/21 C:079 Project: WC2840                                *
      *                WCPOLS: Updates to 03 Record                   *
      *                                                               *
      * 12/13/21 C:333 Support: 94286 (LUBA)                          *
      *                Owner/Officer Nominal change endorsement.      *
      *                - Skip inactive owner records when calculating *
      *                  the total percentage                         *
      *                                                               *
      * 11/23/21 C:300 Support: 96027 (SUNZ)                          *
      *                Policy Entry and Quote Entry not allowing      *
      *                billing plan to be entered                     *
      *                WPILOCMST DSPF changes on rcdBillDiv record:   *
      *                -- Added JavaScript change in following fields:
      *                   Onload event, OnChange event of ChkA/RSummary checkbox
      *                   ChkStateRep (State Reporting Division) checkbox,
      *                   TxbPolicyEffectiveDate and both State fields
      *                -- Added hidden field TxbTempBillingCode (scBSCDtmp)
      *                   and assign value in each javascript along with
      *                   TxbBillingScheduleCode and READ that in program
      *                -- Removed Database File properties of (scsvBSCD)
      *                   TxbSaveBillingCode                          *
      *                -- Changes in StateChange() runtime-989390279d.js
      *                   /www/pdsdev/htdocs/profoundui/userdata/js/  *
      *                   by Abhijeet Panse.
      *                                                               *
      * 11/04/21 C:33  Project: 150WDC  #99220                        *
      *                Remove reset of Employee Leasing ID error ind  *
      *                                                               *
      * 10/05/21 C:079 Support: 99-98556                              *
      *                Set top for Save/Save add/next and Cancel      *
      *                                                               *
      * 09/30/21 C:300 Project: WC2663, Support: 98388 (In-House)     *
      *                Change 3rd Party Grid in Quote Review          *
      *                to look/act like grid in Location Master       *
      *                WPILOCMST DSPF changes on rcdBillDiv record:   *
      *                -- Added Containers ConThirdPartyPayment and   *
      *                   ConThirdPartyLog                            *
      *                -- onrowclick event added in SflThirdPartyPay  *
      *                   and removed last column of grid (Lnk3PLog)  *
      *                   and its function thirdPartyDetail() added   *
      *                   in onload screen javascript                 *
      *                -- Everything from 'Third Party Information'   *
      *                   grid moved 40 px UP to remove white spaces. *
      *                -- Moved 'Prior Carrier Information' section to*
      *                   bottom of screen                            *
      *                -- Removed record format "Sfl3pLog" (3rd Party *
      *                   Info Log subfile) which was added recently  *
      *                   under (05/11/21 C:180 Project: WC1986)      *
      *                                                               *
      *                Prevent the 3rd Party subfile from being loaded*
      *                if the 3rd Party module is not enabled.        *
      *                                                               *
      *                Additional changes to 3rd Pty and Opt In grids *
      *                from LMILOCMAIN (08/17/21 C:079 Project:WC2663)*
      *                                                               *
      * 08/24/21 C:291 Project:WC2812                                 *
      *                Call procedure DefaultRatingFactors to load    *
      *                rating Factors.                                *
      *                                                               *
     h*  07/01/21 C:33  Support: 96462                                *
     h*                 Implement standard employer deletion routine. *
      *                                                               *
      * 06/23/21 C:318 Project WC2302 - Auto tab closure for          *
      *                deletion.                                      *
      *                                                               *
      * 08/06/21 C:295 Ticket: 96354                                  *
      *               -Fix the zip code extention for Carewest        *
      *                                                               *
      * 07/29/21 C:062 Ticket: 97036(99) WC2663                       *
      *               -Fix account#                                   *
      *          C:33 -Load a value to 3rd Party grid RRN             *
      *                                                               *
      * 05/11/21 C:180 Project: WC1986                                *
      *                Add 3rd Party Info Log subfile                 *
      *                                                               *
      *                Display note for non-state reporting divisions *
      *                if the state reporting division                *
      *                uses defaulted eft or 3rd party info.          *
      *                                                               *
      *                When loading 3rd party subfile, distinguish    *
      *                between Quote and Live Records.                *
      *                                                               *
      *                Pass Policy year to EFT process                *
      *                                                               *
      * 04/20/21 C:136 Project: 150WEX (Removed and Replaced by WC2812)
      *                Verify factors copied from PEO to new client   *
      *                                                               *
      * 04/16/21 C:298 Support:94286                                  *
      *                Owner Officer percentage out of range          *
      *
      * 04/15/21 C:180 Support  - No number                           *
      *                Issue with using wrong parm found in           *
      *                UpdateBillDiv                                  *
      *                                                               *
      *                Add EFT subfile.                               *
      *                                                               *
      *  04/06/21 C:300 Support: 94604 (SUNZ)                         *
      *                 Policy rating screen allows old deductible    *
      *                 descriptions to be selected.                  *
      *                 Recompile over WTPOLDTL DSPF changes          *
      *
      * 03/11/21 C:33  Project: WC2663                                *
      *                Add 3rd Party Payments to 'Third Party Information'
      *                                                               *
      * 12/24/21 C:318 Support:94631                                  *
      *                Added call of JS validation for ssn on save and*
      *                save/Add Next Btn on Owner/officer edit screen *
      *                                                               *
      * 11/18/20 C:180 Project WC2757                                 *
      *                Recompiled only.                               *
      *                                                               *
      * 07/28/20 C:291 Project: PHW88231                              *
      *                Initialize Billing Division flexible heading   *
      *                fields after Billing Division pointer is       *
      *                retrieved. This was causing decimal data error *
      *                due to xxFYR getting initialized with non-zero *
      *                data when flexPrmFin was initialized.          *
      *                                                               *
      * 06/09/20 C:298 Project: SY420                                 *
      *                Mask SSN                                       *
      *                                                               *
      * 06/04/20 C:291 Project: WC2608                                *
      *                Require Owner/Officer Title if flagged as      *
      *                required at Endorsement stat info.             *
      *                                                               *
      * 05/15/20 C:062 Project: 102WIB                                *
      *                Write deposit cap history record.              *
      *
      * 04/29/20 C:136 Project: SY407 (DE675)                         *
      *                3rd Party Payment flag wasn't persistent.      *
      *                Reset P3RRN and scPayRrn to correct.           *
      *                                                               *
      * 02/07/20 C:053 Project WC2689
      *                Location Deletion Claim Edit
      *                                                               *
      * 02/26/20 C:079 Support: 87653                                 *
      *                Fixed info msg if total ownership percrnt is   *
      *                not 100 while adding/editing/deleting owner
      *                                                               *
      * 02/25/20 C:295 Support: 86489                                 *
      *                Corrected - Non Specific Address checkbox is   *
      *                defaulting on new divisions                    *
      *                                                               *
      * 02/12/20 C:318 Support: 86478                                 *
      *                Corrected division when FNAutd='N' to check    *
      *                validations                                    *
      *                                                               *
      * 02/11/20 C:180 Project: 150WDU                                *
      *                Tech specs changes on PEO conditioning.        *
      *                                                               *
      * 01/10/20 C:318 Support: 82084                                 *
      *                Added info msg if total ownership percrnt is   *
      *                not 100 while adding/editing/deleting owner    *
      *                                                               *
      * 12/30/19 C:180 Project: 150WDU                                *
      *                Change selection criteria for type of bus      *
      *                                                               *
      * 11/04/19 C:298 Project: 102WIB                                *
      *                Deposit Cap at Policy Level                    *
      *                                                               *
      * 10/09/19 C:62  Ticket: 83413                                  *
      *                Clear Audit flag when adding a division.       *
      *                                                               *
      * 09/05/19 C:33  Project: 150WDC  #81760                        *
      *                Only edit leasing id when editing available    *
      *                                                               *
      * 08/02/19 C:33  Project: SY406                                 *
      *                Add straight through processing flag           *
      *                                                               *
      * 06/26/19 C:65  Project: SY407                                 *
      *                Add third party online payment information to  *
      *                the bottom of location master window, below    *
      *                the rating and prior carrier info.  Only       *
      *                display if online payment module is enabled.   *
      *
      *  06/12/19  C:300 Support: 79730 (CoveRisk)
      *                  Changes to Billing plan field on Add Division
      *                  Made following changes in WPILOCMST Display File:
      *                  1) Added 2 hidden fields scsvBSCD and scsvBSDT
      *                  2) On-load of rcdBillDiv record format
      *                  3) On-change of Summarize A/R Across All Divisions
      *                  4) On-change of Policy Effective From Date
      *
      *  05/21/19  C:180 Project Disable 150WDU:
      *                  Temporarily using client id 999 for
      *                  change dated 04/05/19
      *                                                               *
      * 04/17/19 C:298 Support: 78727                                 *
      *                Increase Additional insured # only in Add Mode *
      *                                                               *
      * 04/05/19 C:180 Project: 150WDU                               *
      *                Add type of business for ANI                  *
      *                                                               *
      * 01/16/19 C:267 Support: 76243                                 *
      *                Revise Choice DB File selection criteria in the*
      *                display file to use an RPG bound field for RTCD.
      *                Add logic to determine where Rate Set code     *
      *                values will be retrieved from - WMGRP or WMRSP.*
      *                                                               *
      * 11/07/18 C:180 Project: 150WDC                                *
      *                Override PEO name control                      *
      *                                                               *
      * 10/01/18 C:291 Project: WC2457                                *
      *                Allow user-defined division # when the group   *
      *                is not flagged to auto-assign the division #.  *
      *                Added Division # text box, added Visibility    *
      *                indicator on Division # heading.               *
      *                                                               *
      * 09/24/18 C:79  Support: 99/73450                              *
      *                Increase panel height and Cancel left position *
      *                                                               *
      * 09/20/18 C:291 Support: 72916                                 *
      *                Add WDCXP and WDXHP records when division is   *
      *                added on a cancelled policy.                   *
      *                                                               *
      * 06/11/18 C:65  Project: 150WAQ                                *
      *                Populate collateral expiration date maint dt.  *
      *                                                               *
      * 05/16/18 C:291 Project: 150WCB                                *
      *                Added hidden field scOvrMP rcdRating for       *
      *                for PEO Master/MCP override.                   *
      *                Add Override to Report as PEO.                 *
      *                                                               *
      * 04/10/18 C:33  Project: 150WCF                                *
      *               -Require Employer Link Code based on group setup*
      *               -Remove 'Prior Carrier Information' from display*
      *                                                               *
      * 02/21/18 C:136 Project: WC2588                                *
      *                Clear quote transfer date when adding division *
      *                                                               *
      * 02/19/18 C:291 Project: WC2521                                *
      *                Add Non-Specific Address Mailing and Physical. *
      *                                                               *
      * 02/09/18 C:291 Support: 64173                                 *
      *                Fixed Policy Entry, Staff Assignment,          *
      *                Loss Control field spacing. Screen change only.*
      *                                                               *
      * 02/07/18 C:291 Support: 63953                                 *
      *                Change Division dropdown "max choices" property*
      *                to 100,000. Screen change only.                *
      *                Modified records: rcdOwnOff & rcdLocWdw.       *
      *                                                               *
      * 01/14/18 C:136 Project: 150WCJ                                *
      *                Add PEO Named Insured for Sunz                 *
      *                                                               *
      * 01/12/18 C:291 Support: 63251                                 *
      *                Default experience rating to Inter and         *
      *                Intrastate rated for SUNZ.                     *
      *                                                               *
      * 12/12/17 C:33  Project: 150WAQ                                *
      *               -Add collateral                                 *
      *                                                               *
      * 12/06/17 C:33  Support: 51525                                 *
      *               -On add of division                             *
      *                - Do not show division 'Billing Division Add'  *
      *                - Get division number just before writing      *
      *                                                               *
      * 10/06/17 C:136 Support: 50056 (150WAP Priss)                  *
      *                Remove rate code edit logic.  It's already     *
      *                invoked in WPIPOLERR.                          *
      *                                                               *
      *  09/08/17 C:65  Project: 150WAR                               *
      *                 Default % of manual premium from Group Year   *
      *                 for client 150 policy/division entries.       *
      *                                                               *
      * 09/05/17 C:062 Support: 49256(99)                             *
      *                Don't run rating edit if no rate code          *
      *                                                               *
      * 08/30/17 C:291 Project: WC2504                                *
      *                Load Employee Leasing Id in Add mode from      *
      *                Defalut leasing Id for that State.             *
      *                                                               *
      * 08/07/17 C:062 Project 150WAP                                 *
      *                Clear new error fields.                        *
      *                                                               *
      * 07/28/17 C:291 Project 150WAP                                 *
      *                Add Individual Check boxes                     *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Add Territory Rating for CareWest.             *
      *                                                               *
      * 02/23/17 C:62  Ticket 45825(148)                              *
      *                Prevent defaults from being applied if changes *
      *                were made                                      *
      *                                                               *
      * 01/31/17 C:33  44211                                          *
      *               -Changed JavaScript to keep the location/entity *
      *                 selection from having to return to the server.*
      *               -Auto-creation of Location are writing a 'Y' as *
      *                 the type which is invalid... should be 'L'    *
      *                                                               *
      * 01/24/17 C:136 Support: 45118                                 *
      *                Clear the Location/Entity radio buttons on     *
      *                Save/AddNext. Protect fields on Add until      *
      *                Location or Entity is selected.                *
      *                                                               *
      * 06/21/16 C:308 Project SY378 Recompile only due to change     *
      *                in WPILOCMAIN screen                           *
      *                                                               *
      * 04/12/16 C:136 Phaseware: 37786                               *
      *                Default waiver information from state rpt div  *
      *                if waiver code is set up and default waiver    *
      *                information doesn't exist in ratings and       *
      *                statistics                                     *
      *                                                               *
      * 12/15/15 C:242 Support: 36871                                 *
      *                Remove Default Physical Add on Location Add    *
      *                                                               *
      * 10/16/15 C:291 Support: 32185                                 *
      *                Fixed Effective date edit on Owner/Officer     *
      *                screen.                                        *
      *                                                               *
      * 09/30/15 C:298 Support: 37509                                 *
      *                Expanded 'Servicing Office' field on display   *
      *                file to show Office # and Office name.         *
      *                                                               *
      * 08/24/15 C:136 Support: 36892 (114WRG)                        *
      *                Fix edits on Building Construction Year and    *
      *                Peak # Employees in Location Year Detail       *
      *                                                               *
      * 08/15/15 C:65  Phase: 36646  (Support: 33960)                 *
      *                Do not lock wdelp for this support change. Was *
      *                leaving a record lock other programs would     *
      *                encounter.                                     *
      *                                                               *
      * 07/22/15 C:306 Phaseware: 31820                               *
      *                Added the edit check to validate the effective *
      *                and expiration date on Additional named insured*
      *                tab.                                           *
      *                                                               *
      * 06/26/15 C:308 Phaseware: 35528                               *
      *                Correct the blank division issue on screen.    *
      *                                                               *
      * 06/25/15 C:308 Phaseware: 33291                               *
      *                Correct the clsChoices i.e where clause        *
      *                to display drop down list from WMPRL8 file.    *
      *                                                               *
      * 06/01/15 C:307 Phaseware: 34938                               *
      *                NAICS code-not shown as required               *
      *                Added javascript code on state field in        *
      *                WPILOCMST display file to handle display of    *
      *                astreisk(*) on NAICS code field.               *
      *                Created AJAX program WTPOLLMS05 to process the *
      *                '*' on NAICS code.                             *
      *                                                               *
      * 06/05/15 C:298 Phaseware: 35158                               *
      *                Shifted position of Cancel, Save/Add Next,     *
      *                Save buttons upwards on display file.          *

      * 05/29/15 C:306 Phaseware: 34976                               *
      *                Fix the tab swapping issue when save the infor *
      *                mation on billing division tab.                *
      *                (Recompilation Only)                           *
      *                                                               *
      * 05/26/15 C:307 Phaseware: 34944                               *
      *                Error when adding location                     *
      *                                                               *
      * 05/18/15 C:295 Support: 33960                                 *
      *                Correct state reporting flag edit while adding *
      *                owner/officer and entity location.             *
    *                                                               *
      * 05/12/15 C:306 Phaseware: 29738 LUBA                          *
      *                Recompile only                                 *
      *                                                               *
      * 04/20/15 C:250 Phaseware: 31303 LCTA                          *
      *                add fields for addition employer informatinon  *
      *
      * 04/17/15 C:242 Project: 132WQS                               *
      *                Retrofit of Custom Code to Base for CompSuite:*
      *                Added #ofShiftEmployees, MPNprovideName/Add.  *
      *                Moved wdigets below MPN into container with   *
      *                variable top position.                        *
      *                Added MPN fields, Phys Addredd & Contacts to  *
      *                LoadDefaults when switching between Loct/Ent  *
      *                                                              *
      * 04/11/15 C:295 Support: 33960                                 *
      *                Correct Division on policies with blank effec  *
      *                -tive date (EMefd1)                            *
    *                                                               *
      * 04/03/15 C:242 PW# 29590, PW# 33987, Support:84803
      *                When Adding a Division, only save Rating values*
      *                applicable to the new Division's Rate State    *
      *                                                               *
      * 03/30/15 C:290 Support: 33886                                 *
      *                Added a hidden field scPFDT3 in WTPOLDTL       *
      *                display file to capture the effective date of  *
      *                the state reporting division.Changed JavaScript*
      *                associated with Experience Rating Modifiers.   *
      *                Added logic to populate the rate information   *
      *                effective dates with current div's effective   *
      *                date. (contains DSPF changes of WTPOLDTL)      *
      *                                                               *
      * 03/16/15 C:062 Support: 33798                                 *
      *                Correct the last division used logic.          *
    *                                                               *
      * 03/03/15 C:302 PROJECT: 114WRG                               *
      *                Policy Concentration Details                  *
      *                                                              *
      * 01/12/15 C:279 Redmine: 4892                                  *
      *                Corrected invalid FEIN Check Edit conidtion.   *
    *                                                               *
      * 01/09/15 C:283 RedMine: 4742                                  *
      *                All the screen fields should be editable in    *
      *                Edit Mode.                                     *
      *                                                               *
      * 12/25/14 C:279 Support: 86889  WA136                          *
      *                Modified to populate correct Owner/Officer     *
      *                record on edit.                                *
      *
    *  11/04/14 C:65  Redmine: 4638 - T.H.E.                        *
    *                 Update the Fyr specific fields which are      *
      *                 available in quote review mode to not be      *
      *                 protected for renewal quotes.  These fields   *
      *                 pertain to employer detail, not emplr master. *
      *                                                               *
      * 10/31/14 C:283 RedMine: 4741                                  *
      *                Save the Increased Limits and Deductible       *
      *                Discounts only for the applicable states.      *
      *
      * 10/13/14 C:291 RED 3161                                       *
      *                Change the date definition on the grid from    *
      *                Number to date data type.                      *
      *                                                               *
      * 10/13/14 C:180 Support WA133 85852                            *
      *                Validate FEIN when adding loc/entity           *
      *                                                               *
      * 10/11/14 C:290 Support: WA129 86302                           *
      *                Added edits on expiration date of Owner/Officer*
      *                edit pop-up.                                   *
      *                                                               *
      * 09/04/14 C:279 Project: 114WOE                                *
      *                Added link for Claim Handling Instructions.    *
      *                                                               *
      * 09/03/14 C:291 Support: WA136 85791                           *
      *                Fix NAICS required field.                      *
      *                                                               *
      * 07/23/14 C:136 Support: WA114 85323                           *
      *                Adjust edit for checking duplicate location    *
      *                numbers                                        *
      *                                                               *
      *  07/22/14a C:298 Support: WC099 85969                         *
      *                 Added logic to display inactive staff codes   *
      *                 if value of field has not changed.            *
      *                                                               *
      * 07/22/14 C:136 Support: WA114 86111                           *
      *                Fix owner/officer eff/exp date issue           *
      *                                                               *
      *  07/03/14 C:136 Support: WA133 85650                          *
      *                 Prevent doubling of premium on Premium Info   *
      *                 Endorsement when Adding Division              *
      *                                                               *
      *  04/28/14 C:189 Redmine: 4139                                 *
      *                 Under Additional insured tab, Expiry date     *
      *                 being moved wrongly to effective date.        *
      *                                                               *
      *  03/19/14 C:295 Project: 89WAX                                *
      *                 - To make customer id mandatory field.        *
      *                                                               *
      *  03/14/14 C:291 Redmine: 3801                                 *
      *                 Fixed Type of Policy field heading to display *
      *                 with a golden asterisk as required.           *
      *                                                               *
      *  03/06/14 C:283 Support: WA045 84836                          *
      *                 Correctly populate the expiry dates when      *
      *                 Owner/Officers Include/Exclude is processed.  *
      *                                                               *
      *  02/28/14 C:191 Support: 84952                                *
      *                 Correct error msg ids on location master      *
      *                 screen under department information.          *
      *                                                               *
      *  01/29/14 C:191 Redmine: 2093                                 *
      *                 Added an edit to allow Duplicate FEIN         *
      *                 save in Location Master.                      *
      *                                                               *
      *  01/13/14 C:190 Project: 136WFZ                               *
      *                 - Added an edit to make 4 digit zip code ext  *
      *                   mandatory field.                            *
      *                 - Added checkbox 'Personal Name' for client   *
      *                   '136'.                                      *
      *                                                               *
      *  11/22/13 C:279 Project: 129WSB                               *
      *                 Add Claim Contact field on rcdBillDiv record  *
      *                 format.                                       *
      *                                                               *
      *  11/11/13 C:180 Support: WA090 82964                          *
      *                 Fix inception date of added division.         *
      *                                                               *
      *  10/07/13 C:190 Project: 25WSS                                *
      *                 - Change the program to determine if          *
      *                   Premium or Contribution should be printed   *
      *                   /displayed based on Group setup.            *
      *                                                               *
      *  10/15/13 C:279 Redmine: 2614                                 *
      *                 Division selection for Add Entity/Location    *
      *                 and Add Owner/Office screen.                  *
      *                                                               *
      *  09/19/13 C:189 redmine: 2826                                 *
      *                 Underwriter field should not have PDS* users  *
      *                                                               *
      *  09/19/13 C:136 Support: WA114 83306                          *
      *                 Fix Owner/Officer Percentage Edit             *
      *                                                               *
      *  09/16/13 C:290 Support  WA114 83098                          *
      *                 Added indicator for visibility of SIC Code    *
      *                 asterisk.                                     *
      *                                                               *
      *  09/13/13 C:283 Support  WA133 83281                          *
      *                 Change the Endorsement Process for Entity/Loc *
      *                 add to use '+' in the Alstat. "A" should only *
      *                 be used to designate 'Active'.                *
      *                                                               *
      *  08/29/13 C:180 Support  WA133 83307                          *
      *                 Do not allow leading blank on employer's name *
      *                                                               *
      *  08/28/13 C:287 Project: CI126                                *
      *                 Add division to CIEMPCHG program call         *
      *                                                               *
      *  08/09/13 C:283 Support: WA133 83120                          *
      *                 Use Read with no lock when loading the        *
      *                 Owner/Officer grid.                           *
      *                                                               *
      *  08/05/13 C:283 Support: WA133 82980                          *
      *                 Correct the key to update WAQTP file.         *
      *                                                               *
      *  07/08/13 C:283 Support: WA099 81912                          *
      *                 Do not allow editing the fields on renewal    *
      *                 quotes. There are three feilds on screen      *
      *                 which are disabled based on an indicator.     *
    *                                                               *
      *  07/05/13 C:282 Redmine 3351                                  *
      *                 Owner/Officers Inclued Exclude handling.      *
      *                                                               *
      *  06/17/13 C:283 SUPPORT WA146 82578                           *
      *                 Correctly populate the Optional Headings      *
      *                 on Billing screen.                            *
      *                                                               *
      *  06/14/13 C:180 SUPPORT WA114 82624                           *
      *             283 Populate record status when writing to        *
      *                 Employer 3rd address file WMA3P.              *
    *                                                               *
    *  06/13/13 C:65  Redmine: 3150                                 *
    *                 Default safety fees.                          *
      *                                                               *
      *  05/21/13 C:279 Redmine: 2967                                 *
      *                 Modified the Location Delete Logic.           *
      *                                                               *
      *  05/08/13 C:189 Project: 133WNP                               *
      *                 Chapter should have a golden splat for LUBA   *
      *                                                               *
      *  05/06/13 C:279 Redmine: 2746                                 *
      *                 Modified to default Waiver fee and charges.   *
      *                                                               *
      *  03/22/13 C:65  Project: WC2005 (Redmine 2328)                *
      *                 Add API WA085R to change EFT payee name/FEIN, *
    *                 a re-implement of the project that was done   *
    *                 in W0050R02 employer master maintenance.      *
    *                                                               *
    *  03/06/13 C:136 Redmine 2516                                  *
    *                 Populate ELNWRW (renewal status) on policy    *
    *                 year add                                      *
    *                                                               *
      *  01/22/13  C:277  PROJECT: 133WNZ                             *
      *                   Change Procedure PrcLocEntWdw               *
     *                                                               *
    *  01/15/13 C:191 Redmine #2093                                 *
    *                 Fix Fein edit to run only once                *
    *                                                               *
    *  09/14/12 C:282 Project 114WGD                                *
    *              -  Add new checkbox field "Exclude from DCRx     *
    *                 Employer Export" on the Billing division tab  *
    *                 that will be visible to client 114 only.      *
    *                                                               *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    *===============================================================*

     fwpilocmst cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(SflLocYr:RrnLocYr)
     f                                     sfile(SflOwnOff:RrnOwnOff)
     f                                     sfile(Sfl3rdPay:rrn3rdPay)
     f*** begin add ***  05/11/21  ****************************************
     f                                     sfile(SflEftInfM:rrnEftInfM)
     f*** begin del ***  09/30/21  ****************************************
     f***                                  sfile(Sfl3pLog:rrn3pLog)
     f*** End   del ***  09/30/21  ****************************************
     f*** begin add ***  09/30/21  ****************************************
     f                                     sfile(SflOptIn:rrnOptIn)
     f                                     sfile(Sfl3pLog:rrn3pLog)
     f*** End   add ***  09/30/21  ****************************************
     f*** End   add ***  05/11/21  ****************************************
     f                                     usropn
     fwtpoldtl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdRateSt)
     f                                     include(rcdBSCDChg)
     f                                     include(rcdSpreadF)
     f                                     include(rcdRateChg)
     f                                     include(ElementSFL)
     f                                     include(rcdAnnivDt)
     f                                     include(rcdClaimDt)
     f                                     Sfile(ElementSFL:rrnElement)
     f                                     usropn
     fwmalp     uf a e             disk    rename(pal01:pal0p)
     f                                     infds(alfds)
     fwmall     if   e           k disk    rename(pal01:pal00)
     f                                     prefix(a0:2)
     f                                     infds(a0fds)
     fwmall4    uf   e           k disk    rename(pal01:pal04)
     fwmanl     if   e           k disk
     fwmanp     uf a e             disk    rename(pan01:pan0p)
     fwmemp     uf a e             disk    rename(pem01:pem0p)
     f                                     infds(dbfdsp)
     fwmemph    o    e             disk    rename(pemh01:pemh0p)
     fwmeml     uf   e           k disk    rename(pem01:pem00)
     f                                     infds(dbfds)
     fwmeml12   if   e           k disk    rename(pem01:pem12)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwdell     uf a e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds2)
     FWDELL1    UF   E           K DISK    rename(PEL01:PEL01)
     F*** begin add ***  04/10/23  ******************************************
     FWDELL7    IF   E           K DISK    rename(PEL01:PEL07) prefix(E3:2)
     F*** end add *****  04/10/23  ******************************************
     Fwdell2    UF   E           K DISK    rename(pel01:pel02)
     Fsdacl     if   e           k disk    rename(pac01:pac00)
     FWDF1L2    IF   E           K DISK    rename(PF101:PF102) prefix(L2)
     **** Beg Add - 03/11/22  **************************************************
     f***wmtml     if   e           k disk    rename(ptm01:ptm00)
     fwmtml1    if   e           k disk    rename(ptm01:ptm00)
     **** End Add - 03/11/22  **************************************************
      * Statistical Reporting/Rating file
     fwdrrl1    if   e           k disk
     FWDRRL     IF   E           K DISK    rename(PRR01:PRR00)
     FWMGRL     IF   E           K DISK    rename(PGR01:PGR00)
     FWMRSL     IF   E           K DISK    rename(PRS01:PRS00)
     fwma3l     uf a e           k disk    rename(pa301:pa300)
     fwdldl     uf   e           k disk    rename(pld01:pld00)
     f                                     infds(ldfds0)
     fwmcyp     if   e             disk    rename(pcy01:pcy0p)
     fwmczl     if   e           k disk    rename(pcz01:pcz00)
     fwmoop     uf a e             disk    rename(poo01:poo0p)
     fwmool     uf   e           k disk
     f                                     rename(poo01:poo00)
     f                                     infds(oofds0)
     fwmool7    if   e           k disk    rename(poo01:poo07)
     f                                     prefix(o7:2)
     fwdldp     uf a e             disk    rename(pld01:pld0p)
     fwmdpp     uf a e             disk    rename(pdp01:pdp0p)
     fwmdpl     if   e           k disk    rename(pdp01:pdp00)
     f                                     infds(DPfds0)
     fwmprl     if   e           k disk    rename(ppr01:ppr00)
     fwmprl1    if   e           k disk
     fststl3    if   e           k disk    rename(pst01:pst03)
     fwtcnl1    if   e           k disk
     fwmrel     if   e           k disk    rename(pre01:pre00)
     fwmcpl     if   e           k disk    rename(pcp01:pcp00)
     fwmexl     if   e           k disk    rename(pex01:pex00)
      *** Beg Chg ***** 03/19/24 *******************************
     f*wmenl3    if a e           k disk    rename(pen01:pen03)
     fwmenl3    uf a e           k disk    rename(pen01:pen03)
      *** End Chg ***** 03/19/24 *******************************
     fwmnbl     uf a e           k disk    rename(pnb01:pnb00)
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     fwmdxl     if   e           k disk    rename(pdx01:pdx00)
      *** Beg Chg ***** 06/02/25 ***************************
     f*wmecl1    if   e           k disk
     fwmecl1    uf a e           k disk
     fwmecpcrt  uf a e           k disk    rename(pec01:pec0p_cr)
     f                                     prefix(CR_)
      *** End Chg ***** 06/02/25 ***************************
     fwaqtl7    if   e           k disk    rename(pqt01:pqt07)
     f                                     infds(QTfds7)
     fwaqtp     uf   e             disk    rename(pqt01:pqt0p)
     fwtmyp     if   e             disk    rename(pmy01:pmy0p)
     fwttbl     if   e           k disk    rename(ptb01:ptb00)
     fssylf     if   e           k disk    include(swc01)
     FWTABL     IF   E           K DISK
     F                                     ignore(PRT01)
     F                                     ignore(PCN01)
     FWMLDL1    UF   E           K DISK    rename(PLD01:PLD01)
     FWDDDL1    IF   E           K DISK    rename(PDD01:PDD01)
     FWMCMLC    IF   E           K DISK    rename(PCM01:PCM0C)
     FWTAVL     IF   E           K DISK    rename(PAV01:PAV00)
     FWMPFL     IF   E           K DISK    rename(PPF01:PPF00)
     FWMEALC    IF   E           K DISK    rename(PEA01:PEA0C)
     FWMEAL6    IF   E           k disk    rename(PEA01:PEA06)
     FWMFNL     IF   E           K DISK    rename(PFN01:PFN00)
     FWDF1L     IF   E           K DISK    rename(PF101:PF100)
     FWDFHL     IF   E           K DISK    rename(PFH01:PFH00)
     FWMEPL     IF   E           K DISK    rename(PEP01:PEP00)
     fstctl1    if   e           k disk
     fsmupl1    if   e           k disk
     f*** Beg Add *** 01/25/22 *************************************************
     fsmupl     if   e           k disk    rename(pup01:pup02) Prefix(U2:2)
     f*** End Add *** 01/25/22 *************************************************
     fwtlml1    if   e           k disk    rename(PLM01:PLM01)
     fwtddl2    if   e           k disk    rename(PDD01:PDD02)
     fwdchl     if   e           k disk    rename(pch01:pch00)
     fwmmpl     uf a e           k disk    rename(pmp01:pmp00)
     fwdcxp     o    e             disk    rename(pcx01:pcx0p)
     fwdcxl     if   e           k disk    rename(pcx01:pcx00)
     fwdxhp     o    e             disk    rename(pxh01:pxh0p)
     fwdxhl     if   e           k disk    rename(pxh01:pxh00)
     Fwtp3l     if   e           k disk    Rename(pp301:pp300t)
     F*** Beg Add *** 04/24/25 ****************************************
     Fwmp3p     if   e           k disk    UsrOpn
     F*** End Add *** 04/24/25 ****************************************
     f*** begin chg ***  04/11/23  ****************************************
     F*****wmp3l     if a e           k disk    Rename(pp301:pp300)
     F*** Beg Chg *** 04/24/25 ****************************************
     F***wmp3l     uf a e           k disk    Rename(pp301:pp300)
     Fwmp3l     uf a e           k disk    Rename(pp301:pp300) UsrOpn
     F*** End Chg *** 04/24/25 ****************************************
     f*** end chg *****  04/11/23  ****************************************
     f                                     infds(p3FDS)
     Fwmnxl     if   e           k disk    rename(pnx01:pnx00)
     f*** begin add ***  05/11/21  ****************************************
     f*** begin chg ***  04/11/23  ****************************************
     f*****wmefl6    if   e           k disk    rename(pef01:pef06)
     fwmefl6    uf   e           k disk    rename(pef01:pef06)
     f*** end chg *****  04/11/23  ****************************************
     f                                     infds(dbef6ds)
     f*** end   add ***  05/11/21  ************************************** **
     f*** Beg Add *** 09/30/21 ********************************************
     Fwipsphist if   e             disk
     f*** End Add *** 09/30/21 ********************************************
     f*** Beg Add ***** 03/13/23 ******************************************
     Fwmmul5    uf   e           k disk    rename(pmu01:pmu05)
     f*** End Add ***** 03/13/23 ******************************************
      *
     d SvSfPolSrv      s                   like(EMpsrv) inz
     d SvSfPrmAcc      s                   like(EMpacc) inz
     d SvSfAdtRvw      s                   like(EMarvw) inz
     d SvSfAdtMgr      s                   like(EMamgr) inz
     d SvSfLosRep      s                   like(EMlrep) inz
     d SvSfUndWtr      s                   like(EMuwtr) inz
     d SvClmCnt        s                   like(EMclmc) inz
     d kkDiv           s                   like(OOdiv)
     D keySDACL        s             10
     D inUTYP          s              1
     D inULVL          s              3
     D outRes          s              1
     dxxlrmk           s                   like(ACRMCD) inz
     dxxlusr           s                   like(ACRUSR) inz
     d ##exdt          s             10
     d ##count         s              7
     d ##delid         s             10
     d ##pol#          s             20
     d ##polnam        s             40
     d ##div#          s              5
     d ##divnam        s             40
     d ##clm#          s             15
     d ##clmname       s             20
     d ##doi           s             10
     d ##adjust        s             20
     D svelfyr         s              3
     d svemname        s             40
     d SvDbaNam        s             40
     d SvFedNam        s             40
     d ##Name          s             40
     d LocDWritn       s              1    Inz
       //*** Begin Add *** 05/11/21 ***********************************
     d strEftMod       s              1a   inz
     d strEftTyp       s              1a   inz
     d strEnv          s              1a   inz
     d intEftRrn       s              9  0 inz
     d intEmRrn        s              9  0 inz
     d intSfyr         s              3  0 inz
     d intStRptDiv     s                   like(EFdiv) inz
     d intFyr3p        s                   like(ELfyr) inz
       //*** End   Add *** 05/11/21 ************************
     d*** Begin Add *** 02/22/22 ***********************************
     d isSunz          s               n   Inz
     d*** End   Add *** 02/22/22 ***********************************
     d***  Begin Add  ***  06/23/21  ****************************************
     d OutElRrn        s              9  0 import
     d OutElRrnTm      s              9  0
     d***  End   Add  ***  06/23/21  ****************************************
     d*** Beg Add *** 09/30/21 **********************************************
        Dcl-s strAlias char(500) inz;
     d*** End Add *** 09/30/21 **********************************************
     d*** begin add ***  03/02/23  ******************************************
     d wmempAdded      s               n   Inz
     d*** end add *****  03/02/23  ******************************************
      *** Beg Add ***** 06/02/25 **************************
     d xxfsn           s             15
     d xxlsn           s             30
     d xxlen           s              2  0
     d spacePos        s              2  0
     d maxseq#         s             15  0
      *** End Add ***** 06/02/25 **************************
      *** Begin Add *** 11/11/25 *******************************************
     d cntWIPSP        s              5  0
      *** End   Add *** 11/11/25 *******************************************
      *
     DscreenDS         DS                  Inz
     d scdDbaName
     d scdFedName
     d scdMadd1
     d scdMadd2
     d scdMCity
     d scDMstate
     d scDMzip
     d scdPhone#
     d scdFax#
     d scdTypBus
     d scDSic
     d scDNaic
     d scDSRisk
     d scDSUnEmp#
     d scdPadd1
     d scdPadd2
     d scdPCity
     d scDPstate
     d scDPzip
     d scDCont1
     d scDCont2
     d scDCont3
     d sc#ShftEmp
     d scMPNName
     d scMPNAdd1
     d scMPNAdd2
     d scMPNCity
     d scMPNState
     d scMPNZip
     d scMPNTyp
     d****
     DlocationDS       DS
     d lodDbaName                          like(scdDbaName) Inz
     d lodFedName                          like(scdFedName) Inz
     d lodMadd1                            like(scdMadd1) Inz
     d lodMadd2                            like(scdMadd2) Inz
     d lodMCity                            like(scdMCity) Inz
     d loDMstate                           like(scDMstate) Inz
     d loDMzip                             like(scDMzip) Inz
     d lodPhone#                           like(scdPhone#) Inz
     d lodFax#                             like(scdFax#) Inz
     d lodTypBus                           like(scdTypBus) Inz
     d loDSic                              like(scDSic) Inz
     d loDNaic                             like(scDNaic) Inz
     d loDSRisk                            like(scDSRisk) Inz
     d loDSUnEmp#                          like(scDSUnEmp#) Inz
     d lodPadd1                            like(scdPadd1) Inz
     d lodPadd2                            like(scdPadd2) Inz
     d lodPCity                            like(scdPCity) Inz
     d loDPstate                           like(scDPstate) Inz
     d loDPzip                             like(scDPzip) Inz
     d loDCont1                            like(scDCont1) Inz
     d loDCont2                            like(scDCont2) Inz
     d loDCont3                            like(scDCont3) Inz
     d lo#ShftEmp                          like(sc#ShftEmp) Inz
     d loMPNName                           like(scMPNName) Inz
     d loMPNAdd1                           like(scMPNAdd1) Inz
     d loMPNAdd2                           like(scMPNAdd2) Inz
     d loMPNCity                           like(scMPNCity) Inz
     d loMPNState                          like(scMPNState) Inz
     d loMPNZip                            like(scMPNZip) Inz
     d loMPNTyp                            like(scMPNTyp) Inz
     d****
     DentityDS         DS
     d endDbaName                          like(scdDbaName) Inz
     d endFedName                          like(scdFedName) Inz
     d endMadd1                            like(scdMadd1) Inz
     d endMadd2                            like(scdMadd2) Inz
     d endMCity                            like(scdMCity) Inz
     d enDMstate                           like(scDMstate) Inz
     d enDMzip                             like(scDMzip) Inz
     d endPhone#                           like(scdPhone#) Inz
     d endFax#                             like(scdFax#) Inz
     d endTypBus                           like(scdTypBus) Inz
     d enDSic                              like(scDSic) Inz
     d enDNaic                             like(scDNaic) Inz
     d enDSRisk                            like(scDSRisk) Inz
     d enDSUnEmp#                          like(scDSUnEmp#) Inz
     d endPadd1                            like(scdPadd1) Inz
     d endPadd2                            like(scdPadd2) Inz
     d endPCity                            like(scdPCity) Inz
     d enDPstate                           like(scDPstate) Inz
     d enDPzip                             like(scDPzip) Inz
     d enDCont1                            like(scDCont1) Inz
     d enDCont2                            like(scDCont2) Inz
     d enDCont3                            like(scDCont3) Inz
     d en#ShftEmp                          like(sc#ShftEmp) Inz
     d enMPNName                           like(scMPNName) Inz
     d enMPNAdd1                           like(scMPNAdd1) Inz
     d enMPNAdd2                           like(scMPNAdd2) Inz
     d enMPNCity                           like(scMPNCity) Inz
     d enMPNState                          like(scMPNState) Inz
     d enMPNZip                            like(scMPNZip) Inz
     d enMPNTyp                            like(scMPNTyp) Inz
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
      *
     d dbfdsp          ds
     d  emrrnp               397    400b 0
      *
     d alfds           ds
     d  alrrn                397    400b 0
     d a0fds           ds
     d  a0rrn                397    400b 0
     d WMALPrrn        s              9  0
     d ldfds0          ds
     d  ldrrn                397    400b 0
     d oofds0          ds
     d  oorrn                397    400b 0
     d qtfds7          ds
     d  qtrrn7               397    400b 0
     d DPfds0          ds
     d  DPrrn                397    400b 0
     d p3FDS           ds
     d  p3RRN                397    400b 0

     D mod3rdPay       s              1
     f*** begin add ***  05/11/21  ****************************************
     d dbEf6ds         ds
     d  ef6rrn               397    400b 0
     d intAddEftDiv    s              5  0 inz
     f*** end add *****  05/11/21  ****************************************

     D intCanlDat      s              7  0 Inz(0)
     D intIncpDat      s              7  0 Inz(0)
     D intDatCYMD      s              7  0 Inz(0)
     D intToDate       s              7  0 Inz(0)
     D intCurrDate     s              7  0 Inz(0)

     d***  Begin Add  ***  07/01/21  ********************************
     d node0           s                   Like(yajl_val)
     d i0              s             10i 0 inz
     d key             s             50a   varying
     d val             s                   Like(yajl_val)
     d***  End   Add  ***  07/01/21  ********************************
      *
     D***  Begin Add  ***  05/11/21  *****************************************
     D/copy wPrBank
     D/copy wPrEft
     D***  End   Add  ***  05/11/21  *****************************************
      /copy wprLocMst
      /copy wprLocMain
      /copy wprUsrPrms
      /copy wprTable
      /copy wprGenPop
      /copy wprGroup
     D***  Begin Add  ***  03/11/21  ****************************************
     D/copy wPrPmtSvc
     D***  End   Add  ***  03/11/21  ****************************************
      /copy wPrRateSet
      /copy wprPolErr
      /copy wprPolPop
      /copy wprEmplr
      /copy sprString
      /copy sprDate
      /copy sprGenErr
      /copy WTSearchDs
      /copy sPrAtrium
      /copy sCpyClmHdI
     d*** Beg Add *** 04/24/25 ************************************************
     d/copy sPrFiles
     d*** End Add *** 04/24/25 ************************************************

     d***  Begin Add  ***  07/01/21  ********************************
      /copy sPrJson
     d***  End   Add  ***  07/01/21  ********************************
      /copy sPrMask
      *
      /copy sPrSecure
     D/copy sPrModule
      /copy sPrSql
      //*** Beg Add ***** 06/06/24 ***********************************
      /copy siWebSvcPr
      //*** End Add ***** 06/06/24 ***********************************
     d***  Begin Add  ***  07/01/21  ********************************
      /include yajl_h
     d***  End   Add  ***  07/01/21  ********************************
      *
     d OwnOffShare     pr                  extpgm('W1057R01')
     d   inRRN#                       9  0
     d   inELFL                       1
     d   outErr                       1

     d ComAPI          pr                  extpgm('WA001R')
     d   ##CO#                        3  0
     d   ##FND                        3  0
     d   ##Conm                      40
     d   ##Fnam                      40
     d   ##Lvl                        3  0
     d   ##Type                       1

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Check for daily batch running
     d CheckDailyBat   pr                  extpgm('WA044R')
     d   outRTCD                      1

     d* Get Claims/Admin only status
     dGetClmAdmOnly    pr                  extpgm('SA043R')
     d outClmAdmOnly                  1

     d* Update CiCenter for Employer change
     dCiCEmpChange     pr                  extpgm('CIEMPCHG')
     d inRRN                          9  0 const
     d inType                        10    const
     d inOfid                              like(EMFEIN)
     d inODBA                       100
     d inODiv                         5  0 const options(*nopass)

     d* Get managed care installed flag
     dGetMancFlag      pr                  extpgm('SA001R')
     d outWCIF                        1
     d outFBIF                        1
     d outFL01                        1

     d* Check FEIN in quote env
     dCheckQuoteTaxId  pr                  extpgm('WA032C')
     d inCO#                          3    const
     d inFeinC                        9    const
     d inQTE                          1    const
     d outErr                         1

     d* Check LOC FEIN in quote env
     dCheckQuoteFein   pr                  extpgm('WA032C1')
     d inCO#                          3    const
     d inFeinC                        9    const
     d inEmp#                         9    const
     d inQTE                          1    const
     d outErr                         1
     dGetRenewStatus   pr                  extpgm('WA069R')
     d inCO#                          3  0 const
     d inFND                          3  0 const
     d inEMP#                         9  0 const
     d inFYR                          3  0 const
     d renewFlagValue                 1

     dGetEmployer      pr              n
     d inRRN                          9  0
     d inLock                         1    const options(*nopass)
      //*** Begin Add *** 05/11/21 *****************************************
        Dcl-Pr GetObjLib Extpgm('RTVOBJLIB');
        inObject   char(10)   const;
        inTyp      char(7)    const;
        outLibrary char(10);
       End-Pr;
      //*** End   Add *** 05/11/21 *****************************************
     D***  Begin Add  ***  05/03/23  **************************************
     DRtvEnv           pr                  extpgm('RTVENV')
     D env                            4a
     D***  End   Add  ***  05/03/23  **************************************
     dValidCollat      pr              n
     d inMode                         1    const
     d inRRN                          9  0 const
     d inEmpRRN                       9  0 const
     dValidLocEnt      pr              n
     d inMode                         1    const
     d inLocRrn                       9  0
     dValidOwnOff      pr              n
     d inMode                         1    const
     d inOwnRrn                       9  0
     dValidLocYr       pr              n
     d inMode                         1    const
     d inLDRrn                        9  0
     dValidAddNam      pr              n
     d inMode                         1    const
     d inInsRrn                       9  0
      *
     dValidDept        pr              n
     d inMode                         1    const
     d inDPRrn                        9  0 const

     D RcdRateChgDsp   pr
     D  indValue                      1n   const
      *
     dCheckRepChanges  pr
     dClearBillErrors  pr
     DCheckAppSec      pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTP1                         1    const
     D inCLV1                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D inCTP2                         1    const
     D inCTP3                         1    const
     D inCTP4                         1    const
     D inCTP5                         1    const
     D inCTP6                         1    const
     D inCLV2                         3    const
     D inCLV3                         3    const
     D inCLV4                         3    const
     D inCLV5                         3    const
     D inCLV6                         3    const
     D inCTP7                         1    const
     D inCTP8                         1    const
     D inCTP9                         1    const
     D inCTP0                         1    const
     D inCLV7                         3    const
     D inCLV8                         3    const
     D inCLV9                         3    const
     D inCLV0                         3    const
     d Darts           pr                  extpgm('DA700R')
     d ##key                         40
     d ##comm                       256
     d ##cDat                         8
     d ##dcId                        12
     d ##pNam                        10
     d ##secl                         3
     d ##user                        10
     d ##pgm                         10
     d ##fold                        80
     d ##diFl                         1
     d ##iPgm                        10
     d ##fl01                         1
     d ##fl02                         1
     d ##rmCd                         5
     d ##seq#                         5
     d ##sbyl                         1
     d ##com2                        19
     d ##apId                         2
     d ##iDoc                        12
     d ##iQue                        10
     d ##imgF                         1
     d ##iPth                        10
     d ##pIdx                         5
     d ##qPty                         1
     d ##dcTp                         5
     d ##csId                         2
     d ##code                         2
     d ##key           s             40
     d ##comm          s            256
     d ##cDat          s              8
     d ##dcId          s             12
     d ##pNam          s             10
     d ##secl          s              3
     d ##user          s             10
     d ##pgm           s             10
     d ##fold          s             80
     d ##diFl          s              1
     d ##iPgm          s             10
     d ##fl01          s              1
     d ##fl02          s              1
     d ##rmCd          s              5
     d ##seq#          s              5
     d ##sbyl          s              1
     d ##com2          s             19
     d ##apId          s              2
     d ##iDoc          s             12
     d ##iQue          s             10
     d ##imgF          s              1
     d ##iPth          s             10
     d ##pIdx          s              5
     d ##qPty          s              1
     d ##dcTp          s              5
     d ##csId          s              2
     d ##code          s              2
     d Darts1          pr                  extpgm('DA701R')
     d ##key                         40
     d ##seq#                         5
     d ##cm75                        75
     d ##pNam                        10
     d ##secl                         3
     d ##user                        10
     d ##pgm                         10
     d ##rmCd                         5
     d ##asq#                         5
     d ##cm75          s             75
     d ##asq#          s              5
     d*** Beg Add *** 04/24/25 *********************************************
       Dcl-Pr RunCommand ExtPgm('QCMDEXC');
        inCmd    char(2750) const;
        inCmdLen packed(15:5) const;
       End-Pr;
     d*** End Add *** 04/24/25 *********************************************
      *
     d psds          esds                  extname(szq1p)
     D STA             S             75    DIM(11) CTDATA PERRCD(1)
     d intCo#          s              3  0 inz
     d intFnd          s              3  0 inz
     d SVchap          s                   like(EMchap) inz
     d SVname          s                   like(EMname) inz
     d SVnam2          s                   like(QTname) inz
     d SVfdnm          s                   like(EMfdnm) inz
     d SVtpob          s                   like(EMtpob) inz
     d SVnamp          s                   like(EMnamp) inz
     d SVfdnp          s                   like(EMfdnp) inz
     d SVfein          s                   like(EMFein) inz
     d SVbillDt        s                   like(ELbsdt) inz
     D svBsdt          s                   like(ELBSDT)
     D svBscd          s                   like(ELBSCD)
     D SVdiv           s                   like(EMDIV)
     d intZip6         s              4  0
     d programIn       s             10
     d tabNameIn       s             50
     d modeIn          s             50    inz('E')
     d callIn          s             50    inz(' ')
     d rrnOut          s             50    inz('000000000')
     d selectIn        s           1024
     d iOpen1          s               n   inz
     D compName        s             40
     D fundName        s             40
     D secType         s              1
     D secLevel        s              3  0
     d SVdba           s            100    inz
     d SVExAuto        s              1    inz
     d flgUpdEacd      s              1    inz
     d flgAddAuth      s               n   inz
     d xxCndr          s                   inz like(EXcndr)
     d xxCnst          s                   inz like(EXcnst)
     d ##lvl           s              3  0 inz
     d ##type          s              1    inz
     d ##type1         s              1    inz
     d lastDiv         s              5  0 inz
     d fnRrn           s              9  0 inz
     d svElid          s                   inz like(ALelid)
     d svEfd1          s                   inz like(EMefd1)
     d saveDate        s                   inz like(EMdt)
     d saveTime        s                   inz like(EMtime)
     d saveALDt        s                   inz like(EMdt)
     d saveALTm        s                   inz like(EMtime)
     d saveLDDt        s                   inz like(EMdt)
     d saveLDTm        s                   inz like(EMtime)
     d saveOODt        s                   inz like(EMdt)
     d saveOOTm        s                   inz like(EMtime)
     d saveDPDt        s                   inz like(EMdt)
     d saveDPTm        s                   inz like(EMtime)
     d saveInDt        s                   inz like(EMdt)
     d saveInTm        s                   inz like(EMtime)
     d RrnLocYr        s              9  0 inz
     d RrnOwnOff       s              9  0 inz
     D svOOefdt        s              8  0
     D isDisplay       s               n   inz
     d isFound         s               n   inz
     d rateState       s              2
     d errorMsg        s            100
      *** Beg Add ***** 08/24/21 *********************
     D inELrrn         s              9  0
     d inEntryType     s             30a
     d flgQuoteSv      s               n
     d dsEntryType     ds                  Qualified
     d entryType                     20a
     d rateCode                       3a
     d billPlan                       3p 0
      *** End Add ***** 08/24/21 *********************
     d***  Begin Add  ***  05/03/23  **************************************
     dstrEnvmt         s              4a
     d***  End   Add  ***  05/03/23  **************************************
      *
     D flgOMP          s              1A   Inz('0')
     D xxPeoSovr       s              1A
      //*** Beg Add ***** 06/06/24 **********************************
     d jsonIC          s                   Sqltype(clob:64000) Inz
     d strJsonIC       s          64000a   Varying Inz
     d isInvCloud      s               n   Inz(*Off)
     d isGroupIncluded...
     d                 s               n   Inz(*Off)
     d strAdd1         s             40a
     d strAdd2         s             40a
     d strCity         s             25a
     d strState        s              2a
     d intZip          s              9  0
      //*** End Add ***** 06/06/24 **********************************
     d*
     D                 DS                  inz
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
     D*
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
      *
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
       //***  Begin Add  ***  03/11/21  ***************************
       //==============================================================
       // Never processed. Only used for setting COMMIT
       //==============================================================
     pAAA              b
      /Free
       Exec SQL
        set option commit = *none;
      /End-free
     pAAA              e
       //***  End   Add  ***  03/11/21  ***************************

     p*=====================================================================
     p* LoadBillSel - Load Selection Criterion
     P*=====================================================================
     PLoadBillSel      b
     DLoadBillSel      pi
     D inPState                       2
     D inMState                       2
      *
     DxxState          s              2    inz
      /Free

       Setll (scCO#:scFND) PGR00;
       Dou %Eof;
          Reade (scCO#:scFND) PGR00;
          If %Eof Or GRstat = 'A';
             Leave;
          Endif;
       Enddo;
       If Not %Eof(WMGRL);
          rteDBFile = 'WMGRP Join WMRSP' +
           ' On RSco# = GRco# And GRrtcd = RSrtcd';
          rteChoices = 'GRCO# = ' + %char(scCO#) + ' AND GRFND = ' +
                    %char(scfnd) + ' AND GRSTAT = ''A''' +
                     ' AND RSCO# = ' + %char(scCO#);
       Else;
          rteDBFile = 'WMRSP';
          rteChoices = 'RSCO# = ' + %char(scCO#) + ' AND RSSTAT = ''A''';
       EndIf;
       scLsidSel = 'GTCO# = ' + %char(scCO#) +
        ' AND GTTBID = ''NREL'' AND GTSTAT = ''A''';

       scTobSel = 'GTCO# = ' + %char(scCO#) +
        ' AND GTTBID = ''BUSI'' AND GTSTAT = ''A''';

       scSicSel = 'GTCO# = ' + %char(scCO#) +
        ' AND GTTBID = ''SIC '' AND GTSTAT = ''A''';

       scNaicSel = 'GTCO# = ' + %char(scCO#) +
        ' AND GTTBID = ''NAIC'' AND GTSTAT = ''A''';

       //*** Beg Del *** 03/04/24 ******************************************
       //***scExrtSel = 'GTCO# = ' + %char(scCO#) +
       //*** ' AND GTTBID = ''EXRT'' AND GTSTAT = ''A''';
       //*** End Del *** 03/04/24 ******************************************

       If inPState <> *Blanks;
         xxState = inPState;
       Else;
         xxState = inMState;
       EndIf;
       scCntySel = 'CNCO# = ' + %char(scCO#) + ' AND SUBSTR(CNKEY,1,2) = '''
        + xxState + ''' AND CNSTAT = ''A''';

       scRegSel = 'RECO# = ' + %char(scCO#) + ' AND REFND = '
        + %char(scFnd) + ' AND RESTAT = ''A''';

       scChpSel = 'CPCO# = ' + %char(scCO#) + ' AND CPFND = '
        + %char(scFnd) + ' AND CPSTAT = ''A''';

       scOffSel = 'OFCO# = ' + %char(scCO#)
        + ' AND OFSTAT = ''A''';

       Clear pfcChoices;

       If quote = *on;
          pfcChoices = 'PFCO# = ' + %char(scCO#) + ' AND PFFND = '
           + %char(scFND) + ' AND PFEFDT <= ' + %char(ELPFDT);
       Endif;

       Return;
      /End-Free
     PLoadBillSel      E
     p*=====================================================================
     p* DspBillDiv - DspBillDiv Screen
     p*  Parm1   - Employer RRN
     p*  Parm2   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - En
     p*  Parm3   - Mode(1A) 'A'-Add, 'C'-Change, 'D'-Delete, 'I'-Inquiry, 'M'-Ma
     p*  Returns - success indicator
     P*=====================================================================
     pDspBillDiv       b                   Export
     dDspBillDiv       pi              n
     d inEMrrn                        9  0
     d in_Call                        3    const
     d inMode                         1
      *
     d dsLocBillDiv  e ds                  extname(wpilocmst:rcdBillDiv) inz
     d ptrLocBillDiv   s               *   inz
     d inCall          s              3
     d isError         s               n   inz
     d xxFyr           s              3  0 inz
     d xxDelResp       s               n   inz
     d ExitNow         s               n   inz
     d iOpen           s               n   inz
     d xxRtcd          s               n   inz
     d qteMode         s               n   inz
     d begDate         s              8  0
     d endDate         s              8  0
     d bscd            s              3  0
     d inStRrn         s              9  0 inz
     d*** begin add ***  04/12/23  *************************************
     d xrrn            s              9  0 inz
     d*** end add *****  04/12/23  *************************************
      *
     C     keyWMEML      KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C     keyWMEAL6     Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    ELEMP#
     C                   Kfld                    ELDIV
     C                   Kfld                    ELFYR
      *
      /Free

        inCall = in_Call;
        //*** Beg Add ***** 08/24/21 *****************
        Clear dsEntryType;
        //Populate entryType
        Select;
        When inCall = 'QTE' and inMode = 'M';                 //Add Quote
           dsEntryType.entryType = 'QTE Add Policy';
        When inCall = 'QTE' and inMode = 'A';                 //Add division: Qu
           dsEntryType.entryType = 'QTE Add Division';
        When inCall = 'LIV' and inMode = 'M';                 //Add policy
           dsEntryType.entryType = 'LIV Add Policy';
        When inCall = 'LIV' and inMode = 'A';                 //Add division: Po
           dsEntryType.entryType = 'LIV Add Division';
        When inCall = 'DIV' and inMode = 'A';                 //Add Division: Po
           dsEntryType.entryType = 'LIV Add Division';
        When inCall = '190' and inMode = 'A';                 //Add Division: Mi
           dsEntryType.entryType = 'LIV Add Division';
        Endsl;
        //*** End Add ***** 08/24/21 *****************

        xxRtcd = *Off;
        If inCall <> 'QTE' And inCall <> 'LOC' And
         inCall <> '110' And inCall <> '120' And
         inCall <> '130' And inCall <> '140' And
         inCall <> '180' And inCall <> '190' And
         inCall <> 'DIV' and inCall <> 'LIV';
          Return xxRtcd;
        EndIf;

        If inMode <> 'A' And inMode <> 'C' And inMode <> 'D' And
         inMode <> 'M' And inMode <> 'I';
          Return xxRtcd;
        Endif;

        qteMode = *off;
        //*** begin add ***  03/02/23  *****************************************
        wmempAdded = *off;
        //*** end add *****  03/02/23  *****************************************

        If inCall = 'QTE';
           qteMode = *on;
        Endif;

        If inCall = 'LIV';
           inCall = 'QTE';
        Endif;

        Clear xxFyr;

        If inCall = 'QTE' Or inCall = '190' Or inCall = 'DIV';
           Chain inEMrrn PEL0P;
           Chain(n) keyWMEML PEM00;
           xxFyr   = ELFyr;
           inEMrrn = dbrrn;
        Else;
          If not GetEmployer(inEMrrn);
            Return xxRtcd;
          EndIf;
        Endif;

        chgMode = *Off;
        addMode = *Off;
        Select;
        When inMode = 'A' Or inMode = 'M';
          addMode = *On;
        When inMode = 'C';
          chgMode = *On;
        EndSl;

        If inMode = 'A';
          SetGt (EMco#:EMfnd:EMemp#) pem00;
          ReadPe(n) (EMco#:EMfnd:EMEmp#) pem00;
          scCo# = EMco#;
          scFnd = EMfnd;
          scEmp# = EMemp#;
          lastDiv = GetLastDivision(EMco#:EMfnd:EMemp#);
          scFyr = xxFyr;
          scFyr = xxFyr;
        Endif;

        If not %Open(WpiLocMst);
          Open wpiLocMst;
          iOpen = *On;
        Endif;
        Clear dsLocBillDiv;
        Clear dsInitData;
        ptrLocBillDiv = %Addr(dsLocBillDiv);
        ptrInitData = %Addr(dsInitData);

        If qteMode;
          flgQte = 'Q';
        Else;
          flgQte = ' ';
        Endif;

        scFlgQte = flgQte;

        If not InitBillDiv(inEMrrn:inMode:ptrLocBillDiv:ptrInitData:xxFyr);
          Return xxRtcd;
        Endif;
        dsInitData = dsB;
        If inMode = 'M';
          svBillDt = ELPFDT;
        Endif;
        If inMode = 'A';
          Exsr InitializeAddMode;
        Else;
          If not LoadBillDiv(inEMrrn :ptrLocBillDiv :inCall :inMode :xxFyr);
            Return xxRtcd;
          Endif;
        EndIf;
        Exsr SetDisplayIndicators;
        buttonTop = 1050;
        mod3rdPay = IsModEnabled('Third Party Payment');
        mod3rdPayQ = *off;
        Select;
        When inCall = 'QTE' Or inCall = 'DIV';
          //*** Beg Chg *** 12/22/21 ****************************************
          //***scPnlHght = 1030;
          scPnlHght = 1070;
          //*** End Chg *** 12/22/21 ****************************************
          If mod3rdPay = *on;
             mod3rdPayQ = *on;
             //*** Begin Change *** 05/11/21 *****************************
             //buttonTop = 1220;
             //scPnlHght = 1200;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop = 1530;
             //***scPnlHght = 1660;
             buttonTop = 1570;
             scPnlHght = 1700;
             //*** End Chg *** 12/22/21 ****************************************
             //*** End   Change *** 05/11/21 *****************************
          Endif;
          //*** Beg Add *** 10/05/21 *******************************************
          //*** Beg Chg *** 12/22/21 *******************************************
          //***TptyHdTop  = 1285;
          //***TptyGrdTop = 1315;
          //***TptyMsTop  = 1290;
          TptyHdTop  = 1325;
          TptyGrdTop = 1355;
          TptyMsTop  = 1330;
          //*** End Chg *** 12/22/21 *******************************************
          If mod3rdPay <> *on and EFT = *on;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop = 1245;
             //***scPnlHght = 1220;
             buttonTop = 1285;
             scPnlHght = 1260;
             //*** End Chg *** 12/22/21 ****************************************
          Endif;
          If mod3rdPay = *on and EFT = *on;
             //*** Beg Chg  ***  09/30/21  *************************
             //*** buttonTop = 1480;
             //*** scPnlHght = 1450;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop = 1710;
             //***scPnlHght = 1720;
             buttonTop = 1750;
             scPnlHght = 1760;
             //*** End Chg *** 12/22/21 ****************************************
             //*** End Chg  ***  09/30/21  *************************
          Endif;
          If mod3rdPay = *on and EFT = *off;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***TptyHdTop  = 1020;
             //***TptyGrdTop = 1050;
             //***TptyMsTop  = 1027;
             TptyHdTop  = 1060;
             TptyGrdTop = 1090;
             TptyMsTop  = 1067;
             //*** End Chg *** 12/22/21 ****************************************
             //*** Beg Chg  ***  09/30/21  *************************
             //*** buttonTop  = 1245;
             //*** scPnlHght  = 1220;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop = 1710;
             //***scPnlHght = 1720;
             buttonTop = 1750;
             scPnlHght = 1760;
             //*** End Chg *** 12/22/21 ****************************************
             //*** End Chg  ***  09/30/21  *************************
          Endif;
          //*** End Add *** 10/05/21 *******************************************
          flgShwWdw = *On;
          flgOverLay = *Off;
          Midend = *off;
          quoteDiv = *Off;
          quote = *on;
          If inCall = 'DIV';
            quoteDiv = *Off;
            scQCanL = 1003;
          Endif;

          scPnlTitle = 'Billing Division';
          Select;
          When inMode = 'M';
            scPnlTitle = %Trim(scPnlTitle) + ' Master Add';
            quoteDiv = *On;
             //*** Begin Change *** 05/11/21 *****************************
            //scQCanL = 900;
            scQCanL = 885;
             //*** End   Change *** 05/11/21 *****************************
          When inMode = 'C';
            scPnlTitle = %Trim(scPnlTitle) + ' Edit';
            quoteDiv = *Off;
            scQCanL = 1000;
          When inMode = 'A';
            scPnlTitle = %Trim(scPnlTitle) + ' Add';
            quoteDiv = *On;
             //*** Begin Change *** 05/11/21 *****************************
            //scQCanL = 900;
            scQCanL = 885;
             //*** End   Change *** 05/11/21 *****************************
          When inMode = 'D';
            scPnlTitle = %Trim(scPnlTitle) + ' Delete';
            quote = *off;
            quoteDiv = *off;
          EndSl;

          If inCall = 'QTE';
          Else;
            quoteDiv = *Off;
            scQCanL = 1000;
          Endif;
        When inCall = 'LOC';
          SCPnlTitle = 'Location Master Information';
          flgOverLay = *On;
          flgShwWdw = *Off;
          flgQte = ' ';
          quote = *off;
          Midend = *off;
        When inCall = 'LCQ';
          SCPnlTitle = 'Location Master Information';
          flgOverLay = *On;
          flgShwWdw = *Off;
          flgQte = 'Q';
          quote = *off;
          Midend = *off;
        When inCall = '190';
          SCPnlTitle = 'Addition of Division Endorsement';
          //*** Bed Add *** 01/22/22 ****************************************
          If mod3rdPay = *on;
             mod3rdPayQ = *on;
          Endif;
          //*** End Add *** 01/22/22 ****************************************
          //*** Beg Chg *** 12/22/21 *******************************************
          //***scPnlHght = 1030;
          scPnlHght = 1070;
          //*** Beg Chg *** 12/22/21 ****************************************
          flgShwWdw = *On;
          flgOverLay = *Off;
          flgQte = ' ';
          quote = *on;
          quoteDiv = *Off;
          scQCanL = 1000;
          In *Lock *dtaara;
          Out *dtaara;
          scPfdt = LDEEDT;
          Midend = *off; //This controls Cancel/Next buttons in middle of the pa
          //*** Beg Add *** 10/05/21 *******************************************
          If EFT = *on;
             //*** Beg Chg  ***  09/30/21  *************************
             //*** buttonTop  = 1245;
             //*** scPnlHght  = 1220;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop = 1710;
             //***scPnlHght = 1720;
             buttonTop = 1750;
             scPnlHght = 1760;
             //*** End Chg *** 12/22/21 ****************************************
             //*** End Chg  ***  09/30/21  *************************
          Endif;
          //*** End Add *** 10/05/21 *******************************************
          //*** begin add ***  03/08/23  ***************************************
          If mod3rdPay <> *on and EFT = *on;
             buttonTop = 1285;
             scPnlHght = 1260;
          Endif;
          If mod3rdPay = *on and EFT = *on;
             buttonTop = 1750;
             scPnlHght = 1760;
          Endif;
          If mod3rdPay = *on and EFT = *off;
             TptyHdTop  = 1060;
             TptyGrdTop = 1090;
             TptyMsTop  = 1067;
             buttonTop = 1750;
             scPnlHght = 1760;
          Endif;
          //*** end add *****  03/08/23  ***************************************
        Other;
          Select;
          When inCall = '110';
            SCPnlTitle = 'Employer Name Change Endorsement';
          When inCall = '120';
            SCPnlTitle = 'Mailing Address Endorsement';
          When inCall = '130';
            SCPnlTitle = 'Physical Address Endorsement';
          When inCall = '140';
            SCPnlTitle = 'Legal Status Change Endorsement';
          When inCall = '180';
            SCPnlTitle = 'Federal ID Change Endorsement';
          EndSl;
          //*** Beg Chg *** 12/22/21 *******************************************
          //***scPnlHght = 815;
          scPnlHght = 855;
          //*** End Chg *** 12/22/21 *******************************************
          flgShwWdw = *On;
          flgOverLay = *Off;
          flgQte = ' ';
          quote = *off;
          Midend = *on;
        EndSl;

        // This (quote) was the original visibility indicator
        isDspPrCar = quote;
        // Do not display prior carrier information
        isDspPrCar = *off;
       // save values from state reporting division
       scsvBSCD = scBSCD;
       scsvBSDT = scBSDT;
       // Default Billing Plan to Blank and billing start date to the inception
       // of the new division if if the division's policy term doesn't match the
       // reporting division's and policy is not summarized
       If scARSM <> 'Y';
          If scPfdt <> dtMDYY(ELPFDT);
             //*** Beg Del ***** 01/19/23 *********************
             //***scBSCD = *Zeros;
             //*** End Del ***** 01/19/23 *********************
             scBSDT = scPFDT;
          Endif;
       Endif;

        DoU ExitNow = *On;
          LoadBillSel(scPState:scMState);
          Exsr SaveAddress;
          Select;
          When inMode = 'D';
            Write rcdBillDiv;
            xxDelresp = PopTwoBtn('Do you want to delete this division?':
             'Confirm Delete':'Yes':'No');
            Select;
            When xxDelResp = *On;
              If DeleteBillDiv(inEMrrn:inCall);
                  PopOneBtn('Division deleted!': 'Delete');
                  xxRtcd = *On;
                Else;
                  PopOneBtn('Delete failed!': 'Delete');
                  xxRtcd = *Off;
                Endif;
                ExitNow = *On;
            When xxDelResp = *Off;
              xxRtcd = *On;
              ExitNow = *On;
            EndSl;
          Other;
            GetClientId(ClientID);
            If ClientID = '133';
               isLuba = *on;
            Endif;
            //*** Begin Add *** 02/22/22 ***********************************
            If ClientID = '150';
               isSunz = *on;
            EndIf;
            //*** End   Add *** 02/22/22 ***********************************
            ExFmt rcdBillDiv;
          EndSl;
          //*** Beg Add ***** 11/23/21 *********************
          // Value generated in javascript is also saved in scBsCdtmp
          If scBsCd = 0 and scBsCdtmp <> 0;
             scBsCd = scBsCdtmp;
             scBsCdtmp = 0;
          Endif;
          //*** End Add ***** 11/23/21 *********************
          Select;
          When lossClick = *On;
            ValidBillDiv(inMode:ptrLocBillDiv:inEMrrn);
            lossClick = *off;
            isError = *on;
          When BtnPSave = *On;
            //*** Beg Add ***** 08/24/21 *********************
            dsEntryType.rateCode = scRtCd;
            dsEntryType.billPlan = scBsCd;
            //*** End Add ***** 08/24/21 *********************
            Exsr ProcessSave;
          When BtnPSvNxt = *On;
            //*** Beg Add ***** 08/24/21 *********************
            dsEntryType.rateCode = scRtCd;
            dsEntryType.billPlan = scBsCd;
            //*** End Add ***** 08/24/21 *********************
            Exsr ProcessSave;
            If ExitNow = *On;
              PopOneBtn('Division ' + %editc(emdiv: 'X') + ' has been added.':
               'Save/Add Next':'Enter');
              ExitNow = *Off;
              If inMode = 'M';
                inMode = 'A';
                AddDiv = *On;
              Endif;
              Exsr InitializeAddMode;
            Endif;
             //*  Call billing auxiliary program if billing type selected
             When auxSelect = *on;
                If scBSDT <> 0;
                   begDate = scBSDT;
                Else;
                   begDate = scPFDT;
                Endif;
                endDate = scPTDT;
                bscd = BillAuxiliary(scBSTY:begDate:endDate:fnRRN);
                If bscd <> 0;
                   scBSCD = bscd;
                Endif;
                Clear scBSTY;
                auxSelect = *off;
          //*** Begin Add   05/11/21 ****************************************
          When  BtnAddEftM = *On;
             If inMode = 'A' or inMode = 'M' or inMode = 'C';
                If inMode = 'A';           // run processSave only on Add mode
                   //*** Beg Add ***** 08/24/21 ********************************
                   flgQuoteSv = Quote;
                   Quote = *Off;
                   //*** End Add ***** 08/24/21 ********************************
                   Exsr ProcessSave;      //Add employer master before adding EF
                   //*** Beg Add ***** 08/24/21 ********************************
                   Quote = flgQuoteSv;
                   //*** End Add ***** 08/24/21 ********************************
                //*** begin add ***  03/02/23  *********************************
                   wmempAdded = *on;
                //*** end add *****  03/02/23  *********************************
                   If ExitNow = *On;
                      ExitNow = *Off;
                   Endif;
                Endif;
                BtnAddEftM = *Off;
                strEftMod = 'A';
                strEftTyp = 'E';
                intEftRrn = 0;
                intSfyr = ELfyr;
             //*** begin chg ***  04/05/23  ************************************
             //*****   EmpDivName(ELemp#:ELdiv:intEmRrn:ELfnd:ELco#);
                EmpDivName(ELemp#:scDiv:intEmRrn:ELfnd:ELco#);
             //*** end chg *****  04/05/23  ************************************
                EftAdd(strEftMod:strEftTyp:ELfnd:intEftRrn:intEmRrn:strEnv:
                 intSfyr);
             Endif;
          //*** End Add     05/11/21 ****************************************
          When BtnPCancel = *On;
            if quote = *on and inMode = 'M';
               Exsr AbortMaster;
            Else;
            //*** begin add ***  04/11/23  *************************************
               If inMode = 'A';
                  Exsr DeleteAdd;
               Endif;
            //*** end add *****  04/11/23  *************************************
               ExitNow = *On;
               xxRtcd = *Off;
            Endif;
          Other;
          //*** Begin Change 05/11/21 ****************************************
            //isError = not ValidBillDiv(inMode:ptrLocBillDiv:inEMrrn);
             ProcessEftInfoM(inMode);
             Select;
             When lnkEditEft = *On;
                lnkEditEft = *Off;
             When lnkBankEft = *On;
                lnkBankEft = *Off;
             When lnkPreNote = *On;
                lnkPreNote = *Off;
             //*** begin del ***  09/30/21  ************************************
             //*** When lnk3pLog   = *On;
             //***    lnk3pLog = *Off;
             //*** end   del ***  09/30/21  ************************************
             Other;
                isError = not ValidBillDiv(inMode:ptrLocBillDiv:inEMrrn);
             EndsL;
          //*** End Change  05/11/21 ****************************************
          EndSl;
          //*** Begin  Add   05/11/21 ****************************************
          //Load EFT subfile
          If ExitNow = *Off;
             rrnEftInfM = 0;
             clrEftInfM = *On;
             dspEftInfM = *Off;
             Write rcdBillDiv;
             clrEftInfM = *Off;
             dspEftInfM = *On;
             LoadEftInfoM(flgQte:inMode);
          Endif;
          //*** End    Add   05/11/21 ****************************************
        EndDo;
        If %Open(WpiLocMst) And iOpen;
          Close wpiLocMst;
        Endif;
        Return xxRtcd;


       //***********************************************************************
       // InitializeAddMode
       //***********************************************************************
       BegSr InitializeAddMode;
         //*** Begin Add *** 05/11/21 *****************************
         Chain (scCo#:scFnd) PFN00;
         //*** begin add *** 04/11/23 *****************************
         lastDiv = GetLastDivision(EMco#:EMfnd:EMemp#);
         If FNAutd <> 'Y';
            Exsr AssignDivNum;
         Endif;
         //*** end add ***** 04/11/23 *****************************
         If FNAutd = 'Y';
           intAddEftDiv = lastDiv + 1;
         Else;
           intAddEftDiv = scDivision;
         Endif;
         //*** End   Add *** 05/11/21 *****************************
         stateRepDiv = GetStRptDv(scCo#:scFnd:scEmp#:inStRrn);
         If not LoadBillDiv(inStRrn :ptrLocBillDiv :inCall :inMode :xxFyr);
           Return xxRtcd;
         Endif;
         Chain (scCo#:scFnd) PFN00;
         If FNAutd = 'Y';
           scDiv = lastDiv + 1;
         Else;
           scDiv = scDivision;
         Endif;
         scxDiv = scDiv;
         //*** begin add ***  04/11/23  ***************************************
         //* Create shell records for the division being added in WMEMP and WDEL

         Clear pem0p;
         EMCo# = scCo#;
         EMFnd = scFnd;
         EMEmp# = scEmp#;
         EMDiv = scxDiv;
         EMName = 'New Division Add';
         EMMad1 = 'New Division Address';
         EMMCty = 'New City';
         EMMSt  = scmstate;
         EMMZip = scmzip;
         EMEfd1 = ELPfdt;
         EMIPgm = Q1PGM;
         EMIUsr = Q1USER;
         EMITim = Timeto6(SystemTime());
         EMIDt  = Dateto7(SystemDate());
         Write pem0p;


         //*** Beg Chg ***** 09/25/24 *************************************
         //***Clear pel0p;
         Clear pel00;
         //*** End Chg ***** 09/25/24 *************************************
         ELCo# = scCo#;
         ELFnd = scFnd;
         ELEmp# = scEmp#;
         ELDiv = scxDiv;
         ELFyr = scFyr;
         ELPfdt = EMEfd1;
         ELPtdt = EMEfd1;
         ELRtcd = scRtcd;
         //*** Beg Add ***** 02/27/25 *************************************
         ELsrpr = 'P';
         //*** End Add ***** 02/27/25 *************************************
         //*** Bed Add ***** 03/28/25 *************************************
         ELexrt = '3';
         If clientID = '150';
            ELexrt = '2';
            ELncci = 'Y';
         Endif;
         //*** End Add ***** 03/28/25 *************************************
         ELIPgm = Q1PGM;
         ELIUsr = Q1USER;
         ELITm  = Timeto6(SystemTime());
         ELIDt  = Dateto7(SystemDate());
         Write pel00;

         wmempAdded = *on;
         inEMRRN = GetEmpRRN(EMEmp#:EMDiv:EMFnd:EMCo#);
         //*** end add *****  04/11/23  ***************************************
         //*** begin add ***  04/12/23  ***************************************
         //* Update 3rd party subfile with new RRN for employer master.
         For xrrn = 1 to rrn3rdPay;
            Chain xrrn sfl3rdPay;
            If %found;
               scPayEMRRN = inEMRRN;
               Update sfl3rdPay;
            Endif;
         Endfor;
         //*** end add *****  04/12/23  ***************************************
         scStRep = 'N';
         Clear scNonMl ;
         Clear scNonPh ;
         Clear pem0p;
         Clear pel0p;
       Endsr;
       //***********************************************************************
       // AbortMaster - Provide confirmation window if master add is cancelled.
       //***********************************************************************
       BegSr AbortMaster;

         xxDelresp = PopTwoBtn('Do you want to abort the entry and exit?':
             'Abort Confirmation':'Confirm':'Cancel');
         Select;
         When xxDelResp = *On;
            ExitNow = *On;
            xxRtcd = *Off;
         When xxDelResp = *Off;
            isError = *On;
         EndSl;

       Endsr;
       //*** begin add ***  04/11/23  *************************************
       //***********************************************************************
       // DeleteAdd - Remove shell records created at beginning of division add.
       //***********************************************************************
       BegSr DeleteAdd;

          If wmempAdded;

             //* Employer Master
             Chain (scCo#:scFnd:scEmp#:scxDiv) pem00;
             If %found;
                Delete pem00;
             Endif;

             //* Employer Detail
             Chain (scCo#:scFnd:scEmp#:scxDiv:scFyr) pel00;
             If %found;
                Delete pel00;
             Endif;

             //* Eft
             Setll (scCo#:scFnd:scEmp#:scxDiv) pef06;
             Reade (scCo#:scFnd:scEmp#:scxDiv) pef06;
             Dow not %eof;
                Delete pef06;
                Reade (scCo#:scFnd:scEmp#:scxDiv) pef06;
             Enddo;

             //* 3rd Party Pay
             Setll (scCo#:scFnd:scEmp#:scxDiv) pp300;
             Reade (scCo#:scFnd:scEmp#:scxDiv) pp300;
             Dow not %eof;
                Delete pp300;
                Reade (scCo#:scFnd:scEmp#:scxDiv) pp300;
             Enddo;

          Endif;

       Endsr;
       //***********************************************************************
       // AssignDivNum - Request division # when not auto-assigned.
       //***********************************************************************
       BegSr AssignDivNum;

          //* Initialize window/value.
          Clear msDivision;
          Clear erDivision;
          scDivision = lastDiv + 1;

          //* Display popup until cancel or next pressed.
          Dou btnDivCnl = *on or btnDivNxt = *on;
             Clear btnDivCnl;
             Clear btnDivNxt;
             Exfmt rcdAssignD;
             //* Verify division # is available.
             If btnDivNxt = *on;
                erDivision = *Off;
                msDivision = ' ';
                Chain(n)(scCo#:scFnd:scEmp#:scDivision) pem00;
                If %found();
                   erDivision = *On;
                   msDivision = 'WCR4829';
                   btnDivNxt = *off;
                Endif;
             Endif;
          Enddo;

          //* Return out of procedure if cancelling.
          If btnDivCnl = *on;
             ExitNow = *on;
             If %Open(WpiLocMst) And iOpen;
                Close WpiLocMst;
             Endif;
             Return *off;
          Endif;
       Endsr;
       //*** end add *****  04/11/23  *************************************
       //***********************************************************************
       // ProcessSave
       //***********************************************************************
       BegSr ProcessSave;
         If ValidBillDiv(inMode:ptrLocBillDiv:inEMrrn);
           If UpdateBillDiv(ptrLocBillDiv:inEMrrn:inMode:flgQte);
             ExitNow = *On;
             xxRtcd = *On;
           EndIf;
         EndIf;
       Endsr;
       //***********************************************************************
       // SaveAddress
       //***********************************************************************
       BegSr SaveAddress;
         svMCity = scMCity;
         svMSt   = scMState;
         svPCity = scPCity;
         svPSt   = scPState;
       Endsr;
       //***********************************************************************
       // SetDisplayIndicators - Subroutine
       //***********************************************************************
       BegSr SetDisplayIndicators;
       isDisplay = *off;
       If inCall = 'QTE' and GetRnwlQte(ELco#:ELfnd:ELemp#:ELdiv) = 'Y' and
          inMode <> 'A' and
          GetRnwlQteDiv(ELco#:ELfnd:ELemp#:ELdiv) <> 'Y';
          isDisplay = *on;
       Endif;

       Select;
       When inMode='D' or isDisplay = *on;
         dspMod = *On;
         protBSCD = *on;
         dspQte = *On;
         If inMode <> 'D';
            dspQte = *off;
            protBSCD = *off;
         Endif;
         SCe110 = *On;
         SCe120 = *On;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *On;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *On;
         SCe140 = *On;
         SCe180 = *On;
       When inMode='A' Or inMode = 'C';
         dspQte = *Off;
         dspMod = *Off;
         protBSCD = *off;
         SCe110 = *Off;
         SCe120 = *Off;
         //*** Beg Add ***** 07/11/24 ******************************
         SCe120_1 = *Off;
         //*** End Add ***** 07/11/24 ******************************
         SCe130 = *Off;
         SCe140 = *Off;
         SCe180 = *Off;

         If inMode = 'C';
            xxxDTE = DateTo7(SystemDate);
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

         Select;
         When inCall = '110';
           dspMod = *On;
           protBSCD = *on;
           SCe110 = *Off;
           SCe120 = *On;
           //*** Beg Add ***** 07/11/24 ******************************
           SCe120_1 = *On;
           //*** End Add ***** 07/11/24 ******************************
           SCe130 = *On;
           SCe140 = *Off;
           SCe180 = *Off;
         When inCall = '120';
           dspMod = *On;
           protBSCD = *on;
           SCe110 = *On;
           SCe120 = *Off;
           //*** Beg Add ***** 07/11/24 **************************
           If scPState <> '';
              SCe120_1 = *On;
           Else;
              SCe120_1 = *Off;
           Endif;
           //*** End Add ***** 07/11/24 **************************
           SCe130 = *On;
           SCe140 = *On;
           SCe180 = *On;
         When inCall = '130';
           dspMod = *On;
           protBSCD = *on;
           SCe110 = *On;
           SCe120 = *On;
           //*** Beg Add ***** 07/11/24 **********************
           SCe120_1 = *Off;
           //*** End Add ***** 07/11/24 **********************
           SCe130 = *Off;
           SCe140 = *On;
           SCe180 = *On;
         When inCall = '140';
           dspMod = *On;
           protBSCD = *on;
           SCe110 = *On;
           SCe120 = *On;
           //*** Beg Add ***** 07/11/24 **********************
           SCe120_1 = *On;
           //*** End Add ***** 07/11/24 **********************
           SCe130 = *On;
           SCe140 = *Off;
           SCe180 = *On;
         When inCall = '180';
           dspMod = *On;
           protBSCD = *on;
           SCe110 = *On;
           SCe120 = *On;
           //*** Beg Add ***** 07/11/24 **********************
           SCe120_1 = *On;
           //*** End Add ***** 07/11/24 **********************
           SCe130 = *On;
           SCe140 = *On;
           SCe180 = *Off;
         Endsl;
       Endsl;
       // Deleted to prevent division# field from printing
       EndSr;
      /End-Free
     PDspBillDiv       E
     p*=====================================================================
     p* ClearBillDiv - ClearBillDiv Screen
     p*  Parm1   - Pointer to format DS
     P*=====================================================================
     PClearBillDiv     B                   Export
     DClearBillDiv     PI              n
     D inPtrBillDiv                    *
      *
      /Free
       Return *On;
      /End-Free
     PClearBillDiv     E
     p*=====================================================================
     p* DeleteBillDiv - ClearBillDiv Screen
     p*  Parm1   - RRN (9A) Employer master (WMEMP)
     p*  Parm2   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - En
     P*=====================================================================
     PDeleteBillDiv    b                   Export
     DDeleteBillDiv    pi              n
     D inrrn                          9  0
     D inCall                         3    Const
      *
     d***  Begin Add  ***  07/01/21  ********************************
     D ioJson          s               a   len(1000) varying
     d strErrorMsg     s            500a   varying
     d***  End   Add  ***  07/01/21  ********************************
      *
      /Free
       If not GetEmployer(inRRN);
         Return *Off;
       EndIf;
      /End-Free
      *
      *  CALL PROGRAM TO DELETE EMPLOYER
      *
      //***  Begin Chg  ***  07/01/21  ******************
     C**********         CALL      'W0050R02'                           99
     C**********         PARM                    #1KC              1
     C**********         PARM                    #1KL              1
     C**********         PARM      'E'           #1MODE            1
     C**********         PARM      inRRN         #1RRN             9 0
     C**********         PARM      flgQte        #1QTE             1
      /Free
       ioJson = '{';
       ioJson += BuildJsonNode('rrn':%char(inRRN)) + ',';
       ioJson += BuildJsonNode('display':'*YES') + ',';
       ioJson += BuildJsonNode('quote':flgQte) + ',';
       ioJson = %scanrpl(',':'}':ioJson:%len(ioJson)-1);
       If not DeleteEmployer(ioJson);
          Return *off;
       Endif;
       node0 = yajl_string_load_tree(ioJson:strErrorMsg);
       Dow YAJL_object_loop(node0:i0:key:val) and strErrorMsg = *blank;
          Select;
          When key = 'ind';
          When key = 'msgId';
          When key = 'msg';
           //message = YAJL_get_string(val);
          Endsl;
       Enddo;
       yajl_tree_free(node0);
      /End-free
      //***  End   Chg  ***  07/01/21  ******************
      /Free
       //CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
       CheckRepChanges();
       Return *On;
      /End-Free
     PDeleteBillDiv    E
     p*=====================================================================
     p* LoadBillDiv - Load Billing division screen
     p*  Parm1   - RRN (9A) Employer master (WMEMP)
     p*  Parm2   - Pointer to format DS
     p*  Parm3   - Caller(3A) 'LOC' - Location Master, 'QTE' - Quote, '###' - En
     p*  Parm4   - Mode(1A) 'C'-Change, 'D'-Display,
     p*  Parm5   - Fyr(3.0) Employer Detail FYR
     p*  Returns - Pointer to format DS
     P*=====================================================================
     pLoadBillDiv      b                   Export
     dLoadBillDiv      pi              n
     d inRRN                          9  0
     d inPtrBillDiv                    *
     d inCaller                       3a   const
     d inMode                         1a   const
     d inFyr                          3  0 const options(*nopass)
      *
     d xxFyr           s              3  0 inz
     d xxState         s              2    inz
     d xxDate          s              7  0 inz
     d xxKey           s             10    inz
     d isContribution  s               n   inz(*off)
       //***  Begin Add  ***  03/11/21  *********************
     Dis3rdPartyPay    s              1n   inz

     DdsWIPSP        e ds                  extname(WIPSP) inz qualified
     DdsWIPSPHIST    e ds                  extname(WIPSPHIST) inz qualified
       //***  End   Add  ***  03/11/21  *********************
       //*** Begin Add 05/11/21 **********************************
       Dcl-S isDftSrD3p ind inz(*Off);
       Dcl-S strDftSrD3p char(1) inz('X');
       Dcl-S sysDate7 packed(7:0);
       //*** End  Add 05/11/21 ***********************************
       //*** beg add ***** 09/21/21 ***********************
       Dcl-S hasQteSrOnRcd Ind;
       Dcl-S hasLivSrOnRcd Ind;
       //*** end add ***** 09/21/21 ***********************
       //*** Beg Add *** 04/24/25 ***************************************
       Dcl-S intCmdLen packed(15:5) inz(36);
       Dcl-S CmdOvrDbf char(36);
       Dcl-S CmdDltOvr char(36);
       //*** End Add *** 04/24/25 ***************************************
     C*
     C* Table File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
     C*
     C     keyWDELL      KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    xxFyr
      *
     C     keyWMEXL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxFYR
      *
      /Free
       isConnect = IsModEnabled('Portal Module');
       //***  Begin Add  ***  03/11/21  *********************
       Select;
       When IsModEnabled('Invoice Cloud') and ICGroupOpt(ELCo#:ELFnd);
          is3rdPartyPay = *on;
       Endsl;
       //***  End   Add  ***  03/11/21  *********************

       Clear xxFyr;

       If %parms >= 5;
         xxFyr = inFyr;
       Endif;

       If not GetEmployer(inRRN);
         Return *Off;
       EndIf;

       If xxFyr <> 0;
         Chain(n) keyWDELL PEL00;
         If not %found;
           Return *Off;
         Endif;
       EndIf;
       //*** begin add ***  08/08/24  ******************************
       chgMode = *Off;
       addMode = *Off;
       Select;
       When inMode = 'A' Or inMode = 'M';
         addMode = *On;
       When inMode = 'C';
         chgMode = *On;
       EndSl;
       //*** end add *****  08/08/24  ******************************

       intCo# = EMco#;
       scCo# = EMco#;
       intFnd = EMfnd;
       scFnd = EMfnd;
       scEmp# = EMemp#;
       scDiv = EMdiv;
       scxDiv = scdiv;
       visOnPrNam = *off;
     **** Beg Add - 03/11/22  **************************************************
       //*** Chain EMco# ptm00;
          Chain (EMco# : 0) ptm00;
     **** End Add - 03/11/22  **************************************************
       If %found and TMapnd = 'Y';
          visOnPrNam = *on;
       Endif;
       visOnInNam = *off;
       Chain (EmCo#:EmFnd) PFN00;
       If %found and FNpeof <> ' ';
          visOnInNam = *on;
       Endif;
       visOvrPEO = *off;
       If FNpeof <> ' ' and
       //*** begin chg ***  02/26/22  ***************************************
       //*****   ((EmEmp# = 1 and EmDiv <> 0) or EmEmp# <> 1);
          ((EmEmp# = 1 and EmDiv <> 0) or EmEmp# <> 1  or (EMEmp# = 1
          and EMDiv = 0 and inCaller <> 'QTE' and inCaller <> 'LOC'));
       //*** end chg *****  02/26/22  ***************************************
          visOvrPEO = *on;
       Endif;
       //*** begin del ***  04/11/23  ***************************************
       //*****Clear scDivision;
       //*** end del *****  04/11/23  ***************************************
       visDivisn = *off;
       If inMode = 'A' and FNAutd = 'N';
          visDivisn = *on;
       Endif;

       ptrBillDiv = inPtrBillDiv;
       ClearBillErrors();
       saveDate =  Dateto7(systemDate());
       saveTime = Timeto6(SystemTime());
       scMode = inMode;
       flgOvrStrp = *Off;
       isContribution = GetPrmContflg(scCO#:scFND);
       If isContribution;
          flexPrmAcc = 'Contribution Accountant';
          flexPrmFin = 'Contribution Finance Company';
          flexPrior  = 'Prior Contribution';
       Else;
          flexPrmAcc = 'Premium Accountant';
          flexPrmFin = 'Premium Finance Company';
          flexPrior  = 'Prior Premium';
       Endif;
       SCstRep = EMstrp;
       SCDbaNam = EMname + EMnam2;
       SCFedNam = EMfdnm + EMfdn2;
       SCphn# = %Int(%Editc(EMarea:'X') + %Editc(EMphn#:'X'));
       SCfax# = %Int(%Editc(EMfaxa:'X') + %Editc(EMfax#:'X'));
       //*** Beg Add *** 12/22/21 **********************************************
       SCmail = EMmail;
       //*** End Add *** 12/22/21 **********************************************
       //*** begin add ***  08/08/24  ***************************************
       If addMode and scMail = ' ' and FNEmail <> ' ';
          scMail = FNEmail;
       EndIf;
       //*** end add *****  08/08/24  ***************************************
       SCEmpWeb = EMiadr;
       SCmadd1 = EMmad1;
       SCmadd2 = EMmad2;
       SCmcity = EMmcty;
       SCmstate = EMmst;
       SCmzip = EMmzip;
       SCpadd1 = EMpad1;
       SCpadd2 = EMpad2;
       SCpcity = EMpcty;
       SCpstate = EMpst;
       SCpzip = EMpzip;
       SCAddFlg = EMmpfl;
       SCcounty = EMcnty;
       SCTaxId = EMfein;
       SCNRisk = EMrsk#;
       SCSRisk = EMsemp;
       SCSUnEmp = EMue#;
       scNaNam = EMnamp;
       scFdNam = EMfdnp;
       scNaInNam = EMnami;
       scFdInNam = EMfdni;
       scOvRptPEO = EMRPEO;
       //*** Beg Add ***** 06/06/24 ****************************************
       SCDbaNmHld = EMname + EMnam2;
       SCMAdd1Hld = EMmad1;
       SCMAdd2Hld = EMmad2;
       SCMCityHld = EMmcty;
       SCMStHld = EMmst;
       SCMZipHld = EMmzip;
       SCPAdd1Hld = EMpad1;
       SCPAdd2Hld = EMpad2;
       SCPCityHld = EMpcty;
       SCPStHld = EMpst;
       SCPZipHld = EMpzip;
       //*** End Add ***** 06/06/24 ****************************************
       //*** Begin Add *** 02/22/22 ****************************************
       isSunz = *Off;
       GetClientId(ClientID);
       If ClientID = '150';
          isSunz = *On;
       EndIf;
       // Create Drop Down Options
       scPEOOpts = ' ,Override to Report as PEO,' +
                   'Print Under Client Name';
       If isSunz;
          scPEOOpts = ' ,Override to Report as PEO,' +
                      'Override Insured Name Output';
       EndIf;
       // Set Default Position
       topPNCt    = 95;
       topPNC     = 110;
       leftPNC    = 580;
       //*** End   Add *** 02/22/22 ***********************************
       //*** Beg Add ***  04/29/24  *** WC2965 *************************

       reqMail = *Off;
       If SCmstate <> ' ' Or SCpstate <> ' ';

          xxDate = %int(%Char(%Date() : *CYMD0));
          SetGT (scCo#:scFnd:SCmstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCmstate) WDRRL;
          If Not %EOF and RREmail = 'Y';
             reqMail = *On;
          EndIf;

          SetGT (scCo#:scFnd:SCpstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCpstate) WDRRL;
          If Not %EOF and RREmail = 'Y';
             reqMail = *On;
          EndIf;

       //*** begin del ***  08/08/24  ***************************************
       //*If EMStrp = 'Y' and EMMail = ' ';
       //*** Chain (scCo#:scFnd) WMFNL;
       //***  If %Found and FNEmail <> ' ';
       //***     scMail = FNEmail;
       //***  EndIf;
       //*EndIf;
       //***
       //*If EMStrp = 'N';                                   // Non SR Div
       //*** SetLL (scCo#:scFnd:EMemp#:0) PEM00;
       //*** ReadE(n) (scCo#:scFnd:EMemp#) PEM00;
       //*** Dow Not %EOF;
       //***     If EMStrp = 'Y';
       //***     Leave;
       //***     EndIf;
       //***     ReadE(n) (scCo#:scFnd:EMemp#) PEM00;
       //*** EndDo;
       //***If EMMail = ' ';                               // If SR Email is Bla
       //*** Chain (scCo#:scFnd) WMFNL;
       //***  If %Found and FNEmail <> ' ';
       //***     scMail = FNEmail;
       //***  EndIf;
       //***EndIf;
       //*EndIf;
       //*** end del *****  08/08/24  ***************************************
       EndIf;
       //*** End Add ***  04/29/24  *** WC2965 *************************
       scOvrMP = EMOVRMP;
       scNonMl = EMnonm;
       scNonPh = EMnonp;
       //*** Beg Chg *** 09/29/22 PHW103722 *****************************   *
       //***** If inCaller = 'QTE' or inCaller = 'MTE';
       //*** Beg Chg *** 08/03/23 ***************************************
       //*** If inCaller = 'QTE' or inCaller = 'MTE' or inCaller = 'DIV';
       // WTPOLQTE is passing '190' for Mid-Term Endorsement - Add Division
       If inCaller = 'QTE' or inCaller = 'MTE' or inCaller = 'DIV'
        or inCaller = '190';
       //*** End Chg *** 08/03/23 ***************************************
       //*** End Chg *** 09/29/22 PHW103722 *****************************   *
          topDepCapt = 85;
          topDepCap  = 100;
          //*** Begin Chg *** 03/28/23 ************************************
          //**** leftDepCap = 920;
          leftDepCap = 945;
          //*** End   Chg *** 03/28/23 ************************************
          //*** Begin Add *** 02/22/22 ************************************
          topPNCt    = 85;
          topPNC     = 100;
          leftPNC    = 755;
          //*** End   Add *** 02/22/22 ************************************
       Endif;
       If inCaller = 'LOC';
          topDepCapt = 95 ;
          topDepCap  = 110;
          leftDepCap = 740;
          //*** Begin Chg *** 03/28/23 ************************************
          //**** leftDepCap = 740;
          leftDepCap = 765;
          //*** End   Chg *** 03/28/23 ************************************
       Endif;
       scDEPCAP = EMdepcap;
       SCTypBus = EMtpob;
       SCNatBus = EMnaob;
       SCSIC = EMSIC;
       SCNaics = EMnaic;
       If SCPstate <> *Blanks;
         xxState = SCPState;
       Else;
         xxState = SCMState;
       Endif;
       Chain(n) (SCco#:SCfnd:xxState) prr01;
       If %Found(wdrrl1) And RRnaic = 'Y' And flgQte <> 'Q';
         reqNaics = *On;
       Else;
         reqNaics = *Off;
       Endif;
       If EMexrt = *Blanks;
          EMexrt = '3';
       Endif;
       If ClientID = '150' and (inMode = 'A' or inMode = 'M');         //Add mod
          EMexrt = '2';                                                //Inter a
       Endif;
       //*** Beg Del *** 03/04/24 ******************************************
       //***SCExpRat = EMexrt;
       //*** End Del *** 03/04/24 ******************************************
       SCShEmp = EMshft;
       SCRegion = EMrgn#;
       If addMode And SCRegion = *Zeros;
         SCRegion = EXdrgn;
       Endif;
       SCChapter = EMchap;
       SCServOff = EMoff#;
       If addMode And SCServOff = *Blanks;
         SCServOff = EXdoff;
       Endif;
       SCWatch = EMwals;
       If inMode = 'A';
         EMvip = 'N';
       Endif;
       SCvip = EMvip;
       SCSplAcc = EMspac;
       SCEmpLnk = EMlink;
       SCCustId = EMcust;
       If not addMode;
         SCEmpLsId = EMlsid;
         xxPeoSovr = EMpeosovr ;


       Else;
         SCEmpLsId = '1';
         Eval xxDate = EMefd1;
         Setll (SCco#:SCfnd:xxState:xxDate) prr01;
         Reade (SCco#:SCfnd:xxState) prr01;
         If not %Eof(wdrrl1) And RRLsid <> ' ';
            SCEmpLsId = RRLsid;
         Endif;
         xxPeoSovr = *blanks;
       Endif;


       If clientID = '150' and
         (FNpeof = 'M' or FNpeof = 'C') and scemp# <> 1 and
                   scdiv = GetStRptDv(scCo#:scFnd:scEmp#);
          is150MCP = *On;
       Else;
          is150MCP = *Off;
       Endif;
       scPeoNmIn = EMPeoNmIn;
       scLivWMANP = %Trim(RtvQuoteLib('*LIV')) + '/WMANP';
       scNmInSlct = 'ANCO# = ' + %CHAR(FNCO#) +
        ' AND ANFND = ' + %CHAR(FNFND) +
        ' AND ANEMP# = 1 AND ANDIV = 0 AND ANSTAT = ''A''';

       //GET ADMIN/DARTS INFORMATION & LOAD CONTACT HEADINGS

       Chain (SCCo#:SCFnd) pex00;

       isReqLnkCd = *off;
       If EXreqLnkCd = 'Y';
          isReqLnkCd = *on;
       Endif;

       If EXflxh = *blanks;
          protFLXH = *off;
       Else;
          SCFlxh = EXflxh;
          protFLXH = *on;
       Endif;

       selFLXT='GTTBID=''' + EXFLXT + ''' AND GTSTAT<>''I''';

       Chain(n) (SCCo#:SCFnd:SCEmp#:SCDiv) pem00;

       SCFlxv = EMFLXV;

       SCExAuto = EMeacd;
       SVExAuto = EMeacd;
       SVdba = SCDbaNam;
       SVfein = EMfein;
       SVchap = EMchap;
       SVname = EMname;
       SVfdnm = EMfdnm;
       SVtpob = EMtpob;
       SVnamp = EMnamp;
       SVfdnp = EMfdnp;
       SVdiv = EMDIV ;
       If ClientID = '114';
          dcremp = *On;
       Else;
          dcremp = *Off;
       EndIf;

       If dcremp = *On;
          SCDcrEmp = EMdcrx;
       EndIf;
       scSTP = EMsTP;
       SCSfPolSrv = EMpsrv;
       SCSfUndWtr = EMuwtr;
       SCSfPrmAcc = EMpacc;
       SCSfAdtRvw = EMarvw;
       SCSfAdtMgr = EMamgr;
       SCSfLosRep = EMlrep;
       SCComments = EMcom1 + EMcom2 + EMcom3;
       scClmCnt   = EMclmc;

       SvSfPolSrv = EMpsrv;
       SvSfPrmAcc = EMpacc;
       SvSfAdtRvw = EMarvw;
       SvSfAdtMgr = EMamgr;
       SvSfLosRep = EMlrep;
       SvClmCnt   = EMclmc;
       SvSfUndWtr = EMuwtr;
       SCSfPolSr2 = 'UPstat <> ''I'' or UPstaf = ''' + EMpsrv + '''';
       SCSfPrmAc2 = 'UPstat <> ''I'' or UPstaf = ''' + EMpacc + '''';
       SCSfAdtRv2 = 'UPstat <> ''I'' or UPstaf = ''' + EMarvw + '''';
       SCSfAdtMg2 = 'UPstat <> ''I'' or UPstaf = ''' + EMamgr + '''';
       SCSfLosRe2 = 'UPstat <> ''I'' or UPstaf = ''' + EMlrep + '''';
       SCClmCnt2  = 'UPstat <> ''I'' or UPstaf = ''' + EMclmc + '''';
       SCSfUndWt2 = '(UPstat = ''A''' + ' AND UPtitl <> ''COMPSUITE STAFF'')'
                    + ' OR (UPstaf = '+ '''' + %Trim(EMuwtr) + '''' + ')';
       //Access third address file
       Chain(n) (EMco#:EMfnd:EMemp#:EMdiv) pa300;
       If not %found(wma3l);
         clear pa300;
       Endif;
       SCbadd1 = A3adr1;
       SCbadd2 = A3adr2;
       SCbcity =  A3cty;
       SCbstate = A3st;
       If A3zip1 = *Blanks;
         A3zip1 = *Zeros;
       Endif;
       If A3zip2 = *Blanks;
         A3zip2 = *Zeros;
       Endif;
       SCbzip = %Int(A3zip1)*10000 + %Int(A3zip2);
       SCbcontact = A3cont;
       SCOutAdd = A3ooc;

       // Check for heading at W/W group level
       Chain keyWMEXL PEX00;
       If EXahlc = *blanks;
          scCsfLsRHd = 'Loss Control Representative';
       Else;
          scCsfLsRHd = %trim(EXahlc);
       Endif;
       If EXahwl = *blanks;
          scWatchVal = 'Watch list';
       Else;
          scWatchVal = %trim(EXahwl);
       Endif;

       If EXahvp = *blanks;
          scVipVal = 'VIP member';
       Else;
          scVipVal = %trim(EXahvp);
       Endif;

       If EXahsa = *blanks;
          scSpaVal = 'Special acceptance';
       Else;
          scSpaVal = %trim(EXahsa);
       Endif;
       rrn3rdPay = 0;
       dsp3rdPay = *On;
       //*** begin add ***  05/11/21  ********************************
       rrnEftInfM = 0;
       dspEftInfM = *On;
       //*** end add *****  05/11/21  ********************************

       If xxFyr <> 0;
          scRTCD = ELRTCD;
          If scRtcd = *Blanks;
            If SCPstate <> *Blanks;
              xxState = SCPstate;
            Else;
              xxState = SCMstate;
            Endif;
            Eval xxDate = EMefd1;
            Setll (SCco#:SCfnd:xxState:xxDate) prr01;
            Reade (SCco#:SCfnd:xxState) prr01;
            If not %Eof(wdrrl1);
              SCrtcd = RRdrat;
            Endif;
          Endif;
          scBSTY = ' ';
          scBSCD = ELBSCD;
          If inMode = 'M';
            ELbsdt = SVBillDt;
          Endif;
          scPFDT = dtMDYY(ELPFDT);
          scPTDT = dtMDYY(ELPTDT);
          scANDT = dtMDYY(ELANDT);
          scBSDT = dtMDYY(ELBSDT);
          scEMPY = ELempy;

          poscurName = *off;
          If scPFDT <> 0;
             poscurName = *on;
          Endif;

          scSTAT = ELSTAT;
          If scStat = ' ';
             scStat = 'N';
          Endif;
          SCqtyp = ELqtyp;

          scPolTypHd = 'Type of Policy';
          PolTypReq = *off;
          Chain (scCo#:scFnd) PFH00;

          If %found(WDFHL);
            If FHFH#6 <> *blanks;
               scPolTypHd = FHFH#6;
            Endif;

            If FlgQte = 'Q';
               XXRQD6 = FHFH6Q;
            Else;
               XXRQD6 = FHFH6W;
            Endif;

            If XXRQD6 = 'Y';
              PolTypReq = *on;
            Else;
              PolTypReq = *off;
            Endif;
          Endif;

          SCtypb = ELtypb;

          Chain keyWDF1L PF100;
          If inMode = 'A';
             ELBLFL = F1BLFL;
          Endif;

          scBLFL = ELBLFL;
          scASCD = ELASCD;
          scPFIN = ELPFIN;
          scFFL1 = ELFFL1;
          If scFFL1 = *Blanks;
            scFFL1 = EXajfl;
          Endif;
          scARSM = ELARSM;
          scIFFL = ELIFFL;
          scBSTY = ' ';

          xxSvSt = RatingSt(ELCO#:ELRTCD);
          s2BSCD = ELBSCD;
          saveScBsCD = scBsCD;
          svRTCD = ELRTCD;
          Clear xxKey;
          xxKey = %editc(elbscd : 'X');
          Chain keyWTABL PBS01;
          saveBSLPGM = BSLPGM;
          rrn3rdPay = 0;
          clr3rdPay = *On;
          dsp3rdPay = *Off;
          //*** Begin Add *** 05/11/21 **********************************
          rrnEftInfM = 0;
          clrEftInfM = *On;
          dspEftInfM = *Off;
          //*** End   Add *** 05/11/21 **********************************
          //*** Beg Add  ***  09/30/21  *************************
          rrnOptIn = 0;
          clrOptIn = *On;
          dspOptIn = *Off;
          rrn3pLog = 0;
          clr3pLog = *On;
          dsp3pLog = *Off;
          //*** Beg Chg *** 12/22/21 ***************************************
          //***conLogTop = 1470;    // Top of 3rd Party Info Log Grid
          conLogTop = 1510;    // Top of 3rd Party Info Log Grid
          //*** End Chg *** 12/22/21 ***************************************
          //*** End Add  ***  09/30/21  *************************
          Write rcdBillDiv;
          clr3rdPay = *Off;
          dsp3rdPay = *On;
          //*** Beg Add  ***  09/30/21  *************************
          clrOptIn = *Off;
          dspOptIn = *On;
          clr3pLog = *Off;
          dsp3pLog = *On;
          //*** End Add  ***  09/30/21  *************************
          //*** Begin Add *** 05/11/21 **********************************
          clrEftInfM = *Off;
          dspEftInfM = *On;
          LoadEftInfoM(flgQte:inMode);
          //*** End   Add *** 05/11/21 **********************************

          //Load Third Pary Grid
         //*** beg chg ***** 09/22/21 *********************
         //   //*** Begin Add *** 05/11/21 *************************************
         //   isDftSrD3p = *Off;
         //   strDftSrD3p = 'X';
         //   sysDate7 = DateTo7(systemDate);
         //   //*** End   Add *** 05/11/21 *************************************
         //   Setll ELCo# pp300t;
         //   Reade ELCo# pp300t;
         //   Dow not %eof(wtp3l);
         //
         //      //***  Begin Add  ***  03/11/21  *********************
         //      Reset dsWIPSP;
         //      Reset dsWIPSPHIST;
         //      dis3rd = dspMod;
         //      vis3rd = *on;
         //      Select;
         //      When P3PrType = 'Third Party Payments';
         //         Select;
         //         When not is3rdPartyPay;
         //            P3stat = *blank;
         //         Endsl;
         //      Endsl;
         //      //***  End   Add  ***  03/11/21  *********************
         //      If P3Stat = 'A';
         //         scPayCo# = %editc(ELCO#:'X');
         //         scPayEMRRN = inRRN;
         //       //***  Begin Chg  ***  07/29/21  ******************
         //       //scPayRRN = *zeros;
         //         scPayRRN = 'No RRN';
         //       //***  End   Chg  ***  07/29/21  ******************
         //         scP3PrType = P3PrType;
         //         scP3PrTypT = P3PrType;
         //       //***  Begin Del  ***  03/11/21  *********************
         //       //If P3PrType = 'S/R Online';
         //       //   scP3PrType = %trim(P3PrType) + ' ' +
         //       //    %trim(GetModuleProperty('Portal Module':'Name'));
         //       //Endif;
         //       //***  End   Del  ***  03/11/21  *********************
         //       //*** Begin Change 05/11/21 **************************
         //         //Chain(n) (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
         //         //If %found;
         //         Clear scP3enbld;
         //         Clear scP3efDt;
         //         setll  (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
         //         If ELdiv <> stateRepDiv;
         //            scDftSfyr = xxfyr;
         //            isDftSrD3p = Has3rdPartyAcct(ELco#:ELfnd:ELemp#:
         //             ELdiv:P3prtype:sysDate7:xxfyr:flgQte:strDftSrD3p);
         //         Endif;
         //         If %equal(wmp3l);
         //            reade(n) (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
         //            Dow not %eof(wmp3l);
         //               If (flgQte = 'Q' ) or
         //                  (flgQte <> 'Q' and P3envflg = *blank);
         //                  dis3rd = dspMod;
         //       //*** End   Change 05/11/21 *****************************
         //                  scP3Enbld = P3Enbld;
         //                  scP3Efdt  = dtMdyy(P3Efdt);
         //                  scP3Exdt  = dtMdyy(P3Exdt);
         //                  scPayRRN = %editc(p3RRN:'X');
         //            //** Begin Add *** 05/11/21 ************************
         //                  scEnvFlgTp = P3envflg;   //Env
         //                  scDftInf   = P3dftSrD;   //Default State Rept Div I
         //                  scSfyrTp   = P3sfyr;     //Starting Year
         //                  If flgQte = 'Q' and scSfyrTp = *Zeros;
         //                     scSfyrTp  = xxFyr;      //Default starting year
         //                  Endif;
         //                  Exsr Write3rdPsfl;
         //               Endif;
         //            //** End   Add *** 05/11/21 ************************
         //
         //            //*** Begin Delete *** 05/11/21 *************************
         //            //Else;
         //            //   Clear P3rrn;
         //            //   Clear scPayRRN;
         //            //*** End  Delete *** 05/11/21 **************************
         //
         //            //*** Begin change *** 05/11/21 *************************
         //            //Endif;
         //               reade(n) (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
         //            Enddo;
         //
         //         Else;
         //            Clear P3rrn;
         //          //***  Begin Chg  ***  07/29/21  ******************
         //          //Clear scPayRRN;
         //            scPayRRN = 'No RRN';
         //          //***  End   Chg  ***  07/29/21  ******************
         //            If scSfyrTp = *Zeros and flgQte = 'Q';     //Default star
         //               scEnvFlgTp = 'Q';
         //               scSfyrTp = xxFyr;
         //            Endif;
         //            Exsr Write3rdPsfl;
         //         Endif;

         //*** End   change *** 05/11/21 *******************************
         //*** Begin Delete *** 05/11/21 *******************************
         //***  Begin Add  ***  03/11/21  *********************
         //Clear scAccount#;
         //Select;
         //When scP3PrType = 'S/R Online';
         //   scP3PrType = %trim(GetModuleProperty('S/R Online':'Name'))
         //    + ' ' + %trim(GetModuleProperty('Portal Module':'Name'));
         //When scP3PrType = 'Third Party Payments';
         //   Exec sql
         //    select * into :dsWIPSP
         //     from WIPSP
         //     where PSco# = :ELco# and PSfnd = :ELfnd
         //      and PSemp# = :ELemp#;
         //   Exec sql
         //    select * into :dsWIPSPHIST
         //     from WIPSPHIST
         //     where PSco# = :ELco# and PSfnd = :ELfnd
         //      and PSemp# = :ELemp#
         //      and PSfield = 'PSOPTFLAG'
         //     order by PStrDt desc, PStrTm desc
         //     limit 1;
         //   dis3rd = *on;
         //   vis3rd = *off;
         //   Clear scP3enbld;
         //   Clear scP3efDt;
         //   Select;
         //   When dsWIPSP.PSoptFlag = 'I';
         //      scP3enbld = 'Y';
         //      scP3efDt  = dtMDYY(dsWIPSPHIST.PStrDt);
         //   Endsl;
         //   scAccount# = %char(ELfnd)+%char(ELemp#)+%char(ELdiv);
         //Endsl;
         //***  End   Add  ***  03/11/21  *********************
         //rrn3rdPay = rrn3rdPay + 1;
         //Write Sfl3rdPay;
         //*** End   Delete *** 05/11/21 *******************************
         //      Endif;
         //
         //      Reade ELCo# pp300t;
         //   Enddo;
         //   rrn3rdPay = 1;
         //   //*** Begin Add *** 05/11/21 *************************************
         //   If ELdiv <> stateRepdiv and isDftSrD3p = *On and strDftSrD3p = 'Y'
         //      visDftMsg3 = *On;
         //   Else;
         //      visDftMsg3 = *Off;
         //   Endif;
         //   //*** End   Add *** 05/11/21 *************************************
         //*** End Add *** 04/24/25 *********************************
         // Read through Third Party Table. Load grid with detail
         // from Third Party Policy File if it exists. If it
         // doesn't, provide input fields to the grid.
          Clear Sfl3rdPay;
          Setll ELCo# pp300t;
          Dou %Eof(WTP3L);
          Reade ELCo# pp300t;
             If %Eof(WTP3L);
                Leave;
             Endif;
             //*** Begin Add *** 09/30/21 **************************************
             // Prevent the 3rd Party subfile from being loaded
             // if the 3rd Party module is not enabled.
             If mod3rdPay = *off;
                Leave;
             Endif;
             //*** End   Add *** 09/30/21 **************************************
            // Initialize fields and display indicators.
            // Might have to populate grid with blank values if
            // record doesn't exist in environment.
             Clear scP3Enbld;
             Clear scSFyrTp;
             Clear scEnvFlgTp;
             Clear scP3EfDt;
             Clear scP3ExDt;
             Clear scDftInf;
             Clear P3rrn;
             scPayCo# = %editc(ELCO#:'X');
             scPayEMRRN = inRRN;
             scPayRRN = 'No RRN';
             scP3PrType = P3PrType;
             scP3PrTypT = P3PrType;
             scPayRRN = 'No RRN';
             Select;
             When P3PrType = 'S/R Online';
                scP3PrType = %trim(GetModuleProperty('S/R Online':'Name'))
                 + ' ' + %trim(GetModuleProperty('Portal Module':'Name'));
                visForSrOn = *On;
                //*** Beg Add  ***  09/30/21  *************************
                //*** Beg Chg *** 12/22/21 *********************************
                //***conLogTop = 1470;    // Top of 3rd Party Info Log Grid
                conLogTop = 1510;    // Top of 3rd Party Info Log Grid
                //*** End Add  ***  09/30/21  *************************
                isDftSrD3p = *Off;
                strDftSrD3p = 'X';
                sysDate7 = DateTo7(systemDate);
               // Find out if this division is defaulting S/R Online
               //  from the state reporting division.
                //*** Beg Chg *** 08/04/25 *********************************
                //***If ELdiv <> stateRepDiv;
                If (ELdiv <> stateRepDiv) or
                 (InCaller = 'DIV' and InMode = 'A');
                //*** End Chg *** 08/04/25 *********************************
                   scDftSfyr = xxfyr;
                   isDftSrD3p = Has3rdPartyAcct(ELco#:ELfnd:ELemp#:
                    ELdiv:P3prtype:sysDate7:xxfyr:flgQte:strDftSrD3p);
                Endif;
                //*** Beg Chg *** 08/04/25 ******************************
                //***If ELdiv <> stateRepdiv And isDftSrD3p = *On
                //*** And strDftSrD3p = 'Y';
                If (ELdiv <> stateRepdiv And isDftSrD3p = *On
                 And strDftSrD3p = 'Y') Or
                 (inCaller = 'DIV' and inMode = 'A' and
                 isDftSrD3p = *on);
                //*** End Chg *** 08/04/25 ******************************
                   visDftMsg3 = *On;
                //*** Beg Chg *** 08/04/25 *********************************
                   //***strSRDMsg =
                   //*** %Trim(scP3PrType) +
                   //*** ' is defaulted from the state reporting division.';
                   Chain (ELco#:%editc(ELbsCd:'X')) pbs01;

                   If %Found(WTABL) and BSSEFL = 'S' and BSONLI = 'Y';
                      strSRDMsg =
                       %Trim(scP3PrType) +
                       ' is defaulted from the state reporting division.';
                   Else;
                      strSRDMsg = 'This Billing Division is not currently +
                       set for On-line Billing';
                   Endif;
                   //*** End Chg *** 08/04/25 ******************************
                Else;
                   Clear strSRDMsg;
                   visDftMsg3 = *Off;
                Endif;
                //*** Beg Add *** 04/24/25 *********************************
                //Override File to WMP3L member
                If not %Open(WMP3L);
                   CmdOvrDbf = %Trim('OVRDBF FILE(WMP3L) MBR(WMP3L)');
                   RunCommand(CmdOvrDbf:intCmdLen);
                   Open WMP3L;
                Endif;
                If not %Open(WMP3P);
                   CmdOvrDbf = %Trim('OVRDBF FILE(WMP3P) MBR(WMP3P)');
                   RunCommand(CmdOvrDbf:intCmdLen);
                   Open WMP3P;
                Endif;
                //*** End Add *** 04/24/25 *********************************
         //*** begin chg ***  04/11/23  ***************************************
         //*****Setll (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
                If inMode = 'A';
                   Setll (ELCo#:ELFnd:ELEmp#:intAddEftDiv:P3PrType) pp300;
                Else;
                   Setll (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
                Endif;
         //*** end chg *****  04/11/23  ***************************************
                Dou %Eof(WMP3L);
         //*** begin chg ***  04/11/23  ***************************************
         //*****   Reade (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
                   If inMode = 'A';
                      Reade(n) (ELCo#:ELFnd:ELEmp#:intAddEftDiv:P3PrType) pp300;
                   Else;
                      Reade(n) (ELCo#:ELFnd:ELEmp#:ELDiv:P3PrType) pp300;
                   Endif;
         //*** end chg *****  04/11/23  ***************************************
                   If %Eof(WMP3L);
                      Leave;
                   Endif;
                  // Don't display quote records in live.
                   If flgQte = *Blank And P3EnvFlg = 'Q';
                      Iter;
                   Endif;
                  // Initialize fields and display indicators.
                   Clear scP3Enbld;
                   Clear scSFyrTp;
                   Clear scEnvFlgTp;
                   Clear scP3EfDt;
                   Clear scP3ExDt;
                   Clear scDftInf;
                   disP3Enbl = *Off;
                   dis3rdSfyr = *Off;
                   disP3Dates = *Off;
                   disP3Dflt = *Off;
                   Select;
                   When dspMod = *On Or
                    (flgQte = 'Q' and P3EnvFlg = *Blank);
                      disP3Enbl = *On;
                      dis3rdSfyr = *On;
                      disP3Dates = *On;
                      disP3Dflt = *On;
                   When flgQte = 'Q' And P3EnvFlg = 'Q';
                      dis3rdSfyr = *On;
                   EndSl;
                  // Might have to populate grid with blank values if
                  // record doesn't exist in environment.
                   If P3EnvFlg = 'Q';
                      hasQteSrOnRcd = *On;
                   Else;
                      hasLivSrOnRcd = *On;
                   Endif;
                  // Populate grid row
                   Select;
                   When dspMod = *On Or
                    (flgQte = 'Q' and P3EnvFlg = *Blank);
                      disP3Enbl = *On;
                      dis3rdSfyr = *On;
                      disP3Dates = *On;
                      disP3Dflt = *On;
                   When flgQte = 'Q' and P3EnvFlg = 'Q';
                      dis3rdSfyr = *On;
                   EndSl;
                   scP3Enbld = P3Enbld;
                   scP3Efdt  = DtMdyy(P3Efdt);
                   scP3Exdt  = DtMdyy(P3Exdt);
                   scPayRRN  = %Editc(p3RRN:'X');
                   scEnvFlgTp = P3envflg;
                   scDftInf   = P3dftSrD;
                   scSfyrTp   = P3sfyr;
                   Exsr Write3rdPsfl;
                Enddo;
                //*** Beg Add *** 04/24/25 ************************************
                If %Open(WMP3L);
                   Close WMP3L;
                   CmdDltOvr = %Trim('DLTOVR FILE(WMP3L)');
                   RunCommand(CmdDltOvr:intCmdLen);
                Endif;
                If %Open(WMP3P);
                   Close WMP3P;
                   CmdDltOvr = %Trim('DLTOVR FILE(WMP3P)');
                   RunCommand(CmdDltOvr:intCmdLen);
                Endif;
                //*** End Add *** 04/24/25 ************************************
               // Shut down entry in display mode
               // in case no WMP3P records exist.
               // Shut down entry in display mode
               // in case no WMP3P records exist.
                If dspMod = *On;
                   disP3Enbl = *On;
                   dis3rdSfyr = *On;
                   disP3Dates = *On;
                   disP3Dflt = *On;
                Else;
                   disP3Enbl = *Off;
                   dis3rdSfyr = *Off;
                   disP3Dates = *Off;
                   disP3Dflt = *Off;
                Endif;
               // Write blank record to grid row if
               // record doesn't exist in environment.
               // Enable writes record in WTPOLLMS06.
                If flgQte = 'Q' And hasQteSrOnRcd =  *Off;
                   Clear scP3Enbld;
                   Clear scSFyrTp;
                   Clear scEnvFlgTp;
                   Clear scP3EfDt;
                   Clear scP3ExDt;
                   Clear scDftInf;
                   Clear P3rrn;
                   dis3rdSfyr = *On;
                   scPayRRN = 'No RRN';
                   If scSfyrTp = *Zeros;
                      scEnvFlgTp = 'Q';
                      scSfyrTp = xxFyr;
                   Endif;
                   Exsr Write3rdPsfl;
                Endif;
                If flgQte = *Blank And hasLivSrOnRcd =  *Off;
                   Clear scP3Enbld;
                   Clear scSFyrTp;
                   Clear scEnvFlgTp;
                   Clear scP3EfDt;
                   Clear scP3ExDt;
                   Clear scDftInf;
                   Clear P3rrn;
                   scPayRRN = 'No RRN';
                   If scSfyrTp = *Zeros;
                      scEnvFlgTp = *Blank;
                      scSfyrTp = xxFyr;
                   Endif;
                   Exsr Write3rdPsfl;
                Endif;
             When P3PrType = 'Third Party Payments';
               // Just shows if policy is opted in or not.
               // Detail box shows details. Third Party
               // Info is in WIPSP*, not WMP3P.
                If is3rdPartyPay = *Off;
                   Iter;
                Endif;
                disP3Enbl = *On;
                visForSrOn = *Off;
                Exsr Write3rdPsfl;
             EndSl;
          Enddo;
         // Set cursor to first row
          rrn3rdPay = 1;
         //*** end chg ***** 09/22/21 *********************
       Endif;

       xxOnce =  'N';
       xxOflg =  'N';
       xxUPDB =  ' ';
       xxUPDC =  ' ';

       Select;
       When flgQte = 'Q';
          isClmInstV = *off;

       When inMode = 'C';
          isClmInstV = *on;

       When isInstExist(EMco#:EMfnd:EMemp#:EMdiv) = *on;
          isClmInstV = *on;

       Other;
          isClmInstV = *off;
       Endsl;

       LoadBillSel(scPState:scMState);

       Return *On;
      //***** Begin Add *** 05/11/21 *****************************************
      //*---------------------------------------------------------------------
      //* Write3rdPsfl - Write Rec to 3rd Party Sfl
      //*---------------------------------------------------------------------
       Begsr Write3rdPsfl;
          Clear scAccount#;
          Select;
          //*** Beg Chg *** 09/30/21 *******************************************
          //*** When scP3PrType = 'S/R Online';
          When P3PrType = 'S/R Online';
          //*** End Chg *** 09/30/21 *******************************************
             scP3PrType = %trim(GetModuleProperty('S/R Online':'Name'))
              + ' ' + %trim(GetModuleProperty('Portal Module':'Name'));
              If scPayRrn = *blanks;
                 scPayRrn = *Zeros;
              Endif;
                //*** Beg Add *** 09/30/21 *************************************
                //*** Beg Del *** 04/24/25 *************************************
                //***strAlias = 'CREATE ALIAS QTEMP/WMP3P_ARC FOR +
                //***WMP3P(ARCHIVE)';
                //***Exec sql
                //***Execute immediate :strAlias;
                //*** End Del *** 04/24/25 *************************************
                sqlTrans = 'Select P3pgm, +
                 case when p3dt = 0 +
                 then '' '' +
                 else Substr(Char(P3dt + 19000000),5,2) || ''/'' || +
                 Substr(Char(P3dt + 19000000),7,2) || ''/'' || +
                 Substr(Char(P3dt + 19000000),1,4) +
                 end +
                  , P3envFlg, P3prType, P3enbld, +
                 case when p3efdt = 0 +
                 then '' '' +
                 else Substr(Char(P3efDt + 19000000),5,2) || ''/'' || +
                 Substr(Char(P3efDt + 19000000),7,2) || ''/'' || +
                 Substr(Char(P3efDt + 19000000),1,4) +
                 end, +
                 case when p3exdt = 0 +
                 then '' '' +
                 else Substr(Char(P3exDt + 19000000),5,2) || ''/'' || +
                 Substr(Char(P3exDt + 19000000),7,2) || ''/'' || +
                 Substr(Char(P3exDt + 19000000),1,4) +
                 end +
                 from WMP3P_ARC where P3co# = ' + %Char(Elco#) +
                 ' and P3fnd = ' + %Char(Elfnd) + ' and P3emp# = ' +
                 %Char(ELemp#) + ' and P3div = ' + %Char(ELdiv) +
                 ' order by rrn(WMP3P_ARC) desc';
                //*** End Add *** 09/30/21 *************************************
          When scP3PrType = 'Third Party Payments';      //Other 3rd party types
             Exec sql
              select * into :dsWIPSP
               from WIPSP
               where PSco# = :ELco# and PSfnd = :ELfnd
                and PSemp# = :ELemp#;
             Exec sql
              select * into :dsWIPSPHIST
               from WIPSPHIST
               where PSco# = :ELco# and PSfnd = :ELfnd
                and PSemp# = :ELemp#
                and PSfield = 'PSOPTFLAG'
               order by PStrDt desc, PStrTm desc
               limit 1;
           //*** beg chg**** 09/22/21 ************
           //        dis3rd = *on;
           //        vis3rd = *off;
           //        Clear scP3enbld;
           //        Clear scP3efDt;
           //      Select;
           //      When dsWIPSP.PSoptFlag = 'I';
           //         scP3enbld = 'Y';
           //         scP3efDt  = dtMDYY(dsWIPSPHIST.PStrDt);
           //      Endsl;
           //      //*** Begin Chg *** 07/29/21 ********************************
           //      //*** scAccount# = %char(ELfnd)+%char(ELemp#)+%char(ELdiv);
           //      scAccount# = %EditC(ELco#:'X') + %EditC(ELfnd:'X') +
           //                   %EditC(ELemp#:'X');
           //      //*** End   Chg *** 07/29/21 ********************************
           //   Endsl;
           //   If flgQte = 'Q' and ELnwrw = 'R';
           //      dis3rdSfyr = *on;
           //   Else;
           //      dis3rdSfyr = dis3rd;
           //   Endif;
           //
           //   If  not %equal(WMP3L);
           //   Else;
           //      If flgQte = 'Q' and P3envFlg = *blank;
           //         dis3rd = *On;
           //         dis3rdSfyr = *On;
           //      Endif;
           //   Endif;

             If dsWIPSP.PSoptFlag = 'I';
                scP3enbld = 'Y';
             Endif;
             scAccount# = %EditC(ELco#:'X') + %EditC(ELfnd:'X') +
              %EditC(ELemp#:'X');

                //*** Beg Add *** 09/30/21 *************************************
                sqlAction = 'select PSaction, substr(Char(PStrDtDsp),6,2) || +
                 ''/'' || substr(Char(PStrDtDsp),9,2) || ''/'' || +
                 substr(Char(PStrDtDsp),1,4)' +
                 'from WIPSPHIST '+
                 'where PSco#='+%char(ELco#) +
                 ' and PSfnd='+%char(ELfnd) +
                 ' and PSemp#='+%char(ELemp#) +
                 ' and PSfield = ''PSOPTFLAG''' +
                 ' order by PStrDt desc, PStrTm desc';
                //*** End Add *** 09/30/21 *************************************
          Endsl;
           //*** end chg ***** 09/22/21 ************

          rrn3rdPay = rrn3rdPay + 1;
          Write Sfl3rdPay;
       Endsr;
      //***** End   Add *** 05/11/21 *****************************************
      /End-Free
     PLoadBillDiv      E
     p*=====================================================================
     p* ClearBillErrors
     P*=====================================================================
     pClearBillErrors  b
     dClearBillErrors  pi
      /Free
          ERTabErr   = *Off;
          ERaddflg   = *Off;
          ERbadd1    = *Off;
          ERbadd2    = *Off;
          ERbcity    = *Off;
          ERbcontact = *Off;
          ERbstate   = *Off;
          ERbzip     = *Off;
          ERchaptER = *Off;
          ERcomments = *Off;
          ERcounty    = *Off;
          ERcustid    = *Off;
          ERdbanam    = *Off;
          ERemplnk    = *Off;
          ERemplsid   = *Off;
          ERempweb    = *Off;
          ERexpauto   = *Off;
          //*** Beg Del *** 03/04/24 ***************************************
          //***ERexprat    = *Off;
          //*** End Del *** 03/04/24 ***************************************
          ERfax#      = *Off;
          ERfednam    = *Off;
          ERmadd1     = *Off;
          ERmadd2     = *Off;
          ERmcity     = *Off;
          ERmstate    = *Off;
          ERmzip      = *Off;
          ERnaics     = *Off;
          ERnatbus    = *Off;
          ERnrisk     = *Off;
          ERoutadd    = *Off;
          ERpadd1     = *Off;
          ERpadd2     = *Off;
          ERpcity     = *Off;
          ERphn#       = *Off;
          ERpzip       = *Off;
          ERbadd1     = *Off;
          ERbadd2     = *Off;
          ERbcity     = *Off;
          ERbzip       = *Off;
          ERregion     = *Off;
          ERservoff    = *Off;
          ERsfadtmgr   = *Off;
          ERsfadtrvw   = *Off;
          erClmCnt     = *Off;
          ERsflosrep   = *Off;
          ERsfpolsrv   = *Off;
          ERsfprmacc   = *Off;
          ERsfundwtr   = *Off;
          ERshemp      = *Off;
          ERsic        = *Off;
          ERsplacc     = *Off;
          ERsrisk      = *Off;
          ERstatERep = *Off;
          ERsunemp    = *Off;
          ERtaxid     = *Off;
          ERtypbus    = *Off;
          ERvip       = *Off;
          ERwatch     = *Off;
          erEMPY = *off;
          erRTCD = *off;
          erRTCD2 = *off;
          erBSCD = *off;
          erBSDT = *off;
          erASCD = *off;
          erPFIN = *off;
          erSTAT = *off;
          erBLFL = *off;
          erFFL1 = *off;
          erARSM = *off;
          erIFFL = *off;
          erPCName = *off;
          erPPrem  = *off;
          erPFDT = *off;
          erPTDT = *off;
          erANDT = *off;
          msPFDT = *blanks;
          msPTDT = *blanks;
          msANDT = *blanks;
          msRTCD  = *blanks;
          mgRTCD  = *blanks;
          erDivision = *off;
          //*** Beg Add *** 12/22/21 *******************************************
          ermail = *Off;
          //*** End Add *** 12/22/21 *******************************************
      /End-Free
     PClearBillErrors  E

     p*=====================================================================
     p* ValidBillDiv - Validate Billing Division data
     p*  Parm1   - inMode
     p*  Parm2   - Pointer to format DS
     p*  Parm3   - Employer RRN
     p*  Returns - Error flag '0' - error, '1' - no error
     P*=====================================================================
     pValidBillDiv     b                   Export
     dValidBillDiv     pi              n
     d inMode                         1    const
     d inPtrBillDiv                    *
     d inEMrrn                        9  0
      *
     d isError         s               n   inz
     d error           s               n   inz
     D inEditData      s            500
     d xxState         s              2    inz
     d xxCity          s             25    inz
     d xxZip5          s              5  0 inz
     d xxKey           s             10    inz
     d xxSt            s              2    inz
     d stRPFL          s               n   inz
     d outErr          s              1    inz
     d date            s              8  0 inz
     D message         s            100
     d elRRN           s              9  0 inz
     d demoRRN         s              9  0 inz
     d demoCity        s             25    inz
     d demoSt          s              2    inz
     D sysDt7          s              7  0
     D c2date7         s              7  0
     D x2cfdt          s              7  0
     D x2redt          s              7  0
     D xxDate          s              7  0
     D xxFCOD          s                   like(PFFCOD)
     D xxMXEX          s                   like(FNMXEX)
     D xxMXCY          s                   like(X7MXCY)
     D flex            s              1
     D xxRQD6          s                   LIKE(FHFH6Q)
     D xxSRRB          s              1    inz(' ')
     D xxSRRQ          s              1    inz(' ')
     D xxNRRB          s              1    inz(' ')
     D xxNRRQ          s              1    inz(' ')
     D clearWMLDP      s              1n
      *
     d conMsg          c                   const('Exclude from Auto Cash Dissol-
     d                                     ve flag is changed on master divisio-
     d                                     n. Do you want to update all divisio-
     d                                     ns?')
     d conTtl          c                   const('Information Message')
      *
     d conTtlOMP       c                   const('PEO override')
     d conMsgOMP       c                   const('The selected leasing ID of 1 -
     d                                     indicates a non-PEO policy is to be -
     d                                     set up within a PEO group.  Press -
     d                                     Override to confirm. ')
     ***** Begin Del *** 11/16/22 ***************************************
     ***** Begin Add *** 09/22/22 ***************************************
     d**** intPolpfdt      s                   like(ELpfdt)
     d**** intPolfyr       s                   like(ELfyr)
     ***** End   Add *** 09/22/22 ***************************************
     ***** Begin Del *** 11/16/22 ***************************************
     C*
     C* Table File Key List
     C*
     C     keyWTABL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
     C*
     C* Employer Master File Key List
     C*
     C     keyWMEML      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C*
     C     key1WMEML     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C*
     C* Employer Detail File Key List
     C*
     C     keyWDELL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
     C*
     C     key1WDELL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C*
     C     key2WDELL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    EMDIV
     C                   Kfld                    scFYR
     C*
     C     key3WDELL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    EADIV
     C                   Kfld                    scFYR
     C*
     C* Employer Payroll File Key List
     C*
     C     keyWMEPL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
     C*
     C* Group Master File Key List
     C*
     C     keyWMFNL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C*
     C* Admin/Darts File Key List
     C*
     C     keyWMEXL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
     C* Group Rate Set Code File Key List
     C*
     C     keyWMGRL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C*
     C     key1WMGRL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scRTCD
     C*
     C*  Rate Set Code File Key List
     C*
     C     keyWMRSL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
     C*
     C     key1WMRSL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    ELRTCD
     C*
     C*  Statistical Reporting/Rating File Key List
     C*
     C     keyWDRRL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDate
     C*
     C     key1WDRRL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C*
     C* Activity Status File Key List
     C*
     C     keyWTAVL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    xxKey
     C*
     C* Premium Finance Company File Key List
     C*
     C     keyWMPFL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxFcod
     C                   Kfld                    xxDate
     C*
     C     key1WMPFL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxFcod
     C*
     C* Employer A/R File Key List
     C*
     C     keyWMEALC     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
     C*
     C* Payroll Class File Key List
     C*
     C     key4WMPRL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
     C*
     C* Large Deductible File Key List
     C*
     C     keyWMLDL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
     C*
     C* Deductible Detail File Key List
     C*
     C     keyWDDDL1     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scFYR
      *
      *  Claims Master
      *
     C     keyWMCMLC     KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    scFND
     C                   KFLD                    scEMP#
     C                   KFLD                    scDIV
     C                   KFLD                    xxDate
      *
     C     key1WMCMLC    KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    scFND
     C                   KFLD                    scEMP#
     C                   KFLD                    scDIV
      *
      /Free

       ptrBillDiv = inPtrBillDiv;

       Chain(n) (scCo#:scFnd:scEmp#:scDiv) pem00;

       intCo# = scCo#;
       intFnd = scFnd;
       ComAPI(intCo#:intFnd:compName:fundName:secLevel:secType);

       ClearBillErrors();

       c2Date7 = GetCxDate1(scCO#:scFND:scEMP#:scDIV:scFYR:x2cfdt:x2redt);
       sysDt7 = DateTo7(SystemDate);

       Chain keyWMFNL PFN00;
       Chain keyWDF1L PF100;
       intCo#=FNco#;
       intFnd=FNfnd;

       If lossClick = *on or ((svMcity <> scMCity or svMst <> scMState or
        svPCity <> scPCity or svPSt <> scPState) and lossVis = *on);
          Exsr Demographics;
          If lossclick = *on;
             Return *on;
          Endif;
       Endif;

       Open WTPOLDTL;

       // Setup the requirement flags for State and NCCI Risk IDs.
       Exsr RiskFlags;

       //Validate state reporting flag
        If ClmAdmOnly <> 'C';
          Exsr EditStateRepFl;
        Endif;

       //Validate Division
       If FNAutd = 'N' and inMode = 'A';
          erDivision = *Off;
          msDivision = ' ';
          Chain(n)(scCo#:scFnd:scEmp#:scDivision) pem00;
          If %found();
             isError = *On;
             erDivision = *On;
             msDivision = 'WCR4829';
          Endif;
       Endif;

       //Validate FEIN
        If scTaxId <> EMfein and mgTaxId <> 'WC30154';
         Exsr EditFein;
        Endif;

       //Doing business as cannot be blank

        Select;
           When SCDbaNam = *Blanks;
                isError = *On;
                erDbaNam = *On;
                mgDbaNam = 'WCR0503';
           When %subst(SCDbaNam:1:1) = *Blanks;
                isError = *On;
                erDbaNam = *On;
                mgDbaNam = 'WCR0503';
           Other;
                erDbaNam = *Off;
        Endsl;

        //If the mailing address is entered
        If SCMAdd1 <> *Blanks;
         //The Mailing City Must be Entered
          If SCMCity = *Blanks;
            isError = *On;
            erMCity = *On;
            mgMCity = 'WCR0503';
          Else;
            erMCity = *Off;
          Endif;

         //The Mailing State must be valid
          xxKey = SCMState;
          Chain xxKey pst03;
          If Not %Found(ststl3);
            isError = *On;
            erMState = *On;
            mgMState = 'WCR0504';
          Else;
            erMState = *Off;
          Endif;
          //Zip code must be entered
          If SCMZip <= *Zeros;
            isError = *On;
            erMZip = *On;
            mgMZip = 'WCR0502';
          Else;
            erMZip = *Off;
          Endif;
          //****** Beg Del ****** 08/06/21 ************************
          //***** If ClientID = '136' and erMZip = *Off;
          //*****   intZip6 = %int(%subst(%editc(scMZip:'X'):6:4));
          //*****   If intZip6 <= 0;
          //*****      isError = *On;
          //*****      erMZip = *On;
          //*****      mgMZip = 'WCR0502';
          //*****   Endif;
          //*****Endif;
          //****** End Del ****** 08/06/21 ************************
          If erMZip <> *on;
             xxState = scMState;
             xxZip5 = %Int(%Subst(%Editc(scMZip:'X'):1:5));
             Chain (scCo#:xxState:xxZip5) pcz00;
             If %Found(wmczl) And CZstat <> 'A';
                isError = *On;
                erMZip = *On;
                mgMZip = 'WC30707';
             Endif;
          Endif;
        Endif;

       //If the physical address if entered:
        If SCPAdd1 <> *Blanks;
          // The physical city must be entered
          If SCPCity = *Blanks;
            isError = *On;
            erPcity = *On;
            mgPCity = 'WCR0503';
          Else;
            erPCity = *Off;
          Endif;
          xxKey = SCPState;
          Chain xxKey pst03;
          If Not %Found(ststl3);
            isError = *On;
            erPState = *On;
            mgPState = 'WCR0504';
          Else;
            erPState = *Off;
          Endif;
          //Zip code must be entered
          If SCPZip <= *Zeros;
            isError = *On;
            erPZip = *On;
            mgPZip = 'WCR0502';
          Else;
            erPZip = *Off;
          Endif;
          //***** Beg Del ******* 08/06/21********************************
          //***** If ClientID = '136' and erPZip = *Off;
          //*****   intZip6 = %int(%subst(%editc(scPZip:'X'):6:4));
          //*****   If intZip6 <= 0;
          //*****      isError = *On;
          //*****      erPZip = *On;
          //*****      mgPZip = 'WCR0502';
          //*****   Endif;
          //*****Endif;
          //***** End Del ******* 08/06/21********************************
          If erPZip <> *on;
             xxState = scPState;
             xxZip5 = %Int(%Subst(%Editc(scPZip:'X'):1:5));
             Chain (scCo#:xxState:xxZip5) pcz00;
             If %Found(wmczl) And CZstat <> 'A';
                isError = *On;
                erPZip = *On;
                mgPZip = 'WC30707';
             Endif;
          Endif;
        Endif;

        //The flag must be M or P
        If SCAddFlg <> 'M' And
         SCAddFlg <> 'P';
          isError = *On;
          erAddFlg = *On;
          mgAddFlg = 'WCR0524';
        Else;
          //The mailing address line cannot be blank if code selected
          //The physical address line cannot be blank if code selected
          If (SCAddFlg = 'M' And SCMAdd1 = *Blanks)
           Or (SCAddFlg = 'P' And SCPAdd1 = *Blanks);
            isError = *On;
            erAddFlg = *On;
            mgAddFlg = 'WCR0525';
          Else;
            erAddFlg = *Off;
          Endif;
        Endif;

         // Physical ST & Zip present, use it, else use Mailing ST, ZIP
          If SCPState <> *Blanks And SCPZip > 0;
            xxState = SCPState;
            xxZip5 = %Int(%Subst(%Editc(SCPZip:'X'):1:5));
          Else;
            xxState = SCMState;
            xxZip5 = %Int(%Subst(%Editc(SCMZip:'X'):1:5));
          Endif;
          Chain (intCO#:xxState:xxZip5) pcz00;
          If %Found(wmczl) And CZstat = 'A';
            Select;
            When ClientId = '102';
              If SCCounty = *Blanks;
                SCCounty = CZcnty;
              Endif;
            Other;
              SCCounty = CZcnty;
            Endsl;
          Endif;

        //ADR1 is required if any other data has
        //      been entered.
        If SCBcontact <> *Blanks Or SCBadd2 <> *Blanks Or SCOutAdd <> *Blanks
         Or SCBcity<>*Blanks Or SCBstate<>*Blanks Or SCBzip <> *Zeros;
          If SCBadd1 = *Blanks;
            isError = *On;
            erBadd1 = *On;
            mgBadd1 = 'WCR0503';
          Endif;
        Endif;

        //EITHER CITY/STATE OR OUT OF COUNTRY ADDRESS MUST BE ENTERED
        If SCBadd1 <> *Blanks;
          If SCBCity = *Blanks And SCOutAdd = *Blanks;
            isError = *On;
            erBCity = *On;
            erOutAdd = *On;
            mgBCity = 'WCR0503';
            mgOutAdd = 'WCR0503';
          Endif;
        Endif;

        //CITY, STATE & ZIP CANNOT BE ENTERED ALONG WITH OUT OF COUNTRY
        If SCBcity <> *Blanks Or SCBstate <> *Blanks Or SCBzip<>*Zeros;
          If SCOutAdd <> *Blanks;
            isError = *On;
            erOutAdd = *On;
            mgOutAdd = 'WCR0590';
          Endif;
        Endif;

        If SCBstate <> *Blanks Or SCBcity <> *Blanks;
          xxKey = SCBState;
          Chain xxKey pst03;
          If Not %Found(ststl3);
            isError = *On;
            erBState = *On;
            mgBState = 'WCR0504';
          Else;
            erBState = *Off;
          Endif;
        Endif;

        If SCBcity <> *Blanks;
          If %Int(SCBzip/10000) <= *Zeros;
            isError = *On;
            erBzip = *On;
            mgBzip = 'WCR0502';
          Endif;
          If ClientID = '136' and erBZip = *Off;
             intZip6 = %int(%subst(%editc(scBZip:'X'):6:4));
             If intZip6 <= 0;
                isError = *On;
                erBZip = *On;
                mgBZip = 'WCR0502';
             Endif;
          Endif;
          If erBZip <> *on;
             xxState = scBState;
             xxZip5 = %Int(%Subst(%Editc(scBZip:'X'):1:5));
             Chain (scCo#:xxState:xxZip5) pcz00;
             If %Found(wmczl) And CZstat <> 'A';
                isError = *On;
                erBZip = *On;
                mgBZip = 'WC30707';
             Endif;
          Endif;
        Endif;

       ##type1 = 'P';
       erPhn = Check_Phone#(scPhn#:##type1:msPhn#);
       If erPhn = *on;
          isError = *on;
       Endif;

       // If Fax Number entered has 10 digit
       ##type1 = 'F';
       erFax = Check_Phone#(scFax#:##type1:msFax#);
       If erFax = *on;
          isError = *on;
       Endif;

       //*** Beg Add ***  04/29/24  *** WC2965 *************************

       If SCmstate <> ' ' Or SCpstate <> ' ';

          xxDate = %int(%Char(%Date() : *CYMD0));
          SetGT (scCo#:scFnd:SCmstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCmstate) WDRRL;
          If Not %EOF and RREmail = 'Y' and ScMail = ' ' and
                          EMStrp = 'Y' and EMMail = ' ';
             erMail = *On;
             msMail = 'The email address is required for this state.';
             isError = *On;
          EndIf;

          SetGT (scCo#:scFnd:SCpstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCpstate) WDRRL;
          If Not %EOF and RREmail = 'Y' and ScMail = ' ' and
                          EMStrp = 'Y' and EMMail = ' ';
             erMail = *On;
             msMail = 'The email address is required for this state.';
             isError = *On;
          EndIf;

       EndIf;
       //*** End Add ***  04/29/24  *** WC2965 *************************
       //*** Beg Add *** 12/22/21 ******************************************
       // If Email Address entered verify correct format
       If scMail <> *blanks;
          erMail = Check_Email(scmail:msMail);
          If erMail = *on;
             isError = *on;
          Endif;
       Endif;
       //*** End Add *** 12/22/21 ******************************************

        //Validate NAICS code
        If SCPstate <> *Blanks;
          xxState = SCPState;
        Else;
          xxState = SCMState;
        Endif;
        Chain(n) (SCco#:SCfnd:xxState) prr01;
        If %Found(wdrrl1) And RRnaic = 'Y' And SCnaics = *Blanks
           And flgQte <> 'Q';
          isError = *On;
          erNaics = *On;
          mgNaics = 'WC31519';
          reqNaics = *On;
        Else;
          erNaics = *Off;
          reqNaics = *Off;
        Endif;
        If erNaics <> *on And RRnaic = 'Y'
             And flgQte <> 'Q';
           xxKey = SCnaics;
           If SCnaics <> *Blanks And not ValidGenCode(intCo#:'NAIC':xxKey);
              isError = *On;
              erNaics = *On;
              mgNaics = 'WCR0513';
              reqNaics = *On;
           Else;
             erNaics = *Off;
             reqNaics = *Off;
           Endif;
        Endif;

        //Validate Customer ID.
        If scCustId = *blanks and EXcust = 'Y';
           isError  = *On;
           erCustId = *On;
           mgCustid = 'WCR0503';
           reqcustid= *On;
        Else;
           erCustId = *Off;
           reqcustid= *Off;
        Endif;

        //Reload Asteric for customer ID.
        If EXcust = 'Y';
          reqcustid= *On;
        Else;
          reqcustid= *Off;
        Endif;


        //Validate policy services staff code, if eneterd
        If ScSfPolSrv <> SvSfPolSrv;
           If SCSfPolSrv <> *Blanks And Not ValidStaff(SCSfPolSrv);
              isError = *On;
              erSfPolSrv = *On;
              mgSfPolSrv = 'WCR0505';
           Endif;
        Else;
           erSfPolSrv = *Off;
        EndIf;

        //Validate Underwriter staff code, if eneterd
        If ScSfUndWtr <> SvSfUndWtr;
           If SCSfUndWtr <> *Blanks And Not ValidStaff(SCSfUndWtr);
              isError = *On;
              erSfUndWtr = *On;
              mgSfUndWtr = 'WCR0505';
           EndIf;
        Else;
           erSfUndWtr = *Off;
        EndIf;

        //Validate Premium acct staff code, if eneterd
        If ScSfPrmAcc <> SvSfPrmAcc;
           If SCSfPrmAcc <> *Blanks And Not ValidStaff(SCSfPrmAcc);
              isError = *On;
              erSfPrmAcc = *On;
              mgSfPrmAcc = 'WCR0505';
           Endif;
        Else;
           erSfPrmAcc = *Off;
        EndIf;

        //Validate Audit reviewer code, if eneterd
        If ScSfAdtRvw <> SvSfAdtRvw;
           If SCSfAdtRvw <> *Blanks And Not ValidStaff(SCSfAdtRvw);
              isError = *On;
              erSfAdtRvw = *On;
              mgSfAdtRvw = 'WCR0505';
           Endif;
        Else;
           erSfAdtRvw = *Off;
        EndIf;

        //Validate Audit manager code, if eneterd
        If SCSfAdtMgr <> SVSfAdtMgr;
           If SCSfAdtMgr <> *Blanks And Not ValidStaff(SCSfAdtMgr);
              isError = *On;
              erSfAdtMgr = *On;
              mgSfAdtMgr = 'WCR0505';
           EndIf;
        Else;
           erSfAdtMgr = *Off;
        EndIf;

        //Validate Loss Control rep code, if eneterd
        If SCSfLosRep <> SVSfLosRep;
           If SCSfLosRep <> *Blanks And Not ValidStaff(SCSfLosRep);
              isError = *On;
              erSfLosRep = *On;
              mgSfLosRep = 'WCR0505';
           Endif;
        Else;
           erSfLosRep = *Off;
        EndIf;

        // Validate Claim Contact code, if eneterd
        If SCClmCnt <> SVClmCnt;
           If scClmCnt <> *Blanks And Not ValidStaff(scClmCnt);
              isError = *On;
              erClmCnt = *On;
              mgClmCnt = 'WCR0505';
           EndIf;
        Else;
           erClmCnt = *Off;
        EndIf;

        //County code must be valid and active
        If SCPState <> *Blanks;
          xxKey = %Trim(SCPState) + %Trim(SCCounty);
        Else;
          xxKey = %Trim(SCMState) + %Trim(SCCounty);
        EndIf;
        Chain (intCo#:xxKey) pcn01;
        If not %found(wtcnl1);
          isError = *On;
          erCounty = *On;
          mgCounty = 'WCR0513';
        Else;
          If CNStat = 'I';
            isError = *On;
            erCounty = *On;
            mgCounty = 'WC31517';
          Else;
            erCounty = *Off;
          Endif;
        Endif;

        //Region must be valid and active
        Chain (intCo# :intFnd :SCRegion) pre00;
        If not %Found(wmrel);
          isError = *On;
          erRegion = *On;
          mgRegion = 'WCR0513';
        Else;
          If REstat = 'I';
            isError = *On;
            erRegion = *On;
            mgRegion = 'WCR0690';
          Else;
            erRegion = *Off;
          Endif;
        Endif;

        If ClientId = '133' And SCchapter = *Zeros;
          isError = *On;
          erChapter = *On;
          mgChapter = 'WCR0503';
        Endif;

        If SCChapter <> *Zeros;
          Chain (intCo#:intFnd:SCChapter) pcp00;
          If not %found(wmcpl) Or
           (%found(wmcpl) And CPStat = 'I');
            isError = *On;
            erChapter = *On;
            mgChapter = 'WCR0513';
          Else;
            erChapter = *Off;
          Endif;
        Endif;

        // Validate type of business
        xxKey = SCTypBus;
        If not ValidGenCode(intCo#:'BUSI':xxKey);
          isError = *On;
          erTypBus = *On;
          mgTypBus = 'WCR0513';
        Else;
          erTypBus = *Off;
        Endif;

        //*** Beg Add *** 05/03/23 ******************************************
        // error will be display when FEIN is 0 and set to required
        RtvEnv(strEnvmt);
        If strEnvmt = '*LIV';
           Chain (scCo#:scFnd) pex00;
           If %Found(Wmexl);
              If EXfifg = 'Y' and scTaxId = 0;
                 isError = *on;
                 mgTaxId = 'WCR0502';
                 erTaxId = *on;
              Endif;
           Endif;
        Endif;
        //*** End Add *** 05/03/23 ******************************************

        // NCCI Risk Id must be Entered
        If scNRisk = *Blanks and ((flgQte = 'Q' and xxNRRQ = 'Y') or
                                  (flgQte <> 'Q' and xxNRRB = 'Y'));
           isError = *On;
           erNRisk = *On;
           mgNRisk = 'WCR0503';
        Endif;

        // State Risk Id must be Entered
        If scSRisk = *Blanks and ((flgQte = 'Q' and xxSRRQ = 'Y') or
                                  (flgQte <> 'Q' and xxSRRB = 'Y'));
           isError = *On;
           erSRisk = *On;
           mgSRisk = 'WCR0503';
        Endif;

        // Validate State Unemployment #
        Chain(n) (SCco#:SCfnd:xxState) prr01;
        If %Found(wdrrl1) And RRurfl = 'Y' And SCSUnEmp = *Blanks;
          isError  = *On;
          erSUnEmp = *On;
          mgSUnEmp = 'WC31455';
          reqUmEmp = *On;
        Else;
          erSUnEmp = *Off;
          reqUmEmp = *Off;
        Endif;

        // Validate SIC code
        If (scsic = *Blanks And EXsicr = 'Y')
         And flgQte <> 'Q';
          isError = *On;
          erSic = *On;
          mgSic = 'WCR0503';
        Endif;

        If erSic <> *on;
           xxKey = SCSic;
           If SCsic <> *Blanks And not ValidGenCode(intCo#:'SIC':xxKey);
              isError = *On;
              erSic = *On;
              mgSic = 'WCR0513';
           Endif;
        Endif;

        //VALIDATE EMPLOYEE LEASING POLICY ID
        If reqLsid = *On Or SCEmpLsid <> *blanks;
          xxKey = SCEmpLsid;
          If not ValidGenCode(intCo#:'NREL':xxKey);
            isError = *On;
            erEmpLsid = *On;
            mgEmpLsid = 'WCR0513';
          Else;
            erEmpLsid = *Off;
          Endif;
        Endif;

        //***  Begin Del  ***  11/04/21  **********************
        //erEmpLsId = *Off;
        //***  End   Del  ***  11/04/21  **********************
        If FNpeof <> *blank and SCempLsId = '1' and dspMod = *off;
           If xxPeoSovr <> 'Y';
              If CheckSecLevel('MCPMastOvr') = *On;
                 flgOMP = PopTwoBtn(ConMsgOMP:conTtlOMP:'Cancel':'Override');
                 If flgOMP = *On;
                    xxPeoSOvr = *blanks;
                    isError = *On;
                 Else;
                    xxPeoSOvr = 'Y';
                 Endif;
              Else;
                 mgEmpLsId = 'WC40171'; //Leasing ID 1 needs proper security lev
                 erEmpLsId = *On;
                 isError = *On;
              Endif;
           Endif;
        Else;
           xxPeoSOvr = *blanks;
        Endif;

       // Experience Rating must be Entered
       //*** Beg Del *** 03/04/24 ******************************************
       //***If scExpRat = *Blanks and erExpRat <> *on;
       //***   isError = *On;
       //***   erExpRat = *On;
       //***   mgExpRat = 'WCR0503';
       //***Endif;
       //*** End Del *** 03/04/24 ******************************************

       // Servicing office must be Entered
       If scServOff = *Blanks and erServOff <> *on;
          isError = *On;
          erServOff = *On;
          mgServOff = 'WCR0503';
       Endif;

       If isReqLnkCd = *on and scEmpLnk = *blanks;
          isError = *On;
          erEmpLnk = *On;
          mgEmpLnk = 'WCR0503';
       Endif;

        If quote = *on;
           Exsr CheckQuote;
        Endif;

        // Display message-popup for Auto-cash dissolve prompt
        If isError <> *On And scMode = 'C' And scExAuto <> svExAuto
         And flgPEOStatus <> *Blanks And EMstrp = 'Y' And
         NumOfDivs > 1;
          flgUpdEacd = PopThreeBtn(conMsg:conTtl);
          If flgUpdEacd = *Blanks;
            isError = *On;
          Endif;
        Endif;
      /end-free
     C* // VALIDATE THE BILLING ON EFF DAY FLAG

     C     scBLFL        IFNE      'Y'
     C     scBLFL        ANDNE     *BLANK
     C     erBLFL        ANDNE     *ON
     C                   move      *on           iserror
     C                   move      *on           erBLFL
     C                   movel(p)  'WCR1241'     msBLFL
     C                   ENDIF
     C*
     C                   MOVEL(P)  scBSCD        xxKEY
     C*
     C     keyWTABL      CHAIN     PBS01                              60
     C     'WC'          Chain     SWC01
     C*
     C
     C  N60scBLFL        IFEQ      'Y'
     C     erBLFL        ANDNE     *ON
     C*
     c                   if        WCfl23 = 'Y' or
     c                             WCfl24 = 'Y'
     C*
     C     BSFREQ        IFNE      'M'
     C     BSSEFL        ORNE      'E'
     C                   move      *on           iserror
     C                   move      *on           erBLFL
     C                   movel(p)  'WC30390'     msBLFL
     C                   ENDIF
      *
     c                   else
      *
     c                   if        BSSEFL <> 'E'
     C                   move      *on           iserror
     C                   move      *on           erBLFL
     C                   movel(p)  'WC30390'     msBLFL
     c                   endif
      *
     c                   endif
      *
     C                   ENDIF

      /free

       //CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
        GetEmployer(inEMrrn);
        If EMdt > saveDate Or (EMdt = saveDate AND EMtime > saveTime);
          isError = *On;
          erTabErr = *On;
          mgTabErr = 'WC30000';
        Endif;

       Close WTPOLDTL;

        Return not isError;

      /End-Free
      *===============================================================
      * RiskFlags - Setup the State and NCCI risk flags
      *===============================================================
      *
     C     RiskFlags     BEGSR
      /Free

       Clear xxSRRB;
       Clear xxSRRQ;
       Clear xxNRRB;
       Clear xxNRRQ;

       Chain keyWMEXL PEX00;
       If %found(WMEXL);
          //* Save the Admin/DARTS control flags for State and NCCI ID#'s
          xxSRRB = EXSRRB;
          xxSRRQ = EXSRRQ;
          xxNRRB = EXNRRB;
          xxNRRQ = EXNRRQ;
       Endif;

       Clear xxSt;
       If scRTCD <> *blanks;
          Chain keyWMRSL PRS00;
          If %found;
             xxSt = RSJRST;
          Endif;
       Endif;

       If xxSt = *blanks;
          If SCPstate <> *Blanks;
             xxSt = SCPState;
          Else;
             xxSt = SCMState;
          Endif;
       Endif;

       xxDate = dtCYMD(scPFDT);

       Setgt keyWDRRL PRR00;
       Readpe key1WDRRL PRR00;

       If not %eof(WDRRL);
          If xxSRRB <> 'Y';
             xxSRRB = RRSRRB;
          Endif;
          If xxSRRQ <> 'Y';
             xxSRRQ = RRSRRQ;
          Endif;
          If xxNRRB <> 'Y';
             xxNRRB = RRNRRB;
          Endif;
          If xxNRRQ <> 'Y';
             xxNRRQ = RRNRRQ;
          Endif;
       Endif;

      /End-Free
     C                   ENDSR
      *===============================================================
      * CheckQuote - Check policy detail fields for quote mode
      *===============================================================
      *
     C     CheckQuote    BEGSR
     C*
      *
      /free

       Chain(n) keyWDELL PEL00;
       elRRN = dbRRN2;

       xxMailSt = scMState;
       xxPhysSt = scPState;

       inEditData = dsRateEdit;
       If FNAutd='N';
       scDiv=ScDivision;
       Endif;

      ****** Begin Chg *** 11/16/22 ***************************************
      ****** Begin Add *** 09/22/22 ***************************************
       // intPolpfdt = dtCYMD(scPfDT) ;
       // intPolfyr  = %Int(%Subst(%char(intPolpfdt):1:3));
       // scFyr  = intPolfyr ;

       //*** begin chg ***  04/10/23  ****************************************
       //*****Chain(n) keyWMFNL PFN00;

       //*****Setgt keyWMFNL PF100;
       //*****Readpe keyWMFNL PF100;

       //*****Dow  F1Stpo > dtCYMD(scPfDT) ;
       //*****   Readpe keyWMFNL PF100;
       //*****Enddo;

       //*****scFyr = F1Fyr;
       Setgt (ELCo#:ELFnd:ELEmp#:dtCymd(scPfdt)) pel07;
       Readpe (ELCo#:ELFnd:ELEmp#) pel07;
       scFyr = E3Fyr;
      //*** end chg *****  04/10/23  ****************************************
      ****** End   Add *** 09/22/22 ***************************************
      ****** End   Chg *** 11/16/22 ***************************************
       error = not errPolRating('L':' ':flgQte:ptrBillDiv:scCo#:scFnd:scEmp#:
                    scDiv:scFyr:elRRN:inEditData);

       dsRateEdit = inEditData;

      /end-free
     C*
     C                   If        not Error
     C                   EXSR      ClaimDt
     C                   Endif
      *
     C                   If        not Error
     C                   EXSR      ARDDiff
     C                   Endif
      *
     C                   If        not Error
     C                   EXSR      BSCDChg
     C                   Endif
      *
     C                   If        not Error
     C                   EXSR      SpreadFlag
     C                   Endif
      *
      * Set general error indicator used by procedure.
      *
     C                   If        error = *on
     C                   eval      isError = *on
     C                   Endif
     C
     C                   ENDSR
      *
      *===============================================================
      * ARDDiff - Anniversary Rating Date Popup
      *===============================================================
     C     ARDDiff       BEGSR
     C*
     C*  ANNIV DATE ON EACH DIVISION SHOULD BE CONSISTENT WITH THE
     C*  STATE REPORTING DIVISION (IGNORE INFO MSG IF CLAIMS ONLY)
     C*
     C     scANDT        IFNE      xxSRAD
     C     xxSTRP        ANDNE     'Y'
     C     F1CLMS        ANDEQ     'N'
     C*
     C                   eval      ardText = 'state reporting division is' +
     C                              %editw(xxSRAD : '  /  /    ') + '.'
     C*
     C                   Dou       ardCancel = *on or ardConfirm = *on
     C                   EXFMT     rcdAnnivDt
     C                   ENDDO
     C*
     C     ardCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ENDIF
     C*
     C                   ENDIF
     c*
     c                   ENDSR

      *===============================================================
      * ClaimDt - Claim Accident Date Popup
      *===============================================================
     C     ClaimDt       BEGSR
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
     C*
      *
      * If policy eff date range change, check for claims out of new
      * policy eff date range.
      *
     C                   If        ChgMode
      *
     C     xxOFLG        IFEQ      'N'
     C                   MOVE      'N'           xxEFLG            1
      *
     C     keyWDELL      CHAIN(N)  PEL00                              60
      *
     C                   Z-ADD     scPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPFDT            7 0
      *
     C                   Z-ADD     scPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMDp
     C                   MOVEL     SRDTO         xxPTDT            7 0
      *
      *  Check for claims outside policy eff dt range
      *
     C     xxPFDT        IFGT      ELPFDT
     C                   Z-ADD     ELPFDT        xxDATE
      *
     C     keyWMCMLC     SETLL     PCM0C
     C     key1WMCMLC    READE     PCM0C                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     xxPFDT
     C                   XFOOT     AR            xxPMTS           13 2
      *
     C     xxPMTS        IFNE      *ZEROS
     C     CMSTAT        ORNE      'C'
     C                   MOVE      'Y'           xxEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     key1WMCMLC    READE     PCM0C                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
     C     xxPTDT        IFLT      ELPTDT
     C     xxEFLG        ANDEQ     'N'
     C                   Z-ADD     xxPTDT        xxDATE
     C     keyWMCMLC     SETLL     PCM0C
     C     key1WMCMLC    READE     PCM0C                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     ELPTDT
     C                   XFOOT     AR            xxPMTS           13 2
      *
     C     xxPMTS        IFNE      *ZEROS
     C     CMSTAT        ORNE      'C'
     C                   MOVE      'Y'           xxEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     key1WMCMLC    READE     PCM0C                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
      *  If claims with payments found outside new effective date range
      *
     C     xxEFLG        IFEQ      'Y'
      *
      *  Check for user authority to override error msg
      *  and condition display of Y/N override on prompt window
      *
     C     keyWMEXL      CHAIN     PEX00                              60
      *
     C                   move      *off          cadSecLev
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      secType       ##UTYP
     C                   MOVE      secLevel      ##ULVL
     C                   MOVE      EXPDCT        ##CTYP
     C                   MOVE      EXPDCL        ##CLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      *OFF          ##RES             1
      *
     C     ##RES         IFEQ      *OFF
     C                   move      *on           cadSecLev
     C                   ENDIF
      *
     C                   ENDIF
      *
      * Display "Claims outside eff date range" err window
      *
     C                   MOVE      *BLANK        scCADOver
      *
     C*
     C                   move      *off          cadCancel
     C                   move      *off          cadConfirm
     C*
     C                   Dou       cadCancel = *on or cadConfirm = *on
     C                   EXFMT     rcdClaimDt
     C                   ENDDO
     C*
     C     cadCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ELSE
      *
     C     scCADOver     Ifeq      'Y'
     C                   MOVE      'Y'           xxOFlg
     C                   ELSE
     C                   move      *on           error
     C                   Endif
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
     c*
     c                   ENDSR
      *
      *===============================================================
      * BSCDChg - BSCD Change on summarized policy popup
      *===============================================================
     C     BSCDChg       BEGSR
      *
      * POP UP WINDOW INFORMING USER THAT BSCD'S WILL BE CHANGED ON ALL
      * SUMMARIZED DIV IF ST RPT DIV IS CHG'D
      *
     C                   move      ' '           xxupdb
      *
     C     ChgMode       IFEQ      *ON
     C     xxSTRP        ANDEQ     'Y'
     C     xxOTAR        ANDEQ     'Y'
      *
     C     scBSCD        IFNE      ELBSCD
     C*
     C                   Dou       bscCancel = *on or bscConfirm = *on
     C                   EXFMT     rcdBSCDChg
     C                   ENDDO
     C*
     C     bscCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ELSE
     C                   MOVE      'Y'           xxUPDB
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     c*
     c                   ENDSR
     c*
      *===============================================================
      * SpreadFlag - Spread Adjustments Flag Popup
      *===============================================================
     C     SpreadFlag    BEGSR
      *
      * If state reporting division, check other division's (with summarized
      * A/R) Sprd Adj flag. Pop up window informing that other divisions
      * will be changed to match if they are different.
      *
     C                   MOVE      *OFF          *IN82
     C                   MOVE      ' '           xxUPDC
      *
     C     xxSTRP        IFEQ      'Y'
      *
     C     key1WMEML     CHAIN     PEM00                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFNE      'Y'
      *
     C     key2WDELL     CHAIN(N)  PEL00                              60
     C     *IN60         IFEQ      *OFF
     C     scFFL1        ANDNE     ELFFL1
     C                   SETON                                        82
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     key1WMEML     READE     PEM00                                  60
     C                   ENDDO
      *
      * Verify user wants to proceed by updating all summarized divs
      * to have new value for spread adjustments flag.
      *
     C     *IN82         IFEQ      *ON
     C*
     C                   CLEAR                   scUSFAll
     C                   move      *off          usfCancel
     C                   move      *off          usfConfirm
     C*
     C                   Dou       usfCancel = *on or usfConfirm = *on
     C                   EXFMT     rcdSpreadF
     C                   ENDDO
     C*
     C     usfCancel     IFEQ      *ON
     C                   move      *on           error
     C                   ELSE
      *
     C     scUSFAll      IFEQ      'Y'
     C                   move      'Y'           xxUPDC
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
     c*
     c                   ENDSR
      *
      /Free
       //*********************************************************************
       // EditFEIN
       //*********************************************************************
        BegSr EditFEIN;

          If flgFIFG = 'Y' And SCTaxId = *Zeros And flgQte <> 'Q';
            isError = *On;
            erTaxId = *On;
            mgTaxId = 'WC30649';
            LeaveSr;
          Endif;

          If flgPEOStatus = 'M';
             //If the Group PEO flag is "M", then bypass this editing
            LeaveSr;
          EndIf;

          If scMode = 'C' And scTaxId = EMfein;
            LeaveSr;
          Endif;


       // Check for duplicate fein if one has been entered.
       If not %Open(WpiLocMst);
           Open wpiLocMst;
           iOpen1 = *On;
       Endif;
       If scTaxId <> 0 and isError <> *on;
          feinCancel = *off;
          feinContin = *off;

          programIn = 'WTSEARCH';
          tabNameIn = 'Duplicate FEIN Listing';
          Clear dsSrchCrit;
          Clear selectIn;
          dsEmpFein = scTaxId;
          selectIn  = dsSrchCrit;
          If flgQte = *on;
             scDupLink = CreateBIGQteUrl(programIn:tabNameIn:modeIn:callIn:
              rrnOut:selectIn);
          Else;
             scDupLink = CreateBIGUrl(programIn:tabNameIn:modeIn:callIn:
              rrnOut:selectIn);
          Endif;

          Chain (scCo#:scTaxId) pem12;

          If not %found(wmeml12);
             CheckQuoteTaxId(%Editc(scCO#:'X'):%Editc(scTaxId:'X'):
              flgQte:outErr);
          Endif;


          If (%found(wmeml12) and (scFnd <> EMfnd or scEmp# <> EMemp#)) or
           outErr= '1';

             Dou feinCancel = *on or feinContin = *on;
                Exfmt rcdFeinDup;
             Enddo;

             If feinCancel = *on;
                isError = *on;
             Endif;

          Endif;

       Endif;
       If %Open(WpiLocMst) and iOpen1;
          Close wpiLocMst;
       Endif;
        EndSr;
       //*********************************************************************
       // EditStateRepFl
       //*********************************************************************
        BegSr EditStateRepFl;
       //Edit the state reporting flag (if not claims only)
       //  -In add employer mode the flag must be 'y'
       //  -In add division mode the flag should be 'n' if
       //   another division is already flagged 'y'
       //  -We have allowed the user to change a 'y' to a 'n' in update
       //   to eliminate the having to go in and out of divisions (ie.
       //      the user wishes to enter a division 2 w/'y'.  Division 1
       //      is already 'y'.
       //        Change division 1 to 'n' and enter divsion 2 w/'y'.

        // Default to error
        isError = *On;
        erStateRep = *On;

        If scStrep = ' ';
          scStrep = 'N';
        Endif;
        If scStrep <> 'Y' And scStrep <> 'N';
          mgStateRep = 'WCR0501';
          LeaveSr;
        Endif;

        If flgOvrStrp <> *On And scMode = 'C' And
         EMstrp = 'Y' And scStrep = 'N';
          flgOvrStrp  = *On;
          mgStateRep = 'WCR1279';
          LeaveSr;
        Endif;

        flgOvrStrp  = *Off;
        stRPFL = *Off;

        If AddDiv = *Off And scMode = 'A' And
         scStrep = 'N' And flgPEOStatus <> 'S';
          mgStateRep = 'WCR1277';
          LeaveSr;
        Endif;

        Setll (scCo#:scFnd:scEmp#) pem00;
        DoU %Eof(wmeml);
          Reade(n) (scCo#:scFnd:scEmp#) pem00;
          If %Eof(wmeml);
            Leave;
          Endif;
          If EMstrp = 'Y';
            stRPFL = *On;
            If scStrep = 'Y' And
             ((AddDiv = *Off And EMdiv <> SCdiv) Or
             AddDiv = *On);
              mgStateRep = 'WCR1278';
              LeaveSr;
            Endif;
          Endif;
        Enddo;

        If flgOvrStrp <> *On And stRPFL = *Off And
         scStrep <> 'Y';
          mgStateRep = 'WCR1279';
          LeaveSr;
        Endif;

        //reached here no error found!
        isError = *Off;
        erStateRep = *Off;
        flgOvrStrp  = *Off;
        Chain(n) (scCo#:scFnd:scEmp#:scDiv) pem00;

        EndSr;
       //*********************************************************************
       // Demographics - Call the city demographics popup procedure
       //*********************************************************************
        BegSr Demographics;

        If scPCity <> *blanks or scPState <> *blanks;
           demoCity = scPCity;
           demoSt   = scPState;
        Else;
           demoCity = scMCity;
           demoSt   = scMState;
        Endif;

        demoRRN = DemoPopup(scCo#:scFnd:demoSt:demoCity);

        If demoRRN <> 0;

           Chain demoRRN PCY0P;

           If %found;

              If scPCity <> *blanks or scPState <> *blanks;

                 If CYCTY <> *blanks;
                    scPCity  = CYCTY;
                 Endif;
                 If CYST <> *blanks;
                    scPState = CYST;
                 Endif;

              Else;

                 If CYCTY <> *blanks;
                    scMCity  = CYCTY;
                 Endif;
                 If CYST <> *blanks;
                    scMState = CYST;
                 Endif;

              Endif;

              If CYRGN <> 0;
                 scRegion = CYRGN;
              Endif;
              If CYLREP <> *blanks;
                 scSfLosRep = CYLREP;
              Endif;
              If CYCHAP <> 0;
                 scChapter = CYCHAP;
              Endif;
              If CYCNTY <> *blanks;
                 scCounty = CYCNTY;
              Endif;

           Endif;

        Endif;

        EndSr;
       //*********************************************************************
      /End-Free
     pValidBillDiv     E
     p*=====================================================================
     p* WriteBillDiv - Write Billing division data to file
     p*  Parm1   - Pointer to format DS
     p*  Returns - RRN of Employer Master record
     P*=====================================================================
     PWriteBillDiv     B                   Export
     DWriteBillDiv     PI             9  0
     D inPtrBillDiv                    *
      *
      /Free
       Return 1;
      /End-Free
     PWriteBillDiv     E
     p*=====================================================================
     p* UpdateBillDiv - Update Billing division changes to file
     p*  Parm1   - Pointer to format DS
     p*  Parm2   - RRN of Employer Master record
     p*  Parm3   - Mode
     p*  Returns - Error indicator
     P*=====================================================================
     PUpdateBillDiv    b                   Export
     DUpdateBillDiv    pi              n
     D inPtrBillDiv                    *
     D inRRN                          9  0
     D inMode                         1
     D inQTEM                         1    options(*nopass)
     d inFyr                          3  0 const options(*nopass)
     d elXXst          s              2    inz
     d oWDRRLds        ds                  likerec(prr00)
     d DivWDiffSt      s               n
      *
     DXX100            s            100    inz
     D#2Type           s             10    inz
     DxxZip1           s              5  0 inz
     DxxZip2           s              4  0 inz
      *** Beg Add ***** 08/24/21 ************************************
     d xxSt            s              2    inz
     d xxDate          s              7  0 inz
      *** End Add ***** 08/24/21 ************************************
      //*** beg add ***** 04/20/21 **************
       Dcl-S chrEnv Char(1);
       Dcl-S chrPeoRtCd Char(3);
       Dcl-S intPeoPfDt Packed(7:0);
       Dcl-S varSql VarChar(5000);
      //*** end add ***** 04/20/21 **************
      //*** Beg Add ***** 04/25/23 ***********************************
       Dcl-S svReq# like(CXreq#) Inz(0);
      //*** End Add ***** 04/25/23 ***********************************
      //*** Beg Add ***** 06/06/24 ***********************************
       Dcl-S flgBldNamChgJson Ind Inz(*Off);
      //*** End Add ***** 06/06/24 ***********************************
      *
     C     keyWDELL      KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    scFyr
     C*
     C     key1WDELL1    Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C                   Kfld                    scEMP#
     C*
     C*  Rate Set Code File Key List
     C*
     C     keyWMRSL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scRTCD
     C*
     C*  Statistical Reporting/Rating File Key List
     C*
     C     keyWDRRL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C                   Kfld                    xxDate
     C*
     C     key1WDRRL     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    xxSt
     C*
     C* Group Detail File Key List
     C*
     C     keyWDF1L      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scFYR
     C*  Increased Limits File Key List
     C*
     C     key2WTLML1    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
     C*
     C*  Deductible Discount File Key List
     C*
     C     key2WTDDL2    Klist
     C                   Kfld                    ELCO#
     C                   Kfld                    ELFND
     C                   Kfld                    xxSt
      *
      *
      /Free
       If %Parms > 3;
         If inQTEM = 'Q';
           qteStat = inQTEM;
         Else;
           qteStat = 'W';
         Endif;
       Endif;
       If inMode <> 'A';
         If not GetEmployer(inRRN:'L');
           Return *Off;
         EndIf;
       //*** begin add ***  03/02/23  ******************************************
       Else;
         If wmempAdded;
            GetEmployer(inRRN:'L');
         Endif;
       //*** end add *****  03/02/23  ******************************************
       EndIf;

       ptrBillDiv = inPtrBillDiv;

       intCo# = EMco#;
       intFnd = EMfnd;
       //*** Begin Change ** 04/15/21 **************************
       // infyr is the 5th parm, not 4th.
       //If %Parms >= 4;
       If %Parms >= 5;
       //*** End  Change ** 04/15/21 ***************************
          scfyr =  infyr;
       Endif;
       Chain(n) keyWDELL PEL00;
       If %found (WDELL);
          If scPFDT = 0;
             scPFDT = ELpfdt;
          Endif;
       Endif;
       Exsr MoveEMScDb;
       // Reportable changes for mid-term endorsements
       //*** begin chg ***  03/02/23  ******************************************
       //*****CheckRepChanges();
       If btnAddEftM = *off;
          CheckRepChanges();
       Endif;
       //*** end chg *****  03/02/23  ******************************************

       EMpgm  = Q1pgm;
       EMuser = Q1user;
       EMtime = Timeto6(SystemTime());
       EMdt   = Dateto7(SystemDate());

       //*** Begin Add *** 05/11/21 ***************************************
       If BtnAddEftM = *On;
          SaveTime = EMtime;
       Endif;
       //*** End   Add *** 05/11/21 ***************************************

       If inMode = 'A';
         Exsr Writer;
       Else;
         Exsr Updater;
       Endif;
       //*** Beg Add ***** 06/02/25 ************************
       If (SvSfPolSrv <> EMpsrv Or SvSfPrmAcc <> EMpacc Or
           SvSfAdtRvw <> EMarvw Or SvSfAdtMgr <> EMamgr Or
           SvSfLosRep <> EMlrep Or SvSfUndWtr <> EMuwtr Or
           SvClmCnt   <> EMclmc);
          StaffAssign();
       Endif;
       //*** End Add ***** 06/02/25 ************************

       If quote = *on;
         Exsr UpdWDELP;
       Endif;

       //Access third address file
       Chain (EMco#:EMfnd:EMemp#:EMdiv) pa300;
       If %found(wma3l);
         ExSr MoveA3ScDB;
         Update pa300;
       Else;
         // If data entered then write new record
         If SCbadd1 <> *Blanks Or SCbadd2 <> *Blanks Or SCbCity <> *Blanks
          Or SCbState <> *Blanks Or SCbcontact<>*Blanks Or SCOutAdd <> *Blanks;
           ExSr MoveA3ScDB;
           A3co# = EMco#;
           A3fnd = EMfnd;
           A3emp# = EMemp#;
           A3div = EMdiv;
           A3ipgm = EMpgm;
           A3iusr = EMuser;
           A3itm = EMtime;
           A3idt = EMdt;
           A3stat = 'A';
           Write pa300;
         Endif;
       EndIf;

       Return *On;
       //*******************************************************************
       // Writer   - SubRoutine
       //*******************************************************************
       BegSr Writer;

         EMco# = scCo#;
         EMfnd = scFnd;
         EMemp# = scEmp#;
         Select;
       //*** begin add ***  03/02/23  ******************************************
         When wmempAdded;
            scDiv = scXDiv;
       //*** end add *****  03/02/23  ******************************************
         When addDiv = *on;
            If FNAutd = 'Y';
               scDiv = GetLastDivision(EMco#:EMfnd:EMemp#) + 1;
            Else;
               scDiv = scDivision;
            Endif;
            scXDiv = scDiv;
         Endsl;
         EMdiv = scDiv;
         Clear EMqtfr;
         //*** Beg Add ***** 04/25/23 ******************************************
         intCanlDat = GetCxDate(emco#:emfnd:ememp#:stateRepDiv);
         If intCanlDat <> 0;
            EMcad1 = intCanlDat;
         Endif;
         //*** End Add ***** 04/25/23 ******************************************

         EMipgm = Q1pgm;
         EMiusr = Q1user;
         EMitim = Timeto6(SystemTime());
         EMidt = Dateto7(SystemDate());

         If AddDiv = *On and FlgQte = 'Q';
           EMrdfl = 'Y';
         Endif;
       //*** begin chg ***  03/02/23  ******************************************
       //****  Write pem0p;
       //****  lastDiv = EMdiv;
       //****  inRrn = EMrrnp;
       //****  Feod wmemp;
         If wmempAdded;
            Update pem0p;
         Else;
            Write pem0p;
            Feod wmemp;
         Endif;
         lastDiv = EMdiv;
         inRRN = GetEmpRRN(EMEmp#:EMDiv:EMFnd:EMCo#);

         //* No more processing if just adding WMEMP record for EFT.
         If btnAddEftM = *on;
            Return *on;
         Endif;
       //*** end chg *****  03/02/23  ******************************************

         //Create New Business Tracking Record
         Exsr NewBusTracking;

         //Write Cancel Detail and Cancel History recrods
         If getCxDate(emco#:emfnd:ememp#:stateRepDiv) <> 0;
            Exsr WrtCXRecords;
         Endif;

         If FlgManc = *On And FlgQte <> 'Q';
           Exsr MancAPI;
         Endif;
       Endsr;
       //*******************************************************************
       // Updater  - SubRoutine
       //*******************************************************************
       BegSr Updater;
         Update pem0p;
         //Update CiCenter with changes
         Exsr UpdCiCenter;
         //Update Exclude from Auto cash dissolve flag for all divisions
         If flgUpdEACD = *On;
           Exsr UpdExAutoFlg;
         Endif;
       //Update Quote Status record if name or chapter changed
         Exsr UpdQuoteSts;
       //If Employer name changed - update all location records
         Exsr UpdLocations;

       // Call API to update Managed care
         If FlgManc = *On And FlgQte <> 'Q';
           Exsr MancAPI;
         Endif;

       // Write DARTS
         If FlgQte <> 'Q' And xxCndr <> *blanks;
           Exsr WriteDarts;
         EndIf;
       Endsr;
       //*******************************************************************
       // WrtCXRecords  - Write Cancellation Detail and History records
       //*******************************************************************
       BegSr WrtCXRecords;
          //Copy Cncellation detail records of the State reporting division to n
          //*** Beg Chg ***** 04/25/23 *****************************************
          //***Setll (emco#:emfnd:scfyr:ememp#:stateRepDiv) pcx00;
          //***reade (emco#:emfnd:scfyr:ememp#:stateRepDiv) pcx00;
          //***Dow not %eof(wdcxl);
            //*** beg del ***** 02/25/22 *****************
            //*** cxcdat = DateTo7(SystemDate);
            //*** end del ***** 02/25/22 *****************
             //***cxdiv = emdiv;
             //***write pcx0p;
             //***reade (emco#:emfnd:scfyr:ememp#:stateRepDiv) pcx00;
          //***Enddo;

          //Copy Cncellation history records of the State reporting division to
          //*** Beg Chg ***** 04/25/23 *****************************************
          //***Setll (emco#:emfnd:scfyr:ememp#:stateRepDiv) pxh00;
          //***reade (emco#:emfnd:scfyr:ememp#:stateRepDiv) pxh00;
          //***Dow not %eof(wdxhl);
            //*** beg del ***** 02/23/22 *****************
            //*** xhcdat = DateTo7(SystemDate);
            //*** end del ***** 02/23/22 *****************
             //***xhdiv = emdiv;
             //***write pxh0p;
             //***reade (emco#:emfnd:scfyr:ememp#:stateRepDiv) pxh00;
          //***Enddo;
          //Copy Cncellation detail records of the State reporting division to
          //new added Division.
          Setgt  (EMco#:EMfnd:scFyr:EMemp#:stateRepDiv:*Hival) pcx00;
          Readpe (EMco#:EMfnd:scFyr:EMemp#:stateRepDiv) pcx00;
          If not %Eof(wdcxl) and CXrein <> 'Y';
             svReq# = CXreq#;
             CXdiv = EMdiv;
             CXreq# = 1;
             CXdt = DateTo7(SystemDate());
             CXtime = TimeTo6(SystemTime());
             CXuser = Q1user;
             CXpgm = Q1pgm;
             CXidt = CXdt;
             CXitm = CXtime;
             CXiusr = CXuser;
             CXipgm = CXpgm;
             Write pcx0p;

             //Copy Cncellation history records of the State reporting
             //division to new added Division.
             Setll (EMco#:EMfnd:scFyr:EMemp#:stateRepDiv:svReq#) pxh00;
             Reade (EMco#:EMfnd:scFyr:EMemp#:stateRepDiv:svReq#) pxh00;
             Dow not %Eof(wdxhl);
                XHdiv = emdiv;
                XHreq# = 1;
                XHdt = DateTo7(SystemDate());
                XHtime = TimeTo6(SystemTime());
                XHuser = Q1user;
                XHpgm = Q1pgm;
                Write pxh0p;
                Reade (EMco#:EMfnd:scFyr:EMemp#:stateRepDiv:svReq#) pxh00;
             Enddo;
          Endif;
          //*** End Chg ***** 04/25/23 *****************************************
       Endsr;
       //*******************************************************************
       // MoveEmScDB   - SubRoutine
       //*******************************************************************
       BegSr MoveEmScDB;
       EMstrp = SCstRep;
       EMname = %Subst(SCDbaNam:1:40);
       EMnam2 = %Subst(SCDbaNam:41:60);
       EMfdnm = %Subst(SCFedNam:1:40);
       EMfdn2 = %Subst(SCFedNam:41:60);
       If SCphn# > 0;
         EMarea = %Int(SCphn#/10000000);
         EMphn# = SCphn# - (EMarea*10000000);
       Else;
         EMarea = 0;
         EMphn# = 0;
       Endif;
       If SCfax# > 0;
         EMfaxa = %Int(SCfax#/10000000);
         EMfax# = SCfax# - (EMfaxa*10000000);
       Else;
         EMfaxa = 0;
         EMfax# = 0;
       Endif;
       //*** Beg Add *** 12/22/21 **********************************************
       EMmail = SCmail;
       //*** End Add *** 12/22/21 **********************************************

       EMiadr = SCEmpWeb;
       EMmad1 = SCmadd1;
       EMmad2 = SCmadd2;
       EMmcty = SCmcity;
       EMmst = SCmstate;
       EMmzip = SCmzip;
       EMpad1 = SCpadd1;
       EMpad2 = SCpadd2;
       EMpcty = SCpcity;
       EMpst = SCpstate;
       EMpzip = SCpzip;
       EMmpfl = SCAddFlg;
       EMcnty = SCcounty;
       EMfein = SCTaxId;
       EMrsk# = SCNRisk;
       EMsemp = SCSRisk;
       EMue# = SCSUnEmp;
       EMlsid = SCEmpLsId;
       EMpeosovr = xxPeoSovr;
       EMtpob = SCTypBus;
       EMnaob = SCNatBus;
       EMSIC = SCSIC;
       EMnaic = SCNaics;
       //*** Beg Del *** 03/04/24 ******************************************
       //***EMexrt = SCExpRat;
       //*** End Del *** 03/04/24 ******************************************
       EMshft = SCShEmp;
       EMrgn# = SCRegion;
       EMchap = SCChapter;
       EMoff# = SCServOff;
       EMwals = SCWatch;
       EMvip = SCvip;
       EMspac = SCSplAcc;
       EMlink = SCEmpLnk;
       EMcust = SCCustId;
       EMeacd = SCExAuto;
       EMnamp = scNaNam;
       EMfdnp = scFdNam;
       EMnami = scNaInNam;
       EMfdni = scFdInNam;
       EMrpeo = scOvRptPEO;
       EMOvrMP= scOvrMP;
       EMnonm = scNonMl;
       EMnonp = scNonPh;
       // Write Deposit Cap History Record if the value changes
       If scDepCap <> EMdepCap;
          ExSr WriteWMEMPH;
       EndIf;
       EMdepcap = scDEPCAP;
       SVExAuto = SCExAuto;
       If ClientID = '114';
          dcremp = *On;
       Else;
          dcremp = *Off;
       EndIf;

       If dcremp = *On;
          EMdcrx = SCDcrEmp;
       Endif;
       EMsTP = scSTP;
       EMpsrv = SCSfPolSrv;
       EMuwtr = SCSfUndWtr;
       EMpacc = SCSfPrmAcc;
       EMarvw = SCSfAdtRvw;
       EMamgr = SCSfAdtMgr;
       EMlrep = SCSfLosRep;
       EMcom1 = %Subst(SCComments:1:60);
       EMcom2 = %Subst(SCComments:61:60);
       EMcom3 = %Subst(SCComments:121:60);
       EMclmc = scClmCnt;
       If AddDiv;
          EMefd1=dtCYMD(scPFDT);
       Endif;

         If SCFlxv <> *blanks;
           EMflxv = SCFlxv;
         Endif;

       EMPeoNmIn = scPeoNmIn;
       //*** Beg Add ***** 06/06/24 *******************************************
       If isInvCloud And isGroupIncluded;
          flgBldNamChgJson = *Off;
          If SCMAdd1 <> *Blank;
             strAdd1 = SCMAdd1;
             strAdd2 = SCMAdd2;
             strCity = SCMCity;
             strState = SCMState;
             intZip = SCMZip;
          Else;
             strAdd1 = SCPAdd1;
             strAdd2 = SCPAdd2;
             strCity = SCPCity;
             strState = SCPState;
             intZip = SCPZip;
          Endif;
          //*** Begin Add *** 11/11/25 *************************************
          //
          // Don't Send Name Change if not at IVC
          Exec Sql
             Select count(*)
             Into :cntWIPSP
             From WIPSP
             Where PSco#=:EMco# And PSfnd=:EMfnd and PSemp#=:EMemp#;

          If cntWIPSP <> 0;
          //*** End   Add *** 11/11/25 *************************************
          //DBA Name Change
          If SCDbaNam <> SCDbaNmHld;
             flgBldNamChgJson = *On;
          Endif;
          //Mailing Address Change
          If SCMAdd1 <> SCMAdd1Hld or SCMAdd2 <> SCMAdd2Hld
           or SCMCity <> SCMCityHld or SCMState  <> SCMStHld
           or SCMZip <> SCMZipHld;
             flgBldNamChgJson = *On;
          Endif;
          //Physical address change when Mailing address doesn't exist
          If (SCPAdd1 <> SCPAdd1Hld or SCPAdd2 <> SCPAdd2Hld
           or SCPCity <> SCPCityHld or SCPState  <> SCPStHld
           or SCPZip <> SCPZipHld) and SCMAdd1 = *Blank;
             flgBldNamChgJson = *On;
          Endif;

          If flgBldNamChgJson = *On;
             strJsonIC =  '{';
             strJsonIC += '"servicer":"Invoice Cloud",';
             strJsonIC += '"transaction":"Name Change",';
             strJsonIC += '"information": {';
             strJsonIC += BuildJsonNode('accountNumber':
              %editc(EMCo#:'X') + %editc(EMFnd:'X') + %editc(EMEmp#:'X')) + ',';
             strJsonIC += '"customerDetails": {';
             strJsonIC += BuildJsonNode('customerName':%Trim(SCDbaNam)) + ',';
             strJsonIC += BuildJsonNode('address1':%Trim(strAdd1)) + ',';
             strJsonIC += BuildJsonNode('address2':%Trim(strAdd2)) + ',';
             strJsonIC += BuildJsonNode('city':%Trim(strCity)) + ',';
             strJsonIC += BuildJsonNode('state':%Trim(strState)) + ',';
             strJsonIC += BuildJsonNode('zip':%Editc(intZip:'X'));
             strJsonIC += '} } }';
             jsonIC_Data = strJsonIC;
             jsonIC_Len  = %Len(%Trim(strJsonIC));
             LoadSIWEBSVCDataQ(jsonIC);
          Endif;
          //*** Begin Add *** 11/11/25 *************************************
          EndIf;
          //*** End   Add *** 11/11/25 *************************************
       Endif;
       //*** End Add ***** 06/06/24 *******************************************

       EndSr;
       //*******************************************************************
       // UpdWDELP - SubRoutine to update employer detail for Quote
       //*******************************************************************
       BegSr UpdWDELP;

       Chain keyWDF1L PF100;
       Chain keyWDELL PEL00;

       //***Beg Del **** 08/24/21 ******************************************
       // When Rating State change, carry forward only valid Rating Values.
       // ELrtcd is blank if adding new policy.
       //Clear oWDRRLds;
       //IF ELrtcd <> scRTCD and ELrtcd <> '  ';
       //   DivWDiffSt = *on;
       //   CHAIN (scCO#:ELrtcd) PRS00;
       //   IF %found(WMRSL);
       //      elxxST = RSjrst;
       //      xxDate = 0;
       //      xxDate = dtCYMD(scPFDT);
       //      Setgt (scCO#:scFND:elxxST:xxDate) PRR00;
       //      Readpe (scCO#:scFND:elxxST) PRR00 oWDRRLds;
       //   ENDIF;
       //ELSE;
       //   DivWDiffSt = *off;
       //ENDIF;
     C*                   CLEAR                   xxST              2
     C*     keyWMRSL      CHAIN     PRS00                              60
     C*  N60              MOVEL     RSJRST        xxST
     C*                   z-add     0             xxDate            7 0
     C*                   eval      xxDate = dtCYMD(scPFDT)
     C**
     C*     keyWDRRL      SETGT     PRR00
     C*     key1WDRRL     READPE    PRR00                                  60
     C**
     C**  IF THE RATE SET CODE HAS BEEN CHANGED, THEN RELOAD THE STOCK
     C**  DISCOUNT DEFAULTS
     C**
     C*     *IN60         IFEQ      *OFF
     C**
     C*     ELRTCD        IFNE      scRTCD
     C*     xxSVST        ANDNE     xxST
     C*     ELASP3        ANDEQ     *ZEROS
     C**
     C*                   MOVE      *BLANKS       ELDISC
     C**
     C*     RRDISC        IFNE      *BLANKS
     C*                   MOVEL     RRDISC        ELDISC
     C*                   ELSE
     C*                   Z-ADD     RRDSPC        ELASP3
     C*                   ENDIF
     C**
     C*                   ENDIF
       //If not %found(WDELL) or DivWDiffSt = *on;
       // // Waiver setup
       //   Select;
       //   When RRwdtp <> *blank;
       //    // Default from ratings and statistics
       //       ELwvtp = RRwdtp;
       //       ELwvpc = RRwdpc;
       //       ELfchg = RRwdfc;
       //       ELwmin = RRwdmn;
       //       ELwvfl = RRwvfl;
       //   When RRwvac <> *zeros;
       //    // Let state rpt div values copy over
       //   Other;
       //       Clear ELwvtp;
       //       Clear ELwvpc;
       //       Clear ELfchg;
       //       Clear ELwmin;
       //       Clear ELwvfl;
       //   Endsl;
       //    // Safety fee setup
       //   Select;
       //   When RRsfpc <> *zeros;
       //    // Default from ratings and statistics
       //       ELSfpc = RRSfpc;
       //       ELSfmn = RRSfam;
       //   Other;
       //    // Let state rpt div values copy over
       //   Endsl;
       //Endif;
     c*                   Endif

       //IF DivWDiffSt = *on;
       //   IF RRCCCD <> oWDRRLds.RRCCCD; // Contractor Credit
       //      ELASP1 = 0;
       //      ELCREF = 0;
       //      ELCREX = 0;
       //   ENDIF;
       //   IF RRDV1C <> oWDRRLds.RRDV1C; // Deviation 1
       //      ELASP2 = 0;
       //      ELDVEF = 0;
       //      ELDVEX = 0;
       //   ENDIF;
       //   IF RRDV2C <> oWDRRLds.RRDV2C; // Deviation 2
       //      ELDEV2 = 0;
       //      ELD2EF = 0;
       //      ELD2EX = 0;
       //      ELDEV2F = 0;   // CareWest Territory Rating
       //   ENDIF;
       //   IF RR1ACD <> oWDRRLds.RR1ACD; // Subj prem credit 1A
       //      ELASPM = 0;
       //      ELMCEF = 0;
       //      ELMCEX = 0;
       //   ENDIF;
       //   IF RR1BCD <> oWDRRLds.RR1BCD; // Subj prem credit 1B
       //      ELASPS = 0;
       //      ELSCEF = 0;
       //      ELSCEX = 0;
       //   ENDIF;
       //   IF RRCRCD <> oWDRRLds.RRCRCD or RRDBCD <> oWDRRLds.RRDBCD; //Schedul
       //      ELDBCR = 0;
       //      ELSDEF = 0;
       //      ELSDEX = 0;
       //   ENDIF;
       //ENDIF;
       //***End Del ***** 08/24/21 ******************************************
       Chain (scCo#:scFnd) PFH00;
       If %found(WDFHL);
         ELFFD1 = FHF1DF;
         ELFFD2 = FHF2DF;
         ELFFD3 = FHF3DF;
       Else;
         ELFFD1 = EXFDF1;
         ELFFD2 = EXFDF2;
       Endif;

       ELRTCD = scRTCD;
       ELBSCD = scBSCD;
       ELPFDT = dtCYMD(scPFDT);
       ELPTDT = dtCYMD(scPTDT);
       ELANDT = dtCYMD(scANDT);
       ELBSDT = dtCYMD(scBSDT);
       ELqtyp = SCqtyp;
       ELtypb = SCtypb;
       ELSTAT = scSTAT;
       ELBLFL = scBLFL;
       ELASCD = scASCD;
       ELPFIN = scPFIN;
       ELFFL1 = scFFL1;
       ELARSM = scARSM;
       ELIFFL = scIFFL;
       If quote = *on;
          ELEMPY = scEMPY;
       Endif;

       ELPGM  = Q1PGM;
       ELUSER = Q1USER;
       ELTIME = Timeto6(SystemTime());
       ELDt   = Dateto7(SystemDate());

       If %found(WDELL);
         Update PEL00;

          //*** Beg Add ***** 11/28/24 ***********************************
          If inMode = 'M';                 // Policy/Quote Entry
             If EMefd1 <> ELpfdt;          // Inception date <> Policy from date
                Chain (scCo#:scFnd:scEmp#:scDiv) pem00;
                If %found(wmeml);
                   EMefd1 = ELpfdt;
                   Update pem00;
                Endif;
             Endif;
          Endif;
          //*** End Add ***** 11/28/24 ***********************************
       Else;
       //*** Beg Del ***** 08/24/21 ********************************************
       // As currently state reporting divison's record is being read, change
       // the Rating Information screen's effective dates according to the curre
       // division (to prevent their default population with state reporting div

       // For Increased Limits
       //  If ELILCD <> ' ';
       //     ELILEF = dtCYMD(scPFDT);
       //  endif;
       //  If ELINL2 <> ' ';
       //     ELI2EF = dtCYMD(scPFDT);
       //  endif;
       //
       // For Deductible Discount
       //  If ELDCDE <> ' ';
       //     ELDCEF = dtCYMD(scPFDT);
       //  endif;
       //  If ELDCD2 <> ' ';
       //     EL2DEF = dtCYMD(scPFDT);
       //  endif;
       //
       // For Schedule DB/CR
       //  If ELDBCR <> *zeros;
       //     ELSDEF = dtCYMD(scPFDT);
       //  endif;
       //
       // For Deviation 1
       //  If ELASP2 <> *zeros;
       //     ELDVEF = dtCYMD(scPFDT);
       //  endif;
       //  If ELD1S2 <> *zeros;
       //     ELD1D2 = dtCYMD(scPFDT);
       //  endif;

       // For Deviation 2
       //  If ELdev2 <> 0 Or ELdev2F <> 0;
       //     ELD2EF = dtCYMD(scPFDT);
       //  endif;
       //  If ELD2S2 <> *zeros;
       //     ELD2D2 = dtCYMD(scPFDT);
       //  endif;

       // For Subject Premium Credit 1A
       //  If ELASPM <> *zeros;
       //     ELMCEF = dtCYMD(scPFDT);
       //  endif;

       // For Subject Premium Credit 1B
       //  If ELASPS <> *zeros;
       //     ELSCEF = dtCYMD(scPFDT);
       //  endif;

       // For Contractors Credit
       //  If ELASP1 <> *zeros;
       //     ELCREF = dtCYMD(scPFDT);
       //  endif;
       //
       // For Facultative Reinsurance
       //  If ELARCP <> *zeros;
       //     ELAREF = dtCYMD(scPFDT);
       //  endif;
       //***End Del ***** 08/24/21 ***************************************
         ELco# = scCo#;
         ELfnd = scFnd;
         ELemp# = scEmp#;
         ELdiv = scDiv;
         ELfyr = scFyr;

         GetRenewStatus(ELCO#: ELFND: ELEMP#: ELFYR: ELNWRW);
       //*** beg chg ***** 08/24/21 ***********************
       // Clear premium figures on write
       //  Clear ELemnp;
       //  Clear ELEMNP;
       //  Clear ELAMNP;
       //  Clear ELMAPR;
       //  Clear ELAMPR;
       //  Clear ELESBJ;
       //  Clear ELASBJ;
       //  Clear ELSTPR;
       //  Clear ELASPR;
       //  Clear ELEMPM;
       //  Clear ELAMPM;
       //  Clear ELENMP;
       //  Clear ELANMP;
       //  Clear ELANPR;
       //  Clear ELAAPR;
       //  Clear ELINMP;
       //  Clear ELTXPR;
       //  Clear ELTXDU;
       //  Clear ELEDC1;
       //  Clear ELADC1;
       //  Clear ELEDC2;
       //  Clear ELADC2;
       //  Clear ELEDV2;
       //  Clear ELADV2;
       //  Clear ELEDC3;
       //  Clear ELADC3;
       //  Clear ELEXP1;
       //  Clear ELEXP2;
       //  Clear ELEXP3;
       //  Clear ELAXP1;
       //  Clear ELAXP2;
       //  Clear ELAXP3;
       //  Clear ELEIN1;
       //  Clear ELAIN1;
       //  Clear ELEIN2;
       //  Clear ELAIN2;
       //  Clear ELEDCT;
       //  Clear ELADCT;
       //  Clear ELEDCM;
       //  Clear ELADCM;
       //  Clear ELEDCS;
       //  Clear ELADCS;
       //  Clear ELEDCR;
       //  Clear ELADCR;
       //  Clear ELEDC ;
       //  Clear ELADC ;
       //  Clear ELEMIN;
       //  Clear ELAMIN;
       //  Clear ELSTAM;
       //  Clear ELRETN;
       //  Clear ELEFRS;
       //  Clear ELAFRS;
       //  Clear EL1APR;
       //  Clear EL2APR;
       //  Clear ELSBFE;
       //  Clear ELETCA;
       //  Clear ELATCA;
       //  Clear EL1FPR;
       //  Clear EL2FPR;
       //  Clear EL1AAM;
       //  Clear EL2AAM;
       //  Clear ELMFAM;
       //  Clear ELSFAM;
       //  Clear EL3APR;
       //  Clear EL3FPR;
       //  Clear EL3AAM;
       //  Clear ELETER;
       //  Clear ELATER;
       //  Clear ELRPE1;
       //  Clear ELRPA1;
       //  Clear ELRPE2;
       //  Clear ELRPA2;
       //  Clear ELRPE3;
       //  Clear ELRPA3;
       //  Clear EL4APR;
       //  Clear EL4FPR;
       //  Clear EL4AAM;
       //  Clear EL5APR;
       //  Clear EL5FPR;
       //  Clear EL5AAM;
       //  Clear ELEDD2;
       //  Clear ELADD2;
       //  Clear ELEDTR;
       //  Clear ELADTR;
       //  Clear EL6APR;
       //  Clear EL6FPR;
       //  Clear EL6AAM;
       //  Clear EL7APR;
       //  Clear EL7FPR;
       //  Clear EL7AAM;
       //  Clear ELqtfr;
       //  Clear ELfaf;
       //  ELAafl = F1Aafl;
       //  ELApfl = F1Apfl;
       //  ELmfee = F1mfee;
       //  //Disable Increased Limits fields if no active codes are available.
       //  Setll key2WTLML1 PLM01;
       //  Reade key2WTLML1 PLM01;
       //  Dow not %eof and LMSTAT = 'I';
       //  Reade key2WTLML1 PLM01;
       //  Enddo;
       //
       //  If %Eof(wtlml1);
       //     ELilcd = *Blanks;
       //     ELinl2 = *Blanks;
       //     // Increased limits 1 and 2
       //     ELilef = *Zeros;
       //     ELilex = *Zeros;
       //     ELi2ef = *Zeros;
       //     ELi2ex = *Zeros;
       //  Endif;
       //// Check if the Limit code is valid to this state
       //  Chain (ELco#:ELfnd:xxSt:ELilcd) plm01;
       //  IF not %found(wtlml1);
       //     ELilcd = *Blanks;
       //     ELilef = *Zeros;
       //     ELilex = *Zeros;
       //  ENDIF;
       //  Chain (ELco#:ELfnd:xxSt:ELinl2) plm01;
       //  IF not %found(wtlml1);
       //     ELinl2 = *Blanks;
       //     ELi2ef = *Zeros;
       //     ELi2ex = *Zeros;
       //  ENDIF;
       //
       //  //Disable Deductible Discount fields if no active codes are available
       //  Setll key2WTDDL2 PDD02;
       //  Reade key2WTDDL2 PDD02;
       //  Dow not %eof and DDSTAT = 'I';
       //  Reade key2WTDDL2 PDD02;
       //  Enddo;
       //
       //  If %Eof(wtddl2);
       //     ELdcde = *Blanks;
       //     ELdcd2 = *Blanks;
       //     // Deductible Discount 1 and 2
       //     ELdcef = *Zeros;
       //     ELdcex = *Zeros;
       //     EL2def = *Zeros;
       //     EL2dex = *Zeros;
       //  Endif;
       //// Check if the Deductible code is valid to this state
       //  Chain (ELco#:ELfnd:xxSt:ELdcde) pdd02;
       //  IF not %found(wtddl2);
       //     ELdcde = *Blanks;
       //     ELdcef = *Zeros;
       //     ELdcex = *Zeros;
       //  ENDIF;
       //  Chain (ELco#:ELfnd:xxSt:ELdcd2) pdd02;
       //  IF not %found(wtddl2);
       //     ELdcd2 = *Blanks;
       //     EL2def = *Zeros;
       //     EL2dex = *Zeros;
       //  ENDIF;
       //  If clientID = '150';
       //
       //      Chain(n) (ELCo#:ELFnd:ELFyr) pf102;
       //      If %found;
       //         ELManP = L2F1ManP;
       //      Endif;
       //
       //  Endif;
         Exsr ClearPremiums;
         Exsr ClearRatingFactors;
       //*** End Chg**** 08/24/21 ***************************************

         ELIPGM = Q1PGM;
         ELIUSR = Q1USER;
         ELITM = Timeto6(SystemTime());
         ELIDT = Dateto7(SystemDate());
         Write PEL00;
       Endif;

       //*** Beg Add ***** 08/24/21 ***************************
       If dsEntryType <> *Blanks;
          inEntryType = dsEntryType;
          inELrrn = dbRRN2;
          DefaultRatingFactors(inELrrn:inEntryType);
       Endif;
       //*** End Add ***** 08/24/21 ***************************
     C*
     C     XXUPDB        IFEQ      'Y'
     C                   EXSR      SRUPDB
     C                   ENDIF
     C*
     C     XXUPDC        IFEQ      'Y'
     C                   EXSR      SRUPDC
     C                   ENDIF
       EndSr;
       //*** Beg Add ***** 08/24/21 ***************************
       //--------------------------------------------------
       Begsr ClearRatingFactors;
       //--------------------------------------------------
        Clear ELmanP;
        Clear ELfFl2;
        Clear ELcMul;
        Clear ELiLCd;
        Clear ELiLEf;
        Clear ELiLEx;
        Clear ELi2Ef;
        Clear ELi2Ex;
        Clear ELcxBl;
        Clear ELdCde;
        Clear ELdCd2;
        Clear ELdCEf;
        Clear ELdCEx;
        Clear EL2DEf;
        Clear EL2DEx;
        Clear ELasp2;
        Clear ELdvEf;
        Clear ELdvEx;
        Clear ELd1S2;
        Clear ELd1D2;
        Clear ELd1X2;
        Clear ELstFc;
        Clear ELdev2F;
        Clear ELdev2;
        Clear ELd2Ef;
        Clear ELd2Ex;
        Clear ELd2S2;
        Clear ELd2D2;
        Clear ELd2X2;
        Clear ELmcCd;
        Clear ELmcGp;
        Clear ELaspM;
        Clear ELmcEf;
        Clear ELmcEx;
        Clear ELaspS;
        Clear ELscEf;
        Clear ELscEx;
        Clear ELasp1;
        Clear ELcrEf;
        Clear ELcrEx;
        Clear ELdbCr;
        Clear ELsdEf;
        Clear ELsdEx;
        Clear ELprom;
        Clear ELexM1;
        Clear ELeMD1;
        Clear ELeMD2;
        Clear ELexM2;
        Clear ELexM3;
        Clear ELeMD3;
        Clear ELaRp1;
        Clear ELaRp2;
        Clear ELaRp3;
        Clear ELexMd1Nf;
        Clear ELexMd2Nf;
        Clear ELexMd3Nf;
        Clear ELasp3;
        Clear ELdisc;
        Clear ELdisP;
        Clear ELmFee;
        Clear ELsFMn;
        Clear ELsFPc;
        Clear ELwvTp;
        Clear ELwvPc;
        Clear ELfChg;
        Clear ELwMin;
        Clear ELwMax;
        Clear ELwvFl;
        Clear ELarCp;
        Clear ELarCa;
        Clear ELarEf;
        Clear ELarEx;
        Clear ELvLCa;
       EndSr;
       //--------------------------------------------------
       Begsr ClearPremiums;
       //--------------------------------------------------
        Clear ELemnp;
        Clear ELEMNP;
        Clear ELAMNP;
        Clear ELMAPR;
        Clear ELAMPR;
        Clear ELESBJ;
        Clear ELASBJ;
        Clear ELSTPR;
        Clear ELASPR;
        Clear ELEMPM;
        Clear ELAMPM;
        Clear ELENMP;
        Clear ELANMP;
        Clear ELANPR;
        Clear ELAAPR;
        Clear ELINMP;
        Clear ELTXPR;
        Clear ELTXDU;
        Clear ELEDC1;
        Clear ELADC1;
        Clear ELEDC2;
        Clear ELADC2;
        Clear ELEDV2;
        Clear ELADV2;
        Clear ELEDC3;
        Clear ELADC3;
        Clear ELEXP1;
        Clear ELEXP2;
        Clear ELEXP3;
        Clear ELAXP1;
        Clear ELAXP2;
        Clear ELAXP3;
        Clear ELEIN1;
        Clear ELAIN1;
        Clear ELEIN2;
        Clear ELAIN2;
        Clear ELEDCT;
        Clear ELADCT;
        Clear ELEDCM;
        Clear ELADCM;
        Clear ELEDCS;
        Clear ELADCS;
        Clear ELEDCR;
        Clear ELADCR;
        Clear ELEDC ;
        Clear ELADC ;
        Clear ELEMIN;
        Clear ELAMIN;
        Clear ELSTAM;
        Clear ELRETN;
        Clear ELEFRS;
        Clear ELAFRS;
        Clear EL1APR;
        Clear EL2APR;
        Clear ELSBFE;
        Clear ELETCA;
        Clear ELATCA;
        Clear EL1FPR;
        Clear EL2FPR;
        Clear EL1AAM;
        Clear EL2AAM;
        Clear ELMFAM;
        Clear ELSFAM;
        Clear EL3APR;
        Clear EL3FPR;
        Clear EL3AAM;
        Clear ELETER;
        Clear ELATER;
        Clear ELRPE1;
        Clear ELRPA1;
        Clear ELRPE2;
        Clear ELRPA2;
        Clear ELRPE3;
        Clear ELRPA3;
        Clear EL4APR;
        Clear EL4FPR;
        Clear EL4AAM;
        Clear EL5APR;
        Clear EL5FPR;
        Clear EL5AAM;
        Clear ELEDD2;
        Clear ELADD2;
        Clear ELEDTR;
        Clear ELADTR;
        Clear EL6APR;
        Clear EL6FPR;
        Clear EL6AAM;
        Clear EL7APR;
        Clear EL7FPR;
        Clear EL7AAM;
        Clear ELqtfr;
        Clear ELfaf;
        ELAafl = F1Aafl;
        ELApfl = F1Apfl;
        ELmfee = F1mfee;
       Endsr;
       //*** End Add ***** 08/24/21 ***************************
      *====================================================
      * SRUPDB - UPDATE BSCD'S IN ALL SUMMARIZED DIVISIONS
      *====================================================
     C     SRUPDB        BEGSR
     C*
     C     key1WDELL1    SETLL     PEL01
     C     key1WDELL1    READE     PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELARSM        IFEQ      'Y'
     C                   MOVE      scBSCD        ELBSCD
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C     key1WDELL1    READE     PEL01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
      *====================================================
      * SRUPDC - UPDATE SPREAD FLAG IN ALL DIVISIONS
      *====================================================
     C     SRUPDC        BEGSR
     C*
     C     key1WDELL1    SETLL     PEL01
     C     key1WDELL1    READE     PEL01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      scFFl1        ELFFL1
     C                   UPDATE    PEL01
     C*
     C     key1WDELL1    READE     PEL01                                  60
     C                   ENDDO
     C*
     C                   ENDSR
      /free
       //*******************************************************************
       // MoveA3ScDB   - SubRoutine
       //*******************************************************************
       BegSr MoveA3ScDB;
         A3adr1 = SCbadd1;
         A3adr2 = SCbadd2;
         A3cty = SCbcity;
         A3st = SCbstate;
         If SCbzip > 0;
           xxZip1 = %Int(SCbzip/10000);
           A3zip1 = %Editc(xxZip1:'X');
           xxZip2 = %Int(SCbzip - (xxZip1 * 10000));
           A3zip2 = %Editc(xxZip2:'X');
         Else;
           A3zip1 = *Blanks;
           A3zip2 = *Blanks;
         Endif;
         A3cont = SCbcontact;
         A3ooc = SCOutAdd;
         A3pgm = EMpgm;
         A3user = EMuser;
         A3time = EMtime;
         A3dt = EMdt;
       EndSr;
       //*******************************************************************
       // UpdCiCenter  - SubRoutine
       //*******************************************************************
       BegSr UpdCiCenter;
         Clear #2Type;
         XX100 = EMname + EMNam2;
         Select;
         When svDBA <> XX100;
           #2Type = 'DBA';
         When SVfein <> EMFEIN;
           #2Type = 'FEIN';
         EndSl;

         If #2Type <> *Blanks;
           CiCEmpChange(inRRN : #2Type : SVfein : SVdba : SVdiv ) ;
         EndIf;

         //svDBA = EMname + EMnam2;
         //svFein = EMfein;
       EndSr;
       //*******************************************************************
       // UpdExAutoFlg - SubRoutine
       //        Loop thru all records for current employer in
       //        employer master and update new flag.
       //*******************************************************************
       BegSr UpdExAutoFlg;
         Setll (scCo#:scFnd:scEmp#) pem00;
         DoU %Eof(wmeml);
           Reade (scCo#:scFnd:scEmp#) pem00;
           If %Eof(wmeml);
             Leave;
           Endif;
           EMuser = Q1user;
           EMpgm = Q1pgm;
           EMtime = Timeto6(SystemTime());
           EMdt = Dateto7(SystemDate());
           EMeacd = scExAuto;
           Update pem00;
         Enddo;
         Chain(n) (scCo#:scFnd:scEmp#:scDiv) pem00;
       EndSr;
      /End-Free
     C**********************************************************************
     C*// Update Quote Status if name or Chapter changed
     C**********************************************************************
     C     UpdQuoteSts   BegSr
     C     QTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    qteStat           1
      *
     C     EMNAME        IFNE      SVNAME
     C     EMCHAP        ORNE      SVCHAP
     C                   MOVEL     EMNAME        SVNAM2
      *
     C     QTKY          SETLL     PQT07
     C     QTKY          READE     PQT07                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     QTNAME        IFNE      SVNAM2
     C     QTCHAP        ORNE      EMCHAP
     C     QTrrn7        CHAIN     PQT0P                              60
     C                   MOVEL(P)  EMNAME        QTNAME
     C                   MOVEL(P)  EMCHAP        QTCHAP
     C                   MOVE      Q1USER        QTUSER
     C                   MOVE      Q1PGM         QTPGM
     C                   EVAL      QTDT = DateTo7(SystemDate())
     C                   EVAL      QTTIME = TimeTo6(SystemTime())
     C                   UPDATE    PQT0P
     C                   ENDIF
     C     QTKY          READE     PQT07                                  60
     C                   ENDDO
     C                   ENDIF
     C                   EndSr
     C**********************************************************************
     C*// Update Locations
     C**********************************************************************
     C     UpdLocations  BegSr
     C     ALKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV

     C     EMNAME        IFNE      SVNAME
     C     EMFDNM        ORNE      SVFDNM
     C     EMFEIN        ORNE      SVFEIN
     C     EMTPOB        ORNE      SVTPOB
     C     EMNAMP        ORNE      SVNAMP
     C     EMFDNP        ORNE      SVFDNP
      *
     C                   CALL      'WA085R'
     C                   PARM      'E'           x9TYPE            1
     C                   PARM      emrrnp        x9RRN             9 0
     C                   PARM      *BLANK        x9RTN             1
      *
     C     ALKY          CHAIN     PAL04                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ALDLDT        IFEQ      0
     C     ALDLDT        ORGT      EMDT
      *
     C     ALFEIN        IFNE      EMFEIN
     C     ALTPOB        ORNE      EMTPOB
     C                   MOVEL     EMFEIN        ALFEIN
     C                   MOVEL     EMTPOB        ALTPOB
     C                   MOVEL(P)  Q1USER        ALUSER
     C                   MOVEL(P)  Q1PGM         ALPGM
     C                   EVAL      ALDT = DateTo7(SystemDate())
     C                   EVAL      ALTIME = TimeTo6(SystemTime())
     C                   UPDATE    PAL04
     C                   ENDIF
     C                   ENDIF
      *
     C     ALKY          READE     PAL04                                  60
      *
     C                   ENDDO
      *
     C                   ENDIF
     C                   EndSr
      *===============================================================
      * NewBusTracking - Create New Business Tracking Record
      *===============================================================
      *
     C     NewBusTrackingBEGSR
      *
     C     NBKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
      *
     C                   MOVEL     'WC'          XSKEY             2
     C     XSKEY         CHAIN     SWC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     WCFL26        ANDEQ     'Y'
      *
     C     NBKY          CHAIN     PNB00                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PNB00
     C                   ENDIF
      *
     C                   Z-ADD     EMCO#         NBCO#
     C                   Z-ADD     EMFND         NBFND
     C                   Z-ADD     EMEMP#        NBEMP#
      *
     C                   MOVEL     Q1USER        NBIUSR
     C                   MOVEL     Q1USER        NBUSER
     C                   MOVEL     Q1PGM         NBIPGM
     C                   MOVEL     Q1PGM         NBPGM
     C                   EVAL      NBidt = Dateto7(SystemDate())
     C                   EVAL      NBdt = NBidt
     C                   EVAL      NBitm = Timeto6(SystemTime())
     C                   EVAL      NBtime = NBitm
      *
     C                   Z-ADD     NBDT          NBENDT
     C                   WRITE     PNB00
     C                   ENDIF
      *
     C                   ENDSR
      *===============================================================
      * MancAPI - DATA INTERFACE TO MANAGED CARE - FMEMP
      *===============================================================
      *
     C     MancAPI       BEGSR
      *
     C                   MOVE      EMCO#         QQCO#
     C                   MOVE      EMFND         QQFND#
     C                   MOVE      EMEMP#        QQEMP#
     C                   MOVE      EMDIV         QQDIV
      *
     C                   MOVE      EMFEIN        QQFEIN
     C                   MOVE      EMMZIP        QQMZIP
     C                   MOVE      EMPZIP        QQPZIP
     C                   MOVE      EMAREA        QQAREA
     C                   MOVE      EMPHN#        QQPHN#
     C                   MOVE      EMRGN#        QQRGN#
      *
     C                   CALL      'FA010R'
      *
     C                   PARM      *BLANK        QQRTCD            1
      *
     C                   PARM                    QQCO#             3
     C                   PARM      *BLANK        QQSUB#            5
     C                   PARM                    QQFND#            3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
      *
     C                   PARM      EMNAME        QQNAME           40
     C                   PARM      EMMAD1        QQMAD1           40
     C                   PARM      EMMAD2        QQMAD2           40
     C                   PARM      EMMCTY        QQMCTY           25
     C                   PARM      EMMST         QQMST             2
     C                   PARM                    QQMZIP            9
     C                   PARM      EMPAD1        QQPAD1           40
     C                   PARM      EMPAD2        QQPAD2           40
     C                   PARM      EMPCTY        QQPCTY           25
     C                   PARM      EMPST         QQPST             2
     C                   PARM                    QQPZIP            9
     C                   PARM                    QQAREA            3
     C                   PARM                    QQPHN#            7
     C                   PARM                    QQRGN#            3
     C                   PARM                    QQFEIN            9
     C                   PARM      EMVIP         QQVIP             1
     C                   PARM      EMMPFL        QQMPFL            1
     C                   PARM      EMSTRP        QQSTRP            1
     C                   PARM      EMFDNM        QQDBA            40
     C                   PARM      EMNAOB        QQNOB            40
     C                   PARM      *BLANK        QQMCO            10
     C                   PARM      *BLANK        QQMCO#           20
      *
     C                   ENDSR
     C*================================================================
     C* WriteDARTS  - WRITE DART REMAARKS
     C*================================================================
     C     WriteDARTS    BEGSR
    C*
     C                   Z-ADD     EMCO#         @#CO#             3 0
     C                   Z-ADD     EMFND         @#FND             3 0
     C                   Z-ADD     EMEMP#        @#EMP#            9 0
     C                   Z-ADD     EMDIV         @#DIV             5 0
     C*
     C     XXCNDR        CHAIN     PCT01                              89
     C     XXCNST        CHAIN     PUP01                              90
     C*
     C                   EVAL      ##CDAT = %Editc(dtmdyy(Dateto7(SystemDate()))
     C                             :'X')
     C*
     C                   MOVEL(P)  CTDESC        ##COMM
     C                   CAT       'STAFF:':5    ##COMM
     C                   CAT       XXCNST:1      ##COMM
     C                   MOVE      'Y'           ##DIFL
     C*
     C     *IN89         IFEQ      *OFF
     C     *IN90         ANDEQ     *OFF
     C*
     C                   CALL      'DA700R'
     C                   PARM                    QCKEY1           40
     C                   PARM                    ##COMM          256
     C                   PARM                    ##CDAT            8
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'W0050R'      ##PNA1           10
     C                   PARM      '000'         ##SECL            3
     C                   PARM      UPUSR         ##USER           10
     C                   PARM      Q1PGM         ##PGM            10
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      'Y'           ##DIFL            1
     C                   PARM      Q1PGM         ##IPGM           10
     C                   PARM      *BLANKS       ##FL01            1
     C                   PARM      *BLANKS       ##FL02            1
     C                   PARM      XXCNDR        ##RMCD            5
     C                   PARM      '00000'       ##SEQ#            5
     C                   PARM      'N'           Q#SBYL            1
     C                   PARM      *BLANKS       Q#COM2           19
     C                   PARM      *BLANKS       Q#APID            2
     C                   PARM      *BLANKS       Q#IDOC           12
     C                   PARM      *BLANKS       Q#IQUE           10
     C                   PARM      'O'           Q#IMGF            1
     C                   PARM      *BLANKS       Q#IPTH           10
     C                   PARM      *BLANKS       Q#PIDX            5
     C                   PARM      *BLANK        Q#QPTY            1
     C                   PARM      *BLANKS       Q#DCTP            5
     C                   PARM      *BLANKS       Q#CSID            2
     C                   PARM      *BLANKS       Q#CODE            2
     C*
     C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WriteWMEMPH - Write Employer Deposit Cap History Record
     C*================================================================
     C     WriteWMEMPH   BegSR
    C*
     C                   Eval      EMHco#    = EMco#
     C                   Eval      EMHfnd    = EMfnd
     C                   Eval      EMHemp#   = EMemp#
     C                   Eval      EMHdiv    = EMdiv
     C                   Eval      EMHdepCap = EMdepCap
     C                   Eval      EMHuser   = Q1user
     C                   Eval      EMHpgm    = Q1pgm
     C                   Eval      EMHdt     = DateTo7(SystemDate())
     C                   Eval      EMHtime   = TimeTo6(SystemTime())
     C                   Write     pemh0p
    C*
     C                   EndSR
     PUpdateBillDiv    E
     p*=====================================================================
     p* CheckRepChanges Check reportable changes for mid-term endorsemens
     P*=====================================================================
     pCheckRepChanges  b
     dCheckRepChanges  pi
      *
     D AGTAPI          C                   CONST('WA002R')
     D X               s              3  0 inz
     D XXADR1          s                   like(empad1)
     D XXADR2          s                   like(empad1)
     D XXADR3          s                   like(empad1)
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
      *
     C     AGTPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM      EMFND         ##FND             3 0
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      *
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
      *
     C     F2KY          KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    SCFND
      *
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
     C     ELKY2         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    stateRepDiv
      *
     C     ECKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKPCNT            1
      *
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
      *
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C     flgQTE        ANDNE     'Q'
      *
     C     AddDiv        IFEQ      *ON
     C     scMODE        OREQ      'E'
     C     F2KY          CHAIN     PF200                              60
      *
      *  GET ENDORS/DARTS INFORMATION
      *
     C     DXKY          CHAIN     PDX00                              60
      *
     C                   CALL      AGTAPI        AGTPL
      *
      *  SETUP FOR NEXT REQUEST NUMBER
      *
     C     ENKY          SETGT     PEN03
     C     ENKY          READPE    PEN03                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
      *** Beg Del ***** 03/19/24 *******************************   //moving this
     C***                Z-ADD     *ZERO         ENSEQ#
      *
     C***                MOVE      EMCO#         ENCO#
     C***                MOVE      EMFND         ENFND
     C***                MOVE      EMEMP#        ENEMP#
     C***                MOVE      stateRepDiv   ENDIV
     C*
     C*  SAVE VALUE OF DIVISION THAT WAS ADDED FOR W7418R PROCESSING
     C*
     C***                MOVE      EMDIV         ENAGT2
     C***                MOVE      LDFYR         ENFYR
     C***                MOVE      Q1JOBN        ENJOB
     C***                MOVE      LDEND#        ENEND#
     C***                ADD       1             ENREQ#
      *
     C***                EVAL      ENEEDT = dtCYMD(LDEEDT)
      *
     C***                MOVEL     EMNAME        ENNAM1
     C***                MOVEL     EMFDNM        ENFDN1
     C***                MOVEL     EMNAME        ENENM1
     C***                MOVEL(P)  EMNAM2        XXNM10           10
     C***                MOVE      XXNM10        ENENM1
     C***                MOVEL     EMFDNM        ENEFN1
     C***                MOVEL(P)  EMFDN2        XXNM10           10
     C***                MOVE      XXNM10        ENEFN1
     C***                MOVE      EMNAM2        ENENM2
     C***                MOVE      EMFDN2        ENEFN2
      *
      * Get Employer Contact Names / phone
      *
     C***                MOVE      '1'           KKPCNT
     C***                EXSR      SRCONT
     C***                MOVEL(P)  XXCNT         ENCON1
      *
     C***                MOVE      '2'           KKPCNT
     C***                EXSR      SRCONT
     C***                MOVEL(P)  XXCNT         ENATT1
      *
     C***                MOVE      '3'           KKPCNT
     C***                EXSR      SRCONT
     C***                MOVEL(P)  XXCNT         ENCN31
      *
     C***                MOVE      EMFEIN        ENFEN1
     C***                MOVEL     EMTPOB        ENTOB1
     C***                MOVEL     EMMAD1        ENMA11
     C***                MOVEL     EMMAD2        ENMA21
     C***                MOVEL     EMMCTY        ENMCT1
     C***                MOVEL     EMMST         ENMST1
     C***                MOVEL     EMMZIP        ENMZP1
     C***                MOVEL     EMPAD1        ENPA11
     C***                MOVEL     EMPAD2        ENPA21
     C***                MOVEL     EMPCTY        ENPCT1
     C***                MOVEL     EMPST         ENPST1
     C***                MOVEL     EMPZIP        ENPZP1
     C***                MOVEL     EMCNTY        ENCNT1
     C***                MOVE      F2CAR#        ENCAR1
     C***                MOVE      F2CAR#        ENCAR2
     C***  ELKY2         CHAIN(N)  PEL00                              60
      *
     C***  *IN60         DOWEQ     *OFF
     C***  ELPTDT        ANDLE     ENEEDT
      *
     C***  ELKY2         READE(N)  PEL00                                  60
      *
     C***                ENDDO
      *
     C***  *IN60         IFEQ      *OFF
     C***                Z-ADD     ELPFDT        ENPFD1
     C***                ENDIF
      *
     C***  AddDiv        IFEQ      *ON
     C***                MOVE      'Y'           ENADDF
     C***                MOVEL(P)  DXDVAD        ENRMCD
     C***                ELSE
     C***                MOVE      'Y'           ENDLDF
     C***                MOVEL(P)  DXDVDL        ENRMCD
     C***                ENDIF
      *
     C***                EVAL      ENFNNM = GetGrpName(scFnd)
     C***                MOVE      ##AGT#        ENAGT#
     C***                MOVEL     ##ANAM        ENAGNM
     C***                MOVEL     Q1USER        ENRUSR
      *
     C***  LDMYRR        CHAIN     PMY0P                              60
     C***                MOVEL     MYDSC1        ENDSC1
     C***                MOVEL     MYDSC2        ENDSC2
     C***                MOVEL     MYDSC3        ENDSC3
      *
     C***  AddDiv        IFEQ      *ON
     C***                MOVEL     EMNAME        ENNAM2
     C***                MOVEL     EMFDNM        ENFDN2
     C***                MOVEL     EMNAME        ENANM1
     C***                MOVEL(P)  EMNAM2        XXNM10           10
     C***                MOVE      XXNM10        ENANM1
     C***                MOVEL     EMFDNM        ENAFN1
     C***                MOVEL(P)  EMFDN2        XXNM10           10
     C***                MOVE      XXNM10        ENAFN1
     C***                MOVE      EMNAM2        ENANM2
     C***                MOVE      EMFDN2        ENAFN2
     C***                MOVEL     ENCON1        ENCON2
     C***                MOVEL     ENATT1        ENATT2
     C***                MOVEL     ENCN31        ENCN32
     C***                MOVE      EMFEIN        ENFEN2
     C***                MOVEL     EMTPOB        ENTOB2
     C***                MOVEL     EMMAD1        ENMA12
     C***                MOVEL     EMMAD2        ENMA22
     C***                MOVEL     EMMCTY        ENMCT2
     C***                MOVEL     EMMST         ENMST2
     C***                MOVEL     EMMZIP        ENMZP2
     C***                MOVEL     EMPAD1        ENPA12
     C***                MOVEL     EMPAD2        ENPA22
     C***                MOVEL     EMPCTY        ENPCT2
     C***                MOVEL     EMPST         ENPST2
     C***                MOVEL     EMPZIP        ENPZP2
     C***                MOVEL     EMCNTY        ENCNT2
     C***                ELSE
     C***                MOVEL     *BLANKS       ENNAM2
     C***                MOVEL     *BLANKS       ENFDN2
     C***                MOVEL     *BLANKS       ENCON2
     C***                MOVEL     *BLANKS       ENATT2
     C***                MOVEL     *BLANKS       ENCN32
     C***                MOVE      *ZEROS        ENFEN2
     C***                MOVEL     *BLANKS       ENTOB2
     C***                MOVEL     *BLANKS       ENMA12
     C***                MOVEL     *BLANKS       ENMA22
     C***                MOVEL     *BLANKS       ENMCT2
     C***                MOVEL     *BLANKS       ENMST2
     C***                MOVEL     *BLANKS       ENMZP2
     C***                MOVEL     *BLANKS       ENPA12
     C***                MOVEL     *BLANKS       ENPA22
     C***                MOVEL     *BLANKS       ENPCT2
     C***                MOVEL     *BLANKS       ENPST2
     C***                MOVEL     *BLANKS       ENPZP2
     C***                MOVEL     *BLANKS       ENCNT2
     C***                ENDIF
      *
     C***                EVAL      ENDT = Dateto7(SystemDate())
     C***                EVAL      ENTIME = Timeto6(SystemTime())
      *
     C***                MOVEL     Q1USER        ENUSER
     C***                MOVEL     Q1PGM         ENPGM
     C***                MOVE      'A'           ENSTAT
     C***                MOVEL     Q1USER        ENRUSR
      *
      *  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
      *
     C***  EMPAD1        IFNE      *BLANKS
     C***                MOVEL     EMPAD1        XXADR1
     C***                MOVEL     EMPAD2        XXADR2
     C***                MOVE      EMPZIP        XXZIP
     C***  EMPCTY        CAT(P)    ',':0         XXADR3
     C***                CAT       EMPST:1       XXADR3
     C***                CAT       XXZIP5:1      XXADR3
     C***  XXZIP4        IFNE      *BLANKS
     C***  XXZIP4        ANDNE     *ZEROS
     C***                CAT       '-':0         XXADR3
     C***                CAT       XXZIP4        XXADR3
     C***                ENDIF
     C***                ELSE
     C***                MOVEL     EMMAD1        XXADR1
     C***                MOVEL     EMMAD2        XXADR2
     C***                MOVE      EMMZIP        XXZIP
     C***  EMMCTY        CAT(P)    ',':0         XXADR3
     C***                CAT       EMMST:1       XXADR3
     C***                CAT       XXZIP5:1      XXADR3
     C***  XXZIP4        IFNE      *BLANKS
     C***  XXZIP4        ANDNE     *ZEROS
     C***                CAT       '-':0         XXADR3
     C***                CAT       XXZIP4        XXADR3
     C***                ENDIF
     C***                ENDIF
      *
     C***  AddDiv        IFEQ      *ON
     C***                MOVE      *BLANKS       ENBLIN
      *  EMPLOYER NAME
     C***                MOVEL     EMNAME        ENALIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
      *  EMPLOYER FEDERAL NAME
     C***                MOVEL     EMFDNM        ENALIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 1
     C***                MOVEL     XXADR1        ENALIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 2
     C***  XXADR2        IFNE      *BLANKS
     C***                MOVEL     XXADR2        ENALIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
     C***                ENDIF
      *  EMPLOYER CITY/ST/ZIP
     C***                MOVEL     XXADR3        ENALIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
      *
     C***                ELSE
      *
     C***                MOVE      *BLANKS       ENALIN
      *  EMPLOYER NAME
     C***                MOVEL     EMNAME        ENBLIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
      *  EMPLOYER FEDERAL NAME
     C***                MOVEL     EMFDNM        ENBLIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 1
     C***                MOVEL     XXADR1        ENBLIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 2
     C***  XXADR2        IFNE      *BLANKS
     C***                MOVEL     XXADR2        ENBLIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
     C***                ENDIF
      *  EMPLOYER CITY/ST/ZIP
     C***                MOVEL     XXADR3        ENBLIN
     C***                WRITE     PEN03
     C***                ADD       1             ENSEQ#
     C***                ENDIF
      *
      *** End Del ***** 03/19/24 *******************************
      *** Beg Add ***** 03/19/24 *******************************
       Exsr WriteWmenp;
       Else;
       //*** Beg Chg ***** 07/23/24 *******************************
       //***If scMode = 'C';
       If scMode = 'C' and scPnlTitle = 'Billing Division Edit';
       //*** End Chg ***** 07/23/24 *******************************
          Setll (EMCo#:EMFnd:EMEmp#:ENDiv:LDFyr:Q1JobN:LDEnd#) pen03;
          Reade (EMCo#:EMFnd:EMEmp#:ENDiv:LDFyr:Q1JobN:LDEnd#) pen03;
          Dow not %eof(wmenl3);
             Delete pen03;
             Reade (EMCo#:EMFnd:EMEmp#:ENDiv:LDFyr:Q1JobN:LDEnd#) pen03;
          Enddo;

          AddDiv = *On;
          ENreq# = 0;
          Exsr WriteWmenp;
       Endif;

      *** End Add ***** 03/19/24 *******************************
     C                   ENDIF
      *
     C                   ENDIF
      *
      *** Beg Add ***** 03/19/24 *******************************
      *===============================================================
      * WRITEWMENP - Writing records to WMENP
      *===============================================================
      *
        Begsr WriteWmenp;
     C                   Z-ADD     *ZERO         ENSEQ#
      *
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      stateRepDiv   ENDIV
     C*
     C*  SAVE VALUE OF DIVISION THAT WAS ADDED FOR W7418R PROCESSING
     C*
     C                   MOVE      EMDIV         ENAGT2
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
      *
     C                   EVAL      ENEEDT = dtCYMD(LDEEDT)
      *
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMNAM2        ENENM2
     C                   MOVE      EMFDN2        ENEFN2
      *
      * Get Employer Contact Names / phone
      *
     C                   MOVE      '1'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENCON1
      *
     C                   MOVE      '2'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENATT1
      *
     C                   MOVE      '3'           KKPCNT
     C                   EXSR      SRCONT
     C                   MOVEL(P)  XXCNT         ENCN31
      *
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
     C                   MOVE      F2CAR#        ENCAR1
     C                   MOVE      F2CAR#        ENCAR2
     C     ELKY2         CHAIN(N)  PEL00                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     ELPTDT        ANDLE     ENEEDT
      *
     C     ELKY2         READE(N)  PEL00                                  60
      *
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   ENDIF
      *
     C     AddDiv        IFEQ      *ON
     C                   MOVE      'Y'           ENADDF
     C                   MOVEL(P)  DXDVAD        ENRMCD
     C                   ELSE
     C                   MOVE      'Y'           ENDLDF
     C                   MOVEL(P)  DXDVDL        ENRMCD
     C                   ENDIF
      *
     C                   EVAL      ENFNNM = GetGrpName(scFnd)
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
      *
     C     LDMYRR        CHAIN     PMY0P                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
      *
     C     AddDiv        IFEQ      *ON
     C                   MOVEL     EMNAME        ENNAM2
     C                   MOVEL     EMFDNM        ENFDN2
     C                   MOVEL     EMNAME        ENANM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENANM1
     C                   MOVEL     EMFDNM        ENAFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENAFN1
     C                   MOVE      EMNAM2        ENANM2
     C                   MOVE      EMFDN2        ENAFN2
     C                   MOVEL     ENCON1        ENCON2
     C                   MOVEL     ENATT1        ENATT2
     C                   MOVEL     ENCN31        ENCN32
     C                   MOVE      EMFEIN        ENFEN2
     C                   MOVEL     EMTPOB        ENTOB2
     C                   MOVEL     EMMAD1        ENMA12
     C                   MOVEL     EMMAD2        ENMA22
     C                   MOVEL     EMMCTY        ENMCT2
     C                   MOVEL     EMMST         ENMST2
     C                   MOVEL     EMMZIP        ENMZP2
     C                   MOVEL     EMPAD1        ENPA12
     C                   MOVEL     EMPAD2        ENPA22
     C                   MOVEL     EMPCTY        ENPCT2
     C                   MOVEL     EMPST         ENPST2
     C                   MOVEL     EMPZIP        ENPZP2
     C                   MOVEL     EMCNTY        ENCNT2
     C                   ELSE
     C                   MOVEL     *BLANKS       ENNAM2
     C                   MOVEL     *BLANKS       ENFDN2
     C                   MOVEL     *BLANKS       ENCON2
     C                   MOVEL     *BLANKS       ENATT2
     C                   MOVEL     *BLANKS       ENCN32
     C                   MOVE      *ZEROS        ENFEN2
     C                   MOVEL     *BLANKS       ENTOB2
     C                   MOVEL     *BLANKS       ENMA12
     C                   MOVEL     *BLANKS       ENMA22
     C                   MOVEL     *BLANKS       ENMCT2
     C                   MOVEL     *BLANKS       ENMST2
     C                   MOVEL     *BLANKS       ENMZP2
     C                   MOVEL     *BLANKS       ENPA12
     C                   MOVEL     *BLANKS       ENPA22
     C                   MOVEL     *BLANKS       ENPCT2
     C                   MOVEL     *BLANKS       ENPST2
     C                   MOVEL     *BLANKS       ENPZP2
     C                   MOVEL     *BLANKS       ENCNT2
     C                   ENDIF
      *
     C                   EVAL      ENDT = Dateto7(SystemDate())
     C                   EVAL      ENTIME = Timeto6(SystemTime())
      *
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      *
      *  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
      *
     C     EMPAD1        IFNE      *BLANKS
     C                   MOVEL     EMPAD1        XXADR1
     C                   MOVEL     EMPAD2        XXADR2
     C                   MOVE      EMPZIP        XXZIP
     C     EMPCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMPST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     EMMAD1        XXADR1
     C                   MOVEL     EMMAD2        XXADR2
     C                   MOVE      EMMZIP        XXZIP
     C     EMMCTY        CAT(P)    ',':0         XXADR3
     C                   CAT       EMMST:1       XXADR3
     C                   CAT       XXZIP5:1      XXADR3
     C     XXZIP4        IFNE      *BLANKS
     C     XXZIP4        ANDNE     *ZEROS
     C                   CAT       '-':0         XXADR3
     C                   CAT       XXZIP4        XXADR3
     C                   ENDIF
     C                   ENDIF
      *
     C     AddDiv        IFEQ      *ON
     C                   MOVE      *BLANKS       ENBLIN
      *  EMPLOYER NAME
     C                   MOVEL     EMNAME        ENALIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
      *  EMPLOYER FEDERAL NAME
     C                   MOVEL     EMFDNM        ENALIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 1
     C                   MOVEL     XXADR1        ENALIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 2
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENALIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *  EMPLOYER CITY/ST/ZIP
     C                   MOVEL     XXADR3        ENALIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
      *
     C                   ELSE
      *
     C                   MOVE      *BLANKS       ENALIN
      *  EMPLOYER NAME
     C                   MOVEL     EMNAME        ENBLIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
      *  EMPLOYER FEDERAL NAME
     C                   MOVEL     EMFDNM        ENBLIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 1
     C                   MOVEL     XXADR1        ENBLIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
      *  EMPLOYER ADDRESS 2
     C     XXADR2        IFNE      *BLANKS
     C                   MOVEL     XXADR2        ENBLIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
     C                   ENDIF
      *  EMPLOYER CITY/ST/ZIP
     C                   MOVEL     XXADR3        ENBLIN
     C                   WRITE     PEN03
     C                   ADD       1             ENSEQ#
     C                   ENDIF
       Endsr;   //WriteWmenp
      *===============================================================
      *** End Add ***** 03/19/24 *******************************
      *===============================================================
      * SRCONT - Get Employer Contact Names and phone
      *===============================================================
      *
     C     SRCONT        BEGSR
     C                   CLEAR                   XXCNT
     C                   CLEAR                   XXCPH
      *
      * Get Contact Name
      *
     C     ECKY          CHAIN     PEC01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ECFSN         IFNE      *BLANKS
     C     ECFSN         CAT       ECLSN:1       XXCNT            50
      *
     C                   ELSE
      *
     C     ECTITL        CAT       ECLSN:1       XXCNT
     C                   ENDIF
      *
     C                   SELECT
      *****
      *
      * No Phone entered
      *
     C     ECPHN#        WHENEQ    *ZERO
     C     ECCPHN        ANDEQ     *ZERO
     C                   MOVEL(P)  *BLANKS       XXCPH            20
      *****
      *
      * Cell phone
      *
     C     ECPHN#        WHENEQ    *ZERO
     C     ECCPHN        ANDNE     *ZERO
     C     ECPHN#        ORNE      *ZERO
     C     ECCPHN        ANDNE     *ZERO
     C     ECPPHF        ANDEQ     'C'
      *
     C     ECCARA        IFNE      *ZERO
     C                   MOVEL(P)  ECCARA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C                   MOVEL(P)  ECCPHN        XXTMP1            3
     C                   MOVE(P)   ECCPHN        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
      *****
      *
      * Otherwise regular phone
      *
     C                   OTHER
      *
     C     ECAREA        IFNE      *ZERO
     C                   MOVEL(P)  ECAREA        XXTMP             5
     C     '('           CAT(P)    XXTMP:0       XXCPH
     C                   CAT(P)    ')':0         XXCPH
     C                   ENDIF
      *
     C                   MOVEL(P)  ECPHN#        XXTMP1            3
     C                   MOVE(P)   ECPHN#        XXTMP2            4
     C                   CAT(P)    XXTMP1:0      XXCPH
     C                   CAT(P)    '-':0         XXCPH
     C                   CAT(P)    XXTMP2:0      XXCPH
      *
     C     ECEXT#        IFNE      *ZERO
     C                   MOVEL(P)  ECEXT#        XXTMP3            5
     C     '0'           CHECK     XXTMP3        X
     C                   SUBST(P)  XXTMP3:X      XXTMP             5
     C                   CAT(P)    'x':0         XXCPH
     C                   CAT(P)    XXTMP:0       XXCPH
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
     pCheckRepChanges  e
     p*=====================================================================
     p* InitBillDiv - Equivalent of PGMSTR to initialize some global values
     p*  Parm1   - Pointer to format DS
     p*  Returns - RRN of Employer Master record
     P*=====================================================================
     PInitBillDiv      b                   Export
     DInitBillDiv      pi              n
     D inRRN                          9  0
     D inMode                         1
     D inPtrBillDiv                    *
     D inPtrInitData                   *
     d inFyr                          3  0 const options(*nopass)
      *
     D xxRtcd          s              1    inz
     d xxFyr           s              3  0 inz
     D outWCIF         s              1    inz
     D outFBIF         s              1    inz
     D outFL01         s              1    inz
      /Free

       ptrBillDiv = inPtrBillDiv;
       ptrInitData = inPtrInitData;
       // Gets the Id for this client
       GetClientId(ClientID);
       flgPEOStatus = GetPEOFlag(scFnd);

       Clear xxFyr;

       If %parms >= 5;
         xxFyr = inFyr;
       Endif;

       qdjob = q1job;
       qdcusr = q1cusr;
       qdjobn = q1jobn;
       //employer RRN
       GetEmployer(inRRN);
       scCo# = EMco#;
       scbCo# = EMco#;
       scFnd = EMfnd;
       scbFnd = EMfnd;
       scEmp# = EMemp#;
       scDiv = EMdiv;
       outEMrrn =  inRrn;

       scFyr = xxFyr;
       GetGrpName(ELfnd:fnRrn);

       //*** Beg Add ***** 06/06/24 **********************************
       isInvCloud = isModEnabled('Invoice Cloud');
       isGroupIncluded = ICGroupOpt(EMCo#:EMFnd);
       //*** End Add ***** 06/06/24 **********************************

       NumOfDivs = 0;
       Setll (scCo#:scfnd:scemp#) pem00;
       Dou %Eof(wmeml);
         Reade(n) (scCo#:scfnd:scemp#) pem00;
         If %Eof(wmeml);
           Leave;
         Endif;
         NumOfDivs +=1;
       Enddo;

       stateRepDiv = GetStRptDv(scCo#:scFnd:scEmp#);
       //Reset employer RRN
       GetEmployer(inRRN);

       AddDiv = *Off;
       If inMode = 'A';
         AddDiv = *On;
       Endif;
       If addMode;
         //Check for Daily Batch running
         CheckDailyBat(xxRtcd);
         If xxRtcd = 'F';
           Return *Off;
         Endif;
       Endif;

       //GET ADMIN/DARTS INFORMATION & LOAD CONTACT HEADINGS
       Chain (scCo#:scFnd) pex00;

       //Save the Admin/DARTS control flags for State and NCCI ID#'s
       If %Found(wmexl);
         flgSRRB = EXsrrb;
         flgSRRQ = EXsrrq;
         flgNRRB = EXnrrb;
         flgNRRQ = EXnrrq;
         flgFIFG = EXfifg;
       Endif;

       If EXfifg = 'Y';
         reqTaxId = *On;
       Else;
         reqTaxId = *Off;
       Endif;
       If EXsicr = 'Y' And flgQte <> 'Q';
         reqSic = *On;
       Else;
         reqSic = *Off;
       Endif;
       lossVis = *off;
       If clientID = '102' and (dspMod = *off or dspMod = ' ');
          lossVis = *on;
       Endif;
       If EXcust ='Y';
          reqcustId = *On;
       Else;
          reqcustId = *Off;
       Endif;

       //   WCMC Installed determination
       GetMancFlag(outWCIF:outFBIF:outFL01);
       If outFBIF = 'Y' And outFL01 = 'Y';
         flgManc = *On;
       Else;
         flgManc = *Off;
       Endif;

       Chain 'WC' swc01;
       If WCfl03 = 'Y';
         reqLsid = *On;
       Else;
         reqLsid = *Off;
       Endif;

       reqZip136 = *off;
       If ClientID = '136';
          reqZip136 = *on;
       Endif;
       isUse#Emp = *off;
       isUseMPN  = *off;
       If ClientID = '132';
          isUse#Emp = *on;
          isUseMPN  = *on;
       Endif;
       Exsr CheckSecurity;
       //DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
       GetClmAdmOnly(dsB.ClmAdmOnly);
       dsB = dsInitData;
       //*** Beg Add *** 01/25/22 ****************************************  ****
       dspModeStf = *off;
       // Check if user is allowed to update staff assignments in Inquiry Mode
       If inMode = 'D';
          dspModeStf = *on;
          Chain(n) Q1user pup02;
          If %Found(smupl) and U2stafUpd = 'Y';
             dspModeStf = *off;
          Endif;
       Endif;
       //*** End Add *** 01/25/22 ****************************************  ****
       Return *On;

      /End-Free
     C*===============================================================
     C*  CHECK SECURITY LEVEL FOR ADD
     C*===============================================================
     C     CheckSecurity BEGSR
     C*
     C     EXKY          KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    SCFND
     C*
     C                   CLEAR                   EXADLL
     C                   CLEAR                   EXADLT
     C                   CLEAR                   EXADQL
     C                   CLEAR                   EXADQT
     C*
     C     EXKY          CHAIN     PEX00                              60
     C                   MOVE      EXCNDR        XXCNDR
     C                   MOVE      EXCNST        XXCNST
     C*
     C     addMode       IFEQ      *On
     C*
     C     flgQte        IFEQ      'Q'
     C*
     C                   MOVE      EXADQL        ##CLVL
     C                   MOVE      EXADQT        ##CTYP
     C                   ELSE
     C                   MOVE      EXADLL        ##CLVL
     C                   MOVE      EXADLT        ##CTYP
     C                   ENDIF
     C*
     C                   MOVE      ##LVL         ##ULVL
     C                   MOVE      ##TYPE        ##UTYP
     C*
     C                   CALL      'SRCCSC'
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
     C*
     C     ##RES         IFEQ      *ON
     C                   EVAL      FlgAddAuth = *Off
     C                   ELSE
     C                   EVAL      FlgAddAuth = *On
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     PInitBillDiv      E
     p*=====================================================================
     p* GetEmployer - chain to employer master
     p*  inRRN (9,0)
     p*  returns found status
     P*=====================================================================
     PGetEmployer      b
     DGetEmployer      pi              n
     D inRRN                          9  0
     D inLock                         1    options(*nopass) const
      *
     D xxLock          s              1    inz
      /Free
       If %parms > 1;
         xxLock = inLock;
       Endif;

       If xxLock = 'L';
         Chain inRRN pem0p;
       Else;
         Chain(n) inRRN pem0p;
       Endif;
       If %found(wmemp);
         return *On;
       Else;
         return *Off;
       Endif;
      /End-Free
     PGetEmployer      E
       //===================================================================
       // Add/Edit Collateral popup  window
       //===================================================================
     p PrcCollatWdw    b                   Export
     d PrcCollatWdw    pi
     d  inEmpRRN                      9  0 const
     d  inCall                        3    const
     d  inMode                        1    const
     d  inRrn                         9  0 const options(*nopass)
     d*
     d decCurDt7       s              7  0
     d decCurTm6       s              6  0
     d isExit          s               n   inz
     d xxRrn           s              9  0 inz
     d outMode         s              1    inz
     d xxColExDtM      s              7  0 inz
     d svCOExdt        s              7  0 inz

     d ptrCollateral   s               *   inz

     d*dsCollateral  e ds                  extname(wpilocmst:rcdCollat) inz
     d dsWMCOP       e ds                  extname(wmcop) inz
     d                                     qualified
      /free
       //*** beg del ***** 04/20/21 ************************
       //Exec SQL set option COMMIT=*NONE;
       //*** end del ***** 04/20/21 ************************
       If %parms >= 4;
          xxRRN = inRRN;
       Endif;
       Chain inEmpRRN wmemp;
       If not %Open(WpiLocMst);
          Open WpilocMst;

          outMode = inMode;
          If outMode = 'A';
             outMode = ' ';
          Endif;
       Endif;

       ExSr ClearErrors;
       Select;
       When inMode = 'A';
          scColMode = 'Add';
          //isColAdd = *on;
          Exsr InitCollateral;
       When inMode = 'C' Or inMode = 'D';
          Exec SQL Select *
           into :dsWMCOP
           from WMCOP where rrn(WMCOP) = :xxRRN;
          scColPerc = dsWMCOP.COperc;
          scColEff = DtMDYY(dsWMCOP.COefDt);
          scColExp = DtMDYY(dsWMCOP.COexDt);
          scColSts = dsWMCOP.COstat;
          svCOExdt = dsWMCOP.COExdt;
          Select;
          When inMode = 'C';
             scColMode = 'Edit';
          When inMode = 'D';
          EndSl;
       EndSl;

       Dou isExit;
          Select;
          When inMode = 'D';
          Other;
             Exfmt rcdCollat;
             Select;
             When btnColCan = *on;
                isExit = *on;
             When btnColSave = *on;
                Exsr ProcessSave;
             When btnColNext = *on;
                Exsr ProcessSave;
                If isExit;
                   Exsr InitCollateral;
                   isExit = *off;
                Endif;
             Endsl;
          Endsl;
       Enddo;
       If %open(WpiLocMst);
          Close WpiLocMst;
       Endif;
       Return;
       //==================================================================
       // ClearErrors
       //==================================================================
        Begsr ClearErrors;
          erColPerc  = *off;
          erColEff   = *off;
          erColExp   = *off;
        Endsr;
       //==================================================================
       //   Initialize Add Mode
       //==================================================================
       Begsr InitCollateral;
        Clear scColPerc;
        Clear scColEff;
        Clear scColExp;
        scColSts = 'A';
       Endsr;
       //==================================================================
       //   Load Screen Fields to File
       //==================================================================
       Begsr LoadScnToFil;
        dsWMCOP.COefDt = DtCYMD(scColEff);
        dsWMCOP.COexDt = DtCYMD(scColExp);
        dsWMCOP.COperc = scColPerc;
        dsWMCOP.COstat = scColSts;
       Endsr;
       //==================================================================
       // Process Save
       //==================================================================
        Begsr ProcessSave;
          Exsr ClearErrors;
          If ValidCollat(inMode:xxRRN:inEmpRRN);
             Exsr LoadScnToFil;
             Exsr WrtUpdCollat;
             isExit = *on;
          EndIf;
        Endsr;
       //==================================================================
       //   Write/Update Collateral
       //==================================================================
       Begsr WrtUpdCollat;
         decCurTm6 = Timeto6(SystemTime());
         decCurDt7 = Dateto7(SystemDate());
         If dsWMCOP.COExdt <> svCOExdt;
            xxColExDtM = decCurDt7;
         Else;
            xxColExDtM = dsWMCOP.COExDtM;
         Endif;
         Select;
         When inMode = 'A';
            Exec SQL Insert into WMCOP
             (COco#, COfnd, COemp#, COdiv,
              COefDt, COexDt, COperc, COstat,
              COuser, COpgm, COdt, COtime,
              COiUsr, COiPgm, COiDt, COiTim, COExDtM
             )
             values(:EMco#, :EMfnd, :EMemp#, :EMdiv,
              :dsWMCOP.COefDt, :dsWMCOP.COexDt, :dsWMCOP.COperc,
              :dsWMCOP.COstat,
              :q1User, :q1Pgm, :decCurDt7, :decCurTm6,
              :q1User, :q1Pgm, :decCurDt7, :decCurTm6,
              :xxColExDtM
             );
         When inMode = 'C';
            Exec SQL Update WMCOP
             set
              COefDt = :dsWMCOP.COefDt,
              COexDt = :dsWMCOP.COexDt,
              COperc = :dsWMCOP.COperc,
              COstat = :dsWMCOP.COstat,
              COExDtM = :xxColExDtM,
              COuser = :q1User,
              COpgm  = :q1Pgm,
              COdt   = :decCurDt7,
              COtime = :decCurTm6
             where rrn(WMCOP) = :xxRRN;
         Endsl;
       Endsr;
      /end-free
     p PrcCollatWdw    e

       //===================================================================
       // Validate Collateral window                                        xes.
       //===================================================================
     p ValidCollat     b
     d ValidCollat     pi              n
     d  inMode                        1    const
     d  inRRN                         9  0 const
     d  inEmpRRN                      9  0 const

     disError          s               n   inz
     D message         s            100
     DstrSqlCursor     s            500

     d dsWMCOP       e ds                  extname(wmcop) inz
     d                                     qualified
      /free
       If inMode = 'A';

       Endif;

       If scColPerc <= *zero;
          erColPerc = *on;
          msColPerc = 'WCR0502';
          isError = *on;
       Endif;

       scColEff = DtCheck(scColEff);
       If scColEff = *zero;
          erColEff = *on;
          msColEff = 'WCR0500';
          isError = *on;
       Endif;
       If scColExp <> *zero;
          scColExp = DtCheck(scColExp);
          If scColExp = *zero;
             erColExp = *on;
             msColExp = 'WCR0500';
             isError = *on;
          Endif;
       Endif;

       Select;
       When isError = *on;
       When scColExp <> *zero and CvtToDate(scColExp) < CvtToDate(scColEff);
          erColEff = *on;
          erColExp = *on;
          msColEff = 'WCR0529';
          msColExp = 'WCR0529';
          isError = *on;
       Other;
          strSqlCursor = 'select * ' +
           'from WMCOP ' +
           'where COco# = ' + %editc(EMco#:'X') +
           '  and COfnd = ' + %editc(EMfnd:'X') +
           '  and COemp# = ' + %editc(EMemp#:'X') +
           '  and COdiv = ' + %editc(EMdiv:'X') +
           '  and rrn(WMCOP) <> ' + %editc(inRRN:'X');
          Exec SQL Prepare sqlCO From :strSqlCursor;
          Exec SQL Declare cursorCO Cursor For sqlCO;
          Exec SQL Open cursorCO;
          Dou SQLSTT <> '00000' or isError = *on;
             Exec SQL Fetch next From cursorCO into :dsWMCOP;
             Select;
             When SQLSTT <> '00000';
                Iter;
             When RangeOverlap(dsWMCOP.COefDt:dsWMCOP.COexDt:scColEff:scColExp);
                erColEff = *on;
                erColExp = *on;
                msColEff = 'WCR0907';
                msColExp = 'WCR0907';
                isError = *on;
             Endsl;
          Enddo;
          Exec SQL Close cursorCO;
       Endsl;
       Return not isError;
      /End-free
     p ValidCollat     e
       //***********************************************************************
       //===================================================================
       // Add/Edit Entity/Location popup  window
       //===================================================================
     p PrcLocEntWdw    b                   Export
     d PrcLocEntWdw    pi
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLocRrn                      9  0 options(*nopass)
     d  inNBquote                     1    options(*nopass)
     d*
     d ExitNow         s               n   inz
     d xxHdg1          s                   like(excnt1)
     d xxHdg2          s                   like(excnt1)
     d xxHdg3          s                   like(excnt1)
     d xxElid          s             10  0 inz
     d xxKey           s             10    inz
     d xxDelResp       s               n   inz
     d xxMode          s              1    inz
     d xxRtnVal        s             10    inz
     d xxLocRrn        s              9  0 inz
     d svDType         s                   inz like(scDType)
     d iOpen           s               n   inz
     d xxDate          s              7  0 inz
     d xxState         s              2    inz
     d dsLocBillDiv  e ds                  extname(wpilocmst:rcdBillDiv) inz
     d ptrLocBillDiv   s               *   inz
     d svDiv#          s              5  0 inz
     d xxRrn           s              9  0 inz
     d flgNBquote      s              1    inz
     d xxfyr           s                   like(ELfyr)
     d xxModeL         s              1    inz
     C     keyWMOOL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scELID
     C*
     C     keyWDLDL      Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scELID
     C                   Kfld                    scFYR
     C     keyWDLDL#     Klist
     C                   Kfld                    scCO#
     C                   Kfld                    scFND
     C                   Kfld                    scEMP#
     C                   Kfld                    scDIV
     C                   Kfld                    scELID
      /free
       xxMode = inMode;
       If not %Open(WpiLocMst);
         Open WpilocMst;
         iOpen = *On;
          Clear dsLocBillDiv;
          Clear dsInitData;
          ptrLocBillDiv = %Addr(dsLocBillDiv);
          ptrInitData = %Addr(dsInitData);

          xxModeL = inMode;
          If xxmodeL = 'A';
             xxmodeL = ' ';
          Endif;
          InitBillDiv(inRrn:xxModeL:ptrLocBillDiv:ptrInitData);
       Endif;
       GetEmployer(inRRN);
       scCo# = EMco#;
       scLCo# = EMco#;
       scFnd = EMfnd;
       scEmp# = EMemp#;
       scDiv = EMdiv;
       scLocDiv# = EMdiv;
       svDiv# = EMdiv;
       dsbLocDiv# = *on;
       Clear divChoice;
       divChoice = 'EMco# = ' + %char(EMco#) + ' AND EMfnd = '
        + %char(EMfnd) + ' AND EMemp# = ' + %char(EMemp#);
       If inMode = 'A' and inCall = 'QTE' and NumOfDivs > 1;
          dsbLocDiv# = *off;
       Endif;
       svEfd1 = EMefd1;
       saveALdt =  Dateto7(systemDate());
       saveALtm = Timeto6(SystemTime());

       If %Parms > 3;
         xxLocRrn = inLocRrn;
       Endif;
       If %Parms > 4;
         flgNBquote = *on;
       Else;
         flgNBquote = *off;
       Endif;
       Chain (scCo#:scFnd) pex00;
       xxHdg1 = EXcnt1;
       xxHdg2 = EXcnt2;
       xxHdg3 = EXcnt3;
       flgFifg = EXfifg;
       If EXsicr = 'Y' and flgQte <> 'Q';
          reqDSic = *On;
       Else;
          reqDSic = *Off;
       Endif;
       visOnPrNam = *off;
     **** Beg Add - 03/11/22  **************************************************
       // ***Chain EMco# ptm00;
       Chain (EMco# : 0) ptm00;
     **** End Add - 03/11/22  **************************************************
       If %found and TMapnd = 'Y';
          visOnPrNam = *on;
       Endif;
       visOnInNam = *off;
       Chain (EmCo#:EmFnd) PFN00;
       If %found and FNpeof <> ' ';
          visOnInNam = *on;
       Endif;

       ExSr ClearErrors;
       scAAPnlTtl = 'Additional Workplaces';
       Select;
       When xxMode = 'A';
         readOnly = *on;
         Exsr InitializeAddMode;
         scAAPnlTtl = %Trim(scAAPnlTtl) + ' Add';
       When xxMode = 'C' Or xxMode = 'D';
         Chain(n) xxLocRrn pal0p;
         scLocDiv# = ALdiv;
         svDiv#    = ALdiv;
         roChgMod = *On;
         roDType = *On;
         vsDelFlds = *Off;
         readOnly = *Off;
         roFein = *Off;
         visSaveNxt = *Off;
         leftLocCan = 970;
         Select;
         When xxMode = 'C';
           scAAPnlTtl = %Trim(scAAPnlTtl) + ' Edit';
         When xxMode = 'D';
           If flgNBquote = *on;
             scAAPnlTtl = %Trim(scAAPnlTtl) + ' Delete';
           Else;
             scAAPnlTtl = %Trim(scAAPnlTtl) + ' Expire';
           Endif;
         EndSl;
         Exsr LoadFilToScn;
       EndSl;
            Eval xxDate = EMefd1;
            If SCDPstate <> *Blanks;
              xxState = SCDPState;
            Else;
              xxState = SCDMState;
            Endif;
            Setll (SCco#:SCfnd:xxState:xxDate) prr01;
            Reade (SCco#:SCfnd:xxState) prr01;
            If not %Eof(wdrrl1);
               If RRnaic = 'Y';
                  reqDNaics = *On;
               Else;
                  reqDNaics = *Off;
               Endif;
            Endif;

       //*** Beg Add ***  04/29/24  *** WC2965 *************************

       reqDMail = *Off;
       If SCDmstate <> ' ' Or SCDpstate <> ' ';

          xxDate = %int(%Char(%Date() : *CYMD0));
          SetGT (scCo#:scFnd:SCDmstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCDmstate) WDRRL;
          If Not %EOF and RREmail = 'Y';
             reqDMail = *On;
          EndIf;

          SetGT (scCo#:scFnd:SCDpstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCDpstate) WDRRL;
          If Not %EOF and RREmail = 'Y';
             reqDMail = *On;
          EndIf;

       EndIf;
       //*** End Add ***  04/29/24  *** WC2965 *************************

       DoU ExitNow = *On;
         LoadBillSel(scDPState:scDMState);
         Select;
         When xxMode = 'D';
           Write rcdLocWdw;
           If flgNBquote = *on;
             xxDelresp = PopTwoBtn('Do you want to delete this Location/Entity?'
              :'Confirm Delete':'Yes':'No');
           Else;
             xxDelresp = PopTwoBtn('Do you want to expire this Location/Entity?'
              :'Confirm Expire':'Yes':'No');
           Endif;
           Select;
           When xxDelResp = *On;
           scELid = ALelid;
           If flgNBquote = *on;
             Exsr DeleteLocEnt;
             xxDelResp =*Off;
             ExitNow = *On;
           Else;
             xxMode = 'd';
             vsDelFlds = *On;
             roDelFlds = *Off;
             readOnly = *On;
             roFein = *On;
             roDType = *On;
             roChgMod = *On;
           Endif;
           When xxDelResp = *Off;
             ExitNow = *On;
           EndSl;
         Other;
           ExFmt rcdLocWdw;
           Select;
           When btnLocEnt = *On;
             Clear pal0p;
             readOnly = *Off;
             Exsr LoadDefaults;
           When btnLocCan = *On;
             ExitNow = *On;
           When btnLocSave = *On;
             Exsr ProcessSave;
           When btnLocSNxt = *On;
             Exsr ProcessSave;
             If ExitNow = *On;
               Exsr ProcessAddNext;
             Endif;
           When btnLocAYr = *On;
             Exsr ProcessSave;
             If ExitNow = *On;
               xxRtnVal = *Blanks;
               If ALeltp = 'E';
                 xxRtnVal = PrcOwnOffGrd(inRrn:inCall:xxMode:xxLocRrn);
               Endif;
               If xxRtnVal <> 'CANCEL';
                 xxRtnVal = PrcLocYrGrd(inRrn:inCall:xxMode:xxLocRrn);
                 Select;
                 When xxRtnVal = 'ADD_LOCENT';
                   Exsr ProcessAddNext;
                 Other;
                   ExitNow = *On;
                 EndSl;
               Else;
                 ExitNow = *On;
               Endif;
             Endif;
           When BtnLocDiv = *On and scLocDiv# <> svDiv#;
              BtnLocDiv = *off;
              svDiv# = scLocDiv#;
              xxRRN = GetEmpYrRR(0 :EMemp# :svDiv# :EMfnd :EMco# :' ');
              Chain xxRrn pel0p;
              scDiv = ELdiv;
              inRRN = GetEmpRRN(scEmp# :svDiv# :scFnd :scCo#);
              GetEmployer(inRRN);

              scLocDiv# = EMdiv;
              ExSr ClearErrors;
              Clear *all pal00;
              Clear pal0p;
              Exsr LoadDefaults;
           EndSl;
         EndSl;
       EndDo;
       If %Open(WpiLocMst) And iOpen;
         Close WpiLocMst;
       Endif;
       Return;
       //********************************************************
       //*   Initialize Add Mode
       //********************************************************
       BegSr InitializeAddMode;
         Clear pal0p;
         roChgMod = *Off;
         roDType = *Off;
         vsDelFlds = *Off;
         visSaveNxt = *On;
         leftLocCan = 700;
         scDtype = ' ';
         Exsr LoadFilToScn;
         Exsr LoadDefaults;
       Endsr;
       //********************************************************
       //*   Load Default data
       //********************************************************
       BegSr LoadDefaults;

         SetGt (scCo#:scFnd:scEmp#:scDiv) pal00;
         DoU %Eof(wmall);
           Readpe (scCo#:scFnd:scEmp#:scDiv) pal00;
           Monitor;
             xxElid = %Int(A0elid);
             Leave;
           On-error;
             xxElid = 0;
           EndMon;
         EndDo;

         xxElid = xxElid + 1;
         scdElid = %Editc(xxElid:'X');
         svElid = scdElid;
         scdHdg1 = xxHdg1;
         scdHdg2 = xxHdg2;
         scdHdg3 = xxHdg3;
         scdRepNCCI = 'Y';
         scdEfdt = dtMDYY(ELPFDT);
         If scDType = ' ';
           lodDbaName = %Trim(EMname) + %Trim(EMnam2);
           lodFedName = %Trim(EMfdnm) + %Trim(EMfdn2);
           lodMadd1 = EMmad1;
           lodMadd2 = EMmad2;
           lodMCity = EMmcty;
           loDMstate = EMmst;
           loDMzip= EMmzip;
           lodPhone# = EMarea * 10000000 + EMphn#;
           lodFax# = EMfaxa * 10000000 + EMfax#;
           lodTypBus =EMtpob;
           loDSic= EMsic;
           loDNaic= EMnaic;
           loDSRisk = EMsemp;
           loDSUnEmp# = EMue#;
           lodPadd1 = *Blanks;
           lodPadd2 = *Blanks;
           lodPCity = *Blanks;
           loDPstate = *Blanks;
           loDPzip= *Zeros;
           loDCont1 = *Blanks;
           loDCont2 = *Blanks;
           loDCont3 = *Blanks;
           lo#ShftEmp = EMshft;
           loMPNName = *Blanks;
           loMPNAdd1 = *Blanks;
           loMPNAdd2 = *Blanks;
           loMPNCity = *Blanks;
           loMPNState = *Blanks;
           loMPNZip = *Zeros;
           loMPNTyp = *Blanks;
           endDbaName = *Blanks;
           endFedName = *Blanks;
           endMadd1 = *Blanks;
           endMadd2 = *Blanks;
           endMCity = *Blanks;
           enDMstate = *Blanks;
           enDMzip= *Zeros;
           endPhone# = *Zeros;
           endFax# = *Zeros;
           endTypBus = *Blanks;
           enDSic= *Blanks;
           enDNaic= *Blanks;
           enDSRisk = *Blanks;
           enDSUnEmp# = *Blanks;
           endPadd1 = *Blanks;
           endPadd2 = *Blanks;
           endPCity = *Blanks;
           enDPstate = *Blanks;
           enDPzip= *Zeros;
           enDCont1 = *Blanks;
           enDCont2 = *Blanks;
           enDCont3 = *Blanks;
           en#ShftEmp = *Zeros;
           enMPNName = *Blanks;
           enMPNAdd1 = *Blanks;
           enMPNAdd2 = *Blanks;
           enMPNCity = *Blanks;
           enMPNState = *Blanks;
           enMPNZip = *Zeros;
           enMPNTyp = *Blanks;
         EndIf;
         Select;
         When scDType = 'L' or scDType = *Blanks;
           scdFein = EMfein;
           roFein = *On;
           If scDtype = *Blanks Or
              (scDtype = 'L' And svDtype = 'E' And screenDS = entityDS);
              scdDbaName = %Trim(EMname) + %Trim(EMnam2);
              scdFedName = %Trim(EMfdnm) + %Trim(EMfdn2);
              scdMadd1 = EMmad1;
              scdMadd2 = EMmad2;
              scdMCity = EMmcty;
              scDMstate = EMmst;
              scDMzip= EMmzip;
              scdPhone# = EMarea * 10000000 + EMphn#;
              scdFax# = EMfaxa * 10000000 + EMfax#;
              scdTypBus =EMtpob;
              scDSic= EMsic;
              scDNaic= EMnaic;
              scDSRisk = EMsemp;
              scDSUnEmp# = EMue#;
              scdPadd1 = *Blanks;
              scdPadd2 = *Blanks;
              scdPCity = *Blanks;
              scDPstate = *Blanks;
              scDPzip= *Zeros;
              scDCont1 = *Blanks;
              scDCont2 = *Blanks;
              scDCont3 = *Blanks;
              sc#ShftEmp = EMshft;
              scMPNName = *Blanks;
              scMPNAdd1 = *Blanks;
              scMPNAdd2 = *Blanks;
              scMPNCity = *Blanks;
              scMPNState = *Blanks;
              scMPNZip = *Zeros;
              scMPNTyp = *Blanks;
           EndIf;
         When scDType = 'E';
              scdFein = 0;
              roFein = *Off;
           If svDtype = ' ' Or (svDtype = 'L' And screenDS = locationDS);
              scdDbaName = *Blanks;
              scdFedName = *Blanks;
              scdMadd1 = *Blanks;
              scdMadd2 = *Blanks;
              scdMCity = *Blanks;
              scDMstate = *Blanks;
              scDMzip= *Zeros;
              scdPhone# = *Zeros;
              scdFax# = *Zeros;
              scdTypBus = *Blanks;
              scDSic= *Blanks;
              scDNaic= *Blanks;
              scDSRisk = *Blanks;
              scDSUnEmp# = *Blanks;
              scdPadd1 = *Blanks;
              scdPadd2 = *Blanks;
              scdPCity = *Blanks;
              scDPstate = *Blanks;
              scDPzip= *Zeros;
              scDCont1 = *Blanks;
              scDCont2 = *Blanks;
              scDCont3 = *Blanks;
              sc#ShftEmp = *Zeros;
              scMPNName = *Blanks;
              scMPNAdd1 = *Blanks;
              scMPNAdd2 = *Blanks;
              scMPNCity = *Blanks;
              scMPNState = *Blanks;
              scMPNZip = *Zeros;
              scMPNTyp = *Blanks;
           EndIf;
         EndSl;
         svDtype = scDtype;
       EndSr;
       //********************************************************
       //*   Load File fields to screen
       //********************************************************
       BegSr LoadFilToScn;
         scdHdg1 = xxHdg1;
         scdHdg2 = xxHdg2;
         scdHdg3 = xxHdg3;
         Select;
         When ALelTp = 'Y';
            ALelTp = 'L';
         Endsl;
         scdType = ALeltp;
         If ALeltp = 'L';
           roFein = *On;
         Else;
           roFein = *Off;
         Endif;
         //scdDiv = ALdiv;
         scdElid = ALelid;
         scDfein = ALfein;
         scdRepNCCI = ALncci;

         If ALefdt = 0 And xxMode <> 'A';
           ALefdt = dateTo7(SystemDate());
         EndIf;
         scDEFDT = dtmdyy(ALefdt);

         scDDbaName = ALname + ALnam2;
         scDFedName = ALfdnm + ALfdn2;

         scDMAdd1 = ALmad1;
         scDMAdd2 = ALmad2;
         scDMcity = ALmcty;
         scDMstate = ALmst;
         scDMzip= ALmzip;

         scDPAdd1 = ALpad1;
         scDPAdd2 = ALpad2;
         scDPcity = ALpcty;
         scDPstate = ALpst;
         scDPzip= ALpzip;
         scDCounty = ALcnty;

         scdPhone# = ALarea * 10000000 + ALphn#;
         scdFax# = ALfara * 10000000 + ALfax#;

         scdSRisk = ALsemp;
         scdSUnEmp# = ALue#;
         scdTypBus = ALtpob;
         scdSic = ALsic;
         scdNaic = ALnaic;
         //*** Beg Add *** 12/22/21 ********************************************
         scMail  = ALmail;
         //*** End Add *** 12/22/21 ********************************************
       //*** begin add ***  08/08/24  **************************************
         If xxMode = 'A' and scMail = ' ' and FNEmail <> ' ';
            scMail = FNEmail;
         EndIf;
       //*** end add *****  08/08/24  **************************************
       //*** Beg Add ***  04/29/24  *** WC2965 *************************

       reqDMail = *Off;
       If SCDmstate <> ' ' Or SCDpstate <> ' ';

          xxDate = %int(%Char(%Date() : *CYMD0));
          SetGT (scCo#:scFnd:SCDmstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCDmstate) WDRRL;
          If Not %EOF and RREmail = 'Y';
             reqDMail = *On;
          EndIf;

          SetGT (scCo#:scFnd:SCDpstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCDpstate) WDRRL;
          If Not %EOF and RREmail = 'Y';
             reqDMail = *On;
          EndIf;
       EndIf;

       //*** begin del ***  08/08/24  **************************************
       //*If scMail = ' ' and reqDMail = *Off;
       //*** If EMStrp = 'Y';
       //***    Chain (scCo#:scFnd) WMFNL;
       //***     If %Found and FNEmail <> ' ';
       //***        scMail = FNEmail;
       //***     EndIf;
       //*** EndIf;
       //*EndIf;
       //***
       //*If scMail = ' ' and reqDMail = *On;
       //*** If EMStrp = 'Y';
       //***    Chain (scCo#:scFnd) WMFNL;
       //***     If %Found and FNEmail <> ' ';
       //***        scMail = FNEmail;
       //***     EndIf;
       //*** EndIf;
       //*** If EMStrp = 'N';                                   // Non SR Div
       //***    SetLL (scCo#:scFnd:EMemp#:0) PEM00;
       //***    ReadE(n) (scCo#:scFnd:EMemp#) PEM00;
       //***    Dow Not %EOF;
       //***        If EMStrp = 'Y';
       //***        Leave;
       //***        EndIf;
       //***        ReadE(n) (scCo#:scFnd:EMemp#) PEM00;
       //***    EndDo;
       //***   If EMMail = ' ';                               // If SR Email is
       //***    Chain (scCo#:scFnd) WMFNL;
       //***     If %Found and FNEmail <> ' ';
       //***        scMail = FNEmail;
       //***     EndIf;
       //***   EndIf;
       //*** EndIf;
       //*EndIf;
       //*** end del *****  08/08/24  **************************************
       //*** End Add ***  04/29/24  *** WC2965 *************************

         scdCont1 = ALcnt1;
         scdCont2 = ALcnt2;
         scdCont3 = ALcnt3;

         scDLDT = dtmdyy(ALdldt);
         scDelRsn = ALdlrs;
         scAlNaNam = ALnamp;
         scAlFdNam = ALfdnp;
         scAlNaInNm = ALnami;
         scAlFdInNm = ALfdni;
         scMapCoor = ALmapCoor;
         sc#ShftEmp = ALshft;
         scANonMl = ALnonm;
         scANonPh = ALnonp;
         CLEAR pmp00;
         Chain(n) (ALco#:ALfnd:ALemp#:ALdiv:'Y':ALelid) pmp00;
         If %found(wmmpl);
            scMPNName = MPname;
            scMPNAdd1 = MPadr1;
            scMPNAdd2 = MPadr2;
            scMPNcity = MPcity;
            scMPNstate= MPst;
            scMPNzip  = MPzip;
            scMPNTyp  = MPpTyp;
         Endif;
         If scDldt > 0;
           vsDelFlds = *On;
           roDelFlds = *Off;
         Else;
           vsDelFlds = *Off;
           roDelFlds = *On;
         Endif;
       EndSr;
       //********************************************************
       //*   Load Screen Fields to File
       //********************************************************
       BegSr LoadScnToFil;
         ALeltp = scdType;
         ALelid = scdElid;
         ALfein = scDfein;
         ALncci = scdRepNCCI;
         ALefdt = dtCymd(scDEFDT);

         ALname = %subst(scdDbaName:1:40);
         ALnam2 = %subst(scdDbaName:41:60);
         ALfdnm = %subst(scdFedName:1:40);
         ALfdn2 = %subst(scdFedName:41:60);

         ALmad1 = scDMAdd1;
         ALmad2 = scDMAdd2;
         ALmcty = scDMcity;
         ALmst = scDMstate;
         ALmzip = scDMzip;

         ALpad1 = scDPAdd1;
         ALpad2 = scDPAdd2;
         ALpcty = scDPcity;
         ALpst = scDPstate;
         ALpzip = scDPzip;
         ALcnty = scDCounty;

         ALnamp = scAlNaNam;
         ALfdnp = scAlFdNam;
         ALnami = scAlNaInNm;
         ALfdni = scAlFdInNm;
         ALmapCoor = scMapCoor;
         ALnonm = scANonMl;
         ALnonp = scANonPh;
         If scdPhone# > 0;
           ALarea = %Int(scdPhone#/10000000);
           ALphn# = scdPhone# - (ALarea*10000000);
         Else;
           ALarea = 0;
           ALphn# = 0;
         EndIf;
         If scdFax# > 0;
           ALfara = %Int(scdFax#/10000000);
           ALfax# = scdFax# - (ALfara*10000000);
         Else;
           ALfara = 0;
           ALfax# = 0;
         EndIf;

         ALsemp = scdSRisk;
         ALue#  = scdSUnEmp#;
         ALtpob = scdTypBus;
         ALsic  = scdSic;
         ALnaic = scdNaic;
         //*** Beg Add *** 12/22/21 ********************************************
         ALmail = scMail;
         //*** End Add *** 12/22/21 ********************************************

         ALcnt1 = scdCont1;
         ALcnt2 = scdCont2;
         ALcnt3 = scdCont3;

         //*** begin add ***  10/16/25  ****************************************
         //* Populate or clear the deletion transaction date/time fields.
         If ALDldt = 0 and scDldt <> 0;
            ALTrddt  = Dateto7(SystemDate());
            ALTrdtm = Timeto6(SystemTime());
         Endif;
         If scDldt = 0 and ALTrddt <> 0;
            ALTrddt = 0;
            ALTrdtm = 0;
         Endif;
         //*** end add *****  10/16/25  ****************************************
         ALdldt = dtCymd(scDLDT);
         ALdlrs = scDelRsn;
         ALshft = sc#ShftEmp;

         Clear pmp00;
       EndSr;
       //********************************************************
       //*   Write/Update Location file
       //********************************************************
       BegSr WrtUpdLocEnt;
         ALpgm  = Q1pgm;
         ALuser = Q1user;
         ALtime = Timeto6(SystemTime());
         ALdt   = Dateto7(SystemDate());
         ALstat = ' ';
         Select;
         When xxMode = 'A';
           ALco# = scCo#;
           ALfnd = scFnd;
           ALemp# = scEmp#;
           ALdiv = scDiv;
           ALipgm  = Q1pgm;
           ALiusr = Q1user;
           ALitm  = Timeto6(SystemTime());
           ALidt   = Dateto7(SystemDate());
           If inCall =  'MTE'; //Mid Term Endorsement
             ALstat = '+';
           Else;
             ALstat = 'A';
           Endif;
           Write pal0p;
           xxLocRrn = ALrrn;
           Feod wmalp;
         When xxMode = 'C';
           If inCall =  'MTE'; //Mid Term Endorsement
             ALstat = 'C';
           Endif;
           Update pal0p;
         When xxMode = 'd' Or xxMode = 'D';
           If inCall =  'MTE'; //Mid Term Endorsement
             ALstat = 'X';
           Endif;
           Update pal0p;
         EndSl;

         //*** begin add ***  04/12/23  *********************************
         //* Do not write/update MPN data if not being used.
         If isUseMPN <> *on;
            Leavesr;
         Endif;
         //*** end add *****  04/12/23  *********************************
         Chain (ALco#:ALfnd:ALemp#:ALdiv:'Y':ALelid) pmp00;
         MPName = scMPNName;
         MPadr1 = scMPNAdd1;
         MPadr2 = scMPNAdd2;
         MPcity = scMPNcity;
         MPst   = scMPNstate;
         MPzip  = scMPNzip;
         MPpTyp = scMPNTyp;
         If %found(wmmpl);
            MPusr  = Q1user;
            MPpgm  = Q1pgm;
            MPtim  = Timeto6(SystemTime());
            MPdt   = Dateto7(SystemDate());
            update pmp00;
         Else;
            MPco#  = ALco#;
            MPfnd  = ALfnd;
            MPemp# = ALemp#;
            MPdiv  = ALdiv;
            MPelfl = 'Y';
            MPelid = ALelid;
            MPstat = 'A';
            MPiusr = Q1user;
            MPipgm = Q1pgm;
            MPitim = Timeto6(SystemTime());
            MPidt  = Dateto7(SystemDate());
            write pmp00;
         Endif;
       EndSr;
       //***********************************************************************
       // ClearErrors
       //***********************************************************************
        BegSr ClearErrors;
          ERLocErr   = *Off;
          ERDElid    = *Off;
          ERDType    = *Off;
          ERDCONT1   = *Off;
          ERDCONT2   = *Off;
          ERDCONT3   = *Off;
          ERDCOUNTY  = *Off;
          ERDDBANAME = *Off;
          ERDEFDT    = *Off;
          ERDELRSN   = *Off;
          ERDFAX#    =*Off;
          ERDFEDNAME = *Off;
          ERDFEIN     = *Off;
          ERDLDT      = *Off;
          ERDMADD1    = *Off;
          ERDMADD2    = *Off;
          ERDMCITY    = *Off;
          ERDMSTATE   = *Off;
          ERDMZIP     = *Off;
          ERDNAIC     = *Off;
          ERDPADD1    = *Off;
          ERDPADD2    = *Off;
          ERDPCITY    = *Off;
          ERDPHONE#   = *Off;
          ERDPSTATE   = *Off;
          ERDPZIP     = *Off;
          ERDREPNCCI  = *Off;
          ERDSIC      = *Off;
          ERDSRISK    = *Off;
          ERDSUNEMP#  = *Off;
          ERDTYPBUS   = *Off;
          ermail      = *off;
        EndSr;
       //***********************************************************************
       // Process Save
       //***********************************************************************
        BegSr ProcessSave;
          ExSr ClearErrors;
          If ValidLocEnt(inMode: xxLocRrn);
            If xxMode <> 'A';
              Chain xxLocRrn pal0p;
            Endif;
            Exsr LoadScnToFil;
            Exsr WrtUpdLocEnt;
            ExitNow = *On;
          EndIf;
        EndSr;
       //***********************************************************************
       // Process Add/Next
       //***********************************************************************
        BegSr ProcessAddNext;
          readOnly = *on;
          Exsr InitializeAddMode;
          ExitNow = *Off;
        EndSr;
       //***********************************************************************
       // Delete Location/Entity
       //***********************************************************************
        Begsr DeleteLocEnt;

       // Delete all owner/officers attached to this location/entity

          Setll keyWMOOL poo00;
          Reade keyWMOOL poo00;
          Dow not %Eof;
             Delete poo00;
             Reade keyWMOOL poo00;
          Enddo;
       // Delete all location year attached to this location/entity:

          Setll keyWDLDL pld00;
          Reade keyWDLDL# pld00;
          Dow not %Eof;
             Delete pld00;
             Reade keyWDLDL# pld00;
          Enddo;

       // Delete location/entity

          Chain inLocRrn pal0p;
          If %found(WMALP);
             Delete pal0p;
          Endif;

        EndSr;
      /end-free
     p PrcLocEntWdw    e

       //===================================================================
       // Validate Location/Entity window                                   xes.
       //===================================================================
     p ValidLocEnt     b
     d ValidLocEnt     pi              n
     d  inMode                        1    const
     d  inLocRrn                      9  0

     disError          s               n   inz
     D message         s            100
     dxxDate           s              8  0
     dxxDate1          s              7  0
     dxxKey            s             10
     d xxState         s              2    inz
     d xxCity          s             25    inz
     d xxZip5          s              5  0 inz
     d xxpmts          s             11  2 inz
     d conMsg          C                   CONST('MESSAGE: Please note-
     d                                      the defaulted -
     d                                     change to the county-
     d                                      code.')
     d outErr          s              1    inz
      *  Claims Master
      *
     C     keyWMCMLC     KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    scFND
     C                   KFLD                    scEMP#
     C                   KFLD                    scDIV
     C                   KFLD                    xxDate1
      *
     C     key1WMCMLC    KLIST
     C                   KFLD                    scCO#
     C                   KFLD                    scFND
     C                   KFLD                    scEMP#
     C                   KFLD                    scDIV

      /free
       If inMode = 'A';
         If scdElid = *Zeros Or scdElid = *Blanks;
           isError = *On;
           erdElid = *On;
           mgdElid = 'WCR0503';
         Endif;

        //SINCE WE ALLOW USERS TO OVERRIDE THE SYSTEM-ASSIGNED ID#
        //ON AN ADD MODE, DON'T ALLOW AN ID# THAT ALREADY EXISTS.
        Chain(n) (scCo#:scFnd:scEmp#:scDiv:scdElid) pal00;
        If %Found(wmall);
          isError = *On;
          erdElid = *On;
          mgdElid = 'WCR1225';
        Endif;
         If scdType <> 'E' And scdType <> 'L';
           isError = *On;
           erdType = *On;
           mgdType = 'WCR0950';
         Endif;
       Endif;

       //VALIDATE DELETION DATE/REASON
       If scDldt <> 0 Or inMode = 'D';

        // DELETION DATE MUST BE VALID
         xxDate = dtCheck(scDldt);
         If xxDate = 0;
           isError = *On;
           erDldt  = *On;
           mgDldt = 'WCR0500';
         Else;
           scDldt = xxDate;
         Endif;

         //DELETION DATE CANNOT BE LESS THAN EFFECTIVE DATE.
         If dtCymd(scDldt) < dtCymd(scdEfdt) And scDldt <> 0;
           isError = *On;
           erDldt  = *On;
           mgDldt = 'WCR1226';
         Endif;

        // IF A DELETE DATE WAS SPECIFIED AND THERE IS ANY CLAIM THAT
        // EXISTS WITH AN ACCIDENT DATE GREATER THAN OR EQUAL TO THE DELETE
        // DATE, FLAG AN ERROR MESSAGE BUT ALLOW USER TO OVERRIDE.
        scClmCount = 0;
        If scDlDt <> *zeros;
           xxdate1 = dtCymd(scDldt);
           setll keyWMCMLC pcm0c;
           reade key1WMCMLC pcm0c;
           dow not %eof(WMCMLC);
              xxpmts = %XFOOT(AR);

              If (CMstat <> 'C' and CMctg1 = scdelid) or
               (CMstat = 'C' and CMctg1 = scdelid and xxpmts <> *zeros);
                 scClmCount = scClmCount + 1;
              Endif;
              reade key1WMCMLC pcm0c;
           Enddo;
           If scClmCount > *zeros;

                 // ALLOW OVERRIDE ON ERROR MSG ONLY IF USER HAS PROPER AUTHORIT
                 cadCancel = *off;
                 cadConfirm = *off;
                 // Get user sec level/type
                 inULVL = %Editc(GetUsrLvl(Q1user:inUTYP):'X');
                 // Security
                 keySDACL = 'LocDelOver';
                 // Check Authority
                 Chain keySDACL pac00;
                 XXLRMK=ACRMCD;
                 XXLUSR=ACRUSR;
                 CheckAppSec(inUTYP:inULVL:ACstp1:%Editc(ACslv1:'X'):'1':outRes
                 :ACstp2:ACstp3:ACstp4:ACstp5:ACstp6:%Editc(ACslv2:'X')
                 :%Editc(ACslv3:'X'):%Editc(ACslv4:'X'):%Editc(ACslv5:'X')
                 :%Editc(ACslv6:'X'):ACstp7:ACstp8:ACstp9:ACstp0
                 :%Editc(ACslv7:'X')
                 :%Editc(ACslv8:'X'):%Editc(ACslv9:'X'):%Editc(ACslv0:'X'));
                 If outRes = '0';
                    scCADOver = *blank;
                    cadSecLev = *on;
                    Dou cadCancel = *on or cadConfirm = *on;
                       exfmt rcdClmAcDt;
                    Enddo;

                    If cadCancel = *on;
                       isError = *on;
                    Else;
                       If scCADOver <> 'Y';
                          isError = *on;
                       Endif;
                       If XXLRMK <> *BLANKS and XXLUSR <> *BLANKS;

                          If LocDWritn <> 'Y';
                             LocDWritn = 'Y';
                             Exsr WriteDART;
                          Endif;
                       Endif;
                    Endif;
                 Else;
                    cadSecLev = *off;
                    Dou cadCancel = *on;
                       exfmt rcdClmAcDt;
                    Enddo;
                    isError = *on;
                 Endif;
              Endif;
        Endif;

        // DELETION REASON CANNOT BE BLANK
          If scDelRsn = *Blanks;
           isError = *On;
           erDelRsn = *On;
           mgDelRsn = 'WCR0503';
          Endif;
       Endif;
        // All delete processing done, leave here
       If inMode = 'D';
         Return not isError;
       Endif;
        // Continue for other modes
        // EFFECTIVE DATE MUST BE VALID
       erDefdt = ValidDate(scDEfdt:message);
       If erDefdt = *on;
         isError = *On;
         mgDefdt= message;
       Endif;

       //EFFECTIVE DATE MUST BE GREATER THAN INCEPTION DATE
       If erDefdt <> *on and dtCymd(scdEfdt) < svEfd1;
         isError = *On;
         erDefdt = *On;
         mgDefdt= 'WC30155';
       Endif;

       //IF FEDERAL ID REQUIRED, FEIN MUST NOT BE BLANK
       Exsr EditLocFein;

       //ENTITY/LOCATION NAME MUST BE ENTERED
       If scdDbaName = *Blanks;
         isError = *On;
         erDDbaName = *On;
         mgDDbaName = 'WCR0503';
       Endif;

       //CLTID = 136 PERSONAL NAME FIELDS MUST BE VALID IF ENTERED.

       //REQUIRE EITHER MAILING OR PHYSICAL ADDRESS TO BE ENTERED
       If scdMadd1 = *Blanks And scdPadd1 = *Blanks;
         isError = *On;
         erDMadd1 = *On;
         mgDMadd1 = 'WCR1130';
       Endif;

       //VALIDATE MAILING ADDRESS IF REQUIRED
       If scdMadd1 <> *Blanks;
         //MAILING CITY CANNOT BE BLANK
         If scdMcity = *Blanks;
           isError = *On;
           erDMCity = *On;
           mgDMCity = 'WCR0503';
         Endif;
         //THE STATE MUST BE VALID
         xxKey = scDMstate;
         Chain xxKey pst03;
          If Not %Found(ststl3);
            isError = *On;
            erDMState = *On;
            mgDMState = 'WCR0504';
          Endif;
          //THE ZIP CODE CANNOT BE ZERO
          If scdMzip <= 0;
            isError = *On;
            erDMZip = *On;
            mgDMZip = 'WCR0502';
          Endif;
            //***** Beg Del ********* 08/06/21 *********************************
            //***** If ClientID = '136' and erDMZip = *Off;
            //***** intZip6 = %int(%subst(%editc(scdMZip:'X'):6:4));
            //****** If intZip6 <= 0;
            //*****    isError = *On;
            //*****    erDMZip = *On;
            //*****    mgDMZip = 'WCR0502';
            //***** Endif;
          //***** Endif;
          //***** End Del ********* 08/06/21 ***********************************
          If erDMZip <> *on;
             xxState = scdMState;
             xxZip5 = %Int(%Subst(%Editc(scdMZip:'X'):1:5));
             Chain (scCo#:xxState:xxZip5) pcz00;
             If %Found(wmczl) And CZstat <> 'A';
                isError = *On;
                erDMZip = *On;
                mgDMZip = 'WC30707';
             Endif;
          Endif;
       Endif;

       //VALIDATE PHYSICAL ADDRESS IF REQUIRED
       If scdPadd1 <> *Blanks;
         //PHY CITY CANNOT BE BLANK
         If scdPcity = *Blanks;
           isError = *On;
           erDPCity = *On;
           mgDPCity = 'WCR0503';
         Endif;
         //THE STATE MUST BE VALID
         xxKey = scDPstate;
         Chain xxKey pst03;
          If Not %Found(ststl3);
            isError = *On;
            erDPState = *On;
            mgDPState = 'WCR0504';
          Endif;
          //THE ZIP CODE CANNOT BE ZERO
          If scdPzip <= 0;
            isError = *On;
            erDPZip = *On;
            mgDPZip = 'WCR0502';
          Endif;
          //********* 08/06/21 *************** Beg Del *****************
          //***** If ClientID  = '136' and erDPZip <> *on;
          //*****  intZip6 = %int(%subst(%editc(scdPZip:'X'):6:4));
          //***** If intZip6 <= 0;
          //***** isError = *On;
          //***** erDPZip = *On;
          //***** mgDPZip = 'WCR0502';
          //***** Endif;
          //***** Endif;
          //********* 08/06/21 *************** End Del *****************
          If erDPZip <> *on;
             xxState = scdPState;
             xxZip5 = %Int(%Subst(%Editc(scdPZip:'X'):1:5));
             Chain (scCo#:xxState:xxZip5) pcz00;
             If %Found(wmczl) And CZstat <> 'A';
                isError = *On;
                erDPZip = *On;
                mgDPZip = 'WC30707';
             Endif;
          Endif;
       Endif;

         //Default county code
         If scdPState <> *Blanks And scdPZip > 0;
           xxState = scdPState;
           xxZip5 = %Int(%Subst(%Editc(scdPZip:'X'):1:5));
         Else;
           xxState = scdMState;
           xxZip5 = %Int(%Subst(%Editc(scdMZip:'X'):1:5));
         Endif;
         Chain (scCo#:xxState:xxZip5) pcz00;
         If %Found(wmczl) And CZstat = 'A';
           If scdCounty <> CZcnty And scdCounty <> *Blanks;
              isError = *On;
              erCounty = *On;
              mgDCounty = conMsg;
           Endif;
           scdCounty = CZcnty;
         Endif;

       // If Phone# Number entered has 10 digit
       ##type1 = 'P';
       erDPhone# = Check_Phone#(scDPhone#:##type1:mgDPhone#);
       If erDPhone# = *on;
          isError = *on;
       Endif;

       // If Fax Number entered has 10 digit
       ##type1 = 'F';
       erDFax# = Check_Phone#(scDFax#:##type1:mgDFax#);
       If erDFax# = *on;
          isError = *on;
       Endif;

        // Validate type of business
        xxKey = scdTypBus;
        If not ValidGenCode(scCo#:'BUSI':xxKey);
          isError = *On;
          erdTypBus = *On;
          mgdTypBus = 'WCR0588';
        Endif;

        // Validate SIC code
        If (scDsic = *Blanks And EXsicr = 'Y')
         And flgQte <> 'Q';
          isError = *On;
          erdSic = *On;
          mgdSic = 'WCR0503';
        Endif;

        If erdSic <> *on;
           xxKey = scDSic;
           If scdsic <> *Blanks And not ValidGenCode(scCo#:'SIC':xxKey);
              isError = *On;
              erdSic = *On;
              mgdSic = 'WCR0513';
           Endif;
        Endif;

        //Validate NAICS code
        If SCDPstate <> *Blanks;
          xxState = SCDPState;
        Else;
          xxState = SCDMState;
        Endif;
        Chain(n) (SCco#:SCfnd:xxState) prr01;
        If %Found(wdrrl1) And RRnaic = 'Y' And SCDnaic = *Blanks
           And flgQte <> 'Q';
          isError = *On;
          erDNaic = *On;
          mgDNaic = 'WC31519';
          reqDNaics = *On;
        Else;
          erDNaic = *Off;
          reqDNaics = *Off;
        Endif;
        If erDNaic <> *on And RRnaic = 'Y'
           And flgQte <> 'Q';
           xxKey = SCDnaic;
           If SCDnaic <> *Blanks And not ValidGenCode(scCo#:'NAIC':xxKey);
              isError = *On;
              erDNaic = *On;
              mgDNaic = 'WCR0513';
              reqDNaics = *On;
           Else;
             erDNaic = *Off;
             reqDNaics = *Off;
           Endif;
        Endif;

       //*** Beg Add ***  04/29/24  *** WC2965 *************************

       If SCDmstate <> ' ' Or SCDpstate <> ' ';

          xxDate = %int(%Char(%Date() : *CYMD0));
          SetGT (scCo#:scFnd:SCDmstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCDmstate) WDRRL;
          If Not %EOF and RREmail = 'Y' and ScMail = ' ' and
                          EMStrp = 'Y' and EMMail = ' ';
             erMail = *On;
             msMail = 'The email address is required for this state.';
             isError = *On;
          EndIf;

          SetGT (scCo#:scFnd:SCDpstate:xxDate) WDRRL;
          ReadPE (scCo#:scFnd:SCDpstate) WDRRL;
          If Not %EOF and RREmail = 'Y' and ScMail = ' ' and
                          EMStrp = 'Y' and EMMail = ' ';
             erMail = *On;
             msMail = 'The email address is required for this state.';
             isError = *On;
          EndIf;

       EndIf;
       //*** End Add ***  04/29/24  *** WC2965 *************************

       //*** Beg Add *** 12/22/21 ******************************************
       // If Email Address entered verify correct format
       If scMail <> *blanks;
          erMail = Check_Email(scmail:msMail);
          If erMail = *on;
             isError = *on;
          Endif;
       Endif;
       //*** End Add *** 12/22/21 ******************************************

         //Check State Unemployment Flag for required field
        Chain(n) (SCco#:SCfnd:xxState) prr01;
        If %Found(wdrrl1) And RRurfl = 'Y' And scDSUnEmp# = *Blanks;
          isError  = *On;
          erDSunEmp# = *On;
          mgDSunEmp# = 'WC31455';
          reqdUmEmp = *On;
        Else;
          erDSunEmp# = *Off;
          reqdUmEmp   = *Off;
        Endif;

        // CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
        If inMode <> 'A';
          Chain(n) inLocRrn pal0p;
          If ALdt > saveALdt Or (ALdt = saveALdt AND ALtime > saveALtm);
            isError = *On;
            erLocErr = *On;
            mgLocErr = 'WC30000';
          Endif;
        Endif;
       Return not isError;
       //*********************************************************************
       // Edit Loc/Ent FEIN
       //*********************************************************************
        BegSr EditLocFEIN;

       //IF FEDERAL ID REQUIRED, FEIN MUST NOT BE BLANK
          If flgFifg = 'Y' And scdType = 'E' And scDFein = *Zeros;
             isError = *On;
             erDFein = *On;
             mgDFein= 'WC30649';
             LeaveSr;
          Endif;


          If flgPEOStatus = 'M';
             //If the Group PEO flag is "M", then bypass this editing
            LeaveSr;
          EndIf;

          If scMode = 'C' And scDFein = EMfein;
            LeaveSr;
          Endif;


       // Check for duplicate fein if one has been entered.
          If not %Open(WpiLocMst);
              Open wpiLocMst;
              iOpen1 = *On;
          Endif;

          If scDFein <> 0 and isError <> *on;
             feinCancel = *off;
             feinContin = *off;

             programIn = 'WTSEARCH';
             tabNameIn = 'Duplicate FEIN Listing';
             Clear dsSrchCrit;
             Clear selectIn;
             dsEmpFein = scDFein;
             selectIn  = dsSrchCrit;
             If flgQte = *on;
                scDupLink = CreateBIGQteUrl(programIn:tabNameIn:modeIn:callIn:
                 rrnOut:selectIn);
             Else;
                scDupLink = CreateBIGUrl(programIn:tabNameIn:modeIn:callIn:
                 rrnOut:selectIn);
             Endif;

             Setll (scCo#:scDFein) pem12;

             If not %equal(wmeml12);
               CheckQuoteFein(%Editc(scCO#:'X'):%Editc(scDFein:'X'):
               %Editc(scEmp#:'X'):flgQte:outErr);
             Endif;

             If (%equal(wmeml12) and (scFnd <> EMfnd or scEmp# <> EMemp#)) or
              outErr= '1';

                Dou feinCancel = *on or feinContin = *on;
                    Exfmt rcdFeinDup;
                Enddo;

                If feinCancel = *on;
                   isError = *on;
                Endif;

             Endif;

          Endif;
          If %Open(WpiLocMst) and iOpen1;
             Close wpiLocMst;
          Endif;
        EndSr;
      /end-free
       //*********************************************************************
       // WriteDART - Write DARTS remark for location expiration
       //*********************************************************************
      /Free
        BegSr WriteDART;

        ##key  = %EditC(scco#:'X') + %EditC(scfnd:'X') +
                 %EditC(scemp#:'X') + %EditC(scdiv:'X');
        Chain(n) xxlrmk PCT01;
           If Not %Found;
              *in89 = *On;
           Endif;
        Chain(n) xxlusr PUP01;
           If Not %Found;
              *in90 = *On;
           Endif;
        ##CDAT = %Editc(dtmdyy(Dateto7(SystemDate())):'X');

         Chain(n) (scCo#:scFnd:scEmp#:scDiv) PEM00;
         Setgt (scCo#:scFnd:scEmp#:scDiv:dtCYMD(scdefdt)) PEl02;
         Readpe(n) (scCo#:scFnd:scEmp#:scDiv) PEl02;
         svEMname = EMname;
         SvDbaNam = EMname + EMnam2;
         SvFedNam = EMfdnm + EMfdn2;
         svELfyr = %editc(ELfyr:'X');

           If svDbaNam <> *blanks;
              ##Name = svDbaNam;
           Else;
              ##Name = svFedNam;
           Endif;
        ##delid= %triml(scdelid:'0');
        ##count= %triml(%editc(scclmcount:'Z'));
        ##exdt = %editw(scdldt:'0 /  /    ');
        If scCADOver = 'Y';
           ##comm = *blanks;
           ##comm = 'Location:' + ' ' + %trim(##delid) + ' - ' + %trim(##Name)+
                    ' has ben expired as of ' + ##Exdt + '. ' +
                    'There are ' + %trim(##Count) + ' ' +
                    'claim(s) to be reviewed.';
        Else;
           ##comm = 'Location:' + ' ' + %trim(##delid) + ' - ' + %trim(##Name)+
                    ' is pending expiration as of ' + ##Exdt + '. ' +
                    'There are ' + %trim(##Count) + ' ' +
                    'claim(s) to be reviewed.';
        Endif;

        ##user = UPusr;
        ##secl = '000';
        ##pgm  = q1pgm;
        ##iPgm = Q1pgm;
        ##rmcd = xxlrmk;
        ##dcid = *blanks;
        ##pnam = 'W0050R';
        ##fold = *blanks;
        ##difl = 'Y';
        ##seq# = '00000';
        ##fl01 = ' ';
        ##fl02 = ' ';
        ##sbyl = 'B';
        ##com2 = ' ';
        ##apId = ' ';
        ##iDoc = ' ';
        ##iQue = ' ';
        ##imgF = 'O';
        ##iPth = ' ';
        ##pIdx = ' ';
        ##qPty = ' ';
        ##dcTp = ' ';
        ##csId = ' ';
        ##code = ' ';

        If *in89 = *off and *in90 = *off;
        Darts(##key:##comm:##cDat:##dcId:##pNam:##secl:##user:
         ##pgm:##fold:##diFl:##iPgm:##fl01:##fl02:##rmCd:##seq#:
         ##sbyl:##com2:##apId:##iDoc:##iQue:##imgF:##iPth:##pIdx:
         ##qPty:##dcTp:##csId:##code);

        ##pol# = %EditC(scfnd:'X') + '-' + %EditC(scemp#:'X') + '-' +
                    svelfyr;
        ##polnam = svEMname;
        ##div# = %EditC(sclocdiv#:'X');
        if svdbanam <> *blanks;
           ##divnam = svdbanam;
        else;
           ##divnam = svfednam;
        endif;

       ##cm75 = *blanks;
       ##cm75 = 'Policy:' + ' ' + ##pol#  + ' ' + ##polnam;
       Exsr WriteDART1;

       ##cm75 = *blanks;
       ##cm75 = 'Division:' + ' ' + ##div#  + ' ' + ##divnam;
       Exsr WriteDART1;

       ##cm75 = *blanks;
       ##cm75 = 'Location:' + ' ' + %trim(##delid)  + ' - ' + ##name;
       Exsr WriteDART1;

       ##cm75 = *blanks;
       Exsr WriteDART1;

       ##cm75 = *blanks;
       ##cm75 = 'Claim Number       Claimant Name        ' +
                'Date of Injury   Adjuster';
       Exsr WriteDART1;

           xxdate1 = dtCymd(scDldt);
           setll keyWMCMLC pcm0c;
           reade key1WMCMLC pcm0c;
           dow not %eof(WMCMLC);
              xxpmts = %XFOOT(AR);
              If (CMstat <> 'C' and CMctg1 = scdelid) or
               (CMstat = 'C' and CMctg1 = scdelid and xxpmts <> *zeros);
                 ##clm# = %EditC(cmfnd:'X') + '-' +
                   %EditC(cmcase:'X') + '-' + %EditC(cmfyr:'X');
                 ##clmname = %trim(cmfsn) + ' ' + %trim(cmlsn);
                 ##doi  = %editc(dtmdyy(cmacdt):'Y');
                 Chain CMfead smupl1;
                 If %Found(smupl1);
                    ##adjust = %trim(upusnm);
                 Endif;
                 ##cm75 = *blanks;
                 ##cm75 = ##clm# +'   '+##clmname+' '+##doi+'       '+
                            ##adjust;
                 exsr writedart1;
              Endif;
              reade key1WMCMLC pcm0c;
           Enddo;
        Endif;
        EndSr;

       //================================================================
       // WriteDART1 - Write Addl DARTS remarks for location expiration
       //================================================================
       //*********************************************************************
        BegSr WriteDART1;

        ##asq# = *blanks;

        Darts1(##key:##seq#:##cm75:##pNam:##secl:##user:
         ##pgm:##rmCd:##asq#);

        EndSr;
      /End-Free
     p ValidLocEnt     e
       //***********************************************************************
       //===================================================================
       // Add/Edit Owner/Officer popup window
       //===================================================================
     p PrcOwnOffWdw    b                   Export
     d PrcOwnOffWdw    pi
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inOwnRrn                      9  0 options(*nopass) const
     d  inLocRrn                      9  0 options(*nopass) const
     d*
     d  xxRrn          s              9  0 inz
     d  xxOwnRrn       s              9  0 inz
     d  xxLocRrn       s              9  0 inz
     d  xxDelResp      s               n   inz
     d  xxMode         s              1    inz
     d  ExitNow        s               n   inz
     d iOpen           s               n   inz
     d dsLocBillDiv  e ds                  extname(wpilocmst:rcdBillDiv) inz
     d ptrLocBillDiv   s               *   inz
     d  xxModeO        s              1    inz
     d svDiv#          s              5  0 inz
       //***** Beg Chg ***** 04/16/21 ***********************
     d TotalOwnPrcnt   s              7  2 inz
       //***** End Chg ***** 04/16/21 ***********************
      /free
       OwnOffTop = 000;
       If %parms > 3;
         xxOwnRrn = inOwnRrn;
       Endif;
       If %parms > 4;
         xxLocRrn = inLocRrn;
         If inCall = 'LOC';
           OwnOffTop = 650;
         Endif;
       Endif;

       If Not %Open(WpiLocMst);
         Open WpilocMst;
         iOpen = *On;
          Clear dsLocBillDiv;
          Clear dsInitData;
          ptrLocBillDiv = %Addr(dsLocBillDiv);
          ptrInitData = %Addr(dsInitData);
          xxMode = inMode;
          xxModeO = inMode;
          If xxModeO = 'A';
             xxModeO = ' ';
          Endif;
             InitBillDiv(inRrn:xxModeO:ptrLocBillDiv:ptrInitData);
       Endif;
       GetEmployer(inRRN);
       scCo# = EMco#;
       scFnd = EMfnd;
       scEmp# = EMemp#;
       scDiv = EMdiv;
       scxDiv= EMdiv;
       xxRRN = GetEmpYrRR(0 :EMemp# :EMdiv :EMfnd :EMco# :' ');
       Chain xxRrn pel0p;

       scOODiv# = EMdiv;
       svDiv#   = EMdiv;
       dsbOODiv# = *on;
       Clear divChoice;
       divChoice = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);
       If inMode = 'A' and inCall = 'QTE' and NumOfDivs > 1;
          dsbOODiv# = *off;
       Endif;
       clsChoices = 'PRCO# = ' + %char(EMCO#) + ' AND PRRTCD = '''
       + ELRTCD + ''' AND PREFDT = (SELECT MAX(A.PREFDT) FROM WMPRP A WHERE '
       + ' A.PRCO# = '+ %char(ELCO#) +' AND A.PRRTCD = '''+ ELRTCD +'''  AND'
       + ' A.PREFDT <= ''' + %char(ELPFDT) + ''' '
       + ' AND A.PRCLAS = WMPRL8.PRCLAS AND PRSTAT = ''A'') ';

       saveOOdt =  Dateto7(systemDate());
       saveOOtm = Timeto6(SystemTime());

       ExSr ClearErrors;
       If inMode = 'I';
          dspInEx = *On;
       Else;
          dspInEx = *Off;
       EndIf;
       scOOPnlTtl = 'Owner/Officer';
       Select;
       When inMode = 'A';
         Clear poo0p;
         visOwnSNxt = *On;
         leftOwnCan = 655;
         scOOPnlTtl = %Trim(scOOPnlTtl) + ' Add';
       When inMode = 'C' Or inMode = 'D' Or inMode = 'I';
         If xxOwnRrn > 0;
           Chain(n) xxOwnRrn poo0p;
           scOODiv# = OOdiv;
           svDiv#   = OOdiv;
         Else;
           Return;
         Endif;
         visOwnSNxt = *Off;
         leftOwnCan = 765;
         Select;
         When inMode = 'C' or inMode = 'I';
           scOOPnlTtl = %Trim(scOOPnlTtl) + ' Edit';
         When inMode = 'D';
           scOOPnlTtl = %Trim(scOOPnlTtl) + ' Delete';
         EndSl;
       EndSl;
       Exsr LoadFilToScn;

       DoU ExitNow = *On;
         Select;
         When inMode = 'D';
           Write rcdOwnOff;
           xxDelresp = PopTwoBtn('Do you want to delete this Owner/Officer?':
            'Confirm Delete':'Yes':'No':0:OwnOffTop+150);
           Select;
           When xxDelResp = *On;
             OOstat = 'I';
             Chain xxOwnRrn poo0p;
             Exsr WrtUpdOwnOff;
             PopOneBtn('Owner/Officer deleted!':'Delete':'Ok':0:OwnOffTop+150);
             ExitNow = *On;
           When xxDelResp = *Off;
             ExitNow = *On;
           EndSl;
         Other;
           ExFmt rcdOwnOff;
           Select;
           When btnOwnCan = *On;
             ExitNow = *On;
           When btnOwnSave = *On;
             ExSr ProcessSave;
           When btnOwnSNxt = *On;
             ExSr ProcessSave;
             If ExitNow = *On;
               ExitNow = *Off;
               clear poo0p;
               Exsr LoadFilToScn;
             Endif;
           When divChgOO = *On and scOODiv# <> svDiv#;
              divChgOO = *off;
              svDiv# = scOODiv#;
              xxRRN = GetEmpYrRR(0 :EMemp# :svDiv# :EMfnd :EMco# :' ');
              Chain xxRrn pel0p;
              scDiv = ELdiv;
              scxDiv= ELdiv;
              inRRN = GetEmpRRN(scEmp# :svDiv# :scFnd :scCo#);
              GetEmployer(inRRN);

              scOODiv# = EMdiv;
              clsChoices = 'PRCO# = ' + %char(EMCO#) + ' AND PRRTCD = '''
               + ELRTCD + ''' AND PREFDT = (SELECT MAX(A.PREFDT) FROM '
               + 'WMPRP A WHERE A.PRCO# = '+ %char(ELCO#) + ' AND '
               + 'A.PRRTCD = '''+ ELRTCD +'''  AND '
               + 'A.PREFDT <= ''' + %char(ELPFDT) + ''' '
               + 'AND A.PRCLAS = WMPRL8.PRCLAS AND PRSTAT = ''A'') ';
              ExSr ClearErrors;
           EndSl;
         EndSl;
        If btnOwnSave = *on or btnOwnCan = *on;
           Exsr  TotlOwnPrctVal;
        Endif;
       EndDo;
       If %Open(WpiLocMst) And iOpen;
         Close WpiLocMst;
       Endif;
       Return;
       //********************************************************
       //* ProcessSave
       //********************************************************
       BegSr ProcessSave;
         ExSr ClearErrors;
         If ValidOwnOff(inMode:xxOwnRrn);
           If inMode <> 'A';
             Chain xxOwnRrn poo0p;
           Endif;
           Exsr LoadScnToFil;
           Exsr WrtUpdOwnOff;
           ExitNow = *On;
         EndIf;
       EndSr;
       //********************************************************
       //*   Clear error indicators
       //********************************************************
       BegSr ClearErrors;
         erOwnErr = *Off;
         erOOClsCd = *Off;
         erOOcov = *Off;
         erOOdob = *Off;
         erOOefdt = *Off;
         erOOexdt = *Off;
         erOOfname = *Off;
         erOOlname = *Off;
         erOOprcnt = *Off;
         erOOSS# = *Off;
         erOOttl = *Off;
       EndSr;
       //********************************************************
       //*   Load File fields to screen
       //********************************************************
       BegSr LoadFilToScn;
         flgEntLoc = *Off;
         scElid = OOelid;
         If xxLocRrn >0;
           Chain(n) xxLocRrn pal0p;
           If ALeltp = 'E';
             If inMode = 'A';
               scElid = ALelid;
             Endif;
             scEntLoc = ALelid + ' ' + ALname;
             flgEntLoc = *On;
             scOODiv# = ALdiv;
             svDiv#   = ALdiv;
           Endif;
         Else;
           If OOelid <> *Blanks Or OOelid <> *Zeros;
             Chain(n) (OOco#:OOfnd:OOemp#:OOdiv:OOelid) pal00;
             If %Found(wmall);
               scEntLoc = A0elid + ' ' + A0name;
               flgEntLoc = *On;
                scOODiv# = ALdiv;
                svDiv#   = ALdiv;
             Endif;
           Endif;
         Endif;

         scOOLName = OOolsn;
         scOOFName = OOofsn;
         scOOSS# = MaskData('SSN':'OOoss#':OOoss#);
         scOODob = dtmdyy(OOdob);
         scOOprcnt = OOofpc;
         scOOttl = OOtitl;
         scOOcov = OOexfl;
         scOOefdt = dtmdyy(OOefdt);
         svOOefdt = dtmdyy(OOefdt);
         scOOexdt = dtmdyy(OOexdt);
         scOOClsCd = OOclas;
       EndSr;
       //********************************************************
       //*   Load Screen Fields to File
       //********************************************************
       BegSr LoadScnToFil;
          OOelid = scElid;
          OOolsn = scOOLName;
          OOofsn = scOOFName;
          OOoss# = UnMaskData('SSN':'OOoss#':scOOSS#);
          OOdob  = DtCymd(scOOdob);
          OOofpc = scOOprcnt;
          OOtitl = scOOttl;
          OOexfl = scOOcov;
          OOefdt = DtCymd(scOOefdt);
          OOexdt = DtCymd(scOOexdt);
          OOclas = scOOClsCd;
       EndSr;
       //********************************************************
       //*   Write/Update Owner/Officer
       //********************************************************
       BegSr WrtUpdOwnOff;

         OOpgm  = Q1pgm;
         OOuser = Q1user;
         OOtime = Timeto6(SystemTime());
         OOdt   = Dateto7(SystemDate());
         Select;
         When inMode = 'A' or inMode = 'I';
            If inMode = 'I';
               OOefdt = DtCymd(svOOefdt);
               OOexdt = DtCymd(scOOExdt);
               Update poo0p;
               If OOexfl = 'I';
                  OOexfl = 'E';
               Else;
                  OOexfl = 'I';
               Endif;
            EndIf;
           SetGt (scCo#:scFnd:scEmp#:scDiv) poo07;
           Readpe(n) (scCo#:scFnd:scEmp#:scDiv) poo07;
           If not %Eof(wmool7);
             OOseq# = O7seq# + 1;
           Else;
             OOseq# = 1;
           EndIf;
           OOco# = scCo#;
           OOFnd = scFnd;
           OOEmp# = scEmp#;
           OODiv = scDiv;
           OOStat = 'A';
           If inMode = 'I';
              OOefdt = DtCymd(scOOexdt);
              OOexdt = *zeros;
           Else;
              OOefdt = DtCymd(scOOefdt);
              OOexdt = DtCymd(scOOexdt);
           Endif;

           OOipgm  = Q1pgm;
           OOiusr = Q1user;
           OOitim = Timeto6(SystemTime());
           OOidt   = Dateto7(SystemDate());
           Write poo0p;
           Feod wmoop;
         When inMode = 'C';
           Update poo0p;
         When inMode = 'D';
           Delete poo0p;
         EndSl;
       EndSr;
        Begsr TotlOwnPrctVal;
           Clear TotalOwnPrcnt;
           If inMode = 'A';
              kkDiv = scDiv;
           Else;
              kkDiv = OOdiv;
           Endif;
           Setll (EMco#:EMfnd:EMemp#:kkDiv) poo00;
           DoU %Eof(wmool);
             Reade(N) (EMco#:EMfnd:EMemp#:kkDiv) poo00;
             If %Eof(wmool);
             Leave;
             EndIf;
             //*** Beg Chg *** 12/13/21 ************************
             //***eval  TotalOwnPrcnt =TotalOwnPrcnt+OOofpc;
             If OOstat <> 'I';
                eval  TotalOwnPrcnt =TotalOwnPrcnt+OOofpc;
             Endif;
             //*** End Chg *** 12/13/21 ************************
           Enddo;
           If (TotalOwnPrcnt<>100 and btnOwnSNxt <> *on)
           or (TotalOwnPrcnt>100 and btnOwnSNxt =  *on);
             PopOneBtn('Total ownership percent is not 100': 'Information');
           Endif;
        Endsr;
      /end-free
     p PrcOwnOffWdw    e
       //===================================================================
       // Validate Owner/Office window                                      xes.
       //===================================================================
     p ValidOwnOff     b
     d ValidOwnOff     pi              n
     d  inMode                        1    const
     d  inOwnRrn                      9  0

     disError          s               n   inz
     dxxDate           s              8  0
     dxxEfdt           s              7  0
     dxxExdt           s              7  0

      /free
       //THE LAST NAME CANNOT BE BLANK
       If scOOLName = *Blanks And scOOss# = *Blanks;
         isError = *On;
         erOOLname = *On;
         mgOOLname = 'WC30015';
       Endif;

       //DATE OF BIRTH MUST BE VALID, IF ENTERED
       If scOODob <> 0;
         xxDate = dtCheck(scOODob);
         If xxDate = 0;
           isError = *On;
           erOODob = *On;
           mgOODob = 'WCR0500';
         Else;
           scOODob = xxDate;
         Endif;
       Endif;

       //CORPORATION FLAG MUST BE Y/N IF NOT BLANK.
       //
        //N90SCCPFL        IFNE      'Y'
        //   SCCPFL        ANDNE     'N'
        //   SCCPFL        ANDNE     ' '
        //               SETON                                        81  90
        //               ENDIF

       //COVERAGE FLAG MUST BE I, E, OR BLANK
       If scOOLname <> *Blanks And scOOCov <> 'I' And scOOCov <> 'E'
        And scOOCov <> *Blanks;
         isError = *On;
         erOOCov = *On;
         mgOOCov = 'WCR0589';
       Endif;

       //IF EMPLOYER TYPE OF BUSINESS = 1 (SOLE PROPRIETORSHIP) OR
       //2 (PARTNERSHIP) FLAG MUST BE 'I' OR 'E'
       If (EMtpob = '1' Or EMtpob = '2') And scOOCov = *Blanks;
         isError = *On;
         erOOCov = *On;
         mgOOCov = 'WC30010';
       Endif;

       //IF FLAG IS 'I' OR 'E', OR A CHANGE OF INC/EXC, A VALID
       //EFFECTIVE DATE IS REQUIRED
       If scOOCov = 'I' Or scOOCov = 'E';
         xxDate = dtCheck(scOOEfdt);
         If xxDate = 0;
           isError = *On;
           erOOEfdt = *On;
           mgOOEfdt = 'WCR0500';
         Else;
           scOOEfdt = xxDate;
         EndIf;
       EndIf;

       //IF FLAG IS BLANK, THEN AN EFFECTIVE DATE MUST NOT BE ENTERED,
       //IN ADD MODE
       If scOOCov = *Blanks And scOOEfdt <> 0 And inMode = 'A';
           isError = *On;
           erOOCov = *On;
           mgOOCov = 'WCR0585';
       Endif;

       //EXPIRATION DATE MUST BE VALID, IF ENTERED
       If scOOExdt <> 0;
         xxDate = dtCheck(scOOExdt);
         If xxDate = 0;
           isError = *On;
           erOOExdt = *On;
           mgOOExdt = 'WCR0500';
         Else;
           scOOExdt = xxDate;
         EndIf;
       //EXPIRATION DATE IS REQUIRED IF THE MODE IS 'I'
         ElseIf inMode = 'I';
            isError = *On;
            erOOExdt = *On;
            mgOOExdt = 'WCR0503';
       EndIf;
       xxEfdt = dtCymd(scOOEfdt);
       xxExdt = dtCymd(scOOExdt);
       //ONLY MIDTERM EXPIRATION DATE IS ACCEPTABLE IF THE INMODE IS 'I', FLAT-
       //CANCEL DATE IS  NOT ALLOWED IN THIS MODE.
       If scOOExdt <> 0 and inMode = 'I';
          If scOOExdt = scOOEfdt;
             isError = *On;
             erOOExdt = *On;
             mgOOExdt = 'W400074';
          Endif;
       Endif;

       //EFFECTIVE DATE MUST BE GREATER THAN FROM EFFECTIVE DATE, OPT10
       If erOOEfdt <> *on and xxEfdt < OOefdt and
          (inMode = 'I' or inMode = 'E');
         isError = *On;
         erOOEfdt = *On;
         mgOOEfdt = 'WC30016';
       Endif;

       //DATES MUST BE IN ASCENDING ORDER
       If erOOExdt <> *on and xxExdt < xxEfdt And xxExdt > 0;
         isError = *On;
         erOOExdt = *On;
         mgOOExdt = 'WCR0529';
       Endif;

       //EXPIRATION DATE CANNOT BE LESS THAT POLICY INITIAL EFFECTIVE
       //DATE
       If erOOExdt <> *on and xxExdt < EMefd1 And xxExdt > 0;
         isError = *On;
         erOOExdt = *On;
         mgOOExdt = 'WC30017';
       Endif;

       //IF ENTERED, PAYROLL CLASS CODE MUST BE VALID
       If scOOClsCd <> 0;
         If ELrtcd <> *Blanks;
           Chain (scCo#:ELrtcd:scOOClsCd) ppr01;
           If not %found(wmprl1);
             isError = *On;
             erOOClsCd = *On;
             mgOOClsCd = 'WCR0539';
           Endif;
         Else;
           Chain (scCo#:scOOClsCd) ppr00;
           If not %found(wmprl);
             isError = *On;
             erOOClsCd = *On;
             mgOOClsCd = 'WCR0539';
           Endif;
         EndIf;
       Endif;

       //Owner/Officer title is required if flagged as required at Endorsement s
       If scOOttl = *Blanks;
          Setgt (EMCo#:EMFnd:EMEmp#:EMDiv:dtCYMD(scOOEfdt)) Pel02;
          Readpe(n) (EMCo#:EMFnd:EMEmp#:EMDiv) Pel02;
          If not %Eof(wdell2);
             Chain (ELco#:ELrtcd) Prs00;
             Setgt (ELCo#:ELFnd:RSjrst:dtCYMD(scOOEfdt)) Pnx00;
             Readpe (ELCo#:ELFnd:RSjrst) Pnx00;
             If not %Eof(wmnxl);
                If NXOfft = 'Y';
                   isError = *On;
                   erOOTtl = *On;
                   mgOOTtl = 'WCR0503';
                Endif;
             Endif;
          Endif;
       Endif;

        // CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
        Chain(n) inOwnRrn poo0p;
        If OOdt > saveOOdt Or (OOdt = saveOOdt AND OOtime > saveOOtm);
          isError = *On;
          erOwnErr = *On;
          mgOwnErr = 'WC30000';
        Endif;
       Return not isError;
      /end-free
     p ValidOwnOff     e
       //===================================================================
       // Display Owner/Officer popup grid
       //===================================================================
     p PrcOwnOffGrd    b                   Export
     d PrcOwnOffGrd    pi            10
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLocRrn                      9  0 options(*nopass) const
     d*
     d outCount        s              5  0 inz
     d xxCnt           s              5  0 inz
     d xxCall          s              3    inz
     d xxMode          s              1    inz
     d xxRtnVal        s             10    inz
     d doLoad          s               n   inz('1')
     d iOpen           s               n   inz
     d ExitNow         s               n   inz
     d isPrcLoc        s               n   inz
     d dsLocBillDiv  e ds                  extname(wpilocmst:rcdBillDiv) inz
     d ptrLocBillDiv   s               *   inz
     d outErr          s              1    inz
     d inELFL          s              1    inz
     d*
      /free
       If Not %Open(WpiLocMst);
         Open WpilocMst;
         iOpen = *On;
          Clear dsLocBillDiv;
          Clear dsInitData;
          ptrLocBillDiv = %Addr(dsLocBillDiv);
          ptrInitData = %Addr(dsInitData);
          InitBillDiv(inRrn:xxMode:ptrLocBillDiv:ptrInitData);
       Endif;
       GetEmployer(inRrn);
       scDivNm = %Char(EMemp#) + ' ' + %Char(EMdiv) + ' '
        + EMname + EMnam2;

       visEntNm = *Off;
       If %Parms > 3;
         If inLocRrn > 0;
           Chain(n) inLocRrn pal0p;
           scEntLoc = ALelid + ' ' + ALname + ALnam2;
           visEntNm = *On;
         Endif;
         isPrcLoc = *On;
       Endif;

       xxCall = inCall;
       If xxCall = 'LOC';
         xxCall = 'LC1';
       Endif;
       scPnlTitle = 'Owner/Officer Information';
       Select;
       When inCall = 'LOC' Or inCall = 'QTE' Or inCall = 'MTE';
         scPnlTitle = 'Owner/Officer Maintenance';
         SCe160 = *On;
         SCe161 = *On;
         SCe162 = *On;
       When inCall = '160';
         scPnlTitle = 'Mid-Term Endorsement - Addition of Owner/Officer';
         SCe160 = *On;
         SCe161 = *Off;
         SCe162 = *Off;
       When inCall = '161';
         scPnlTitle = 'Mid-Term Endorsement - Change of Owner/Officer';
         SCe160 = *Off;
         SCe161 = *On;
         SCe162 = *Off;
       When inCall = '162';
         scPnlTitle = 'Mid-Term Endorsement - Deletion of Owner/Officer';
         SCe160 = *Off;
         SCe161 = *Off;
         SCe162 = *On;
       EndSl;

       DoU ExitNow = *On;
         If doLoad = *On;
           Exsr ClearSflOwnOff;
           If isPrcLoc;
             ptrMdsOwnOff = BldOwnOffGrd(inRrn:outCount:inLocRrn);
           Else;
             ptrMdsOwnOff = BldOwnOffGrd(inRrn:outCount);
           Endif;
           If outCount > 0;
             Exsr LoadSflOwnOff;
           Endif;
         Endif;
         ExFmt rcdOOgrid;
         Select;
         When btnOadd = *On;
           btnOadd = *Off;
           If isPrcLoc;
             PrcOwnOffWdw(inRrn:xxCall:'A':0:inLocRrn);
           Else;
             PrcOwnOffWdw(inRrn:xxCall:'A');
           Endif;
           doLoad = *On;
         When btnOCancel = *On;
           btnOCancel = *Off;
           xxRtnVal = 'CANCEL';
           ExitNow = *On;
         When btnONext = *On;
           btnONext = *Off;
           ExitNow = *Off;
           xxRtnVal = ' ';
           outErr = ' ';
           inELFL = ' ';
           OwnOffShare(inRrn:inELFL:outErr);
       // Loop through the Entities for the division being processed
           Setll (EMco#:EMfnd:EMemp#:EMdiv) pal00;
           Reade (EMco#:EMfnd:EMemp#:EMdiv) pal00;
           Dow not(%eof) and outErr <> 'L';
              If A0eltp = 'E';
                 inELFL = 'E';
                 WMALPrrn = a0rrn;
                 OwnOffShare(WMALPrrn:inELFL:outErr);
              Endif;
           Reade (EMco#:EMfnd:EMemp#:EMdiv) pal00;
           Enddo;
           If outErr <> 'L';
             xxRtnVal = 'SAVE_NEXT';
             ExitNow = *On;
           Endif;
         Other;
           //Still here so no action on first subfile check second
           Readc SflOwnOff;
           //Process only first click

           isFound = *off;

           Dow not %eof;
             Select;
             When lnkOEdit = *On;
               lnkOEdit = *Off;
               xxMode = 'C';
               isFound = *on;
             When lnkOExpire = *On;
               lnkOExpire = *Off;
               xxMode = 'D';
               isFound = *on;
             When lnkOIE = *On;
               lnkOIE = *Off;
               xxMode = 'I';
               isFound = *on;
             EndSl;
             If isFound = *on ;
                Leave;
             Endif;

             Readc SflOwnOff;
           Enddo;

           If isFound = *on;
             If isPrcLoc;
               PrcOwnOffWdw(inRrn:xxCall:xxMode:scOOrrn:inLocRrn);
             Else;
               PrcOwnOffWdw(inRrn:xxCall:xxMode:scOOrrn);
             EndIf;
             doLoad = *On;
           EndIf;
         EndSl;
       EndDo;
       If %Open(WpiLocMst) And iOpen;
         Close WpiLocMst;
       Endif;
       Return xxRtnVal;
       //***********************************************************************
       // ClearSflOwnOff
       //***********************************************************************
       BegSr ClearSflOwnOff;
         dspOwnOff = *Off;
         clrOwnOff = *On;
         Write rcdOOgrid;
         clrOwnOff = *Off;
         dspOwnOff = *On;
         RrnOwnOff = 0;
       EndSr;
       //***********************************************************************
       // LoadSflOwnOff;
       //***********************************************************************
       BegSr LoadSflOwnOff;
         For xxCnt = 1 to outCount;
           %Occur(mdsOwnOff) = xxCnt;
           dsOwnOff = fldsOO;
           RrnOwnOff += 1;
           Write SflOwnOff;
         EndFor;
       EndSr;
      /end-free
     p PrcOwnOffGrd    e
       //===================================================================
       // Display Location Year popup grid                                  xes.
       //===================================================================
     p PrcLocYrGrd     b                   Export
     d PrcLocYrGrd     pi            10
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLocRrn                      9  0
     d*
     d outCount        s              5  0 inz
     d xxCnt           s              5  0 inz
     d xxCall          s              3    inz
     d xxRtnVal        s             10    inz
     d doLoad          s               n   inz('1')
     d iOpen           s               n   inz
     d ExitNow         s               n   inz
     d*
      /free
       If Not %Open(WpiLocMst);
         Open WpilocMst;
         iOpen = *On;
       Endif;
       GetEmployer(inRrn);
       scDivNm = %Char(EMemp#) + ' ' + %Char(EMdiv) + ' '
        + EMname + EMnam2;
       Chain(n) inLocRrn pal0p;
       scEntLoc = ALelid + ' ' + ALname + ALnam2;
       scPnlTitle = 'Location/Entity Years Maintenance';
       xxCall = 'WDW';

       DoU ExitNow = *On;
         If doLoad = *On;
           Exsr ClearSflLocYr;
           ptrMdsLocYr = BldLocYrGrd(inLocRrn:outCount);
           If outCount > 0;
             Exsr LoadSflLocYr;
           Endif;
         Endif;
         ExFmt rcdLYgrid;
         Select;
         When btnLYadd = *On;
           PrcLocYrWdw(inRrn:xxCall:'A':0:inLocRrn);
           doLoad = *On;
         When btnLYAloc = *On;
           xxRtnVal = 'ADD_LOCENT';
           ExitNow = *On;
         When btnLYFin  = *On;
           xxRtnVal = 'FINISH';
           ExitNow = *On;
         Other;
           //Still here so no action on first subfile check second
           Readc SflLocYr;
           //Process only first click
           If not %Eof;
             Select;
             When lnkLEdit = *On;
               PrcLocYrWdw(inRrn:xxCall:'C':scLrrn:inLocRrn);
             When lnkLDelete = *On;
               PrcLocYrWdw(inRrn:xxCall:'D':scLrrn:inLocRrn);
             EndSl;
             doLoad = *On;
           EndIf;
         EndSl;
       EndDo;
       If %Open(WpiLocMst) And iOpen;
         Close WpiLocMst;
       Endif;
       Return xxRtnVal;
       //***********************************************************************
       // Clear Location year subfile
       //***********************************************************************
        BegSr ClearSflLocYr;
         clrLocYr = *On;
         dspLocYr = *Off;
         Write rcdLYgrid;
         clrLocYr = *Off;
         dspLocYr = *On;
         RrnLocYr = 0;
        EndSr;
       //***********************************************************************
       // Load  Location year subfile
       //***********************************************************************
        BegSr LoadSflLocYr;
         For xxCnt = 1 to outCount;
           %Occur(mdsLocYr) = xxCnt;
           dsLocYr = fldsLY;
           RrnLocYr += 1;
           Write SflLocYr;
         EndFor;
        EndSr;
      /end-free
     p PrcLocYrGrd     e
       //===================================================================
       // Add/Edit Location Year popup window                               xes.
       //===================================================================
     p PrcLocYrWdw     b                   Export
     d PrcLocYrWdw     pi
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inLDRrn                       9  0 options(*nopass) const
     d  inLocRrn                      9  0 options(*nopass) const
     d*
     d  xxLDRrn        s              9  0 inz
     d  xxLocRrn       s              9  0 inz
     d  xxDelResp      s               n   inz
     d  ExitNow        s               n   inz
     d  xxEltp         s              1    inz
     d iOpen           s               n   inz
      /free
       If %parms > 3;
         xxLDRrn = inLDRrn;
       Endif;
       If %parms > 4;
         xxLocRrn = inLocRrn;
         If xxLocRrn = 0;
           PopOneBtn('Location should be added/selected to add Location year':
            'Error!');
           Return;
         Endif;
       Endif;

       If Not %Open(WpiLocMst);
         Open WpilocMst;
         iOpen = *On;
       Endif;
       GetEmployer(inRRN);
       scCo# = EMco#;
       scFnd = EMfnd;
       scEmp# = EMemp#;
       scDiv = EMdiv;
       scxDiv= EMdiv;
       fyrChoices = 'ELCO# = ' + %char(scCo#) + ' AND ELFND = '
        + %char(scFnd) + ' AND ELEMP# = ' + %char(scEmp#) +
        ' AND ELDIV = ' + %char(scDiv);
       If xxLocRrn >0;
         Chain(n) xxLocRrn pal0p;
       Endif;

       saveLDdt =  Dateto7(systemDate());
       saveLDtm = Timeto6(SystemTime());

       ExSr ClearErrors;
       scLYPnlTtl = 'Location Year';
       Select;
       When inMode = 'A';
         Clear pld0p;
         visLYSNxt = *On;
         leftLYCan = 465;
         scLYPnlTtl = %Trim(scLYPnlTtl) + ' Add';
       When inMode = 'C' Or inMode = 'D';
         If xxLDRrn > 0;
           Chain(n) xxLDRrn pld0p;
         Else;
           Return;
         Endif;
         visLYSNxt = *Off;
         leftLYCan = 575;
         Select;
         When inMode = 'C';
           scLYPnlTtl = %Trim(scLYPnlTtl) + ' Edit';
         When inMode = 'D';
           scLYPnlTtl = %Trim(scLYPnlTtl) + ' Delete';
         EndSl;
       EndSl;
       If inCall = 'LOC';
         wTopLocYr = 000;
       Else;
         wTopLocYr = -390;
       Endif;
       Exsr LoadFilToScn;

       DoU ExitNow = *On;
         Select;
         When inMode = 'D';
           Write rcdLocYr;
           xxDelresp = PopTwoBtn('Do you want to delete this Location year?':
            'Confirm Delete':'Yes':'No':0:600);
           Select;
           When xxDelResp = *On;
             Exsr WrtUpdLocYr;
             PopOneBtn('Location Year deleted!': 'Delete');
             ExitNow = *On;
           When xxDelResp = *Off;
             ExitNow = *On;
           EndSl;
         Other;
           ExFmt rcdLocYr;
           Select;
           When btnLYCan = *On;
             ExitNow = *On;
           When btnLYSave = *On;
             ExSr ProcessSave;
           When btnLYSNxt = *On;
             ExSr ProcessSave;
             If ExitNow = *On;
               ExitNow = *Off;
               clear pld0p;
               Exsr LoadFilToScn;
             Endif;
           EndSl;
         EndSl;
       EndDo;
       If %Open(WpiLocMst) And iOpen;
         Close WpiLocMst;
       Endif;
       Return;
       //********************************************************
       //*   Process Save
       //********************************************************
       BegSr ProcessSave;
         ExSr ClearErrors;
         If ValidLocYr(inMode :xxLDRrn);
           If inMode <> 'A';
             Chain xxLDRrn pld0p;
           Endif;
           Exsr LoadScnToFil;
           Exsr WrtUpdLocYr;
           ExitNow =*On;
         EndIf;
       EndSr;
       //********************************************************
       //*   Clear error indicators
       //********************************************************
       BegSr ClearErrors;
         erLYErr = *Off;
         erLYelid = *Off;
         erLYefdt = *Off;
         erLYfyr = *Off;
         erLYempy = *Off;
         erLYtpyr = *Off;
         erLYtypb = *Off;
         erLYpeak = *Off;
         erLYyear = *Off;
       EndSr;
       //********************************************************
       //*   Load File fields to screen
       //********************************************************
       BegSr LoadFilToScn;
         If inMode = 'A';
           scLYelid = ALelid;
           xxEltp = ALeltp;
         Else;
           scLYelid = LDelid;
           xxEltp = LDeltp;
         EndIf;
         Select;
         When xxEltp = 'E';
           scLYtype = 'Entity';
         When xxEltp = 'L';
           scLYtype = 'Location';
         Other;
           scLYtype = *Blanks;
         EndSl;
         scLYefdt = dtmdyy(ALefdt);

         scLYfyr = LDfyr;
         scLYempy = LDempy;
         scLYtpyr = LDtpyr;
         scLYtypb = LDtypb;
         scLBldgYr = LDbuildYr;
         scL#Storie = LD#Stories;
         scLPeak#Em = LDpeakEmp;
       EndSr;
       //********************************************************
       //*   Load Screen Fields to File
       //********************************************************
       BegSr LoadScnToFil;
          LDeltp = xxEltp;
          LDelid = scLYelid;
          LDfyr = scLYfyr;
          LDempy = scLYempy;
          LDtypb = scLYtypb;
          LDtpyr = scLYtpyr;
          LDbuildYr = scLBldgYr;
          LD#Stories = scL#Storie;
          LDpeakEmp = scLPeak#Em;
       EndSr;
       //********************************************************
       //*   Write/Update Location Year
       //********************************************************
       BegSr WrtUpdLocYr;

         LDpgm  = Q1pgm;
         LDuser = Q1user;
         LDtime = Timeto6(SystemTime());
         LDdt   = Dateto7(SystemDate());
         Select;
         When inMode = 'A';
           LDco# = scCo#;
           LDFnd = scFnd;
           LDEmp# = scEmp#;
           LDDiv = scDiv;

           LDipgm  = Q1pgm;
           LDiusr = Q1user;
           LDitm = Timeto6(SystemTime());
           LDidt   = Dateto7(SystemDate());
           Write pld0p;
           Feod wdldp;
         When inMode = 'C';
           Update pld0p;
         When inMode = 'D';
           Chain xxLDRrn pld0p;
           Delete pld0p;
         EndSl;
       EndSr;
      /end-free
     p PrcLocYrWdw     e
       //===================================================================
       // Validate Location/Year Window
       //===================================================================
     p ValidLocYr      b
     d ValidLocYr      pi              n
     d  inMode                        1    const
     d  inLDRrn                       9  0

     disError          s               n   inz

      /free
       //Validate policy year
       If scLYfyr <= 0;
         isError = *On;
         erLYfyr = *On;
         mgLYfyr = 'WCR0502';
       Else;
       //Entered policy year must exist for employer.
         If inMode = 'A';
           Chain(n) (scCo#:scFnd:scEmp#:scDiv:scLYfyr) pel00;
           If not %found(wdell);
             isError = *On;
             erLYfyr = *On;
             mgLYfyr = 'WC31443';
           Endif;
         Endif;
       Endif;

       //Entered type of building code must NOT exist for ADD, and
       //must exist on CHANGE if entered.
         If scLYtypb <> *Blanks;
           Chain(n) (scCo#:scLYtypb) ptb00;
           If not %found(wttbl);
             isError = *On;
             erLYtypb = *On;
             mgLYtypb = 'WCR0505';
           Endif;
         Endif;

       //Validate number of employers
        If scLYempy <= 0;
          isError = *On;
          erLYempy = *On;
          mgLYempy = 'WCR0502';
        Endif;

       //If in add mode and a record already exists display error
        If inMode = 'A';
          Chain(n) (scCo#:scFnd:scEmp#:scDiv:scLYelid:scLYfyr) pld00;
          If %found(wdldl);
            isError = *On;
            erLYfyr = *On;
            mgLYfyr = 'WC30005';
          Endif;
        Endif;

       //Validate - Total Payroll
         If scLYtpyr < 0;
           isError = *On;
           erLYfyr = *On;
           mgLYfyr = 'WCR0502';
         Endif;
        If scLBldgYr <> 0 and (scLBldgYr < 100 or scLBldgYr > 9999);
           isError = *On;
           erLYyear = *On;
        Endif;

        If scLPeak#Em > scLYempy;
           isError = *On;
           erLYpeak = *On;
        Endif;
        Chain(n) inLDrrn pld0p;
        If LDdt > saveLDdt Or (LDdt = saveLDdt AND LDtime > saveLDtm);
          isError = *On;
          erLYErr = *On;
          mgLYErr = 'WC30000';
        Endif;
       Return not isError;
      /end-free
     p ValidLocYr      e
       //===================================================================
       // Build Location/Year Grid data into MODS
       //===================================================================
     p BldLocYrGrd     b                   export
     d BldLocYrGrd     pi              *
     d  inLocRrn                      9  0
     d  outCount                      5  0

     d rrnOccur        s              5  0 inz
      /free
        outCount = 0;
        %Occur(mdsLocYr) = 1;
        clear mdsLocYr;

        ptrMdsLocYr = %Addr(mdsLocYr);
        Chain(n) inLocRrn pal0p;
        //*** Beg Chg *** 03/10/25 ****************************************
        //***Setll (ALco#:ALfnd:ALemp#:ALdiv:ALelid) pld00;
        Setgt (ALco#:ALfnd:ALemp#:ALdiv:ALelid) pld00;
        //*** End Chg *** 03/10/25 ****************************************
         DoU %Eof(wdldl);
           //*** Beg Chg *** 03/10/25 *************************************
           //***Reade (ALco#:ALfnd:ALemp#:ALdiv:ALelid) pld00;
           Readpe (ALco#:ALfnd:ALemp#:ALdiv:ALelid) pld00;
           //*** End Chg *** 03/10/25 *************************************
           If %Eof(wdldl);
             Leave;
           EndIf;
           rrnOccur += 1;
           %Occur(mdsLocYr) = rrnOccur;
           scLrrn = LDrrn;
           scLyear = LDfyr;
           scL#emp = LDempy;
           scLPayroll = LDtpyr;
           Chain (scCo#:LDtypb) ptb00;
           If %found(wttbl);
             scLTypBldg = TBdesc;
           Else;
             scLTypBldg = *blanks;
           EndIf;
           scLBldgYr = LDbuildYr;
           scL#Storie = LD#Stories;
           scLPeak#Em = LDpeakEmp;
           fldsLY = dsLocYr;
         EndDo;
         outCount = rrnOccur;
         Return ptrMdsLocYr;
      /end-free
     p BldLocYrGrd     e
       //===================================================================
       // Build Owner/Officer Grid data into MODS
       //===================================================================
     p BldOwnOffGrd    b                   export
     d BldOwnOffGrd    pi              *
     d  inRrn                         9  0
     d  outCount                      5  0
     d  inLocRrn                      9  0 options(*nopass) const

     d rrnOccur        s              5  0 inz
     d doSubset        s               n   inz
     d xxElid          s                   inz like(ALelid)
      /free
        GetEmployer(inRrn);
        outCount = 0;
        If %parms > 2;
          If inLocRrn > 0;
            Chain(n) inLocRrn pal0p;
            xxElid = ALelid;
          Endif;
          doSubSet = *On;
        Else;
          doSubSet = *Off;
        Endif;
        %Occur(mdsOwnOff) = 1;
        clear mdsOwnOff;
        ptrMdsOwnOff = %Addr(mdsOwnOff);

         Setll (EMco#:EMfnd:EMemp#:EMdiv) poo00;
         DoU %Eof(wmool);
           Reade(N) (EMco#:EMfnd:EMemp#:EMdiv) poo00;
           If %Eof(wmool);
             Leave;
           EndIf;
           If doSubSet And OOelid <> xxElid;
             Iter;
           Endif;
           rrnOccur += 1;
           %Occur(mdsOwnOff) = rrnOccur;
           If rrnOccur = 1;
             scOSelect = *On;
           Else;
             scOSelect = *Off;
           EndIf;
           scOorrn = OOrrn;
           scOLname = OOolsn;
           scOFname = OOofsn;
           scOpercent = OOofpc;
           scOTitle = OOtitl;
           Select;
           When OOexfl = 'I';
             scOCov = 'Include';
           When OOexfl = 'E';
             scOCov = 'Exclude';
           Other;
             scOCov = *blanks;
           EndSl;
           scOEffdt = CvtToDate(OOefdt);
           scOExpdt = CvtToDate(OOexdt);
           scOClass = OOclas;
           scODiv = OOdiv;
           scOEnt# = OOelid;
           PRdesc = *Blanks;
           If OOclas <> 0;
             If ELrtcd <> *Blanks;
               Chain(n) (OOco#:ELrtcd:OOclas) ppr01;
             Else;
               Chain(n) (OOco#:OOclas) ppr00;
             Endif;
           Endif;
           scOClDsc = PRdesc;
           fldsOO = dsOwnOff;
         EndDo;
         outCount = rrnOccur;
         Return ptrMdsOwnOff;
      /end-free
     p BldOwnOffGrd    e
       //===================================================================
       // Build Department Grid data into MODS
       //===================================================================
     p BldDeptGrd      b                   export
     d BldDeptGrd      pi              *
     d  inRrn                         9  0
     d  outCount                      5  0

     d rrnOccur        s              5  0 inz
     d doSubset        s               n   inz
      /free
        GetEmployer(inRrn);
        outCount = 0;
        %Occur(mdsDept) = 1;
        clear mdsDept;
        ptrMdsDept = %Addr(mdsDept);

         Setll (EMco#:EMfnd:EMemp#:EMdiv) pdp00;
         DoU %Eof(wmdpl);
           Reade (EMco#:EMfnd:EMemp#:EMdiv) pdp00;
           If %Eof(wmdpl);
             Leave;
           EndIf;
           rrnOccur += 1;
           %Occur(mdsDept) = rrnOccur;
           scsDPrrn = DPrrn;
           scsDeptNm = DPdesc;
           scsDept = DPdept;
           If DPstat = 'A';
             scsStatus = 'Active';
           Else;
             scsStatus = 'Inactive';
           Endif;
           scsDDiv = DPdiv;
           fldsDP = dsDept;
         EndDo;
         outCount = rrnOccur;
         Return ptrMdsDept;
      /end-free
     p BldDeptGrd      e
       //===================================================================
       // Add/Edit Department popup window
       //===================================================================
     p PrcDeptWdw      b                   Export
     d PrcDeptWdw      pi
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inDPRrn                       9  0 options(*nopass) const
     d*
     d  ExitNow        s               n   inz
     d iOpen           s               n   inz
     d xxDPrrn         s              9  0 inz
      /free
       If Not %Open(WpiLocMst);
         Open WpilocMst;
         iOpen = *On;
       Endif;
       If %Parms > 3;
         xxDPrrn = inDPrrn;
       Endif;
       GetEmployer(inRRN);
       scCo# = EMco#;
       scFnd = EMfnd;
       scEmp# = EMemp#;
       scDiv = EMdiv;
       scDPDivNm = %Char(scEmp#) + ' ' + %Char(scDiv)+ ' ' +EMname+EMnam2;

       saveDPdt =  Dateto7(systemDate());
       saveDPtm = Timeto6(SystemTime());

       ExSr ClearErrors;
       scDeptPnl = 'Department';
       Select;
       When inMode = 'A';
         Clear pdp0p;
         scDeptPnl = %Trim(scDeptPnl) + ' Add';
       When inMode = 'C';
         Chain(n) xxDPrrn pdp0p;
         scDeptPnl = %Trim(scDeptPnl) + ' Edit';
       EndSl;
       Exsr LoadFilToScn;

       DoU ExitNow = *On;
         ExFmt rcdDeptWdw;
         Select;
         When btnDptCanc = *On;
           ExitNow = *On;
         When btnDptSave = *On;
           ExSr ProcessSave;
         EndSl;
       EndDo;
       If %Open(WpiLocMst) And iOpen;
         Close WpiLocMst;
       Endif;
       Return;
       //********************************************************
       //*   Process Save
       //********************************************************
       BegSr ProcessSave;
         ExSr ClearErrors;
         If ValidDept(inMode:xxDPrrn);
           If inMode = 'C';
             Chain xxDPrrn pdp0p;
           Endif;
           Exsr LoadScnToFil;
           Exsr WrtUpdDept;
           ExitNow =*On;
         EndIf;
       EndSr;
       //********************************************************
       //*   Clear error indicators
       //********************************************************
       BegSr ClearErrors;
         erDept = *Off;
         erDeptNm = *Off;
       EndSr;
       //********************************************************
       //*   Load File fields to screen
       //********************************************************
       BegSr LoadFilToScn;
         scDept = DPdept;
         scDeptNm = DPdesc;
         scDPStatus = DPstat;
         If inMode = 'A';
           scDPStatus = 'A';
         Endif;
       EndSr;
       //********************************************************
       //*   Load Screen Fields to File
       //********************************************************
       BegSr LoadScnToFil;
         DPdept = scDept;
         DPdesc = scDeptNm;
         DPstat = scDPStatus;
       EndSr;
       //********************************************************
       //*   Write/Update Department
       //********************************************************
       BegSr WrtUpdDept;

         DPpgm  = Q1pgm;
         DPuser = Q1user;
         DPtime = Timeto6(SystemTime());
         DPdt   = Dateto7(SystemDate());
         Select;
         When inMode = 'A';
           DPco# = scCo#;
           DPFnd = scFnd;
           DPEmp# = scEmp#;
           DPDiv = scDiv;

           DPipgm  = Q1pgm;
           DPiusr = Q1user;
           DPitm = Timeto6(SystemTime());
           DPidt   = Dateto7(SystemDate());
           Write pdp0p;
           Feod wmdpp;
         When inMode = 'C';
           Update pdp0p;
         EndSl;
       EndSr;
      /end-free
     p PrcDeptWdw      e
       //===================================================================
       // Validate Department Window
       //===================================================================
     p ValidDept       b
     d ValidDept       pi              n
     d  inMode                        1    const
     d  inDPrrn                       9  0 const

     disError          s               n   inz

      /free
       //DEPARTMENT CANNOT BE BLANK
        If scDept = *Blanks;
          isError = *On;
          erDept = *On;
          mgDept = 'WCR0503';
        Endif;

       //CHECK TO BE SURE DEPARTMENT HAS NOT BEEN USED PREVIOUSLY
        If inMode = 'A';
          Chain(n) (scCo#:scFnd:scEmp#:scDiv:scDept) pdp00;
          If %Found(wmdpl);
            isError = *On;
            erDept = *On;
            mgDept = 'WC30005';
          Endif;
        Endif;
       //DESCRIPTION CANNOT BE BLANK
        If scDeptNm = *Blanks;
          isError = *On;
          erDeptNm = *On;
          mgDeptNm = 'WCR0503';
        Endif;

       //CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
        Chain(n) inDPrrn pdp0p;
        If DPdt > saveDPdt Or (DPdt = saveDPdt AND DPtime > saveDPtm);
          isError = *On;
          erDeptPnl = *On;
          mgDeptPnl = 'WC30000';
        Endif;
       Return not isError;
      /end-free
     p Validdept       e
       //===================================================================
       // Add/Edit Additional Named Insured
       //===================================================================
     p PrcAddNamWdw    b                   Export
     d PrcAddNamWdw    pi
     d  inRrn                         9  0
     d  inCall                        3    const
     d  inMode                        1    const
     d  inInsRrn                      9  0 options(*nopass) const

     d xxInsRrn        s              9  0 inz
     d xxInsSeq#       s              5  0 inz
     d ExitNow         s               n   inz
     d iOpen           s               n   inz
      //*** Beg Add ***** 03/13/23 *****************************
     d flgClrCxRsn     s               n   Inz(*Off)
     d flgCxRcdFnd     s               n   Inz(*Off)
     d xxRrn           s              9  0 Inz
     d xxForm          s             15a   Inz
     d xxend#          s                   Like(MUend#)
     d svCxFyr         s                   Like(ANcxFyr) Inz
      //*** End Add ***** 03/13/23 *****************************
      /free

       If %parms > 3;
          xxInsRrn = inInsRrn;
       Endif;

       If Not %Open(WpiLocMst);
         Open WpilocMst;
         iOpen = *On;
       Endif;

       GetEmployer(inRRN);
       scCo# = EMco#;
       scFnd = EMfnd;
       scEmp# = EMemp#;
       scDiv = EMdiv;
       scxDiv= EMdiv;
       fyrChoices = 'ELCO# = ' + %char(scCo#) + ' AND ELFND = '
        + %char(scFnd) + ' AND ELEMP# = ' + %char(scEmp#) +
        ' AND ELDIV = ' + %char(scDiv);
       If xxInsRrn > 0;
          Chain(n) xxInsRrn pan0p;
       Endif;

       saveIndt = Dateto7(systemDate());
       saveIntm = Timeto6(SystemTime());

       ExSr ClearErrors;

       If inMode = 'A';
         Clear pan0p;
       Endif;
       Exsr LoadFilToScn;

       DoU ExitNow = *On;
          ExFmt rcdInsNam;
          Select;
          When BtnInsCan = *On;
             ExitNow = *On;
          When BtnInsSave = *On;
             ExSr ProcessSave;
          EndSl;
       EndDo;
       If %Open(WpiLocMst) And iOpen;
          Close WpiLocMst;
       Endif;
       Return;
       //********************************************************
       //*   Process Save
       //********************************************************
       BegSr ProcessSave;
          ExSr ClearErrors;
          If ValidAddNam(inMode:xxInsRrn);
             xxInsSeq# = 0;
             Setll (scCo#:scFnd:scEmp#:scDiv) pan01;
             Reade (scCo#:scFnd:scEmp#:scDiv) pan01;
             Dow not %eof(wmanl);
                If ANseq# > xxInsSeq#;
                   xxInsSeq# = 0;
                   xxInsSeq# = ANseq#;
                Endif;
                Reade (scCo#:scFnd:scEmp#:scDiv) pan01;
             Enddo;
             xxInsSeq# = xxInsSeq# + 1;
             If inMode <> 'A';
                Chain xxInsRrn pan0p;
             Endif;
             Exsr LoadScnToFil;
             Exsr WrtUpdInsNm;
             ExitNow =*On;
          EndIf;
       EndSr;
       //********************************************************
       //*   Clear error indicators
       //********************************************************
       BegSr ClearErrors;
          erANINam1  = *Off;
          erANIFe#   = *Off;
          erANIEffDt = *Off;
          erANIExpDt = *Off;
          erInsNam = *Off;
       EndSr;
       //********************************************************
       //*   Load File fields to screen
       //********************************************************
       BegSr LoadFilToScn;
          If inMode = 'A';
      //**** Begin Add  ***  06/23/21  ****************************************
             OutElRrnTm = OutElRrn;
      //**** End Add  ***  06/23/21  ****************************************
             Clear rcdInsNam;
      //**** Begin Add  ***  06/23/21  ****************************************
             OutElRrn = OutElRrnTm;
      //**** End Add  ***  06/23/21  ****************************************
             dspInsExDt = *off;
          Else;
             scANINam = ANname + ANnam2;
             scANISts  = ANstat;
             scANIFe#  = ANfein;
             scANIEffDt = dtMDYY(ANefdt);
             scANIExpDt = dtMDYY(ANdldt);
             dspInsExDt = *on;
             scAnNaNam = ANnamp;
             scAnNaInNm= ANnami;
          EndIf;
          visOnPrNam = *off;
     **** Beg Add - 03/11/22  **************************************************
          //*** Chain EMco# ptm00;
          Chain (EMco# : 0) ptm00;
     **** End Add - 03/11/22  **************************************************
          If %found and TMapnd = 'Y';
             visOnPrNam = *on;
          Endif;
          visOnInNam = *off;
          Chain (EmCo#:EmFnd) PFN00;
          If %found and FNpeof <> ' ';
             visOnInNam = *on;
          Endif;

         scTypBusAi = ANtpOB;

         //Named Insured Type of Business only available to Master PEO policy.


         If clientID = '150' and (FNpeof  = 'M' or FNpeof = 'C') and
            scEmp# = 1 and scDiv = 0;
            scIs150 = *On;
            scTobSel = 'GTCO# = ' + %char(scCO#) +
            ' AND GTTBID = ''BUSI'' AND GTSTAT = ''A''';
         Else;
            scIs150 = *Off;
          Endif;

       EndSr;
       //********************************************************
       //*   Load Screen Fields to File
       //********************************************************
       BegSr LoadScnToFil;
          //*** Beg Add ***** 03/13/23 **************************
          If ANdldt <> 0 and scANIExpDt = 0;
             svCxFyr = ANcxFyr;
             ANcxRsn = *Blank;
             ANcxFyr = *Zeros;
             flgClrCxRsn = *On;
          Endif;
          //*** End Add ***** 03/13/23 **************************
          If inMode = 'A';
             ANseq# = xxInsSeq#;
          Endif;
          ANname = %Subst(scANINam:1:40);
          ANnam2 = %Subst(scANINam:41:60);
          ANstat = scANISts;
          ANfein = scANIFe#;
          ANefdt = dtCYMD(scANIEffDt);
         //*** begin add ***  10/16/25  ****************************************
         //* Populate or clear the deletion transaction date/time fields.
         If ANDldt = 0 and scANIExpDt <> 0;
            ANTrddt  = Dateto7(SystemDate());
            ANTrdtm = Timeto6(SystemTime());
         Endif;
         If scANIExpDt = 0 and ANTrddt <> 0;
            ANTrddt = 0;
            ANTrdtm = 0;
         Endif;
         //*** end add *****  10/16/25  ****************************************
          ANdldt = dtCYMD(scANIExpDt);
          ANnamp = scAnNaNam;
          ANnami = scAnNaInNm;
          ANtpOB = scTypBusAi;

       EndSr;
       //********************************************************
       //*   Write/Update Insured Named
       //********************************************************
       BegSr WrtUpdInsNm;

          ANpgm  = Q1pgm;
          ANuser = Q1user;
          ANtime = Timeto6(SystemTime());
          ANdt   = Dateto7(SystemDate());
          Select;
          When inMode = 'A';
             ANco#  = scCo#;
             ANFnd  = scFnd;
             ANEmp# = scEmp#;
             ANDiv  = scDiv;

             ANStat = 'A';
             ANipgm = Q1pgm;
             ANiusr = Q1user;
             ANidt  = Dateto7(SystemDate());
             Write pan0p;
             Feod wmanp;
          When inMode = 'C';
             Update pan0p;
          EndSl;
          //*** Beg Add ***** 03/13/23 **************************
          If flgClrCxRsn = *On;
             flgCxRcdFnd = *Off;
             Setll (scCo#:scFnd:scEmp#) pan01;
             Reade (scCo#:scFnd:scEmp#) pan01;
             Dow not %Eof(wmanl);
                If ANCxFyr = svCxFYR and ANcxRsn <> *Blank and ANdldt <> 0;
                   flgCxRcdFnd = *On;
                   Leave;
                Endif;
                Reade (scCo#:scFnd:scEmp#) pan01;
             Enddo;
             If flgCxRcdFnd = *Off;
                xxForm = 'WC480323  ';
                xxEnd# = 0;
                Chain (EMco#:EMfnd:svCxFYR:EMemp#:xxForm:xxEnd#) pmu05;
                If %Found(wmmul5);
                   Delete pmu05;
                Endif;
             Endif;
          Endif;
          //*** End Add ***** 03/13/23 **************************
       EndSr;
      /end-free
     p PrcAddNamWdw    e
       //===================================================================
       // Validate Additional Named Insured
       //===================================================================
     p ValidAddNam     b
     d ValidAddNam     pi              n
     d  inMode                        1    const
     d  inInsRrn                      9  0

     d isError         s               n   inz
     d xxDate          s              8  0
     d xxDate1         s              8  0

      /free

       // Name cannot be blank
       If scANINam = ' ';
         isError = *On;
         erANINam1 = *On;
         msANINam1 = 'WCR0503';
       Endif;

       //FEIN Id# must be entered
       If scANIFe# = *zeros;
          isError = *On;
          erANIFe# = *On;
          msANIFe# = 'WCR0503';
       Endif;

       //Validate Effective Date
       xxDate = dtCheck(scANIEffDt);
       If xxDate = 0;
          isError = *On;
          erANIEffDt = *On;
          msANIEffDt = 'WCR0500';
       Else;
          scANIEffDt = xxDate;
       Endif;
       // Effective date edit check against Inception date

       If xxDate <> 0;
         intIncpDat = 0;
         intDatCYMD = 0;

         intIncpDat = GetEmIncDt(scCo#:scFnd:scEmp#:scDiv);
         intDatCYMD = dtCYMD(xxDate);
           If intDatCYMD < intIncpDat;
              isError = *On;
              erANIEffDt = *On;
              msANIEffDt = 'WCR4809';
           EndIf;
       EndIf;

       //Validate Deletion Date
       If inMode <> 'A';
          If scANIExpDt > 0;
             xxDate1 = dtCheck(scANIExpDt);
             If xxDate1 = 0;
                isError = *On;
                erANIExpDt = *On;
                msANIExpDt = 'WCR0500';
             Endif;
       // Expiration date edit check against Cancellation date
          intDatCYMD = dtCYMD(xxDate1);

          If xxDate1 <> 0;
          intCanlDat = 0;

            intCanlDat = GetCxDate(scCo#:scFnd:scEmp#:scDiv);
            If intCanlDat <> 0;
                If intDatCYMD > intCanlDat;
                  isError = *On;
                  erANIExpDt = *On;
                  msANIExpDt = 'WCR4810';
                EndIf;
            EndIf;
          EndIf;

        // Expiration date edit check against Expiry date and
        // Future date.
          If erANIExpDt <> *On;                         //error-if
            Setgt (scCo#:scFnd:scEmp#:scDiv:*hival) pel00;
            Readpe(n) (scCo#:scFnd:scEmp#:scDiv) pel00;
            If %Found(wdell);                                //eq- wdell
               intToDate = ELptdt;
               intCurrDate =  Dateto7(systemDate());

            If intToDate > intCurrDate;
               If intDatCYMD > intToDate;
                  isError = *On;
                  erANIExpDt = *On;
                  msANIExpDt = 'WC30295';
               EndIf;
            Else;
               If intDatCYMD > intToDate;
                 isError = *On;
                 erANIExpDt = *On;
                 msANIExpDt = 'WCR4811';
               EndIf;
            EndIf;

          EndIf;                                          //eq- wdell

        EndIf;                                        //iserror-endif
             If isError = *off and dtCYMD(xxDate1) < dtCYMD(xxDate);
                isError = *On;
                erANIExpDt = *On;
                msANIExpDt = 'WCR1226';
             Else;
                scANIExpDt = xxDate1;
             Endif;
          Endif;
       Endif;

       //CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
       If inMode <> 'A';
          Chain(n) inInsRrn pan0p;
          If ANdt > saveIndt Or (ANdt = saveIndt AND ANtime > saveIntm);
            isError = *On;
            erInsNam = *On;
            mgInsNam = 'WC30000';
          Endif;
       EndIf;

       Return not isError;
      /end-free
     p ValidAddNam     e

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
       //*** Begin Add *** 05/11/21/ ****************************************
       //===================================================================
       // Get Default State Reporting Div 3rd Party info flag
       //===================================================================
       Dcl-Proc GetDftSrD;
        Dcl-Pi *N ind;
        End-Pi;
        Dcl-S isDftSrd ind;
        Dcl-S sysDate7 packed(7:0) inz;
        isDftSrd = *Off;
        sysDate7 = DateTo7(SystemDate);
        setll  (ELCo#:ELFnd:ELEmp#:stateRepDiv:P3PrType) pp300;
        If %equal(wmp3l);
           reade(n) (ELCo#:ELFnd:ELEmp#:stateRepDiv:P3PrType) pp300;
           Dow not %eof(wmp3l);
              If (flgQte = 'Q' ) or
                 (flgQte <> 'Q' and P3envflg = *blank);
                 If P3dftsrd = 'Y';
                    If P3efDt <=sysDate7 and                    //Active
                    (P3exdt = *Zeros or P3exdt > sysDate7);
                       If ELfyr <=P3sfyr;                       //Most recent Ac
                          isDftSrD = *On;
                       Endif;
                    Endif;
                 Endif;
              Endif;
              reade(n) (ELCo#:ELFnd:ELEmp#:stateRepDiv:P3PrType) pp300;
           Enddo;
        Endif;
        return isDftSrD;
       End-Proc;
       //===================================================================
       // Load EFT information data
       //===================================================================
       Dcl-Proc LoadEftInfoM;
        Dcl-Pi *N;
          inEnv  char(1) const;
          inMode char(1) const;
        End-Pi;
        Dcl-s strSkey  char(2) inz;
        Dcl-s strType  char(1) inz;
        Dcl-s strMode  char(1) inz;
        Dcl-s qteMode  ind inz(*off);
        Dcl-s sysDate7 packed(7:0) inz;
        Dcl-s savEditEft  ind ;
        Dcl-s savBankEft  ind ;
        Dcl-Ds KeyEft6 likerec(pef06:*key);       //Key Data structure
        Dcl-S strCo# char(3) inz;
        Dcl-S strFnd char(3) inz;
        Dcl-S strEftType Char(1) inz('E');
        Dcl-S strEftKey  Char(30);
        Dcl-S strEfDt Char(7);
        Dcl-S strPre Char(1);
        Dcl-S strRRN Char(9);
        Dcl-S strEobType Char(3);
        Dcl-S strCkTp Char(10);
        Dcl-S strSfyr char(3);
        Dcl-S chrEnv char(1);
        Dcl-S strDftSrd char(1);
        Dcl-S intStRptDiv like(EFdiv) inz;
        Dcl-S strSrFlg char(1);
        Dcl-S intEMrrn packed(9:0) inz;
        Dcl-S isEft ind inz(*Off);

        //Load incoming parms
        strEnv = inEnv;
        strMode = inMode;

        If strEnv = 'Q';
           qteMode = *On;
        Else;
           qteMode = *Off;
        Endif;

        If inMode = 'A' or inMode = 'M' or inMode = 'C';
           savEditEft = *On;
           savBankEft = *On;
           visAddEftM = *On;
        Else;
           visAddEftM = *Off;
           savEditEft  = *Off;
           savBankEft  = *Off;
        Endif;

        sysDate7 = DateTo7(SystemDate);
        mod3rdPay = IsModEnabled('Third Party Payment');

        strSkey = 'WC';
        Chain strSkey SWC01;
        If WCfl24 <> 'Y' And WCfl38 <> 'Y';
           NOEFT = *On;
           EFT = *Off;
           If mod3rdPay = *on;
              noEFT = *off;
           Endif;
         //*** Begin Add *** 05/11/21 ****************************
           visAddEftM = *Off;
         //*** End   Add *** 05/11/21 ****************************
        Endif;
        If WCfl24 = 'Y' Or WCfl38 = 'Y';
           NOEFT = *off;
           EFT =  *On;
           //*** Beging Change *** 05/11/21 **********************
           //buttonTop = 1485;
           //scPnlHght = 1495;
           //*** Beg Chg *** 10/05/21 ******************************************
           //***buttonTop = 1530;
           //***scPnlHght = 1660;
           //*** Beg Chg *** 12/22/21 ******************************************
           //***TptyHdTop  = 1285;
           //***TptyGrdTop = 1315;
           //***TptyMsTop  = 1290;
           TptyHdTop  = 1325;
           TptyGrdTop = 1355;
           TptyMsTop  = 1330;
           //*** End Chg *** 12/22/21 ******************************************
           If mod3rdPay <> *on and EFT = *on;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop  = 1245;
             //***scPnlHght  = 1220;
             buttonTop  = 1285;
             scPnlHght  = 1260;
             //*** End Chg *** 12/22/21 ****************************************
           Endif;
           If mod3rdPay = *on and EFT = *on;
             //*** Beg Chg  ***  09/30/21  *************************
             //*** buttonTop = 1480;
             //*** scPnlHght = 1450;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop = 1710;
             //***scPnlHght = 1720;
             buttonTop = 1750;
             scPnlHght = 1760;
             //*** End Chg *** 12/22/21 ****************************************
             //*** End Chg  ***  09/30/21  *************************
           Endif;
           If mod3rdPay = *on and EFT = *off;
              //*** Beg Chg *** 12/22/21 ***************************************
              //***TptyHdTop  = 1020;
              //***TptyGrdTop = 1050;
              //***TptyMsTop  = 1027;
              TptyHdTop  = 1060;
              TptyGrdTop = 1090;
              TptyMsTop  = 1067;
              //*** End Chg *** 12/22/21 ***************************************
             //*** Beg Chg  ***  09/30/21  *************************
             //*** buttonTop  = 1245;
             //*** scPnlHght  = 1220;
             //*** Beg Chg *** 12/22/21 ****************************************
             //***buttonTop = 1710;
             //***scPnlHght = 1720;
             buttonTop = 1750;
             scPnlHght = 1760;
             //*** End Chg *** 12/22/21 ****************************************
             //*** End Chg  ***  09/30/21  *************************
           Endif;
           //*** End Chg *** 10/05/21 ******************************************
           //*** End    Change *** 05/11/21 **********************
        Endif;

        If EFT = *On;
           Exsr LoadEftSfl;
        Endif;
       //-----------------------------------------------------------------------
       // LoadEftSfl - Load Subfile for EFT information
       //-----------------------------------------------------------------------
       BegSr LoadEftSfl;

       KeyEft6.EFco#  = ELco#;
       KeyEft6.EFfnd  = ELfnd;
       KeyEft6.EFemp# = ELemp#;
       If strMode = 'A';
          KeyEft6.EFdiv =  intAddEftDiv;
       Else;
       KeyEft6.EFdiv =  scDiv;
       Endif;
       Setll %kds(KeyEft6) pef06;
       //*** begin chg ***  04/11/23  **************************************
       //*****Reade %kds(KeyEft6) pef06;
       Reade(n) %kds(KeyEft6) pef06;
       //*** end chg *****  04/11/23  **************************************

       Dow not %eof(WMEFL6);
          If (qteMode = *On) or
             (qteMode <> *On and EFenvflg = *blank);
             scLocDiv#  = EFdiv;
             scLocEffDt = CvtToDate(EFefdt);
             scLocExpDt = CvtToDate(EFexdt);
             scLocPNtDt = CvtToDate(EFpdt);
             scLocAppDt = CvtToDate(EFadt);
             scLocFTrDt = CvtToDate(EFftdt);
             scLocLTrDt = CvtToDate(EFltdt);
             scLocDfInf = EFdftSrD;
             scSfyr   = EFsfyr;
             scEnvFlg = EFenvflg;
             strType = 'E';
             visPreNote = RequiresPreNote(ELco#:ELfnd:strType:sysDate7);
             visEditEft = savEditEft;
             visBankEft = savBankEft;

             If qteMode = *On and EFenvflg = *blank;   //Don't allow edit if in
                visEditEft = *Off;
                visBankEft = *Off;
                visPreNote = *Off;
             Endif;

             If EFpdt = 0;
                scPreNote = 'Req Pre-note';
             Else;
                scPreNote = 'Clr Pre-note';
             Endif;

             scLocPName = EFpnam;
             rrnEFTInfM = rrnEFTInfM + 1;

             scLocRrnM = ef6rrn;

             Write SflEftInfM;
          Endif;
       //*** begin chg ***  04/11/23  **************************************
       //*****   Reade %kds(KeyEft6) pef06;
          Reade(n) %kds(KeyEft6) pef06;
       //*** end chg *****  04/11/23  **************************************
       Enddo;
       rrnEFTInfM = 1;

       intEMrrn = *Zero;
       strSrFlg = *blank;
       intStRptDiv = GetStRptDv(ELco#:ELfnd:ELemp#:intEMrrn:strSrFlg);

       If ELdiv <> intStRptDiv or                      //NSRD
         (ELdiv = intStRptDiv and strSrFlg = *Blank);
          strco#    = %editc(ELco#:'X');
          strEftType = 'E';
          strFnd     = %editc(ELfnd:'X');
          strEftkey  = %Editc(ELemp#:'X') + %Editc(ELdiv:'X');
          sysDate7 = DateTo7(SystemDate);
          strEfDt    = %editc(sysDate7:'X');
          strPre     = *blanks;
          strRrn     = *zeros;
          strEobType = *blanks;
          strCkTp  = *blanks;
          strSfyr    = %Editc(ELfyr:'X');
          chrEnv     = strEnv;
          strDftSrD  = *blanks;

          isEft = GetEftStatus(strCo#:strEftType:strFnd:strEftKey:strEfDt:
           strPre:strRrn:strEobType:strCkTp:strSFyr:chrEnv:strDftSrD);

          If isEft and strDftSrD = 'Y';
             visDftMsg = *On;
          Else;
             visDftMsg = *Off;
          Endif;
       Else;                                                  //SRD
          visDftMsg = *Off;
       Endif;
       Endsr;
       End-Proc;
       //===================================================================
       // Process EFT information data
       //===================================================================
       Dcl-Proc ProcessEftInfoM;
        Dcl-Pi *N;
         inMode char(1);
        End-Pi;
        Dcl-s DtZeroIso date(*iso) inz;
       //*** begin del ***  09/30/21  ****************************************
       //*** Dcl-ds WMP3Parc extname('WMP3P') Qualified Inz End-ds;
       //*** Dcl-s strFileLib  char(10) inz;
       //*** Dcl-s strAlias char(500) inz;
       //*** end   del ***  09/30/21  ****************************************
        Dcl-s intDiv like(ELdiv) inz;
        Dcl-s strMode char(1) inz;
        strMode = inMode;

        // Read EFT subfile changed record
        Readc SflEftInfM;
        Dow not %eof;
           Select;
           When lnkEditEft = *On;
             strEftMod = 'E';
             strEftTyp = 'E';
             intEftRrn = scLocRrnM;
             intEmRrn  = 0;
             intSfyr = ELfyr;
             EftAdd(strEftMod:strEftTyp:ELfnd:intEftRrn:intEmRrn:strEnv:
              intSfyr);
           When lnkBankEft = *On;
              intEftRrn = scLocRrnM;
              EFTBANKINFO(intEftRrn);
           When lnkPreNote = *On;
              intEftRrn = scLocRrnM;
              If scLocPNtDt = DtZeroIso;
                 ReqPreNote(intEftRrn);
              Else;
                 ClrPreNote(intEftRrn);
              Endif;
           EndSl;
           Readc SflEftInfM;
        Enddo;

        //*** begin del ***  09/30/21  ****************************************
        // Read 3rd Party Pay subfile changed record
        //*** Readc Sfl3rdPay;
        //*** If not %eof;
        //***    If lnk3pLog = *On;
        //***       clr3pLog = *On;
        //***       dsp3pLog = *Off;
        //***       Write rcd3pLog;
        //***       clr3pLog = *Off;
        //***       dsp3pLog = *On;
        //***       Exsr  Prepare3pLogSql;
        //***       Exsr LoadSfl3pLog;                        //Load 3r     d Pa
        //***       Exfmt rcd3pLog;                           //Display      3rd
        //***    Endif;
        //*** Endif;
        //*** end   del ***  09/30/21  ****************************************

       //-----------------------------------------------------------------------
       // LoadSfl - Load Subfile for EFT information
       //-----------------------------------------------------------------------
        //*** begin del ***  09/30/21  ****************************************
        //*** BegSr LoadSfl3pLog;
        //***    If SQLCOD >= 0 and SQLCOD <> 100;
        //***       Exec SQL
        //***          Fetch Next from C1 into :WMP3Parc;
        //***       Dow SQLCOD >=0 and SQLCOD <> 100;
        //***          scTrans  = WMP3Parc.P3pgm;
        //***          If WMP3Parc.P3dt > *zero;
        //***             scDt = %Date(WMP3Parc.P3dt:*cymd);
        //***          Else;
        //***             scDt = DtZeroIso;
        //***          Endif;
        //***          scEnvFlg = WMP3Parc.P3envflg;
        //***          If WMP3Parc.P3prtype = 'S/R Online';
        //***             scPrType = %trim(GetModuleProperty('S/R Online':'Name'
        //***              + ' ' + %trim(GetModuleProperty('Portal Module':'Name
        //***          Else;
        //***             scPrType = WMP3Parc.P3prtype;
        //***          Endif;
        //***          scEnBld  = WMP3Parc.P3enbld;
        //***          scSfyr   = WMP3Parc.P3sfyr;
        //***          If WMP3Parc.P3efdt > *zero;
        //***             scEfDt = %Date(WMP3Parc.P3efdt:*cymd);
        //***          Else;
        //***             scEfdt = DtZeroIso;
        //***          Endif;
        //***          If WMP3Parc.P3exdt > *zero;
        //***             scExDt = %Date(WMP3Parc.P3exdt:*cymd);
        //***          Else;
        //***             scExdt = DtZeroIso;
        //***          Endif;
        //***          scDftSrD = WMP3Parc.P3dftsrd;
        //***          rrn3pLog = rrn3pLog + 1;
        //***          write sfl3pLog;
        //***          Exec SQL
        //***             Fetch Next from C1 into :WMP3Parc;
        //***       Enddo;
        //***       Exec Sql
        //***          Close C1;
        //***       rrn3plog = 1;
        //***    Endif;
        //***
        //*** Endsr;
        //*** end   del ***  09/30/21  ****************************************
       //-----------------------------------------------------------------------
       // Prepare3pLogsql - Prepare 3rd Party Info Log Sql
       //-----------------------------------------------------------------------
        //*** begin del ***  09/30/21  ****************************************
        //*** BegSr Prepare3pLogSql;
        //***    If strMode = 'A';
        //***       intDiv = intAddEftDiv;
        //***    Else;
        //***       intDiv = scDiv;
        //***    Endif;
        //***
        //***    GetObjLib('WMP3P':'*FILE':strFileLib);
        //***    strAlias = 'CREATE ALIAS ' + %trim(strFileLib) + '/' + 'WMP3P_A
        //***     ' FOR ' + %trim(strFileLib) + '/' + 'WMP3P(ARCHIVE)';
        //***
        //***    Exec sql
        //***       Execute immediate :strAlias;
        //***
        //***    Exec Sql
        //***       Declare C1 cursor for
        //***        Select * from WMP3P_ARC where P3co# = :ELco# and
        //***         P3fnd = :ELfnd and P3emp# = :ELemp# and P3div = :intDiv ;
        //***
        //***    Exec Sql
        //***       Open C1;
        //*** Endsr;
        //*** end   del ***  09/30/21  ****************************************
        End-Proc;
       //*** End    Add *** 05/11/21/*******************************************
       //*** Beg Add ***** 06/02/25 ************************
       //===================================================================
       // StaffAssign - Assigning Staff to WMECP
      //===================================================================
     P StaffAssign     B
     d StaffAssign     PI

        //*** Writing/Updating/Deleting record from WMECP
        // - POLICY SERVICES
        If EMpsrv <> *blanks and EMpsrv <> SvSfPolSrv;
           //* fetching user name
           Chain EMpsrv pup01;
           If %found(SMUPL1);
              xxlen = %len(%trim(UPusnm));
              If xxlen > 15;
                 spacePos = %scan(' ': UPusnm);
                 If spacePos > 0;
                    xxfsn = %subst(UPusnm: 1: spacePos - 1);
                    xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                 Endif;
              Else;
                 xxfsn = %trim(UPusnm);
                 xxlsn = '';
              Endif;
           Endif;

           Chain (EMco#:EMfnd:EMemp#:EMdiv:'A') pec01;

           If not %found(WMECL1);
              ECco# = EMco#;
              ECfnd = EMfnd;
              Ecemp# = EMemp#;
              ECdiv = EMdiv;
              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECpcnt = 'A';
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECdivspec = 'Y';
              ECiusr = q1user;
              ECipgm = q1pgm;
              ECidt  = Dateto7(SystemDate());
              ECitim = Timeto6(SystemTime());
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());
              Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
               Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
              ECseq# = maxseq# + 1;

              Write pec01;
           Else;

              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());

              Update pec01;

           Endif;
        Endif;

        If EMpsrv = *blanks;
           Chain (EMco#:EMfnd:EMemp#:EMdiv:'A') pec01;
           If %found(WMECL1);
              Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Dow not %eof(wmecpcrt);
                 Delete wmecpcrt;
                 Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Enddo;
              Unlock wmecpcrt;

              Delete pec01;
           Endif;
        Endif;

        // - AUDIT REVIEWER
        If EMarvw <> *blanks and EMarvw <> SvSfAdtRvw;
           //* fetching user name
           Chain EMarvw pup01;
           If %found(SMUPL1);
              xxlen = %len(%trim(UPusnm));
              If xxlen > 15;
                 spacePos = %scan(' ': UPusnm);
                 If spacePos > 0;
                    xxfsn = %subst(UPusnm: 1: spacePos - 1);
                    xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                 Endif;
              Else;
                 xxfsn = %trim(UPusnm);
                 xxlsn = '';
              Endif;
           Endif;

           Chain (EMco#:EMfnd:EMemp#:EMdiv:'B') pec01;

           If not %found(WMECL1);
              ECco# = EMco#;
              ECfnd = EMfnd;
              Ecemp# = EMemp#;
              ECdiv = EMdiv;
              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECpcnt = 'B';
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECdivspec = 'Y';
              ECiusr = q1user;
              ECipgm = q1pgm;
              ECidt  = Dateto7(SystemDate());
              ECitim = Timeto6(SystemTime());
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());
              Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
               Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
              ECseq# = maxseq# + 1;

              Write pec01;
           Else;

              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());

              Update pec01;

           Endif;
        Endif;

        If EMarvw = *blanks;
           Chain (EMco#:EMfnd:EMemp#:EMdiv:'B') pec01;
           If %found(WMECL1);
              Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Dow not %eof(wmecpcrt);
                 Delete wmecpcrt;
                 Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Enddo;
              Unlock wmecpcrt;

              Delete pec01;
           Endif;
        Endif;

        // - CLAIM CONTACT
        If EMclmc <> *blanks and EMclmc <> SvClmCnt;
           //* fetching user name
           Chain EMclmc pup01;
           If %found(SMUPL1);
              xxlen = %len(%trim(UPusnm));
              If xxlen > 15;
                 spacePos = %scan(' ': UPusnm);
                 If spacePos > 0;
                    xxfsn = %subst(UPusnm: 1: spacePos - 1);
                    xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                 Endif;
              Else;
                 xxfsn = %trim(UPusnm);
                 xxlsn = '';
              Endif;
           Endif;

           Chain (EMco#:EMfnd:EMemp#:EMdiv:'C') pec01;

           If not %found(WMECL1);
              ECco# = EMco#;
              ECfnd = EMfnd;
              Ecemp# = EMemp#;
              ECdiv = EMdiv;
              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECpcnt = 'C';
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECdivspec = 'Y';
              ECiusr = q1user;
              ECipgm = q1pgm;
              ECidt  = Dateto7(SystemDate());
              ECitim = Timeto6(SystemTime());
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());
              Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
               Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
              ECseq# = maxseq# + 1;

              Write pec01;
           Else;

              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());

              Update pec01;

           Endif;
        Endif;

        If EMclmc = *blanks;
           Chain (EMco#:EMfnd:EMemp#:EMdiv:'C') pec01;
           If %found(WMECL1);
              Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Dow not %eof(wmecpcrt);
                 Delete wmecpcrt;
                 Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Enddo;
              Unlock wmecpcrt;

              Delete pec01;
           Endif;
        Endif;

        // - UNDERWRITER
        If EMuwtr <> *blanks and EMuwtr <> SvSfUndWtr;
           //* fetching user name
           Chain EMuwtr pup01;
           If %found(SMUPL1);
              xxlen = %len(%trim(UPusnm));
              If xxlen > 15;
                 spacePos = %scan(' ': UPusnm);
                 If spacePos > 0;
                    xxfsn = %subst(UPusnm: 1: spacePos - 1);
                    xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                 Endif;
              Else;
                 xxfsn = %trim(UPusnm);
                 xxlsn = '';
              Endif;
           Endif;

           Chain (EMco#:EMfnd:EMemp#:EMdiv:'D') pec01;

           If not %found(WMECL1);
              ECco# = EMco#;
              ECfnd = EMfnd;
              Ecemp# = EMemp#;
              ECdiv = EMdiv;
              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECpcnt = 'D';
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECdivspec = 'Y';
              ECiusr = q1user;
              ECipgm = q1pgm;
              ECidt  = Dateto7(SystemDate());
              ECitim = Timeto6(SystemTime());
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());
              Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
               Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
              ECseq# = maxseq# + 1;

              Write pec01;
           Else;

              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());

              Update pec01;

           Endif;
        Endif;

        If EMuwtr = *blanks;
           Chain (EMco#:EMfnd:EMemp#:EMdiv:'D') pec01;
           If %found(WMECL1);
              Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Dow not %eof(wmecpcrt);
                 Delete wmecpcrt;
                 Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Enddo;
              Unlock wmecpcrt;

              Delete pec01;
           Endif;
        Endif;

        // - AUDIT MANAGER
        If EMamgr <> *blanks and EMamgr <> SvSfAdtMgr;
           //* fetching user name
           Chain EMamgr pup01;
           If %found(SMUPL1);
              xxlen = %len(%trim(UPusnm));
              If xxlen > 15;
                 spacePos = %scan(' ': UPusnm);
                 If spacePos > 0;
                    xxfsn = %subst(UPusnm: 1: spacePos - 1);
                    xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                 Endif;
              Else;
                xxfsn = %trim(UPusnm);
                xxlsn = '';
              Endif;
           Endif;

           Chain (EMco#:EMfnd:EMemp#:EMdiv:'E') pec01;

           If not %found(WMECL1);
              ECco# = EMco#;
              ECfnd = EMfnd;
              Ecemp# = EMemp#;
              ECdiv = EMdiv;
              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECpcnt = 'E';
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECdivspec = 'Y';
              ECiusr = q1user;
              ECipgm = q1pgm;
              ECidt  = Dateto7(SystemDate());
              ECitim = Timeto6(SystemTime());
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());
              Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
               Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
              ECseq# = maxseq# + 1;

              Write pec01;
           Else;

              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());

              Update pec01;

           Endif;
        Endif;

        If EMamgr = *blanks;
           Chain (EMco#:EMfnd:EMemp#:EMdiv:'E') pec01;
           If %found(WMECL1);
              Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Dow not %eof(wmecpcrt);
                 Delete wmecpcrt;
                 Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Enddo;
              Unlock wmecpcrt;

              Delete pec01;
           Endif;
        Endif;

        // - PREMIUM ACCOUNTANT
        If EMpacc <> *blanks and EMpacc <> SvSfPrmAcc;
           //* fetching user name
           Chain EMpacc pup01;
           If %found(SMUPL1);
              xxlen = %len(%trim(UPusnm));
              If xxlen > 15;
                 spacePos = %scan(' ': UPusnm);
                 If spacePos > 0;
                    xxfsn = %subst(UPusnm: 1: spacePos - 1);
                    xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                 Endif;
              Else;
                 xxfsn = %trim(UPusnm);
                 xxlsn = '';
              Endif;
           Endif;

           Chain (EMco#:EMfnd:EMemp#:EMdiv:'F') pec01;

           If not %found(WMECL1);
              ECco# = EMco#;
              ECfnd = EMfnd;
              Ecemp# = EMemp#;
              ECdiv = EMdiv;
              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECpcnt = 'F';
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECdivspec = 'Y';
              ECiusr = q1user;
              ECipgm = q1pgm;
              ECidt  = Dateto7(SystemDate());
              ECitim = Timeto6(SystemTime());
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());
              Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
               Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
              ECseq# = maxseq# + 1;

              Write pec01;
           Else;

              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());

              Update pec01;

           Endif;
        Endif;

        If EMpacc = *blanks;
           Chain (EMco#:EMfnd:EMemp#:EMdiv:'F') pec01;
           If %found(WMECL1);
              Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Dow not %eof(wmecpcrt);
                 Delete wmecpcrt;
                 Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Enddo;
              Unlock wmecpcrt;

              Delete pec01;
           Endif;
        Endif;

        // - LOSS CONTROL REPRESENTATIVE
        If EMlrep <> *blanks and EMlrep <> SvSfLosRep;
           //* fetching user name
           Chain EMlrep pup01;
           If %found(SMUPL1);
              xxlen = %len(%trim(UPusnm));
              If xxlen > 15;
                 spacePos = %scan(' ': UPusnm);
                 If spacePos > 0;
                    xxfsn = %subst(UPusnm: 1: spacePos - 1);
                    xxlsn = %subst(UPusnm: spacePos + 1: xxlen - spacePos);
                 Endif;
              Else;
                 xxfsn = %trim(UPusnm);
                 xxlsn = '';
              Endif;
           Endif;

           Chain (EMco#:EMfnd:EMemp#:EMdiv:'G') pec01;

           If not %found(WMECL1);
              ECco# = EMco#;
              ECfnd = EMfnd;
              Ecemp# = EMemp#;
              ECdiv = EMdiv;
              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECpcnt = 'G';
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECdivspec = 'Y';
              ECiusr = q1user;
              ECipgm = q1pgm;
              ECidt  = Dateto7(SystemDate());
              ECitim = Timeto6(SystemTime());
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());
              Exec SQL Select Max(ECseq#) Into :maxseq#  From Wmecl1
               Where ECco# = :EMco# And ECfnd = :EMfnd And ECemp# = :EMemp#;
              ECseq# = maxseq# + 1;

              Write pec01;
           Else;

              ECfsn = xxfsn;
              EClsn = xxlsn;
              ECADR1 = UPADR1;
              ECADR2 = UPADR2;
              ECCTY  = UPCTY;
              ECST   = UPST;
              ECZIP  = UPZIP;
              ECEMAL = UPMAIL;
              ECFARA = UPFAXA;
              ECFPHN = UPFAX#;
              ECAREA = UPPHNA;
              ECPHN# = UPPHN#;
              ECuser = q1user;
              ECpgm  = q1pgm;
              ECdt   = Dateto7(SystemDate());
              ECtime = Timeto6(SystemTime());

              Update pec01;

           Endif;
        Endif;

        If EMlrep = *blanks;
           Chain (EMco#:EMfnd:EMemp#:EMdiv:'G') pec01;
           If %found(WMECL1);
              Setll (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Dow not %eof(wmecpcrt);
                 Delete wmecpcrt;
                 Reade (EMco#:EMfnd:EMemp#:EMdiv:ECseq#) pec0p_cr;
              Enddo;
              Unlock wmecpcrt;

              Delete pec01;
           Endif;
        Endif;

     P StaffAssign     E
       //*** End Add ***** 06/02/25 ************************

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
