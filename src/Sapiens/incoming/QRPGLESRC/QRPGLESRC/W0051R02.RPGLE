     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')

     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W0051R02 - Employer Detail Information
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD RECORDS IN THE EMPLOYER DETAIL FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0051R - WORK WITH POLICY YEARS                            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0060R - WORK WITH PAYROLLS (CALLED ONLY IN ADD MODE)      *
     H*    W0553R - WORK WITH AGT HIST (CALLED ONLY IN ADD MODE)      *
     H*    W0051R58 - WAIVER INFORMATION (CALLED ONLY IN ADD MODE)    *
     H*    W0045R - WORK WITH PREMIUM FINANCE COMPANIES               *
     H*                                                               *
     H*  CREATED: 03/06/96  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/25/13 C:283 Support: WA133 81481                           *
     H*                For flat cancelled policies, the edit to       *
     H*                compare the policy end date with the State     *
     H*                Reporting Div's policy year dates should be    *
     H*                skipped and should not be invoked.             *
     H*                                                               *
     H* 05/07/12 C:283 SUPPORT: WA090 80094                           *
     H*                Correct The Billing Start Date Zero Issue.     *
     H*                                                               *
     H* 02/03/12 C:180 SUPPORT 78921
     H*                Remove changes under support 76706
     H*
     H* 01/27/12 C:279 Support: 79057                                 *
     H*                Modified to display DB/CR in diaplay mode.     *
     H*                                                               *
     H* 01/12/12 C:136 Support: WA089 78856                           *
     H*               -Change error indicator on rate set error msg   *
     H*               -Fix default logic for policy exp date (WC1061) *
     H*               -Removed 12/08/11 fix                           *
     H*                                                               *
     H* 12/21/11 C:180 SUPPORT 78781
     H*                Allow MOD eff date not to be entered and
     H*                if entered, it should be after pol eff date.
     H*
     H* 12/08/11 C:279 Support: 78325                                 *
     H*                Changed to populate the correct policy year    *
     H*                while adding a new division.                   *
     H*                                                               *
     H* 11/25/11 C:279 Support: 78611                                 *
     H*                Changed to retain the Premium finance code.    *
     H*                                                               *
     H* 08/22/11 C:136 Project: WC1918 (114WZC - PRISS 22)            *
     H*               -Project scope change                           *
     H*               -Inhibit WC1918 for clients with mod code       *
     H*                (065 and 093 at the moment)                    *
     H*                                                               *
     H* 08/19/11 C:65  Project: WC1851                                *
     H*               -Prevent fee screen from appearing in Add mode  *
     H*                when there are no fees setup for group.        *
     H*                                                               *
     H* 07/29/11 C:180 SUPPORT 77420
     H*                Allow MOD eff date after pol eff date
     H*
     H* 07/07/11 C:257 Support: WA139 75332                           *
     H*                Remove record lock problem with file WMAUL2    *
     H*                in MVSCDB subroutine.                          *
     H*                                                               *
     H* 07/06/11 C:33  Project: 114WVY       SY255 Priss #123         *
     H*                Use variable DSPATR instead of indicators for  *
     H*                 deductible display.                           *
     H*                                                               *
     H* 05/18/11 C:189 Support: WA046 76706                           *
     H*                We should not allowed a policy Exp date to be  *
     H*                altered if the policy is  'Renewal Quote'      *
     H*
     H* 05/06/11 C:65  Project: WC1851                                *
     H*               -Update call W0051R63 for Add mode.             *
     H*               -Prevent fee screen from appearing in Add mode  *
     H*                when module flag is not set.                   *
     H*                                                               *
     H* 04/21/11 C:190 Project: 114WYI                                *
     H*                Add new field for "Auto EFT".                  *
     H*                                                               *
     H* 03/11/11 C:190 Project: 132WPN                                *
     H*                Change employer detail maintenance to display  *
     H*                1st Deviation field for a NM policy if the     *
     H*                premium level is not blank (for CLT132).       *
     H*                                                               *
     H* 03/01/11 C:180  PROJECT WC1918
     H*                 Check for existance of proj WC1918.
     H*
     H* 02/11/11 C:33  Project: WC1851                                *
     H*               -Update usage of stop collateral flag.          *
     H*               -Update call W0051R63. (Priss #122)             *
     H*                                                               *
     H* 01/31/11 C:255 Project: WC1918(Priss#27)                      *
     H*                         1) Added change to correctly pull the *
     H*                            description on SCR04.              *
     H*                                                               *
     H* 01/28/11 C:190 Project: WC1954                                *
     H*                Added parameter for W0015R38.                  *
     H*                                                               *
     H* 12/30/10 Support 75013 : 1) Added changes by checking existe- *
     H*                             nce of project 132WOJ.            *
     H*                          2) Based on the object W0010R64 exis-*
     H*                             tence Flexible heading field valu-*
     H*                             es will be populated on SCR05     *
     H*                          3) If object exists than fields will *
     H*                             be populated from WDFHP(132WOJ fu-*
     H*                             nctionality. If object doesn't    *
     H*                             exist than fields will be populat-*
     H*                             ed from WMEXP file(Existing funct-*
     H*                             ionality).                        *
     H*                                                               *
     H* 12/31/10 C:255 Project: WC1918                                *
     H*                         1) Added code for flag at scr04.      *
     H*                         2) Load Policy page 3c info from WDF4P*
     H*                            file, if flag is set to 'Y'        *
     H*                         3) Redisplay screen if flag at screen *
     H*                            gets changed.                      *
     H*                                                               *
     H* 12/28/10 C:136 Project: 139WPL Priss 70                       *
     H*                The HCN screen shouldn't display in -ADD- mode *
     H*                unless the HCN info is set up in groups.       *
     H*                                                               *
     H* 12/17/10 C:255 Project:71WLD                                  *
     H*                Call program W0096R when employer is being cre-*
     H*                ated for client 071 and 3rd Party premium entry*
     H*                flag set to 'Y'.                               *
     H*                                                               *
     H* 12/09/10 C:136 Seq#: WA139/75145                              *
     H*          C:33  Change indicator usage on min fee.             *
     H*                                                               *
     H* 12/01/10 C:257 SUPPORT: 74974                                 *
     H*                Make SCR05 visible only if heading flags       *
     H*                are set in file WDFHP.                         *
     H*                                                               *
     H* 11/23/10 C:255 Project: 114WVZ                                *
     H*                         Added code to call W0051R48 when      *
     H*                         BSLPGM ='R'(Retention Plan).         *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*               -Do not allow state to change if HCN program on *
     H*                 policy.                                       *
     H*               -In ADD mode, call the HCN program information. *
     H*                                                               *
     H* 10/25/10 C:255 Project: 132WOJ(Priss#32)                      *
     H*                         Added error message at Screen01 to for*
     H*                         Policy type.                          *
     H*                                                               *
     H* 09/13/10 C:180 PRISS #9 133WKA                                *
     H*                Missing # of employees prompt.
     H*
     H* 08/30/10 C:189 SUPPORT: 73431                                 *
     H*                Audit tracking  and Employer detail processing *
     H*                should be in sync for for CONTESTED flag       *
     H*                                                               *
     H* 08/25/10 C:33  Project: WC1851  Priss#69                      *
     H*               -Require Y or N decision to update all divisions*
     H*               -Add collateral exclusion and stop bill date to *
     H*                all division update                            *
     H*               -Do not use any control over call to W0051R63   *
     H*                                                               *
     H* 07/28/10 C:255 Project: 132WOJ                                *
     H*                Added handling for new SCR05 for Flexible Inf- *
     H*                ormation. Add changes for the fields moved from*
     H*                SCR04 TO SCR05.                                *
     H*                                                               *
     H* 06/21/10 C:255 Project: C114WVY                               *
     H*                Handle SCR02 and SCR03 changes added new fiel- *
     H*                ds at SCR02(Split deviation) moved fields from *
     H*                SCR02 TO SCR03                                 *
     H*                                                               *
     H* 05/24/10 C:61  Project: WC1851                                *
     H*                Standardize appearance SCR04.
     H*                                                               *
     H* 05/18/10 C:61  Support: WA139 72742 114WRC                    *
     H*              - Do not edit DB/CR for Min/Max if Min/Max not   *
     H*                entered.                                       *
     H*                                                               *
     H* 04/01/10 C:136 Project: WC1851                                *
     H*                Remove Charge Fees and Over-ride Group Default *
     H*                fields per GW                                  *
     H*                                                               *
     H* 03/19/10 C:61  Support: WA065 71903 114WRC                    *
     H*                Re-implement DB/CR factor MIN/MAX edit.
     H*                                                               *
     H* 03/12/10  C:62   Project: 25WRB  Priss #9                     *
     H*           C:33   Require policy type entry for all group types*
     H*                                                               *
     H* 03/01/10  C:65   PROJECT: 25WRE                               *
     H*                  REMOVE THE 1/04/10 CHANGE FOR BILLING OPTION.*
     H*                                                               *
     H* 02/27/10 C:33  Project: WC1838    25WQW  Priss #4             *
     H*                Update edits on Billing Schedule Code for Large*
     H*                 Deductibles                                   *
     H*                                                               *
     H* 02/26/10 C:33  Project: 25WRC  Priss #13                      *
     H*                Process Retro independently of Large Deductible*
     H*                                                               *
     H* 02/25/10  C:65   PROJECT: 25WRC                               *
     H*                  Prevent % from printing in 2nd deviation     *
     H*                  heading for TASB.                            *
     H*                                                               *
     H* 02/17/10  C:190  PROJECT: 25WRE(Corrected issue of 132WNC)    *
     H*                                                               *
     H* 02/05/10  C:65   PROJECT: 25WRB                               *
     H*                  Require entry of policy type code for TASB   *
     H*                  if the claims only flag is set to 'N'.       *
     H*                                                               *
     H* 01/25/10  C:232  PROJECT: WC1851                              *
     H*                  Modified for Administrative Fees for         *
     H*                  Collateral and Claims Project.               *
     H*                                                               *
     H* 01/19/10  C:198  PROJECT: 132WNC Priss 9                      *
     H*                  Edits on flexible field(s) was not working   *
     H*                  for 1st policy year.                         *
     H*                                                               *
     H* 01/14/10 C:136 PROJECT: 25WRC                                 *
     H*          C:191                                                *
     H*                ADD CLIENT SPECIFIC CHANGES TO SECOND SCREEN   *
     H*                -FIELDS MOVED TO THE RIGHT                     *
     H*                -USE STAB FACTOR IN DEV #2'S PLACE             *
     H*                                                               *
     H* 01/04/10  C:190  PROJECT: 25WRE                               *
     H*                  Back to Base                                 *
     H*                                                               *
     H* 12/18/09 C:234 Support: 70888 WA093                           *
     H*                Program bouncing when updating Non-Renew flag  *
     H*                on policies with Multiple divisions.           *
     H*                                                               *
     H* 12/30/09  C:232  PROJECT: WC1838                              *
     H*                  Billing plan cannot be changed from Employer *
     H*                  Policy Years to a Non-Large Deductible plan  *
     H*                  if deductible information still resides in the*
     H*                  Large Deductible Information Screen. Deductible*
     H*                  1 and 2 should not allow entry for Large ded.*
     H*                                                               *
     H* 11/16/09  C:198  PROJECT: 132WNC                              *
     H*                  Edit flexible field(s) value(s) based on     *
     H*                  required flag(s) set at group level.         *
     H*                                                               *
     H* 11/03/09 C:189 Support: 70523                                 *
     H*                Correct the extra words appearing on the screen*
     H*                while creating the Policy year record          *
     H*                                                               *
     H* 10/20/09 C:191 Support: 70115 WA133                           *
     H*                Changes related to project 133WFE were lost    *
     H*                Re-implement - Add DARTS Remark with Activity  *
     H*                Status Changed                                 *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                Update the deductible fields with the details  *
     H*                from the deductible discount table file.       *
     H*                                                               *
     H* 07/05/09 C:220 Support: 69010                                 *
     H*                Handle Deductable discount code edit prop-     *
     H*                erly.                                          *
     H*                                                               *
     H* 06/06/09  C:189  PROJECT: 93WID                               *
     H*                  Addtional flexible felds in WDELP            *
     H*                                                               *
     H* 04/23/09 C:220 Support: 66461                                 *
     H*                Add edit for schd debit/credit limits for      *
     H*                FMIC.                                          *
     H*                                                               *
     H* 04/21/09 C:136 Support: WA139 65431                           *
     H*                Add logic to switch S/R billings to their      *
     H*                reporting diivsion if the summarized flag      *
     H*                is changed from 'Y' to 'N'.                    *
     H*                                                               *
     H* 01/21/09 C:136 Support: WA139 66742 (GIC Client Specific)     *
     H*                Assign vendor only if vendor exists for group  *
     H*                per GW.                                        *
     H*                                                               *
     H* 01/14/09 C:136 Project: WC1611                                *
     H*                Add logic to populate ELNWRW - new/renew flag  *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*
     H* 12/03/08  C:33  Project: 102WEP Priss#15
     H*                 'Exclude from $2500' needs to be on SCR04
     H*                  It was orginally done on SCR02 replacing other
     H*                  fields that need to be displayed.
     H*                                                               *
     H* 07/10/08 C:136 Support: WA136 63885                           *
     H*                Correct edit on 'Bill on Eff Day' flag so that *
     H*                it doesn't invoke incorrectly if the client    *
     H*                has not purchased the vendor and ACH modules.  *
     H*                                                               *
     H* 05/22/08 C:136 PROJECT: 93WHI                                 *
     H*                DISPLAY VERBIAGE IF VIP MEMBER                 *
     H*                                                               *
     H* 04/29/08 C:61  Project 71WIY                                  *
     H*                Add field for "Exclude from Retro Calc".
     H*                                                               *
     H* 04/07/08 C:136 SUPPORT: WA139 62859 (CLIENT SPECIFIC)         *
     H*                ADD EDIT TO PREVENT WDELP FROM BEING ADDED     *
     H*                IF VENDOR DOESN'T EXIST AT COMPANY LEVEL       *
     H*                                                               *
     H* 03/28/08  C:65  Project: CI019                                *
     H*                 Only allow an online s/r bscd when changing   *
     H*                 the bscd if valid ach info is present.        *
     H*                                                               *
      * 03/14/08 C:180 PROJECT: C136WDJ
      *                Derive accurate discount calculation.
      *
     H* 11/02/07  C:65  Project SUPP# 60745 - CLIENT REQUEST          *
     H*                 AUTOMATICALLY CREATE VENDOR HISTORY RECORDS   *
     H*                 FOR CLIENT 146 AS WELL.                       *
     H*                                                               *
     H* 10/11/07  C:183 Project 146WBC                                *
     F*                 Added edits for the new flexible billing flag *
     H*                 This edit will govern whether billings are to *
     F*                 to be posted on the policy effective date or  *
     F*                 first day of the month.                       *
     H*                                                               *
     H* 10/08/07  C:190 Project WC1454  Priss# 20/21                  *
     H*                 Condition the call to CHGRNW within           *
     H*                 ERRCK1 to only execute if EXUPDA is not       *
     H*                 equal to 'Y' value                            *
     H*                 Change ELMBCD to ELQTYP and ELMFAM to         *
     H*                 ELMFEE for Project WC1454.                    *
     H*
     H* 10/08/07  C:190 Project WC1454  Priss# 11.                    *
     H*                 Make sure to address updates to the various   *
     H*                 debit/credit codes and effictive/expiration   *
     H*                 dates.                                        *
     H*                                                               *
     H* 10/02/07  C:65  Project 114WMF                                *
     H*                 CONDITION OVERRIDE PRORATA FACTOR FOR         *
     H*                 DISPLAY/CHANGE.                               *
     H*                                                               *
     H* 10/01/07  C:190 Project WC1454  Priss# 6.
     H*                 Displaying "Work with errors(due to invalid)  *
     H*                 setup on fields that were not changed by      *
     H*                 user.                                         *
     H*                                                               *
     H* 10/01/07  C:190 Project WC1454  Priss# 7/8.
     H*                 Make sure to check the overall processing     *
     H*                 of project WC1454 to ensure that we only      *
     H*                 check for proper setup and produce Ww screen  *
     H*                 listing setup error after requesting all di-  *
     H*                 vision update only for those item that were   *
     H*                 changed by the user.
     H*                 Description of fields should be listed        *
     H*                 instead of database field names               *
     H*
     H* 09/27/07 C:190 Project 93WGX    Priss# 9.                     *
     H*               SCR04 does not allow for any entries            *
     H*               when running for CLT093.                        *
     H*
     H* 09/08/07 C:33  Project: WC1196  Priss# 26.                    *
     H*                Implement new flag.                            *
     H*
     H* 08/09/07 C:136 PROJECT: WC1599                                *
     H*                RATE SET CODE EDIT FOR 114WMF WAS CALLING      *
     H*                THE 'CHG DED AND INC LIMITS' WDW.  COPIED      *
     H*                WDW04 FROM ARC114/W0051F02 AND RENAMED AS      *
     H*                WDW06.                                         *
     H*                                                               *
     H* 07/27/07  C:190 Project WC1454
     H*                 Add a new popup window to appear after the    *
     H*                 final screen is passed to and display the new *
     H*                 work with screen conditionally                *
     H*                                                               *
     H* 07/20/07 C:33  Project: WC1196  Priss# 26.                    *
     H*                On -Add-, default SCAAFL to St Rpt Div.        *
     H*
     H* 07/16/07  C:190 Project 93WGX
     H*                 Add new flag to designate whether or not      *
     H*                     billing are to be on the 15th of each     *
     H*                     for KRMS(Client # 93)                     *
     H*                 Make entry of 'Number of Employee' optional   *
     H*                     processing for KRMS(Client # 93)          *
     H*                                                               *
     H* 06/16/07 C:136 SUPPORT: WA071 58747                           *
     H*                ADD FUNCTION KEYS TO SCR04.                    *
     H*                                                               *
     H* 06/13/07 C:136 SUPPORT: WA132 57014                           *
     H*                PREVENT DELETION IF DIVISION IS SUMMARIZED AND *
     H*                A REVISED BILL ISN'T RUN.                      *
     H*                                                               *
     H* 06/06/07  C:190 Seq#: WA099/39975
     H*                 Correcr error msg or Rate set code.
     H*
     H* 05/25/07  C:190 Seq#: WA136/56532
     H*                 Correct error msgs on Experience modifers 1,2,3.
     H*
     H* 05/10/07 C:132 Project 71WGW                                  *
     H*                 Add check on CLTID in Edit check so do not    *
     H*                 get error message on non-display fields.      *
     H*
     H* 04/26/07  C:33  Seq#: WA099/56913                             *
     H*                 Correct error msgs on Deductible edits        *
     H*
     H* 04/17/07 C:190 Project 71WGW
     H*                 Add new field 'max # of employees per shift'
     H*
     H* 04/13/07 C:65  PROJECT: 135WJD/WC1566                         *
     H*                DISPLAY THE OVERRIDE PRORATA TO 1.00 FIELD     *
     H*                FOR ALL CLIENTS SINCE ITS NEEDED FOR NJ.       *
     H*                                                               *
     H* 04/04/07    C:180      WA114/53245                            *
     H*                        Ensure sched rating info remarks are   *
     H*                        written to DARTS when in quote mode.   *
     H*
     H* 03/06/07 C:61  PROJECT 71WIF                                  *
     H*                Default "Spread revised bill adj future (Y/ )" *
     H*                from group value (ww groups, option 48).       *
     H*                                                               *
     F* 03/01/07 C:180 PROJECT: 114WMF
     F*                Remove *IN97 conflict between 114WMF & 132WDD.
     F*
     H* 02/07/07 C:136 SUPPORT: WA114 55160                           *
     H*               -PREVENT BILL AUX FROM BEING CALLED TWICE       *
     H*                IN A ROW UNEXPECTEDLY. CLEAR SCBSTY AFTER      *
     H*                BSCD SEACRCH.                                  *
     H*               -ALSO ADDED CORRECTIONS FOR BILL AUX AND        *
     H*                BSCD SEARCH.                                   *
     H*                                                               *
     H* 01/16/07 C:136 Support: WA114 61817                           *
     H*                Remove CHECK(ER) attribute from the 'Do not    *
     H*                renew' field on SCR01. It is not needed.       *
     H*                                                               *
     H* 12/06/06A C:136 SUPPORT: WA132 56144                            *
     H*                 DEFAULT RATE SET CODE BASED ON PHYSICAL ADDRESS *
     H*                 FIRST                                           *
     H*                                                               *
     H* 12/06/06 C:61  PROJECT WC1444                                 *
     H*                Create new "Carrier #" with ADD of new policy
     H*                year.
     H*                call to API WA200R.                            *
     H*                                                               *
     H* 12/05/06 C:180 PROJECT: 114WMF
     H*                Modified display file W0051F02 to show error
     H*                message when rate set code is not valid for
     H*                group. No changes were done to pgm W0051R02 just
     H*                to display file W0051F02.
     H*                                                               *
     H* 10/19/06 C:136 SUPPORT: WA071 55436                           *
     H*                CORRECT BILLING CODE AND BILLING AUX SEARCH    *
     H*                                                               *
     H* 10/02/06 C:33  Project: WC1196                                *
     H*                Audit Co. Assignment                           *
     H*                                                               *
     H* 09/06/06 C:136 SUPPORT: WA129 54190                           *
     H*                ADJUST THE BILLING AUXILIARY SEARCH TO         *
     H*                ACCOUNT FOR THE ACTUAL BILLING START DATE      *
     H*                                                               *
     H* 08/24/06 C:180 PROJECT: 114WMF
     H*                EDIT RATE SET CODES.
     H*                OVERRIDE PRO-RATA FACTOR.
     H*                CAPTURE SCHED-RATING DATA.
     H*
     H* 07/11/06 C:65  PROJECT: 132WDD                                *
     H*                CLIENT SPECIFIC: ADD A SCREEN PROMPT FOR       *
     H*                SPREADING ADJUSTMENTS INTO FUTURE ONLY FOR     *
     H*                CLIENT 132 ON THIRD SCREEN.                    *
     H*
     H* 07/06/06 C:168 PROJECT: 132WEW                                *
     H*                Added processing for the Facultative           *
     H*                re-insurance.                                  *
     H*
     H* 06/12/06 C:136 SUPPORT: WA129 53115                           *
     H*                ADJUST SRBILL EDIT AND BSCD SEARCH TO COUNT    *
     H*                THE CORRECT NUMBER OF INSTALLMENTS             *
     H*                                                               *
     H* 05/10/06 C:61  SUPPORT: WC1237
     H*                Correct "array out of range" error.            *
     H*                                                               *
     H* 04/06/06 C:136 SUPPORT: WA139 51831                           *
     H*                ADD POPUP WINDOW INFORMING USER THAT BSCD ON   *
     H*                NON-STATE REPORTING SUMMARIZED DIVISIONS WILL  *
     H*                BE CHANGED IF STATE REPORTING SUMMARIZED       *
     H*                DIVISION IS CHANGED.                           *
     H*                                                               *
     H* 03/20/06 C:136 SUPPORT: WA099 39357                           *
     H*                FIX BSCD SEARCH SO THAT ONLY 'ELIGIBLE'        *
     H*                BSCD'S WILL BE DISPLAYED.                      *
     H*                                                               *
     H* 03/06/06 C:168 Project: WC1416                                *
     H*                Sets the "REquest Renewal" flag to be the same *
     H*                as the one in the "State Reporting" division   *
     H*                record                                         *
     H*                                                               *
     H* 01/27/06 C:15  Support: WC139/50443                           *
     H*                Changed edit to allow ARD on 'A' rated policy. *
     H*                                                               *
     H* 01/20/06 C:33  Project: Client 139 Request                    *
     H*                Client specific: Write Vendor history record   *
     H*                 on ADD.                                       *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC: POPULATE DEBIT/CREDIT FACTOR  *
     H*                WITH NEW COMMISSION MOD MULTIPLER.             *
     H*                                                               *
     H* 10/28/05 C:65  PROJECT: WA139 - 49593                         *
     H*                Change ERRCK1 exit to re-display screen one    *
     H*                If have error on edit for Contractor Adj       *
     H*                Credit and in Add mode.                        *
     H*                                                               *
     H* 10/14/05 C:65  PROJECT: 114WGH                                *
     H*                COPY EXCLUDED STATES IN LIVE ENVIRONMENT.      *
     H*                CHECK TO SEE IF ANY DEFAULT EXCLUDED STATES    *
     H*                NEED TO BE COPIED IN FOR THE NEW WDELP RECORD. *
     H*                A DEFAULT RECORD FOR A GROUP IS DESIGNATED     *
     H*                WITH A ZERO EMPLOYER# AND A ZERO FYR.          *
     H*                                                               *
     H* 08/15/05 C:61  PROJECT: WC1205
     H*              - Correct issues with "Activity Status Code"     *
     H*                search.                                        *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Add Activity status code/ & processing.
     H*                                                               *
     H* 08/02/05 C:136 PROJECT: 135WCW                                *
     H*                POP UP WINDOW IF INCREASED LIMITS OR DED       *
     H*                CODE CHANGES. UPDATE ALL DIVISIONS WITH        *
     H*                CHANGE IF PROMPT = 'Y'                         *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109BD}                                        *
     H*                Add membership dues at the employer level.     *
     H*                                                               *
     H* 05/18/05 C:136 SUPPORT: WA129 46997                           *
     H*                CORRECT EDIT ADDED BY 45812 SO THAT THE BSDT   *
     H*                CAN BE PRIOR TO POLICY TO DATE IF THE DIV      *
     H*                IS SUMMARIZED AND THE BSDT = POLICY TO DATE    *
     H*                OF STATE REPORTING DIVISION.                   *
     H*                                                               *
     H* 06/22/05 C:132 PROJECT WA135 - 47763                          *
     H*                When have 2nd deductible code, and does not    *
     H*                get record on SETLL, then chain to WTDDL2 for  *
     H*                description and eliminate invalid error msg.   *
     H*                                                               *
     H* 04/07/05-2 C:156 PROJECT: 126WDK                              *
     H*                PROJECT: 126WCD                                *
     H*                Multiple Deductible discounts per Year.        *
     H*          C:132 PROJECT: 126WAP                                *
     H*                Change Policy To Date on new adds to the date  *
     H*                from WDF1P/F1ENPO. This is the common date for *
     H*                the group, a change from project WC1061.       *
     H*                                                               *
     H* 04/08/05 C:136 PROJECT: WC1288 (SUMMARIZE S/R POLICIES)       *
     H*                REMOVE EDIT THAT PREVENTS POLICY YEAR FROM     *
     H*                HAVING A/R SUMMARIZED IF SELF REPORTING.       *
     H*                                                               *
     H* 04/07/05 C:61  PROJECT WC1281                                 *
     H*                When ADD mode for PEO group, generate new      *
     H*                certificate holder record for the PEO employer.*
     H*                                                               *
     H* 03/03/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 03/03/05 C:132 PROJECT: WC1260                                *
     H*                ADD new screen field on SCR01 for Exempt from  *
     H*                Installment Fees (Y/ ).                        *
     H*                                                               *
     H* 02/16/05 C:132 PROJECT: SUPP#45812   (chg to 42959)           *
     H*                Prevent BILLING START DATE to be Prior to      *
     H*                CURRENT DATE if Adding or Changing a Policy,   *
     H*                unless being changed to Policy Effective Date. *
     H*                                                               *
     H* 01/05/05 C:61  PROJECT WC1061                                 *
     H*                Correct default of policy expiration date.     *
     H*                                                               *
     H* 11/23/04 C:65  PROJECT: SUPP#42959                            *
     H*                PREVENT BILLING START DATE TO BE PRIOR TO      *
     H*                CURRENT DATE IF ADDING OR CHANGING A POLICY.   *
     H*                                                               *
     H* 11/22/04 C:33   Project: WC1017                               *
     H*                 If the 'Do Not Renew' flag changes, check for *
     H*                  update of all divisions.                     *
     H*                                                               *
     H* 10/05/04 C:132  PROJECT:  132WBF                              *
     H*                 Add new field for Type of Building code and   *
     H*                 edit check on ADD/CHG mode for existing code. *
     H*                                                               *
     H* 09/14/04 C:62  PROJECT DR093 43837                            *
     H*                REMOVE LINE OF CODE WHICH KEEPS REMARK FROM    *
     H*                BEING ASSIGNED TO THE QUOTE.                   *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237
     H*                Add fields for voluntary labor charge.         *
     H*                                                               *
     H* 08/12/04 C:61  PROJECT 114WHL WC1217                          *
     H*                Create "Renewal Tracking" record.              *
     H*                                                               *
     H* 07/09/04 C:15  Project: 114WHK                                *
     H*                Added field for "Adverse acct flag".  This flag*
     H*                will identify accts whose business the client  *
     H*                does not want to retain.                       *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                    *
     H*                When checking rate set code change, include    *
     H*                4th and 5th assessments.                       *
     H*                                                               *
     H* 06/24/04 C:65  PROJECT: 114WHO                                *
     H*                BRING UP ADDITIONAL SCREEN FOR RETRO INFO.     *
     H*                THIS SCREEN WILL APPEAR IF THE BILLING         *
     H*                SCHEDULE CODE PLAN TYPE IS NOT BLANK.          *
     H*                                                               *
     H* 01/09/04 C:136 PROJECT: WC1116                                *
     H*                Remove vendor information from employer detail.*
     H*                It is being put into vendor history.           *
     H*                                                               *
     H* 10/13/03 C:61  PROJECT WC1017                                 *
     H*                Add field: "Do not renew (Y/ )". ELRLFL.       *
     H*                                                               *
     H* 08/05/03 C:136 SUPPORT: WA108 38282                           *
     H*                Edit SCBSDT for Admin-only clients.            *
     H*                                                               *
     H* 07/07/03 C:136 SUPPORT: WA072 37765                           *
     H*                Search not working on SCBSCD if SCBSTY has a   *
     H*                value.                                         *
     H*                                                               *
     H* 06/20/03 C:61  SUPPORT WA099 37606                            *
     H*                Option 2=Change - displays in ADD mode if      *
     H*                accessed immediatly after F12=Change from ADD  *
     H*                mode.                                          *
     H*                                                               *
     H* 06/12/03 C:136 PROJECT: WC1115                                *
     H*                Vendor field not displaying in 'ADD' mode.     *
     H*                                                               *
     H* 04/23/03 C:61  PROJECT WC1061                                 *
     H*                if ADD mode and no previous policy years,      *
     H*                 if commercial group                           *
     H*                   default emp pol expiration date =           *
     H*                      employer inception date + 1 year         *
     H*                 else                                          *
     H*                   default emp pol expiration date =           *
     H*                      group year ending date                   *
     H*                 endif                                         *
     H*                endif                                          *
     H*                                                               *
     H* 04/11/03 C:136 PROJECT: WC1116                                *
     H*                If ACH processing, allow 'T' as a valid        *
     H*                billing schedule type (SCBSTY).                *
     H*                                                               *
     H* 02/18/03 C:136 PROJECT: WC1115                                *
     H*                ADD VENDOR FIELDS TO SCR03.                    *
     H*                                                               *
     H* 01/28/03 C:142 PROJECT: SUPP#35685                            *
     H*                ADD CHECK FOR CLARENDON FLAG.                  *
     H*                                                               *
     H* 01/27/03 C:136 PROJECT: WC744                                 *
     H*                PROVIDE FOR ENTRY OF MASSACHUSETTS ARAP FACTOR.*
     H*                                                               *
     H* 09/05/02 C:65  PROJECT: SUPP#34012                            *
     H*                CLEAR EMPLOYER DETAIL FIELDS BEFORE DISPLAYING *
     H*                A NEW DIVISION, SINCE DEFAULTED INFO FROM      *
     H*                WC1061 SHOULD NOT HAVE CARRIED OVER.           *
     H*                                                               *
     H* 08/22/02 C:142 SUPPORT WA039 SEQ:32891                        *
     H*                Must enter rate set code if Clarendon          *
     H*                Preprocessor flag (WCFL20) is present.         *
     H*                                                               *
     H* 06/06/02 C:136 SUPPORT WA091 SEQ:32904                        *
     H*                Allow billing schedule code to be editable in  *
     H*                quote regardless of prior AR.                  *
     H*                                                               *
     H* 04/03/02 C:136 SUPPORT 31904                                  *
     H*                Found that selecting a BSCD when searching     *
     H*                on SCBSTY would override the protection on     *
     H*                SCBSCD.                                        *
     H*                                                               *
     H* 02/25/02 C:136 SUPPORT: WA099 31398                           *
     H*                If called by revised billing, let the billing  *
     H*                schedule code be editable. Also, changed the   *
     H*                protection on the BSCD to be non-editable if   *
     H*                ARTYPE records exist in previous months that   *
     H*                are adjustment, bill, or deposit (ARTYPE = A,  *
     H*                B, or H.                                       *
     H*                THIS SUPPORT ITEM WILL NEED TO BE INCLUDED IF  *
     H*                WC1015 IS IMPLEMENTED INTO ANY CLIENT MODS.    *
     H*                W0090R IS AFFECTED BY THIS SUPPORT ITEM.       *
     H*                                                               *
     H* 01/17/02 C:132 SEQ: WA107 #29649                              *
     H*                Add Edit to not allow Division Add if Policy   *
     H*                Year date not in Range of State Reporting DIVN *
     H*                                                               *
     H* 12/12/01 C: 53 PROJECT: WC1060                                *
     H*                ADD "EXCLUDE MEDICAL FLAG"                     *
     H*                                                               *
     H* 12/05/01 C:142 SEQ: WA099 #30131                              *
     H*                RESULT OF WC1015, YOU COULD STILL PLACE        *
     H*                CURSOR ON FIELD AND F10 TO SEARCH.             *
     H*                                                               *
     H* 11/20/01 C:65  PROJECT: WC1058                                *
     H*                PULL OPTIONAL HEADING FOR SAFETY FEE.          *
     H*                                                               *
     H* 11/09/01 C:142 PROJECT: WC1061                                *
     H*                CHANGING DEFAULT TO POLICY EFF DATE, POLICY    *
     H*                EXP DATE, AND BILLING START DATE, DEPENDING    *
     H*                ON STATE REPORTING DIVISION, AND WHETHER       *
     H*                OTHER POLICY YEARS EXIST.                      *
     H*                                                               *
     H* 10/29/01 C:142 PROJECT: WC1015                                *
     H*                DO NOT ALLOW A CHANGE TO THE BILLING SCHEDULE  *
     H*                CODE IS A/R HAS BEEN RECOGNIZED IN A PREVIOUS  *
     H*                MONTH. (USED *IN06).                           *
     H*                                                               *
     H* 08/28/01 C:65  PROJECT: WC936                                 *
     H*                DO NOT CHECK POLICY EFFECTIVE DATE FOR A/R     *
     H*                SUMMARY.  BILLING START DATE AND CODE WILL     *
     H*                ONLY BE CHECKED FOR BEING THE SAME.            *
     H*                                                               *
     H* 02/21/01 C:65  PROJECT: WC1026                                *
     H*                ALLOW 'M' OPTION FOR THE PROMULGATED FLAG.     *
     H*                                                               *
     H* 02/19/01 C:09  SEQ#: WA091/24601                              *
     H*                Correct changes previously made under seq #    *
     H*                22877 (see below - 12/22/00).                  *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: WC1024                                *
     H*                WHEN CHECKING RATE SET CODE CHANGE, INCLUDE    *
     H*                3RD ASSESSMENT.                                *
     H*                                                               *
     H* 01/09/01 C:33  SEQ#: WA093/25741                              *
     H*                ACCOUNT FOR EXTENDED MONTHS BEING > 12.        *
     H*                                                               *
     H* 12/22/00 C:09  PROJECT WC577/WA107/SEQ# 22877                 *
     H*                Only change default stock discount info if rate*
     H*                set code change is also a state change.  Do not*
     H*                change default if disc percentage exists, only *
     H*                change default if table value is assigned.     *
     H*                                                               *
     H* 11/09/00 C:90  PROJECT: WC577                                 *
     H*                IF BILLING SCHED. DATE IS BLANK USE POLICY     *
     H*                EFFECTIVE DATE TO CALL BILLING AUXILIARY       *
     H*                CODE SEARCH.                                   *
     H*                                                               *
     H* 08/15/00 C:61  SUPPORT 23970                                  *
     H*              - Disallow F5=Refresh in ADD mode.               *
     H*                                                               *
     H* 08/14/00 C:61  SUPPORT 23937 WC988                            *
     H*                If cx dt exists and is <= policy TO dt then    *
     H*                DO NOT check for installments due subsequent   *
     H*                policy TO date.                                *
     H*                                                               *
     H* 07/20/00 C:61  PROJECT WC994                                  *
     H*                If policy eff dt range chg will result in      *
     H*                claims with accident dt outside pol eff dt     *
     H*                range, then display error window.  Allow       *
     H*                authorized user to override error.             *
     H*                                                               *
     H* 07/05/00 C:09  PROJECT WC577/WA107/SEQ# 22877                 *
     H*                Automatically change default stock discount    *
     H*                info and default waiver info based on rate     *
     H*                set code assigned.                             *
     H*                                                               *
     H* 07/03/00 C:09  PROJECT WC995                                  *
     H*                Add an informational message if the ARD        *
     H*                entered on a division is not equal to the ARD  *
     H*                of the state reporting division.  The ARD      *
     H*                should apply across the entire policy.         *
     H*                                                               *
     H* 05/02/00 C:61  PROJECT WC988                                  *
     H*              - Add edits for when rate set code or policy     *
     H*                effective date is changed.  Ensure that any    *
     H*                statistical elements defined under the new     *
     H*                rate set code and effective date match those   *
     H*                previously defined.                            *
     H*              - Edit billing schedule code to insure no        *
     H*                payments to be made after policy TO date       *
     H*                                                               *
     H* 04/17/00  C:65  PROJECT: WC963                                *
     H*                 ADD PROMPT FOR FLAG TO ESTABLISH CONCURRENCY. *
     H*                                                               *
     H* 03/24/00 C:61  SUPPORT WA093 22335                            *
     H*                Edit Experience Modifier dates into ascending  *
     H*                order.                                         *
     H*                                                               *
     H* 03/10/00  C:35   PROJECT: SUPPORT WA048 #22171                *
     H*           C:118  VALUE OF DTLESS WAS BEING CARRIED FORWARD    *
     H*                  WHEN USER WENT FROM ONE EMPLOYER TO ANOTHER, *
     H*                  CAUSING ERROR MESSAGE THAT POL EXP DATE NOT  *
     H*                  EQUAL TO STATE RPTING EXP DATE OR CX DATE ON *
     H*                  SINGLE-DIV POLICIES THAT WERE NOT CANCELLED. *
     H*                                                               *
     H* 03/02/00  C:35   PROJECT: SUPPORT WA102 #21881                *
     H*                  SINCE INCREASED LIMITS CODES ARE ALPHA, "00" *
     H*                  MUST BE TREATED AS A VALID CODE.             *
     H*                                                               *
     H* 02/29/00  C:65  PROJECT: WC936                                *
     H*                 CONVERT BILLING START DATE TO 8 DIGIT BEFORE  *
     H*                 RUNNING THE COMPARSION TO THE STATE RPT DIV.  *
     H*                                                               *
     H* 02/28/00 C:61  PROJECT WC577                                  *
     H*                Default first policy year entered for employer *
     H*                based on where employer inception date falls   *
     H*                in group policy year dates.                    *
     H*                                                               *
     H* 02/23/00  C:65  PROJECT: WC936                                *
     H*                 DO NOT ALLOW THE BILLING SCHEDULE CODE TO BE  *
     H*                 SELF-REPORTING IF THE POLICY IS SUMMARIZED.   *
     H*                                                               *
     H* 02/10/00  C:33  PROJECT: WC919                                *
     H*                 DEFAULT THE RATE SET CODE FROM THE STAT/RATE  *
     H*                 FILE.                                         *
     H*                                                               *
     H* 02/09/00 C:61  SUPPORT 21646                                  *
     H*              - relax edit which requires all divisions to     *
     H*                have same expiration date as state reporting   *
     H*                division for claims only clients.              *
     H*                                                               *
     H* 12/22/99  C:35   PROJECT: SUPPORT WA101 #21097                *
     H*           C:33   ADD PARAMETER FOR QUOTE ENVIRONMENT TO CALL  *
     H*                  TO DELETE PROGRAM.                           *
     H*                                                               *
     H* 12/01/99 C:33  SEQ#: WA025/20690                              *
     H*                VERIFY THAT THE RECORD HAS NOT BEEN DUPLICATED *
     H*                SINCE THE USER LEFT SCR01.                     *
     H*                                                               *
     H* 11/30/99 C:61  PROJECT WC958                                  *
     H*                Add MCO group code to SCR02.                   *
     H*                                                               *
     H* 11/15/99 C:65  PROJECT: WC936                                 *
     H*                ADD FLAG FOR SUMMARIZE A/R TO ONE DIVISION.    *
     H*                IF THE STATE REPORTING DIVISION IS SET TO      *
     H*                SUMMARIZE, THEN DEFAULT THE BILLING SCHED      *
     H*                CODE AND THE BILLING START DATE IF LEFT BLANK  *
     H*                AND THEY SET THE SUMMARIZE FLAG TO 'Y' ON A    *
     H*                DIVISION.  IF THE STATE REPORTING DIVISION IS  *
     H*                NOT SET TO SUMMARIZE, THEN DO NOT ALLOW ANY    *
     H*                OTHER DIVISIONS TO SET THE FLAG.  DO NOT ALLOW *
     H*                THE STATE REPORTING DIVISION'S SUMMARY FLAG    *
     H*                TO BE REMOVED IF OTHER DIVISIONS HAVE THE FLAG *
     H*                SET ON.  FINALLY, THE BILLING SCHEDULE CODE    *
     H*                AND BILLING START DATE FOR ALL SUMMARY DIVS    *
     H*                MUST EQUAL THE VALUES FOR THE STATE RPT DIV.   *
     H*                                                               *
     H* 09/29/99 C:118 PROJECT: WC939                                 *
     H*                Added new field 'Premium finance co code' to   *
     H*                display file. F10=Search to W0045R WW Premium  *
     H*                Finance Co. Validation against existing        *
     H*                companies and must be effective as of policy   *
     H*                effective date.                                *
     H*                                                               *
    H* 09/29/99 C:113 PROJECT: 48WPK                                 *
    H*                Changed the call to W0051R58 to be called      *
    H*                in ADD, CHANGE or DISPLAY modes.               *
     H*                                                               *
     H* 09/22/99 C:65  PROJECT: WC851                                 *
     H*                SHIFT MISC FIELDS TO THE THIRD SCREEN AND      *
     H*                COMMENT OUT RETRO FIELDS FOR NOW.              *
     H*                SCREEN FIELDS FOR PRIOR SCREEN 3 HAVE BEEN     *
     H*                MOVED TO SCR03TEMP.                            *
     H*                                                               *
     H* 09/16/99 C:65  PROJECT: WC851                                 *
     H*                ADD SHORT-RATE/PRORATE FLAG TO SCR01.          *
     H*                                                               *
     H* 09/15/99 C:113 PROJECT: CM045                                 *
     H*                Moved call to waiver information to start of   *
     H*                PGMEND subroutine.                             *
     H*                                                               *
     H* 08/12/99  C:65  PROJECT WC935                                 *
     H*                 ALLOW FOR 1ST EXP MOD TO BE ZERO.             *
     H*                                                               *
     H* 08/10/99 C:61  PROJECT WC577                                  *
     H*                Don't carry over Billing Schedule Code         *
     H*                Description                                    *
     H*                                                               *
     H*  07/19/99 C:33  PROJECT: WC899                                *
     H*                 IF THE RATING STATE CHANGES, UPDATE RECORDS   *
     H*                 IN WDABP.                                     *
     H*                                                               *
     H*  06/22/99 C:61 PROJECT 48WQJ                                  *
     H*              - Add previous fund number to header info.       *
     H*              - Compare RRWVAC to zero.                        *
     H*                                                               *
     H*  05/12/99 C:113 PROJECT: CM045                                *
     H*                 Waiver of Subro rewrite.                      *
     H*                 Modified program to call W0051R58 (Waiver     *
     H*                 Information) in ADD mode only if a stat code  *
     H*                 was assigned for the waiver of subrogation    *
     H*                 in Opt 54=Stat Rpt/Rating in WW Groups.       *
     H*                                                               *
     H*  04/06/99 C:118 PROJECT:  WC577                               *
     H*                 MODIFIED EXPERIENCE MODIFIERS SO IF CLAIMS    *
     H*                 ONLY GROUP=Y THEN EXP MOD IS OPTIONAL.        *
     H*                                                               *
     H*  02/19/99 C:90  SEQ#: WA091/16721                             *
     H*                 MODIFIED EDITS FOR POLICY YEARS.              *
     H*                                                               *
     H*  02/17/99 C:33  SEQ#: WA091/16684                             *
     H*                 CHANGE INDICATOR USEAGE FOR 02/08/99.         *
     H*                                                               *
     H*  02/08/99 C:90  PROJECT: WA091/16067                          *
     H*                 Modified edits for policy years.              *
     H*                                                               *
     H*  02/05/99 C:87  PROJECT: WC878                                *
     H*                 Add an entry for "Number of employees" to     *
     H*                 SCR01.  Only display/error check this field   *
     H*                 if WCFL14 = 'Y' (in which case it is required)*
     H*                                                               *
     H*  02/04/99 C:90 PROJECT:WA091/16435                            *
     H*                CLEAR SCPP3C, SCPPC3 BEFORE LOADING VALUES.    *
     H*                                                               *
     H*  01/12/99 C:61 PROJECT WC577                                  *
     H*                Add '# of employer divisions' to header info.  *
     H*                                                               *
     H* 01/12/98  C:90   PROJECT: WC858                               *
     H*                  ALLOW ENTRY OF 2ND ITEM 3.C. ON THE POLICY   *
     H*                  INFORMATION PAGE FROM EMPLOYER DETAIL.       *
     H*                                                               *
     H*  1/07/99  C:02   SUPPORT: WA096 15751                         *
     H*                 SCEMD1 / EMOD DATE, ON ENDORSEMENT TO PRINT   *
     H*                 CORRECTLY FOR DATE 1
     H*                                                               *
     H* 12/29/98  C:78   SUPPORT: WA046 15731                         *
     H*                 ALLOW ZERO SAFETY FEE % WITH MINIMUM.         *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H* 11/10/98  C:87   PROJECT: WC852                               *
     H*                  Added following fields and processing to     *
     H*                  screen1: Cancellation notice days;           *
     H*                  Additional days for mailing.                 *
     H*                                                               *
     H* 10/30/98  C:90   PROJECT: WC858                               *
     H*                  ALLOW ENTRY OF ITEM 3.C. ON THE POLICY       *
     H*                  INFORMATION PAGE FROM EMPLOYER DETAIL.       *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H* 09/03/98  C:90  PROJ: WC577                                   *
     H*                 DEFAULT 'A' RATING FLAG TO "N" IN ADD MODE.   *
     H*                                                               *
     H* 09/01/98  C:33  SEQ#: WA033/13523                             *
     H*                 DO NOT VALIDATE THE 2ND INCREASED LIMITS'     *
     H*                 DATES IF THE CODE IS NOT ENTERED              *
     H*                                                               *
     H* 08/13/98  C:33  SEQ#: WA048/13064                             *
     H*                 DEFAULT THE POLICY YEAR BASED ON THE SYSTEM   *
     H*                 DATE                                          *
     H*                                                               *
     H* 08/10/98 C:61 Project: 48WLC                                  *
     H*               Display old Emp#/Div from Employer Cross        *
     H*               Reference (WZEXP).                              *
     H*                                                               *
     H* 07/31/98  C:65  PROJECT: SUPPORT                              *
     H*                 DO NOT DEFAULT DEVIATION %.                   *
     H*                                                               *
     H* 07/22/98  C:108 PROJECT SUPPORT: WA093/12777                  *
     H*                 CHANGE CLAIMS-ONLY FLAG FROM "Y" TO "C".      *
     H*                                                               *
     H* 07/14/98  C:65  PROJECT: WC845                                *
     H*                 LOAD IREP % INTO SCREEN FIELD.                *
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H* 06/12/98  C:33  PER: CG                                       *
     H*                 ONLY DEFAULT THE STOCK DISCOUNT IN ADD MODE   *
     H*                                                               *
     H* 06/01/98  C:33  SEQ#: WA095/11943                             *
     H*                 CHECK THE POLICY DATE RANGE AGAINST THE MAX   *
     H*                 EXTENDED YEAR POLICY.                         *
     H*                                                               *
     H* 05/13/98  C:65  SEQ#: WA048/11591                             *
     H*                 ALLOW EXTENDED YEAR POLICIES FOR COMMERCIAL.  *
     H*                                                               *
     H* 05/07/98  C:33  SEQ#: WA095/11471                             *
     H*                 DO NOT EDIT FIELDS IF THEY ARE NOT DIPLAYED   *
     H*                 ON THE SCREEN.                                *
     H*                                                               *
     H* 04/28/98  C:65  PROJECT: WC705                                *
     H*                 USE RATING STATE WHEN USING A SEARCH.         *
     H*                                                               *
     H* 03/09/98  C:85  PROJECT: WC577                                *
     H*                 CHANGED INDICATOR ON FIELD SCDISC             *
     H*                 FROM 61 TO 91.                                *
     H*                                                               *
     H* 03/06/98  C:35  PROJECT: SUPPORT                              *
     H*                 CHANGE CODE SO THAT THE CANCELLATION DATE     *
     H*                 DISPLAYED ON THE SCREEN IS THE CX DATE        *
     H*                 ASSOCIATED WITH THE MOST RECENT EFFECTIVE     *
     H*                 DATE.  ALSO, THIS FIELD IS NOW BEING CLEARED  *
     H*                 SO THAT IT WON'T CARRY DATA FROM A PREVIOUSLY *
     H*                 DISPLAYED EMPLOYER IF THE CURRENT EMPLOYER    *
     H*                 IS NOT CANCELLED.  ALSO, CONTROL DISPLAY OF   *
     H*                 THIS FIELD WITH AN INDICATOR SO THAT IF THE   *
     H*                 EMPLOYER IS NOT CANCELLED, THE FIELD AND      *
     H*                 HEADING WILL NOT SHOW ON THE SCREEN.          *
     H*                                                               *
     H*  03/05/98  C:33  PROJECT WC577   PER JJ/KK                    *
     H*                  USE BILLING START DATE TO GET THE BILLING    *
     H*                  AUXILIARY RECORD.                            *
     H*                                                               *
     H*  02/25/98  C:65  PROJECT WC705                                *
     H*                  BILLING START DATE CANNOT BE GREATER THAN    *
     H*                  THE POLICY EFFECTIVE DATE.                   *
     H*                                                               *
     H*  02/13/98  C:33  PROJECT WC577                                *
     H*                  CHANGE CALC FOR DIFFERENCE IN POLICY DATES.  *
     H*                                                               *
     H*  02/12/98  C:33  PROJECT WC577                                *
     H*                  DELETE FILES RELATED TO THE YR BEING DELETED.*
     H*                                                               *
     H*  02/03/98  C:57  PROJECT: CM027                               *
     H*                 -SETUP FOR MID TERM ENDORSEMENT TRACKING IF   *
     H*                  CALLED FROM THAT PROCESS.                    *
     H*                                                               *
     H*  02/02/98  C:65  PROJECT WC577                                *
     H*                 -EXPERIENCE MOD #1 CAN BE BEFORE ELPFDT.      *
     H*                                                               *
     H*  12/30/97  C:33  PROJECT WC577                                *
     H*                 -GET STOCK DISCOUNT FROM STAT RPT/RATE FILE.  *
     H*                 -RE-IMPLEMENT DISCOUNT %/TYPE MUTUAL EXCLUSION*
     H*                                                               *
     H*  10/30/97  C:33  PROJECT WC577                                *
     H*                 -CALL W/W PAYROLLS INSTEAD OF -ADD- PAYROLLS  *
     H*                 -CALL W/W AGENT HISTORY IN ADD MODE           *
     H*                                                               *
     H*  10/28/97  C:65  PROJECT V3                                   *
     H*               1) DO NOT DISPLAY DISCOUNTS IF NO CODE IS SET   *
     H*                  UP IN STATISTICAL REPORTING/RATING MAINT.    *
     H*               2) ALSO CHANGED TYPE OF POLICY FROM 1A TO 2A.   *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H*  40 - ADD MODE                                                *
     H* N40 - UPDATE MODE                                             *
     H*  41 - QUOTE MODE                                              *
     H* N41 - LIVE MODE                                               *
     H*  65 - SECURITY LEVEL VALID                                    *
     H* N65 - SECURITY LEVEL INVALID                                  *
     H*  66 - COMMERCIAL APPLICATION INSTALLED (WCFL07)               *
     H*  67 - CLAIMS ONLY INSTALLED (WCFL08)                          *
     H*  69 - DEDUCTIBLES INSTALLED (WCFL06)                          *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  92 - Display old Emp#/Div in header info                     *
     H*  93 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  16 & 17 -      Adverse acct flag invalid (Y/ )               *
     H*  20 - (WCR0517) Policy year Invalid for Fund {ADD mode only}  *
     H*  21 - (WC30004) Policy year cannot be used more than once     *
     H*  22 - (WCR0854) Billing Schedule Type not 'M', 'Q', 'A' or 'O'*
     H*  23 - (WCR0513) Billing Schedule table code invalid           *
     H*  24 - (WCR0500) Billing Start Date invalid                    *
     H*  25 - (WCR1070) Rate Set Code invalid                         *
     H*  26 - (WCR0501) 'A' Rating not 'Y' or 'N'                     *
     H*  27 - (WCR1062) Anniversary Rating Date not allowed w/'A' Rate*
     H*  28 - (WCR0500) Policy From Date invalid                      *
     H*  29 - (WCR0609) From Date < Initial Effective Date            *
     H*  30 - (WCR0811) From Date not valid for existing payroll      *
     H*  31 - (WCR0529) Policy Dates not in ascending order           *
     H*  32 - (WCR0723) Policy Dates not between Group Policy Dates   *
     H*  33 - (WCR0500) Policy To Date invalid                        *
     H*  33 - (WCR0502) Compensation Reimbursement < zero             *
     H*  34 - (WCR1280) To Date > State Reporting To Date             *
     H*  35 - (WCR1281) To Date > 366 days past From Date             *
     H*  36 - (WCR1282) To Date <> CX Date in Employer Master         *
     H*  37 - (WCR0500) Anniversary Rating Date invalid               *
     H*  38 - (WCR0577) Anniversary Rating Date > Policy From Date    *
     H*  39 - (WCR0513) Increased Limit Factor 1 table code invalid   *
     H*  42 - (WC30128) NO CODE IS SET UP FOR THIS CREDIT.            *
     H*  45 - (WCR0513) Increased Limit Factor 2 table code invalid   *
     H*  46 - (WCR0513) Deductible Discount table code invalid        *
     H*  47 - (WCR0501) Deductible Bill if CX not 'Y' or 'N'          *
     H*  48 - (WCR1062) Anniversary date not allowed with 'A' Rating  *
     H*  64 - (WCR1241) Field must be 'Y' or blank.                   *
     H*  71 - (WCR0544) Fields mutually inclusive                     *
     H*  72 - (WCR0500) Invalid Effective Date                        *
     H*  73 - (WCR0930) Effective Date not between Policy Dates       *
     H*  74 - (WCR0529) Dates not in ascending order                  *
     H*  75 - (WCR0500) Invalid Expiration Date                       *
     H*  76 - (WCR0930) Expiration Date not between Policy Dates      *
     H*  77 - (WC30076) Percentage cannot be > 100                    *
     H*  78 - (WCR0513) INVALID TABLE VALUE                           *
     H*  80 - Increased Limit Factor 1 Date errors                    *
     H*  81 - Increased Limit Factor 2 Date errors                    *
     H*  82 - Deductible Discount Date errors                         *
     H*  83 - (WC30138) Date range > max extended year policy         *
     H*  85 - Contractor Adjustment Credit Date errors                *
     H*  86 - (WC30073) Incr Limit 2 entered with no Limit 1 Exp date *
     H*  87 - (WC30074) Incr Limit 2 eff date < Limit 1 Exp date      *
     H*  88 - (WCR0547) Canx notice days cannot be less than zero     *
     H*  89 - (WCR0547) Addtl mailing days cannot be less than zero   *
     H*  98 - (WCR0578) Prem fin co not eff for policy eff date       *
     H*  POSITION CURSOR:                                             *
     H*  49 - (SCBSTY) Billing Schedule Type                          *
     H*  50 - (SCBSCD) Billing Schedule Code                          *
     H*  51 - (SCBSDT) Billing Start Date                             *
     H*  52 - (SCRTCD) Rate Set Code                                  *
     H*  53 - (SCAFLG) 'A' Rating Flag                                *
     H*  54 - (SCILCD) Increased Limit Factor 1                       *
     H*  55 - (SCILEF) Increased Limit Factor 1 Effective Date        *
     H*  56 - (SCINL2) Increased Limit Factor 2                       *
     H*  57 - (SCI2EF) Increased Limit Factor 1 Effective Date        *
     H*  58 - (SCDCDE) Deductible Discount Code                       *
     H*  59 - (SCDCEF) Deductible Discount Effective Date             *
     H*  65 - (SCBLFL) Flexible billing date flag.                    *
     H*  79 - (SCQTYP) POLICY TYPE                                    *
     H*                                                               *
     H*  SCR02:                                                       *
     H*  ERROR:                                                       *
     H*  20 - (WCR0527) Deviation 1 can't be entered without a heading*
     H*  21 - (WCR0527) Deviation 2 can't be entered without a heading*
     H*  22 - (WCR0645) Experience Modifier 1 and Date not entered    *
     H*  23 - (WCR0577) Experience Modifier 1 Date > Policy From Date *
     H*  24 - (WCR0571) Internally Promulgated not 'Y' or blank       *
     H*  25 - (WCR0646) Experience Modifier 2 not entered before 3    *
     H*  26 - (WCR0513) Discount table code invalid                   *
     H*  27 - (WCR0581) Discount types mutually exclusive             *
     H*  28 - (WCR0505) MCO Type Code not found                       *
     H*  29 - (WCR1243) MCO Type Code not entered                     *
     H*  30 - (WCR1247) Managed Care Effective Date not entered       *
     H*  31 - (WCR0571) Contested Audit Flag not 'Y' or blank         *
     H*  32 - (WCR0502) Safety fee % < or = zero                      *
     H*  33 - (WCR0502) Safety fee minimum amount < or = zero         *
     H*  34 - (WCR0547) Schedule DB/CR factor < zero                  *
     H*  36 - (WCR0529) Emp exp mod dates 1 & 2 must be ascending     *
     H*  37 - (WCR0529) Emp exp mod dates 2 & 3 must be ascending     *
     H*  43 - DO NOT DISPLAY DEVIATION                                *
     H*  44 - DO NOT DISPLAY 2ND DEVIATION                            *
     H*  63 - DO NOT DISPLAY SUBJECT 1A CREDIT                        *
     H*  64 - DO NOT DISPLAY SUBJECT 1B CREDIT                        *
     H*  65 - DO NOT DISPLAY SCHEDULE DB/CR                           *
     H*  71 - (WCR0544) Fields mutually inclusive                     *
     H*  72 - (WCR0500) Invalid Effective Date                        *
     H*  73 - (WCR0930) Effective Date not between Policy Dates       *
     H*  74 - (WCR0529) Dates not in ascending order                  *
     H*  75 - (WCR0500) Invalid Expiration Date                       *
     H*  76 - (WCR0930) Expiration Date not between Policy Dates      *
     H*  77 - (WC30076) Percentage cannot be > 100                    *
     H*  78 - (WCR0513) Invalid MCO code.                             *
     H*  80 - Deviation 1                                             *
     H*  81 - Deviation 2                                             *
     H*  82 - Experience Modifier 1                                   *
     H*  83 - Experience Modifier 2                                   *
     H*  84 - Experience Modifier 3                                   *
     H*  85 - Schedule DB/CR Factor                                   *
     H*  86 - Subject Prem Credit 1A                                  *
     H*  87 - Subject Prem Credit 1B                                  *
     H*  88 - Stock discount %                                        *
     H*  89 - Saftey fee %                                            *
     H*  95 - Position cursor to MCO code.                            *
     H*  96 - (WCR0646) Experience modifier 1 not entered before 2
     H*  POSITION CURSOR:                                             *
     H*  50 - (SCDISC) Discount Type                                  *
     H*  51 - (SCMCCD) MCO Type Code                                  *
     H*                                                               *
     H*  SCR03:                                                       *
     H*  ERROR:                                                       *
     H*  30 - (WCR0513) Invalid Table (ASTS/CNTY/RGN#/CHAP/BKCD)      *
     H*  20 - (WCR1229) Deposit % application not 'S' or 'N'          *
     H*  21 - (WCR1229) Minimum Premium application not 'S' or 'N'    *
     H*  22 - (WCR1229) Maximum Premium application not 'S' or 'N'    *
     H*  23 - (WCR0502) Loss Conversion Factor <= zero                *
     H*  24 - (WCR0500) Settlement Date not valid                     *
     H*  25 - (WCR0502) Tax Multiplier <= zero                        *
     H*  26 - (WCR0502) Basic Premium Factor <= zero                  *
     H*  27 - (WCR1229) Basic Premium Fact application not 'S' or 'N' *
     H*  28 - (WCR0502) Reinsurance Factor <= zero                    *
     H*  29 - (WCR1229) Reinsurance Factor application not 'S' or 'N' *
     H*  POSITION CURSOR:                                             *
     H*  78 - Activity Status                                         *
     H*****************************************************************
     FW0051F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F*
     FWDELP     UF A E             DISK    USROPN
     F                                     RENAME(PEL01:DBRCDF)
     F                                     INFDS(DBFDS)
     F*
     FWDELL     IF   E           K DISK    USROPN
      *** BEG ADD ***** 08/02/05 ******************************************
     FWDELL1    UF   E           K DISK
     F                                     RENAME(PEL01:PELL1)
      *** END ADD ***** 08/02/05 ******************************************
     F***  Begin Add  ***  11/22/04  **************************************
     F                                     INFDS(ELFDS)
     F***  End   Add  ***  11/22/04  **************************************
     F***** BEG ADD ******* 08/10/98 **********************************
     F*
     F*  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     F***** END ADD ******* 08/10/98 **********************************
     F*
     F***  BEGIN ADD  ***  10/29/01  *********************************
      *** BEG CHG **** 02/25/02 **********************************************
     F************************** WMEAL   IF  E           K        DISK
     FWMEAL6    IF   E           K DISK
      *** END CHG **** 02/25/02 **********************************************
      *** beg add ***** 04/21/09 *********************************************
     FWMEAL24   uf   e           k disk    rename(pea01 : pea24) prefix(E24)
      *** end add ***** 04/21/09 *********************************************
     F*
     F***  END   ADD  ***  10/29/01  *********************************
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWMEML     IF   E           K DISK
     F*
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     F*
     FWDF1L     IF   E           K DISK
     F***  BEGIN ADD  ***  08/13/98  *********************************
     FWDF1L1    IF   E           K DISK
     F                                     RENAME(PF101:PF102)
     F***  END   ADD  ***  08/13/98  *********************************
     F*
     FWMEPL     IF   E           K DISK    USROPN
     F*
     FWTDDL2    IF   E           K DISK
     FWTDDP     IF   E             DISK
     F                                     RENAME(PDD01:PDD02)
     F*
     FWMPRL1    IF   E           K DISK
     F*
     FWMDSL3    IF   E           K DISK
     FWMDSP     IF   E             DISK
     F                                     RENAME(PDS01:PDS02)
     F*
     FWMRSL     IF   E           K DISK
     F***  BEGIN ADD  ***  07/19/99  *********************************
     FWDABL2    UF   E           K DISK
     F***  END   ADD  ***  07/19/99  *********************************
     F*
     FWDRRL     IF   E           K DISK
     F*
     FWTGTL     IF   E           K DISK
     F*
      ****** Begin Add 08/22/02 ***************************************
     FSWCPF     IF   E             DISK    USROPN
     F                                     RENAME(SWC01:SWC02)
      ****** End   Add 08/22/02 ***************************************
     F*
     FWTLML1    IF   E           K DISK
     FWTLMP     IF   E             DISK
     F                                     RENAME(PLM01:PLM02)
     F*
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
      ***** BEG ADD ******* 08/12/04 **********************************
     F                                     IGNORE(PRT01)
      *
      *  Renewal Tracking
     FWDRTL     UF A E           K DISK
      ***** END ADD ******* 08/12/04 **********************************
     F****** BEG ADD ***** 07/07/05 **************************************
     FWTAVL     IF   E           K DISK
     FWTAVP     IF   E             DISK
     F                                     RENAME(PAV01:PAV02)
     F****** END ADD ***** 07/07/05 **************************************
     F*
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
     F*
     FWTBSP     IF   E             DISK
     F                                     RENAME(PBS01:PBS02)
     F*
     FWMBXP     IF   E             DISK
     F*** BEG ADD ***** 02/03/98 ***********************************************
     F*
     FWTMYP     IF   E             DISK
     F*
     FWMDXL     IF   E           K DISK
     F*
     FWMENL3    IF A E           K DISK
     F*** END ADD ***** 02/03/98 ***********************************************
     F**** BEGIN ADD **** 10/14/98 *********************************************
     FWDF2L     IF   E           K DISK
     F**** END   ADD **** 10/14/98 *********************************************
     F***** BEGIN ADD **** 09/29/99 ***********************************
     F*
     F* PREMIUM FINANCE COMPANY PHYSICAL AND LOGICAL
     F*
     FWMPFP     IF   E             DISK
     F                                     RENAME(PPF01:PPF02)
     F*
     FWMPFL     IF   E           K DISK
     F***** END   ADD **** 09/29/99 ***********************************
      ***** BEG ADD ******* 11/30/99 **********************************
      *
      * MCO Group Code
     FWMMGL     IF   E           K DISK
     FWMMGP     IF   E             DISK
     F                                     RENAME(PMG01:PMG02)
      *
      ***** END ADD ******* 11/30/99 **********************************
      ***** BEG ADD ******* 07/20/00 **********************************
      *
      * Claims Master
     FWMCMLC    IF   E           K DISK
      *
      * Admin/DARTS Control Definition
     FWMEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
      *
      ***** BEG ADD ******* 10/05/04 **********************************
      *
      *  Type of Building File
     FWTTBL     IF   E           K DISK
      ***** END ADD ******* 10/05/04 **********************************
      *
      ***** END ADD ******* 07/20/00 **********************************
      ***** BEG DEL ******* 01/09/04 **********************************
      ***** BEG ADD ******* 02/18/03 **********************************
      *********** VENDOR FILE
     F*********** WMVPP   IF  E                    DISK
     F*********** WMVPL   IF  E           K        DISK
     F*********** PVP01                             KRENAMEPVP02
      ***** END ADD ******* 02/18/03 **********************************
      ***** END DEL ******* 01/09/04 **********************************
      *** BEG ADD ***** 04/07/08 **************************************
     FWMVPL     IF   E           K DISK
      *** END ADD ***** 04/07/08 **************************************
      ***** BEG ADD ******* 04/07/05 **********************************
      *
      *  Certificate Holder
     FWMCEL6    IF A E           K DISK
      ***** END ADD ******* 04/07/05 **********************************
     F*** BEGIN ADD ***  10/14/05  ************************************
     FWDXSL     IF A E           K DISK
     F*** END ADD *****  10/14/05  ************************************
     F***  Begin Add  ***  01/20/06  **************************************
     FWDVHP     O    E             DISK
     F***  End   Add  ***  01/20/06  **************************************
      *** BEG ADD ***** 07/11/06 **************************************
     FWMEALC    IF   E           K DISK
     F                                     RENAME(PEA01:PEA0C)
      *** END ADD ***** 07/11/06 **************************************
      *
      *+*+  BEG ADD *+*+*+*+08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
      * Group Rate Set Code
     FWMGRL     IF   E           K DISK
      *+*+  END ADD *+*+*+*+08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
     F***** BEG ADD ******* 07/27/07 **********************************
      * Error Listing File
     F**WXUAL***OF  E           K        DISK
     FWXUAL     IF A E           K DISK
     F********WDELL2  UF  E           K        DISK
     F********    PEL01                             KRENAMEPELL2
     FWDELL5    IF   E           K DISK    USROPN
     F                                     RENAME(PEL01:PELL5) Prefix(zz:2)
     F***** END ADD ******* 07/27/07 **********************************
      *** beg add ***** 01/12/12 **************************************
     fwdell2    if   e           k disk    rename(pel01:pel02) prefix(l2)
      *** end add ***** 01/12/12 **************************************
     F***** BEG ADD ******* 10/18/07 **********************************
     FWDELL8    IF   E           K DISK
     F                                     RENAME(PEL01:PELL2) Prefix(E2:2)
     FWDELL1Q   IF   E           K DISK
     F                                     RENAME(PEL01:PELL3) Prefix(E3:2)
     F***** END ADD ******* 10/18/07 **********************************
     F*** BEG ADD ****** 06/06/09 ************************************
     FWTGIL     IF   E           K DISK
     F*** END ADD ****** 06/06/09 ************************************
      *
     F*** BEG ADD ****** 12/30/09 ************************************
     F***  Begin Chg  ***  02/27/10  **************************************
     F**********WMLDL1    IF   E           K DISK
     FWMLDL1    if   E           K DISK    infds(LDfds)
     FWMLDP     uf   e             disk    rename(pld01:pld0p)
     F***  End   Chg  ***  02/27/10  **************************************
     F*** END ADD ****** 12/30/09 ************************************
     F***  Begin Add  ***  02/27/10  **************************************
     FWDDDL1    if   e           k disk    rename(pdd01:ddd01)
     F***  End   Add  ***  02/27/10  **************************************
     F*** BEG ADD ****** 01/25/10 ************************************
     F*** beg chg ***** 01/12/12 *************************************
     F*********** WDEFL1    UF   E           K DISK
     FWDEFL1    IF   E           K DISK
     F*** end chg ***** 01/12/12 *************************************
     c*** begin chg ***  08/19/11  ***********************************
     F*****WTFIL     IF   E           K DISK
     FWTFIL3    IF   E           K DISK
     c*** end chg *****  08/19/11  ***********************************
     F*** END ADD ****** 01/25/10 ************************************
     F*** Beg Add *** 07/28/10 ***********************************************
     FWDFHL     IF   E           K DISK
     F*** End Add *** 07/28/10 ***********************************************
     F*** BEG ADD ****** 08/30/10 ************************************
     FWMAUL2    UF   E           K DISK
     F*** END ADD ****** 08/30/10 ************************************
     F*** beg add ***** 12/28/10 *************************************
     FWMHCL     IF   E           K DISK
     F*** end add ***** 12/28/10 *************************************
     f*** begin add ***  12/31/10  ***************************************************
     f*****   wdf4l     if   e           k disk
     fwdf4l     if   e           k disk    usropn
     f*** end   add ***  12/31/10  ***************************************************
      *
     D***  Begin Add  ***  02/27/10  **************************************
     D LDfds           ds
     D  LDrrn                397    400b 0
     D***  End   Add  ***  02/27/10  **************************************
      *
     D**** BEGIN ADD **** 03/06/98 *********************************************
     D**** END   ADD **** 03/06/98 *********************************************
      ***** BEG ADD ******* 07/20/00 **********************************
      ***** END ADD ******* 07/20/00 **********************************
      ***** BEG ADD ******* 05/02/00 **********************************
     D ST              S             75    DIM(11) CTDATA PERRCD(1)             Stat data inf msg
      *** BEG ADD ***** 03/20/06 *************************************
      *** BEG ADD ***** 04/07/08 **************************************
     D STR             S             53    DIM(10) CTDATA PERRCD(1)             STRWDW MESSAGES
      *** END ADD ***** 04/07/08 **************************************
     D***** END ADD ******* 10/08/07 **********************************
      ***** BEG ADD ******* 07/20/00 **********************************
      *
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)                              CLAIM PMTS
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
      ***** END ADD ******* 07/20/00 **********************************
      *
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
      *
      *
      *
      ***** END ADD ******* 05/02/00 **********************************
     F***  BEGIN ADD  ***  10/29/01  *********************************
     D*
     D                 DS
     D  DSDTTX                 1     14  0
     D  DSTIMX                 1      6  0
     D  DSDATX                 7     14  0
     D                 DS
     D  EATRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D                 DS
     D  EAPODT                 1      7  0
     D  XXPOYM                 1      5  0
     D                 DS
     D  XXXDTE                 1      7  0
     D  XXCUYM                 1      5  0
     F***  END   ADD  ***  10/29/01  *********************************
     D*
     D*
     D***  Begin del  ***  10/08/07  **************************************
     D**********/COPY SCOPYMT01I
     D***  End  del  ***  10/08/07  **************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D***  Begin Add  ***  11/22/04  **************************************
     D ELFDS           DS
     D  ELRRN                397    400B 0
     D***  End   Add  ***  11/22/04  **************************************
     d***  Begin Add  ***  12/21/07  **************************************
     d saveDBRRN       s                   like(DBRRN)
     d***  End   Add  ***  12/21/07  **************************************
     D***  Begin Add  ***  12/03/08  **************************************
     d clt102          s                   like(*IN07)
     D***  End   Add  ***  12/03/08  **************************************
     D*
     D OPTNM           C                   CONST('Employer Detail Info-
     D                                     rmation')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D DOTS            C                   CONST(' . . . . . . . . . -
     D                                     . . . . . . . . .')
     D DEV             C                   CONST('Deviation')
     D SPC             C                   CONST('Subject Prem Credit')
     D*** BEGIN ADD ***  11/20/01  ************************************
     D SAF             C                   CONST('Safety Fee')
     D*** END ADD *****  11/20/01  ************************************
     D***** BEG ADD ******* 10/08/07 **********************************
     D POLF            C                   CONST('POLICY FROM DATE ERRORS')
     D POLT            C                   CONST('POLICY TO DATE ERRORS')
     D SUMR            C                   CONST('SUMMARIZE A/R TO ONE-
     D                                      DIV')
     D ORDC            C                   CONST('OVERRIDE ON CHARGING-
     D                                      INSTALLMENT FEE')
     D POLY            C                   CONST('POLICY TYPE')
     D ADVF            C                   CONST('ADVERSE ACCOUNT FLAG')
     D DNRN            C                   CONST('Do not renew to new -
     D                                     year (Y/ )')
     D BILS            C                   CONST('BILLING START DATE ERRORS')
     D ARFG            C                   CONST('"A" RATING FLAG')
     D DEBI            C                   CONST('DEDUCTIBLE BILL IF C-
     D                                     ANCELLED')
     D MCOT            C                   CONST('MCO TYPE CODE')
     D MGOT            C                   CONST('MCO GROUP CODE')
     D SHPF            C                   CONST('SHORT RATE/ PRORATA -
     D                                     FLAG')
     D DICO            C                   CONST('DISCOUNT')
     D INPR            C                   CONST('INTERNAL PROMULGATIO-
     D                                     N')
     D CXND            C                   CONST('CX NOTICE DAYS')
     D PPIC            C                   CONST('POLICY PAGE ITEM 3C')
     D PP3C            C                   CONST('POLICY PG ITEM 3C LI-
     D                                     INE 2')
     D MBRA            C                   CONST('MEMBER FEE AMOUNT')
     D SAFP            C                   CONST('SAFETY FEE PERCENT')
     D SAFA            C                   CONST('SAFETY FEE AMOUNT')
     D SAMA            C                   CONST('SAFETY FEE MIN AMT')
     D AMAL            C                   CONST('ADDITIONAL MAILING D-
     D                                     AYS')
     D ANRD            C                   CONST('ANNIVERSARY RATING D-
     D                                     ATE ERRORS')
     D ECMD            C                   CONST('EXCLUDE MEDICAL')
     D INCR            C                   CONST('INCREASED LIMITS COD-
     D                                     E')
     D INEF            C                   CONST('INCREASED LIM. EFF. -
     D                                     DATE')
     D INEX            C                   CONST('INCREASED LIM. EXP. -
     D                                     DATE')
     D DEDC            C                   CONST('DEDUCTIBLE CODE')
     D DEEF            C                   CONST('DEDUCTIBLE EFF. DATE')
     D DEEX            C                   CONST('DEDUCTIBLE EXP. DATE')
     D DEVN            C                   CONST('DEVIATION')
     D DVEF            C                   CONST('DEVIATION EFF. DATE')
     D DVEX            C                   CONST('DEVIATION EXP. DATE')
     D DEVN2           C                   CONST('DEVIATION 2')
     D DE2EF           C                   CONST('DEVIATION 2 EFF DT')
     D DE2EX           C                   CONST('DEVIATION 2 EXP DT')
     D*** BEG   ADD ****** 06/21/10 ************************************
     D DEVN1SP2        C                   CONST('DEVIATION 1 SPLIT 2')
     D DVEF1SP2        C                   CONST('DEVIATION 1 SPLIT 2 EFF DT')
     D DVEX1SP2        C                   CONST('DEVIATION 1 SPLIT 2 EXP DT')
     D DEVN2SP2        C                   CONST('DEVIATION 2 SPLIT 2')
     D DE2EFSP2        C                   CONST('DEVIATION 2 SPLIT 2 EFF DT')
     D DE2EXSP2        C                   CONST('DEVIATION 2 SPLIT 2 EXP DT')
     D @2NPER          C                   CONST('2nd prd')
     D*** END   ADD ****** 06/21/10 ************************************
     D*
     D*** BEGIN ADD ****** 01/14/10 ************************************
     D STFN2           C                   CONST('STABILIZATION FACTOR')
     D STFEF           C                   CONST('STAB FACTOR EFF DT')
     D STFEX           C                   CONST('STAB FACTOR EXP DT')
     D*** END   ADD ****** 01/14/10 ************************************
     D EXMO1           C                   CONST('EXPERIENCE MODIFIER -
     D                                     1')
     D EXEF1           C                   CONST('EXP MOD EFFECT DATE -
     D                                     1')
     D EXMO2           C                   CONST('EXPERIENCE MODIFIER -
     D                                     2')
     D EXEF2           C                   CONST('EXP MOD EFFECT DATE -
     D                                     2')
     D EXMO3           C                   CONST('EXPERIENCE MODIFIER -
     D                                     3')
     D EXEF3           C                   CONST('EXP MOD EFFECT DATE -
     D                                     3')
     D CONTR           C                   CONST('CONTRACTORS ADJ CRED-
     D                                     IT')
     D CONEF           C                   CONST('CONTRACTORS EFF. DAT-
     D                                     E')
     D CONEX           C                   CONST('CONTRACTORS EXP. DAT-
     D                                     E')
     D SUBJA           C                   CONST('SUBJ PREMIUM CREDIT -
     D                                     1 A PERCENT')
     D SUBEF           C                   CONST('SUBJ PREM 1A EFF. DA-
     D                                     TE')
     D SUBEX           C                   CONST('SUBJ PREM 1A EXP. DA-
     D                                     TE')
     D SUBJB           C                   CONST('SUBJ PREMIUM CREDIT -
     D                                     1 B PERCENT')
     D SUBBE           C                   CONST('SUBJ PREM 1B EFF. DA-
     D                                     TE')
     D SUBBX           C                   CONST('SUBJ PREM 1B EXP. DA-
     D                                     TE')
     D SCHFL           C                   CONST('SCHEDULE DB/CR FACTO-
     D                                     R')
     D SCHEF           C                   CONST('SCHEDULE DB/CR EFF D-
     D                                     T')
     D SCHEX           C                   CONST('SCHEDULE DB/CR EXP D-
     D                                     T')
     D DISCT           C                   CONST('DISC TYPE S,G')
     D WAIVE           C                   CONST('WAIVER TYPE')
     D LVLPR           C                   CONST('WAIVER SURCHRG PREM -
     D                                     LVL')
     D WSPEC           C                   CONST('WAIVER % (SPECIFIC=N-
     D                                     ET%)')
     D MINWAV          C                   CONST('MINIMUM WAIVER SURCH-
     D                                     ARGE')
     D FLATCH          C                   CONST('WAIVER FLAT CHARGE')
     D MAXWAV          C                   CONST('WAIVER MAX CHRG')
     D DEDC2           C                   CONST('2ND DEDUCTIBLE DISCO-
     D                                     UNT CODE')
     D DED2E           C                   CONST('2ND DEDUCTIBLE EFF. -
     D                                     DATE')
     D DED2X           C                   CONST('2ND DEDUCTIBLE EXP. -
     D                                     DATE')
     D INCR2           C                   CONST('INCREASED LIMITS 2 C-
     D                                     ODE')
     D INCEF           C                   CONST('INC LIMITS 2 EFF DT')
     D INCEX           C                   CONST('INC LIMITS 2 EXP DT')
     D ACTST           C                   CONST('ACTIVITY STATUS')
     D SHCOF           C                   CONST('SHORT TERM TO ESTA B-
     D                                     LISH CONCURRENCY')
     D TYPBC           C                   CONST('TYPE OF BUILDING COD-
     D                                     E')
     D PRFIN           C                   CONST('PREMIUM FINANCE CODE')
     D CONAU           C                   CONST('CONTESTED AUDIT')
     D***** END ADD ******* 10/08/07 **********************************
     D*** BEGIN ADD ****** 10/20/09 ************************************
     D ASCTXT          C                   CONST('ACTIVITY STATUS CODE CHANGED')
     D ASCTX1          C                   CONST('Activity Status Changed from')
     D ASCTX2          C                   CONST(' by user ')
     D*** END   ADD ****** 10/20/09 ************************************
     D*** BEG ADD ***** 01/14/10 ***************************************
      *
      * Hex codes to condition display without using indicators
      *
     d editLowLine     c                   const(x'24')
     d editHighLine    c                   const(x'26')
     d displayLow      c                   const(x'A0')
     d displayHigh     c                   const(x'A2')
     d hideProtect     c                   const(x'A7')
     d***  Begin Add  ***  02/11/11  **************************************
     d normal          c                   const(x'20')
     d nonDisplay      c                   const(x'27')
     d***  Begin Add  ***  02/11/11  **************************************
     D*** END ADD ***** 01/14/10 ***************************************
     D*** Beg Add *** 11/16/09 *****************************************
     D*** Beg Chg *** 07/28/10 ***********************************************
     D*****XXRQD1           S                   LIKE(EXERQ1)
     D*****XXRQD2           S                   LIKE(EXERQ1)
     DXXRQD1           S                   LIKE(FHFH1Q)
     DXXRQD2           S                   LIKE(FHFH2Q)
     DXXRQD3           S                   LIKE(FHFH3Q)
     DXXRQD4           S                   LIKE(FHFH4Q)
     DXXRQD5           S                   LIKE(FHFH5Q)
     D*** Beg Add *** 10/25/10 ***********************************************
     DXXRQD6           S                   LIKE(FHFH6Q)
     D*** End Add *** 10/25/10 ***********************************************
     D*** End Chg *** 07/28/10 ***********************************************
     D*** End Add *** 11/16/09 *****************************************
     D***  Begin Add  ***  02/27/10  **************************************
     DsaveScBsCD       s                   like(scBsCd)
     DsaveBSLPGM       s                   like(BSLPGM)
     DclearWMLDP       s              1n
     Dfirst            s              1n   inz(*on)
     D***  End   Add  ***  02/27/10  **************************************
     D****** Beg Add ***** 12/08/11 ***************************************
      *** beg del ***** 01/12/12 ******************************************
     D*********** SAVDIV           s              5  0 inz(99999)
      *** end del ***** 01/12/12 ******************************************
     D****** End Add ***** 12/08/11  **************************************
      *** beg add ***** 01/12/12 ******************************************
     d kkdiv           s                   like(ELDIV)
     d kkefdt          s                   like(EMEFD1)
     d workDate        s               d
      *** end add ***** 01/12/12 ******************************************
     D***  Begin Add  ***  07/06/11  **************************************
     DdeductInstalled  s              1n
     D*** beg add ***** 08/22/11 ******************************************
     d WC1918instald   s              1n
     d svffla          s              1
     D*** end add ***** 08/22/11 ******************************************
     D***  Begin Add  ***  08/25/10  **************************************
     DsaveEFEXCF       s                   like(EFEXCF)
     DsaveEFEXCO       s                   like(EFEXCO)
     DsaveEFSCBD       s                   like(EFSCBD)
     D***  End   Add  ***  08/25/10  **************************************
     D***  Begin Add  ***  09/13/10  **************************************
     DI@94             s              1n   inz(*oFF)
     D***  End Add    ***  09/13/10  **************************************
      *
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D  QCFND                  4      6  0
     D                 DS
     D  XXDT                   1      7  0
     D  XXYY                   1      3  0
     D  XXMM                   4      5  0
     D  XXDD                   6      7  0
      ****  BEG  ADD 03/20/06 *****************************************
     D  XXMDD                  4      7  0
      ****  BEG  ADD 03/20/06 *****************************************
     D  XXMMDD                 4      7  0
     D                 DS
     D  XXPFD                  1      8  0
     D  XXPDD                  3      4  0
     D                 DS
     D  XXWK80                 1      8  0
     D  XXWDD                  3      4  0
     D                 DS
     D  BDAYS                  1      8  0
     D  XXBDD                  3      4  0
     D                 DS
     D  SC2                    1     66
     D                                     DIM(33)
     D  SCSRBB                 1      2
     D  TEMP01                 3      4
     D  TEMP02                 5      6
     D  SCCDEB                 7      8
     D  SCCDEP                 9     10
     D  SCCDET                11     12
     D  SCRDBL                13     14
     D  SCRDPY                15     16
     D  TEMP03                17     18
     D  TEMP04                19     20
     D  SCNBPT                21     22
     D  SCNBTR                23     24
     D  TEMP05                25     26
     D  TEMP06                27     28
     D  SCPRTR                29     30
     D  TEMP07                31     32
     D  TEMP08                33     34
     D  SCDPTR                35     36
     D  SCTXBL                37     38
     D  SCTXPY                39     40
     D  SCTXTX                41     42
     D  SCMFBL                43     44
     D  SCMFPY                45     46
     D  SCMFPT                47     48
     D  SCSFBL                49     50
     D  SCSFPY                51     52
     D  SCSFTX                53     54
     D  SCLPBL                55     56
     D  SCLPPY                57     58
     D  TEMP09                59     60
     D  SCCRBL                61     62
     D  SCCRPY                63     64
     D  TEMP10                65     66
     D                 DS
     D  SFP                    1     15  0
     D                                     DIM(3)
     D  SCSFP1                 1      5  0
     D  SCSFP2                 6     10  0
     D  SCSFP3                11     15  0
     D                 DS
     D  SFA                    1     21  0
     D                                     DIM(3)
     D  SCSFA1                 1      7  0
     D  SCSFA2                 8     14  0
     D  SCSFA3                15     21  0
     D                 DS
     D  SFD                    1     24  0
     D                                     DIM(3)
     D  SCSFD1                 1      8  0
     D  SCSFD2                 9     16  0
     D  SCSFD3                17     24  0
     D*** BEG ADD ***** 02/03/98 ***********************************************
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D                 DS
     D  XXZIP                  1      9
     D  XXZIP5                 1      5
     D  XXZIP4                 6      9
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
     D*** END ADD ***** 02/03/98 ***********************************************
     D**** BEGIN ADD **** 03/06/98 *********************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D**** END   ADD **** 03/06/98 *********************************************
     D***  BEGIN ADD  ***  06/01/98  *********************************
     D                 DS
     D  X7MXDT                 1      7  0
     D  X7MXCY                 1      3  0
     D  X7MXMM                 4      5  0
     D***  END   ADD  ***  06/01/98  *********************************
     D*** BEG ADD ***** 03/20/06 *************************************
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
     D*** END ADD ***** 03/20/06 *************************************

     D useDots         s              1n   inz('0')
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *** END ADD ***** 03/20/06 *************************************
     D*** BEGIN ADD ****** 10/20/09 ************************************
     DWriteDarts       PR
     D  dkey                         40
     D  comm                        256
     D  rmcd                         10
     D*** END   ADD ****** 10/20/09 ************************************
     D*** Beg Add *** 07/28/10 ***********************************************
     D@#per            c                   '. . . . . . . . . :'
     D@#per25          c                   '. . . . . . . . . . . . :'
     D@@per            s             19    inz('                  :')
     D@@per25          s             25    inz('                        :')
     D@@arr1           S              1    dim(13)
     D@@arr2           S              1    dim(19)
     D@@arr3           S              1    dim(19)
     D@@arr4           S              1    dim(25)
     D@@len            S              3  0
     D@@rem            S              3  0
     D*** End Add *** 07/28/10 ***********************************************
     D*** Beg Add *** 12/30/10 ***********************************************
     D@@ptyp           C                   'Type of policy'
     D*** End Add *** 12/30/10 ***********************************************
     D*
     D***  Begin Add  ***  11/10/10  **************************************
     c/copy wPrEmplr
     D***  End   Add  ***  11/10/10  **************************************
     D*** begin add ***  12/31/10  ***************************************************
     D xxf4dt          s                   like(elpfdt)
     D xxf4ky          s                   like(f4key)
     D xxstat          s                   like(f4stat)
     D @@ftim          s              1    inz('Y')
     D*** end   add ***  12/31/10  ***************************************************
     D*
     D***** BEG ADD ******* 05/07/12 **********************************
     D##datein         S              8  0
     D##fromfmt        S              8
     D##tofmt          S              8
     D@@dateout        S             10
     D@@err            S              1

     DSRCVTDAT         PR                  Extpgm('SRCVTDAT')
     D##datein                        8  0
     D##fromfmt                       8
     D##tofmt                         8
     D@@dateout                      10
     D@@err                           1
     D***** END ADD ******* 05/07/12 **********************************
     c/COPY SCOPYMT05C
      *
     I***** BEG ADD ******* 10/08/07 **********************************
     I*****PELL5
     I*****         ELWVTP                      ZZWVTP
     I****          ELWVFL                      ZZWVFL
     I****          ELWVPC                      ZZWVPC
     I****          ELWMIN                      ZZWMIN
     I*****         ELFCHG                      ZZFCHG
     I*****         ELWMAX                      ZZWMAX
     I*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      ***** BEG ADD ******* 07/20/00 **********************************
     C                   PARM                    ##LVL             3 0
     C                   PARM                    ##TYPE            1
      ***** END ADD ******* 07/20/00 **********************************
     C*
     C****** BEG ADD ***** 07/07/05 **************************************
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C****** END ADD ***** 07/07/05 **************************************
     C*
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C     SRCHPL        PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    ##ST              2
      ***** BEG ADD ******* 08/12/04 **********************************
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      ***** BEG ADD ******* 04/07/05 **********************************
      *
      *  Certificate Holder
      *
     C     CEKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    K1EMP#
     C                   KFLD                    K1DIV
      *
      ***** END ADD ******* 04/07/05 **********************************
      *
      *  Renewal Tracking
      *
     C     RTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
      *
      ***** END ADD ******* 08/12/04 **********************************
     C*
     C* EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C*
     C***** BEG ADD ******* 01/12/99 **********************************
     C     EMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C***** END ADD ******* 01/12/99 **********************************
     C*
     C*** BEG ADD ***** 08/02/05 **************************************
     C     EMKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*** END ADD ***** 08/02/05 **************************************
     C*** BEG ADD ***** 03/06/06 **************************************
     C     EMKY4         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    X#FYR
     C*** END ADD ***** 03/06/06 **************************************
     C*
     C* EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     ELKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
      ***** BEG ADD ******* 11/09/01 **********************************
     C     ELKY3         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
      ***** END ADD ******* 11/09/01 **********************************
      ***** BEG ADD ******* 04/07/05 **********************************
      *
     C     ELKY4         KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    K1EMP#
     C                   KFLD                    K1DIV
     C                   KFLD                    SVFYR
      *
     C     ELKY5         KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
      ***** BEG ADD ******* 12/18/09 **********************************
     C     ELKY6         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    EMDIV
      ***** END ADD ******* 12/18/09 **********************************
      *
      ***** END ADD ******* 04/07/05 **********************************
      *** BEG ADD ***** 08/01/05 **************************************
     C     ELKYL1        KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
      *
     C     ELKYL2        KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *** END ADD ***** 08/01/05 **************************************
     C*
     C***** Beg Add ******* 12/08/11 **********************************
     C*** beg del ***** 01/12/12 **************************************
     C*****ELKEY*        KLIST
     C***********        KFLD                    KKCO#
     C***********        KFLD                    KKFND
     C***********        KFLD                    KKEMP#
     C***********        KFLD                    savDiv
     C***********
     C*****ELKEY4        KLIST
     C***********        KFLD                    KKCO#
     C***********        KFLD                    KKFND
     C***********        KFLD                    KKEMP#
     C***********        KFLD                    EMDIV
     C***********        KFLD                    ZZFYR
     C*** end del ***** 01/12/12 **************************************
     C***** End Add ******* 12/08/11 **********************************
     C*** beg add ****** 01/12/12 *************************************
     c     elky2l        klist
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     c                   kfld                    kkdiv

     c     elky2l1       klist
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     c                   kfld                    kkdiv
     c                   kfld                    kkefdt
     C*** end add ****** 01/12/12 *************************************
      ***** BEG ADD ******* 11/30/99 **********************************
      *
      *  MCO group code
      *
     C     MGKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCMCO
      *
     C     MGKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCMCO
     C                   KFLD                    ##PFDT
      *
      ***** END ADD ******* 11/30/99 **********************************
     C***** BEG ADD ******* 08/10/98 **********************************
     C*
     C*  Employer Cross Reference
     C*
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C***** END ADD ******* 08/10/98 **********************************
     C*
     C*  GROUP MASTER/AUXILIARY FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C**** BEGIN ADD **** 10/14/98 *********************************************
     C*
     C* KEY LIST FOR THE GROUP AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C**** END   ADD **** 10/14/98 *********************************************
     C*
     C*  GROUP DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C*
     C     F1KY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C*
     C***  BEGIN ADD  ***  08/13/98  *********************************
     C     F1KY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
      ***** BEG CHG ******* 02/28/00 **********************************
     C**************       KFLD           SYSDT
     C                   KFLD                    EMEFD1
      ***** END CHG ******* 02/28/00 **********************************
     C***  END   ADD  ***  08/13/98  *********************************
     C*
     C*  DISCOUNT FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    SCDISC
     C                   KFLD                    XXPFDT
     C*
     C     DSKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    SCDISC
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     EPKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXPFDT
     C*
     C     LMKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C***  BEGIN CHG  ***  04/07/05-2  **********************************
     C**********           KFLD           SCDCDE
     C                   KFLD                    XXDDCD
     C***  END   CHG  ***  04/07/05-2  **********************************
     C                   KFLD                    XXPFDT
     C*
     C     DDKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C***  BEGIN CHG  ***  04/07/05-2 ***********************************
     C**********           KFLD           SCDCDE
     C                   KFLD                    XXDDCD
     C***  END   CHG  ***  04/07/05-2 ***********************************
     C*
     C     DDKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C*
     C***  BEG  ADD  ****  09/22/09 *************************************
     C*   KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*   FETCH BASED ON ARD DATE.
     C     DDKY3         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDT             7 0
     C***  END  ADD  ****  09/22/09 *************************************
     C*
     C*  PAYROLL CLASSIFICATION FILE (FOR RATE SET CODE) WMPRP
     C*
     C     PRKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCRTCD
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCRTCD
      ***** BEG ADD ******* 05/02/00 **********************************
      *
     C     RSKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELRTCD
      *
      ***** END ADD ******* 05/02/00 **********************************
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C*
     C*  AGENCY BILLING DETAIL FILE
     C*
     C     ABKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C*  STATISTICAL REPORTING/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXPFDT
     C*
     C***  BEGIN ADD  ***  02/10/00  *********************************
     C     RRKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDATE
     C***  END   ADD  ***  02/10/00  *********************************
     C*
     C     RRKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
      ***** BEG ADD ******* 05/02/00 **********************************
      *
     C     RRKY3         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDTE
      *
      ***** END ADD ******* 05/02/00 **********************************
     C*
     C*  KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*  TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*** BEG ADD ***** 02/03/98 ***********************************************
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*** END ADD ***** 02/03/98 ***********************************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  PREMIUM FINANCE COMPANY KEY LIST
     C*
     C     PFKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFCOD
     C*
     C     PFKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFCOD
     C                   KFLD                    KKPFDT
     C*
     C***** END   ADD **** 09/29/99 ***********************************
      ***** BEG ADD ******* 07/20/00 **********************************
      *
      *  Claims Master
      *
     C     CMKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXDATE
      *
     C     CMKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      * Admin/DARTS Control Definition
      *
     C     ADKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      ***** END ADD ******* 07/20/00 **********************************
     C***  BEGIN ADD  ***  10/29/01  *********************************
     C*
     C     EAKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C***  END   ADD  ***  10/29/01  *********************************
     C*
     C***  BEGIN DEL  ***  01/09/04  *********************************
     C***  BEG   ADD  ***  02/18/03  *********************************
     C***********VPKY      KLIST
     C***********          KFLD           EMCO#
     C***********          KFLD           SCVDR#
     C***  END   ADD  ***  02/18/03  *********************************
     C***  END   DEL  ***  01/09/04  *********************************
     C*** BEG ADD ***** 04/07/08 *************************************
     C     VPKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    KKVDR#            5 0
     C*** END ADD ***** 04/07/08 *************************************
     C*
      ***** BEG ADD ******* 10/05/04 **********************************
     C*
     C*  Type of Building Code file
     C*
     C     BUKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCTYPB
      ***** END ADD ******* 10/05/04 **********************************
     C*** BEGIN ADD ***  10/14/05  ************************************
      *
      *   EXCLUDED STATES FILE
      *
     C     XSKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    X1EMP#            9 0
     C                   KFLD                    X1FYR             3 0
     C*
     C     XSKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    SCFYR
     C                   KFLD                    XSST
     C*** END ADD *****  10/14/05  ************************************
     C*
     C*
      *+*+  BEG ADD *+*+*+  08/24/06 *+*+*+*+*+*+*+*+**+*+*+*+*+*+*+
      *
      *  Group Rate Set Code
      *
     C     GRKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
      *
     C     GRKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCRTCD
      *
      *+*+  END ADD *+*+*+  08/24/06 *+*+*+*+*+*+*+*+**+*+*+*+*+*+*+
      ***** BEG ADD ***** 07/11/06 ************************************
      *
      *  KEYS FOR SPREAD BILLINGS ADJUSTMENTS EDIT
      *
     C     ELKYC         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
      *
     C     ELKYD         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
      *
     C     EAKYC         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
      ***** END ADD ***** 07/11/06 ************************************
      ***** BEG ADD ******* 07/27/07 **********************************
      * WORKERS' COMP RATE SET CODE MASTER FILE
      *
      ***** BEG CHG ******* 10/08/07 **********************************
     C     DKKY          KLIST
     C                   KFLD                    E2CO#
     C                   KFLD                    E2RTCD
      *
     C     DKKY_1        KLIST
     C                   KFLD                    E3CO#
     C                   KFLD                    E3RTCD
      *
     C     DKKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    ELPFDT
      *
     C     DKKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
      *
     C     DKKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C                   KFLD                    ELPFDT
      *
     C     DKKY4         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
      *
     C     DKKY5         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
      *
     C     DKKY6         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
      *
     C     DKKY7         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDISC
     C                   KFLD                    ELPFDT
      *
     C     DKKY8         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDISC
      *
     C**** DKKY***       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELRTCD
      ************
     C*****DKKY1**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
     C************       KFLD                    XXKEY
     C************       KFLD                    ELPFDT
      ************
     C*****DKKY2**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
     C************       KFLD                    XXKEY
      ************
     C*****DKKY3**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
     C************       KFLD                    XXDDCD
     C************       KFLD                    ELPFDT
      ************
     C*****DKKY4**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
     C************       KFLD                    XXDDCD
      ************
     C*****DKKY5**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
     C************       KFLD                    ELPFDT
      ************
     C*****DKKY6**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
      ************
     C*****DKKY7**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
     C************       KFLD                    ELDISC
     C************       KFLD                    ELPFDT
      ************
     C*****DKKY8**       KLIST
     C************       KFLD                    ELCO#
     C************       KFLD                    ELFND
     C************       KFLD                    XXST
     C************       KFLD                    ELDISC
      ************
      ***** END CHG ******* 10/08/07 **********************************
      ***** BEG CHG ******* 10/18/07 **********************************
     C********** DKKY9     KLIST
     C**********           KFLD           ELCO#
     C**********           KFLD           ELFND
     C**********           KFLD           ELFYR
     C**********           KFLD           ELEMP#
     C**********           KFLD           ELDIV
     C     DKKY9         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      ***** END CHG ******* 10/18/07 **********************************
      *
     C     DKKY10        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      *
     C     DKKY11        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
     C     WXU_K         KLIST
     C                   KFLD                    QQUSER
     C                   KFLD                    QQDATE
     C                   KFLD                    QQTIME
     C***** END ADD ******* 07/27/07 **********************************
     C***** BEG ADD ******* 10/08/07 **********************************
     C     DKKY12        KLIST
     C                   KFLD                    UAUSER
     C                   KFLD                    UADT
     C                   KFLD                    UATIME
     C                   KFLD                    UACO#
     C                   KFLD                    UAFND
     C                   KFLD                    UAFYR
     C                   KFLD                    UAEMP#
     C                   KFLD                    @@ST              2
     C                   KFLD                    UADIV
     C                   KFLD                    UADATA
     C*
     C     DKKY13        KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C                   KFLD                    @@FYR
     C*
     C     DMKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
      *
     C     DKKY14        KLIST
     C                   KFLD                    @#CO#
     C                   KFLD                    @#FND
      *
     C     DKKY15        KLIST
     C                   KFLD                    @#CO#
     C                   KFLD                    @#FND
     C                   KFLD                    @#EMP#
     C                   KFLD                    @#FYR
      *
     C     DKKY16        KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@EMP#
     C                   KFLD                    @@DIV
     C                   KFLD                    XXDATE
      *
     C     DKKY17        KLIST
     C                   KFLD                    @#CO#
     C                   KFLD                    @#FND
     C                   KFLD                    @#FYR
     C***** BEG ADD ******* 10/08/07 **********************************
     C*** BEG ADD ****** 06/06/09 ************************************
     C     GIKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ##TBID            4
     C*
     C*** END ADD ****** 06/06/09 ************************************
     C*** BEG ADD *** 01/19/10 ***************************************
     C     GIKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ##TBID            4
     C*
     C*** END ADD *** 01/19/10 ***************************************
     C*** BEG ADD ***** 12/30/09 **************************************
      * KEY LIST OF THE LARGE DEDUCTIBLE FILE (WMLDP)
      *
     C     LDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*** END ADD ***** 12/30/09 **************************************
     C*
     C***  Begin Add  ***  02/27/10  **************************************
     C     dDDKY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C***  Ed  n Add  ***  02/27/10  **************************************
      *
     C*** BEG ADD ***** 01/25/10 **************************************
      * KEY LIST OF THE EMPLOYER DETAILS FEE FILE (WDEFL1)
      *
     C     EFKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     CIKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXPFDT
      *
     C     CIKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
      *
     C*** END ADD ***** 01/25/10 **************************************
     C*** Beg Add *** 07/28/10 ***********************************************
     C     TBKEY         KLIST
     C                   KFLD                    FHCO#
     C                   KFLD                    XXFTB
     C                   KFLD                    XXFFD
     C*** End Add *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C     exky1         KLIST
     C                   KFLD                    emco#
     C                   KFLD                    emfnd
     C*** End Add *** 12/30/10 ***********************************************
     C*** BEG ADD ***** 08/30/10 **************************************
     C     AUKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*** END ADD ***** 08/30/10 **************************************
     C*** beg add ***** 12/28/10 **************************************
     C     hcky          klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    xxst
     C     hcky1         klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    xxst
     C                   kfld                    xxDate
     C*** end add ***** 12/28/10 **************************************
     C*** begin add ***  12/31/10  ***************************************************
     C*WDF4P Keylist.
     C     xxfkey        klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    xxf4ky
     C                   kfld                    xxf4dt
     C*
     C     xxfkey1       klist
     C                   kfld                    emco#
     C                   kfld                    emfnd
     C                   kfld                    xxf4ky
     C*** end   add ***  12/31/10  ***************************************************
      *
      *
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          wdelp
     C                   PARM                    @@RRN2            9 0          wmemp
     C                   PARM                    @@QTE             1
     C*
     C*** Beg Add *** 07/28/10 ***********************************************
     C     *LIKE         DEFINE    SCFFD1        XXFFD
     C     *LIKE         DEFINE    FHF1TB        XXFTB
     C*** End Add *** 07/28/10 ***********************************************
      ***** BEG ADD ******* 03/06/07 **********************************
     C     *LIKE         DEFINE    EXAJFL        SVAJFL
      ***** END ADD ******* 03/06/07 **********************************
      ***** BEG ADD ******* 10/20/09 **********************************
     C     *LIKE         DEFINE    EXASRM        SVASRM
      ***** END ADD ******* 10/20/09 **********************************
      ***** BEG ADD ******* 10/18/07 **********************************
     C     *LIKE         DEFINE    FNPEOF        QQMPO
      ***** END ADD ******* 10/18/07 **********************************
      ***** BEG ADD ******* 04/07/05 **********************************
     C     *LIKE         DEFINE    CECER#        XXCER#
     C     *LIKE         DEFINE    CEEMP#        K1EMP#
     C     *LIKE         DEFINE    CEDIV         K1DIV
     C     *LIKE         DEFINE    ELCO#         SVCO#
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    ELFYR         SVFYR
      ***** END ADD ******* 04/07/05 **********************************
      ***** BEG ADD ******* 08/14/00 **********************************
     C     *LIKE         DEFINE    EMCAD1        X1CXDT
      ***** END ADD ******* 08/14/00 **********************************
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    SCDVH1        XXDVHD
     C     *LIKE         DEFINE    SCPFDT        XXPF
     C     *LIKE         DEFINE    SCPTDT        XXPT
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELPTDT        XXPTDT
     C     *LIKE         DEFINE    ELANDT        XXANDT
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    SCPFDT        XXEF
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    SCPTDT        XXEX
     C     *LIKE         DEFINE    ELPTDT        XXEXDT
     C     *LIKE         DEFINE    ELPFDT        XXDAT1
     C     *LIKE         DEFINE    ELPTDT        XXDAT2
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C*** BEG ADD ***** 02/03/98 ***********************************************
     C     *LIKE         DEFINE    EMPAD1        XXADR1
     C     *LIKE         DEFINE    EMPAD1        XXADR2
     C     *LIKE         DEFINE    EMPAD1        XXADR3
     C     *LIKE         DEFINE    SCILCD        S#ILCD
     C     *LIKE         DEFINE    SCINL2        S#INL2
     C     *LIKE         DEFINE    SCILDS        S#ILDS
     C     *LIKE         DEFINE    SCI2DS        S#I2DS
     C     *LIKE         DEFINE    SCEXM1        S#EXM1
     C     *LIKE         DEFINE    SCEXM2        S#EXM2
     C     *LIKE         DEFINE    SCEXM3        S#EXM3
     C     *LIKE         DEFINE    SCEMD1        S#EXD1
     C     *LIKE         DEFINE    SCEMD2        S#EXD2
     C     *LIKE         DEFINE    SCEMD3        S#EXD3
     C     *LIKE         DEFINE    ENREQ#        XXREQ#
     C*** END ADD ***** 02/03/98 ***********************************************
     C***  BEGIN ADD  ***  06/01/98  *********************************
     C     *LIKE         DEFINE    FNMXEX        XXMXEX
     C     *LIKE         DEFINE    X7MXCY        XXMXCY
     C***  END   ADD  ***  06/01/98  *********************************
     C***  BEGIN ADD  ***  08/13/98  *********************************
     C     *LIKE         DEFINE    F1STPO        SYSDT
     C***  END   ADD  ***  08/13/98  *********************************
     C***** BEG ADD ******* 01/12/99 **********************************
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C***** END ADD ******* 01/12/99 **********************************
     C***  BEGIN ADD  ***  02/10/00  *********************************
     C     *LIKE         DEFINE    EMEFD1        XXDATE
     C***  END   ADD  ***  02/10/00  *********************************
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C     *LIKE         DEFINE    ABRST         XXRST
     C     *LIKE         DEFINE    ABRST         SVRST
     C***  END   ADD  ***  07/19/99  *********************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C     *LIKE         DEFINE    ELPFDT        KKPFDT
     C***** END   ADD **** 09/29/99 ***********************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     *LIKE         DEFINE    ELBSCD        SVBSCD
     C     *LIKE         DEFINE    ELBSDT        SVBSDT
     C*** END ADD *****  11/15/99  ************************************
      ***** BEG ADD ******* 03/24/00 **********************************
     C     *LIKE         DEFINE    ELEMD1        X1EMD1
     C     *LIKE         DEFINE    ELEMD2        X1EMD2
     C     *LIKE         DEFINE    ELEMD3        X1EMD3
      ***** END ADD ******* 03/24/00 **********************************
      ***** BEG ADD ******* 05/02/00 **********************************
      *
     C     *LIKE         DEFINE    RRDV1H        SVDV1H
     C     *LIKE         DEFINE    RRDV2H        SVDV2H
     C     *LIKE         DEFINE    RR1AHD        SV1AHD
     C     *LIKE         DEFINE    RR1BHD        SV1BHD
     C     *LIKE         DEFINE    RRCRHD        SVCRHD
     C     *LIKE         DEFINE    RRDBHD        SVDBHD
     C     *LIKE         DEFINE    RRWVHD        SVWVHD
      *
     C     *LIKE         DEFINE    RRRDPR        SVRDPR
     C     *LIKE         DEFINE    RRDV1L        SVDV1L
     C     *LIKE         DEFINE    RRDV2L        SVDV2L
     C     *LIKE         DEFINE    RRDSPR        SVDSPR
     C     *LIKE         DEFINE    RRSFAP        SVSFAP
     C     *LIKE         DEFINE    RRSFPC        SVSFPC
     C     *LIKE         DEFINE    RRSFAM        SVSFAM
     C     *LIKE         DEFINE    RRTXPC        SVTXPC
     C     *LIKE         DEFINE    RRTDFL        SVTDFL
     C     *LIKE         DEFINE    RR1STP        SV1STP
     C     *LIKE         DEFINE    RR1STL        SV1STL
     C     *LIKE         DEFINE    RR2NDP        SV2NDP
     C     *LIKE         DEFINE    RR2NDL        SV2NDL
     C*** BEGIN ADD ***  01/02/01  ************************************
     C     *LIKE         DEFINE    RR3RDP        SV3RDP
     C     *LIKE         DEFINE    RR3RDL        SV3RDL
     C     *LIKE         DEFINE    RR3RDH        SV3RDH
     C     *LIKE         DEFINE    RR3RDF        SV3RDF
     C     *LIKE         DEFINE    RR3IDD        SV3IDD
     C     *LIKE         DEFINE    RR3IDV        SV3IDV
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
     C     *LIKE         DEFINE    RR4THP        SV4THP
     C     *LIKE         DEFINE    RR4THL        SV4THL
     C     *LIKE         DEFINE    RR4THH        SV4THH
     C     *LIKE         DEFINE    RR4THF        SV4THF
     C     *LIKE         DEFINE    RR4IDD        SV4IDD
     C     *LIKE         DEFINE    RR4IDV        SV4IDV
     C     *LIKE         DEFINE    RR5THP        SV5THP
     C     *LIKE         DEFINE    RR5THL        SV5THL
     C     *LIKE         DEFINE    RR5THH        SV5THH
     C     *LIKE         DEFINE    RR5THF        SV5THF
     C     *LIKE         DEFINE    RR5IDD        SV5IDD
     C     *LIKE         DEFINE    RR5IDV        SV5IDV
     C*** END ADD *****  07/02/04  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C     *LIKE         DEFINE    RR6THP        SV6THP
     C     *LIKE         DEFINE    RR6THL        SV6THL
     C     *LIKE         DEFINE    RR6THH        SV6THH
     C     *LIKE         DEFINE    RR6THF        SV6THF
     C     *LIKE         DEFINE    RR6IDD        SV6IDD
     C     *LIKE         DEFINE    RR6IDV        SV6IDV
     C     *LIKE         DEFINE    RR7THP        SV7THP
     C     *LIKE         DEFINE    RR7THL        SV7THL
     C     *LIKE         DEFINE    RR7THH        SV7THH
     C     *LIKE         DEFINE    RR7THF        SV7THF
     C     *LIKE         DEFINE    RR7IDD        SV7IDD
     C     *LIKE         DEFINE    RR7IDV        SV7IDV
     C***  End   Add  ***  12/11/08  **************************************
     C     *LIKE         DEFINE    RRASFL        SVASFL
     C     *LIKE         DEFINE    RRWVFL        SVWVFL
     C     *LIKE         DEFINE    RRACC         SVACC
     C     *LIKE         DEFINE    RRDIS         SVDIS
     C     *LIKE         DEFINE    RRAGG         SVAGG
     C     *LIKE         DEFINE    RREMLB        SVEMLB
     C     *LIKE         DEFINE    RRSPFL        SVSPFL
     C     *LIKE         DEFINE    RRYRAD        SVYRAD
     C     *LIKE         DEFINE    RRCXAD        SVCXAD
     C     *LIKE         DEFINE    RRIDTP        SVIDTP
     C     *LIKE         DEFINE    RRTXHD        SVTXHD
     C     *LIKE         DEFINE    RRWDTP        SVWDTP
     C     *LIKE         DEFINE    RRWDPC        SVWDPC
     C     *LIKE         DEFINE    RRWDMN        SVWDMN
     C     *LIKE         DEFINE    RRWDFC        SVWDFC
     C     *LIKE         DEFINE    RR1STH        SV1STH
     C     *LIKE         DEFINE    RR1STF        SV1STF
     C     *LIKE         DEFINE    RR1IDD        SV1IDD
     C     *LIKE         DEFINE    RR1IDV        SV1IDV
     C     *LIKE         DEFINE    RR2NDH        SV2NDH
     C     *LIKE         DEFINE    RR2NDF        SV2NDF
     C     *LIKE         DEFINE    RR2IDD        SV2IDD
     C     *LIKE         DEFINE    RR2IDV        SV2IDV
     C     *LIKE         DEFINE    RRDSIN        SVDSIN
     C     *LIKE         DEFINE    RRDSHD        SVDSHD
     C     *LIKE         DEFINE    RRDISC        SVDISC
     C     *LIKE         DEFINE    RRDSPC        SVDSPC
      *
     C     *LIKE         DEFINE    ELPFDT        XXDTE
      *
      ***** END ADD ******* 05/02/00 **********************************
     C**** BEGIN ADD **** 12/22/00 ************************************
     C     *LIKE         DEFINE    RSJRST        XXSVST
     C**** END ADD ****** 12/22/00 ************************************
     C**** BEG ADD ****** 09/01/04 ************************************
     C     *LIKE         DEFINE    RRVLCH        SVVLCH
     C     *LIKE         DEFINE    ELVLCA        SVVLCA
     C**** END ADD ****** 09/01/04 ************************************
hs01 C**** BEGIN ADD **** 03/06/98 *********************************************
     C*************      Z-ADD     0             I1                3 0
     C**** END   ADD **** 03/06/98 *********************************************
     C*
     C*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C     *LIKE         DEFINE    ELDBCR        XXDBCR
     C     *LIKE         DEFINE    ELSDEF        XXSDEF
     C     *LIKE         DEFINE    ELSDEX        XXSDEX
     C     *LIKE         DEFINE    *IN90         ERROR
     C     *LIKE         DEFINE    ELRTCD        SVRTCD
      *+*+  END ADD *+*+*+* 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C***** BEG ADD ******* 07/27/07 **********************************
     C     *LIKE         DEFINE    uaemp#        #uaemp#
     C     *LIKE         DEFINE    uadiv         #uadiv
     C     *LIKE         DEFINE    ELPFDT        S@PFDT
     C     *LIKE         DEFINE    ELPTDT        S@PTDT
     C     *LIKE         DEFINE    ELARSM        S@ARSM
     C     *LIKE         DEFINE    ELIFFL        S@IFFL
     C***** BEG CHG ******* 10/08/07 **********************************
     C     *LIKE         DEFINE    ELQTYP        S@QTYP
     C*********  *LIKE     DEFN ELMBCD    S@MBCD
     C***** END CHG ******* 10/08/07 **********************************
     C     *LIKE         DEFINE    ELADVR        S@ADVR
     C     *LIKE         DEFINE    ELRLFL        S@RLFL
     C     *LIKE         DEFINE    ELBSDT        S@BSDT
     C     *LIKE         DEFINE    ELSTAT        S@STAT
     C     *LIKE         DEFINE    ELILCD        S@ILCD
     C     *LIKE         DEFINE    ELILEF        S@ILEF
     C     *LIKE         DEFINE    ELILEX        S@ILEX
     C     *LIKE         DEFINE    ELDCDE        S@DCDE
     C     *LIKE         DEFINE    ELDCEF        S@DCEF
     C     *LIKE         DEFINE    ELDCEX        S@DCEX
     C     *LIKE         DEFINE    ELCXBL        S@CXBL
     C     *LIKE         DEFINE    ELASP2        S@ASP2
     C     *LIKE         DEFINE    ELDVEF        S@DVEF
     C     *LIKE         DEFINE    ELDVEX        S@DVEX
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     *LIKE         DEFINE    ELD1S2        S@D1S2
     C     *LIKE         DEFINE    ELD1D2        S@D1D2
     C     *LIKE         DEFINE    ELD1X2        S@D1X2
     C*** END   ADD ****** 06/21/10 ************************************
     C     *LIKE         DEFINE    ELASP1        S@ASP1
     C     *LIKE         DEFINE    ELCREF        S@CREF
     C     *LIKE         DEFINE    ELCREX        S@CREX
     C     *LIKE         DEFINE    ELASPM        S@ASPM
     C     *LIKE         DEFINE    ELMCEF        S@MCEF
     C     *LIKE         DEFINE    ELMCEX        S@MCEX
     C     *LIKE         DEFINE    ELMCCD        S@MCCD
     C     *LIKE         DEFINE    ELMCGP        S@MCGP
     C     *LIKE         DEFINE    ELDEV2        S@DEV2
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     *LIKE         DEFINE    ELSTFC        S@STFC
     C*** END   ADD ****** 01/14/10 ************************************
     C     *LIKE         DEFINE    ELD2EF        S@D2EF
     C     *LIKE         DEFINE    ELD2EX        S@D2EX
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     *LIKE         DEFINE    ELD2S2        S@D2S2
     C     *LIKE         DEFINE    ELD2D2        S@D2D2
     C     *LIKE         DEFINE    ELD2X2        S@D2X2
     C*** END   ADD ****** 06/21/10 ************************************
     C     *LIKE         DEFINE    ELEXM1        S@EXM1
     C     *LIKE         DEFINE    ELEMD1        S@EMD1
     C     *LIKE         DEFINE    ELASPS        S@ASPS
     C     *LIKE         DEFINE    ELSCEF        S@SCEF
     C     *LIKE         DEFINE    ELSCEX        S@SCEX
     C     *LIKE         DEFINE    ELSRPR        S@SRPR
     C     *LIKE         DEFINE    ELASP3        S@ASP3
     C     *LIKE         DEFINE    ELDISC        S@DISC
     C     *LIKE         DEFINE    ELPROM        S@PROM
     C     *LIKE         DEFINE    ELDBCR        S@DBCR
     C     *LIKE         DEFINE    ELSDEF        S@SDEF
     C     *LIKE         DEFINE    ELSDEX        S@SDEX
     C     *LIKE         DEFINE    ELCXDY        S@CXDY
     C     *LIKE         DEFINE    ELPP3C        S@PP3C
     C     *LIKE         DEFINE    ELPPC3        S@PPC3
     C***** BEG CHG ******* 10/08/07 **********************************
     C     *LIKE         DEFINE    ELMFEE        S@MFEE
     C*********  *LIKE     DEFN ELMFAM    S@MFAM
     C***** END CHG ******* 10/08/07 **********************************
     C     *LIKE         DEFINE    ELSFPC        S@SFPC
     C     *LIKE         DEFINE    ELSFAM        S@SFAM
     C     *LIKE         DEFINE    ELSFMN        S@SFMN
     C     *LIKE         DEFINE    ELADDM        S@ADDM
     C     *LIKE         DEFINE    ELANDT        S@ANDT
     C     *LIKE         DEFINE    ELDCD2        S@DCD2
     C     *LIKE         DEFINE    EL2DEF        S@2DEF
     C     *LIKE         DEFINE    EL2DEX        S@2DEX
     C     *LIKE         DEFINE    ELINL2        S@INL2
     C     *LIKE         DEFINE    ELI2EF        S@I2EF
     C     *LIKE         DEFINE    ELI2EX        S@I2EX
     C     *LIKE         DEFINE    ELEXMF        S@EXMF
     C     *LIKE         DEFINE    ELWVTP        S@WVTP
     C     *LIKE         DEFINE    ELWVTP        E@WVTP
     C     *LIKE         DEFINE    ELWVFL        S@WVFL
     C     *LIKE         DEFINE    ELWVFL        E@WVFL
     C     *LIKE         DEFINE    ELWVPC        S@WVPC
     C     *LIKE         DEFINE    ELWVPC        E@WVPC
     C     *LIKE         DEFINE    ELWMIN        S@WMIN
     C     *LIKE         DEFINE    ELWMIN        E@WMIN
     C     *LIKE         DEFINE    ELFCHG        S@FCHG
     C     *LIKE         DEFINE    ELFCHG        E@FCHG
     C     *LIKE         DEFINE    ELWMAX        S@WMAX
     C     *LIKE         DEFINE    ELWMAX        E@WMAX
     C     *LIKE         DEFINE    EMEFD1        S2DATE
     C     *LIKE         DEFINE    ELTIME        S2TIME
     C     *LIKE         DEFINE    ENRUSR        S2USER
     C***** BEG CHG ******* 10/18/07 **********************************
     C     *LIKE         DEFINE    ELASCD        S@ASTS
     C     *LIKE         DEFINE    *IN60         XXCHK
     C*********  *LIKE     DEFN ELASTS    S@ASTS
     C***** END CHG ******* 10/18/07 **********************************
     C     *LIKE         DEFINE    ELCOFL        S@COFL
     C     *LIKE         DEFINE    ELTYPB        S@TYPB
     C     *LIKE         DEFINE    ELPFIN        S@PFIN
     C     *LIKE         DEFINE    ELCTST        S@CTST
     C     *LIKE         DEFINE    *IN20         XXFLG
     C***** END ADD ******* 07/27/07 **********************************
     C***** BEG ADD ******* 10/18/07 **********************************
     C     *LIKE         DEFINE    ELEMP#        @@EMP#
     C     *LIKE         DEFINE    ELFND         @@FND
     C     *LIKE         DEFINE    ELDIV         @@DIV
     C     *LIKE         DEFINE    ELFYR         @@FYR
     C     *LIKE         DEFINE    ELPFDT        @@PFDT
     C     *LIKE         DEFINE    ELEXM2        S@EXM2
     C     *LIKE         DEFINE    ELEXM3        S@EXM3
     C     *LIKE         DEFINE    ELEMD2        S@EMD2
     C     *LIKE         DEFINE    ELEMD3        S@EMD3
     C     *LIKE         DEFINE    *IN20         XXFL5
     C     *LIKE         DEFINE    *IN20         XXFL6
     C     *LIKE         DEFINE    ELCO#         @@CO#
     C     *LIKE         DEFINE    ELCO#         @#CO#
     C     *LIKE         DEFINE    ELDIV         @#DIV
     C     *LIKE         DEFINE    ELEMP#        @#EMP#
     C     *LIKE         DEFINE    ELFND         @#FND
     C     *LIKE         DEFINE    ELFYR         @#FYR
     C     *LIKE         DEFINE    ELDISC        @#DISC
     C***** END ADD ******* 10/18/07 **********************************
     C*** BEG ADD ***** 09/22/09 ***********************************************
     C     *LIKE         DEFINE    SCDCDE        SVDCDE
     C     *LIKE         DEFINE    SCDCD2        SVDCD2
     C*** END ADD ***** 09/22/09 ***********************************************
     C***  Begin Chg  ***  10/02/06  **************************************
     C***COPY SCOPYMT01C
     C***  BEGIN chg  ***  10/08/07  *********************************
     C**************/COPY SCOPYMT05C
     C***  End   Chg  ***  10/08/07  **************************************
     C/EJECT
     C*===============================================================
     C* BILAUX - SUBROUTINE FOR GETTING THE BILLING AUXILLARY CODE
     C*===============================================================
     C*
     C     BILAUX        BEGSR
     C*
     C***  BEGIN ADD  ***  02/13/98  *********************************
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KY1         CHAIN     PF101                              60
     C***  END   ADD  ***  02/13/98  *********************************
     C*
     C*  VALIDATE THE BILLING SCHEDULE TYPE IF ENTERED
      *** BEG CHG ***** 04/11/03 ***************************************
     C*********  SCBSTY    IFNE *BLANKS
     C*********  SCBSTY    ANDNE'Q'
     C*********  SCBSTY    ANDNE'M'
     C*********  SCBSTY    ANDNE'A'
     C*********  SCBSTY    ANDNE'O'
     C**********           SETON                     22  90
      *** BEG ADD ***** 07/07/03 **********************************
     C                   MOVE      *OFF          X1ERR             1
      *** END ADD ***** 07/07/03 **********************************
      *
     C     SCBSTY        IFNE      *BLANK
     C                   SELECT
     C     VNDRFL        WHENEQ    *OFF
     C     ACHFLG        ANDEQ     *OFF
     C     SCBSTY        ANDNE     'Q'
     C     SCBSTY        ANDNE     'M'
     C     SCBSTY        ANDNE     'A'
     C     SCBSTY        ANDNE     'O'
     C                   SETON                                        22  90
      ***** BEG ADD ******* 10/13/03 **********************************
     C                   SETOFF                                       78
      ***** END ADD ******* 10/13/03 **********************************
      *** BEG ADD ***** 07/07/03 **********************************
     C                   MOVE      *ON           X1ERR             1
      *** END ADD ***** 07/07/03 **********************************
     C     VNDRFL        WHENEQ    *ON
     C     ACHFLG        ANDEQ     *OFF
     C     SCBSTY        ANDNE     'Q'
     C     SCBSTY        ANDNE     'M'
     C     SCBSTY        ANDNE     'A'
     C     SCBSTY        ANDNE     'O'
     C     SCBSTY        ANDNE     'W'
     C                   SETON                                        88  90
      *** BEG ADD ***** 07/07/03 **********************************
     C                   MOVE      *ON           X1ERR             1
      *** END ADD ***** 07/07/03 **********************************
     C     ACHFLG        WHENEQ    *ON
     C     SCBSTY        ANDNE     'Q'
     C     SCBSTY        ANDNE     'M'
     C     SCBSTY        ANDNE     'A'
     C     SCBSTY        ANDNE     'O'
     C     SCBSTY        ANDNE     'W'
     C     SCBSTY        ANDNE     'T'
     C                   SETON                                        89  90
      *** BEG ADD ***** 07/07/03 **********************************
     C                   MOVE      *ON           X1ERR             1
      *** END ADD ***** 07/07/03 **********************************
     C                   ENDSL
      *** END CHG ***** 04/11/03 ***************************************
     C*
      *** BEG CHG ***** 07/07/03 ***************************************
     C******************** ELSE
     C                   ENDIF
      *
     C     X1ERR         IFEQ      *OFF
      *** END CHG ***** 07/07/03 ***************************************
      *** BEG DEL ***** 03/20/06 ***************************************
     C***********
     C*****BEGIN*CHG  ***  03/05/98  ********************************
     C***********          Z-ADDSCPFDT    XXPF
     C*****BEGIN*ADD **** 11/09/00 ************************************
     C***********SCBSDT    IFNE *ZEROS
     C*****END***ADD **** 11/09/00 ************************************
     C***********          Z-ADDSCBSDT    XXPF
     C*****END***CHG  ***  03/05/98  ********************************
     C*****BEGIN*ADD **** 11/09/00 ************************************
     C***********          ELSE
     C***********          Z-ADDSCPFDT    XXPF
     C***********          ENDIF
     C*****END***ADD **** 11/09/00 ************************************
     C***********
     C***********FNCGFL    IFEQ 'Y'
     C***********
     C*****BEGIN*ADD **** 11/09/00 ************************************
     C***********SCBSDT    IFNE *ZEROS
     C***********          Z-ADDSCBSDT    ##BSDT
     C***********          ELSE
     C***********          Z-ADDSCPFDT    ##BSDT
     C***********          ENDIF
     C*****END***ADD **** 11/09/00 ************************************
     C***********          CALL 'SRDDIF8'
     C*****BEGIN*CHG  ***  02/13/98  *********************************
     C***********          PARM           SCPTDT
     C***********          PARM           SCPFDT
     C*****BEGIN*CHG  ***  03/05/98  ********************************
     C***********          PARM           SCPFDT
     C*****BEGIN*CHG **** 11/09/00 ************************************
     C******************** PARM           SCBSDT
     C***********          PARM           ##BSDT  80
     C*****END***CHG **** 11/09/00 ************************************
     C*****END***CHG  ***  03/05/98  ********************************
     C***********          PARM           SCPTDT
     C*****END***CHG  ***  02/13/98  *********************************
     C***********          PARM           QQDIFF  50
     C***********          PARM           QQERR
     C***********
     C***********QQDIFF    IFGT 364
     C***********
     C***********          Z-ADDF1STPO    SRDTI
     C***********          CALL 'SRCVTDAT'DTMDYY
     C***********          MOVELSRDTO     XXWK80
     C***********
     C*****BEGIN*CHG  ***  03/05/98  ********************************
     C***********          Z-ADDSCPFDT    XXPFD
     C*****BEGIN*ADD **** 11/09/00 ************************************
     C***********SCBSDT    IFNE *ZEROS
     C*****END***ADD **** 11/09/00 ************************************
     C***********          Z-ADDSCBSDT    XXPFD
     C*****END***CHG  ***  03/05/98  ********************************
     C*****BEGIN*ADD **** 11/09/00 ************************************
     C***********          ELSE
     C***********          Z-ADDSCPFDT    XXPFD
     C***********          ENDIF
     C*****END***ADD **** 11/09/00 ************************************
     C***********          MOVE XXPDD     XXWDD
     C***********          MOVE XXWK80    XXPF
     C***********
     C***********          ELSE
     C***********
     C***********          Z-ADD*ZEROS    PDAYS
     C***********          Z-ADD*ZEROS    BDAYS
     C***********
     C***********365       SUB  QQDIFF    PDAYS   50
     C***********
     C***********          Z-ADDF1STPO    SRDTI
     C***********          CALL 'SRCVTDAT'DTMDYY
     C***********          MOVELSRDTO     XXEF
     C***********
     C***********          CALL 'SRFUTR8'
     C***********          PARM           XXEF
     C***********          PARM           PDAYS
     C***********          PARM           QQERR   1
     C***********          PARM '1'       QQTERM  1
     C***********
     C***********          MOVE XXEF      BDAYS
     C*****BEGIN*CHG  ***  03/05/98  ********************************
     C***********          Z-ADDSCPFDT    XXPFD
     C*****BEGIN*ADD **** 11/09/00 ************************************
     C***********SCBSDT    IFNE *ZEROS
     C*****END***ADD **** 11/09/00 ************************************
     C***********          Z-ADDSCBSDT    XXPFD
     C*****END***CHG  ***  03/05/98  ********************************
     C*****BEGIN*ADD **** 11/09/00 ************************************
     C***********          ELSE
     C***********          Z-ADDSCPFDT    XXPFD
     C***********          ENDIF
     C*****END***ADD **** 11/09/00 ************************************
     C***********          MOVE XXPDD     XXBDD
     C***********
     C***********          MOVE BDAYS     XXPF
     C***********
     C***********          ENDIF                           QQDIFF>364
     C***********
     C***********          ENDIF                           FNCGFL=Y
     C***********
     C***********          MOVE XXPF      QQPFDT  8
      *** END DEL ***** 03/20/06 ***************************************
      *** BEG ADD ***** 03/20/06 **************************************
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
     C     SCBSDT        IFNE      *ZERO
     C                   Z-ADD     SCBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0
     C                   ELSE
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT
     C                   ENDIF
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
     C                   Z-ADD     XXBGDT        XXDT
     C                   Z-ADD     1             XXDD
      *** BEG ADD ***** 06/12/06 *************************************
      *** BEG CHG ***** 09/06/06 *************************************
     C*****************    Z-ADDXXDT      XXBGDT
     C                   Z-ADD     XXDT          XXSTDT            7 0
      *** END CHG ***** 09/06/06 *************************************
      *** END ADD ***** 06/12/06 *************************************
     C                   Z-ADD     0             XXINCT            2 0
      *** BEG ADD ***** 02/07/07 *************************************
     C                   Z-ADD     XXPTDT        SVPTDT            7 0
     C                   MOVE      '01'          SVPTDT
     C     XXSTDT        IFLT      XXBGDT
     C     XXPTDT        ANDEQ     SVPTDT
     C                   ADD       1             XXINCT
     C                   ENDIF
      *** END ADD ***** 02/07/07 *************************************
     C     XXDT          DOWLE     XXPTDT
      *** BEG CHG ***** 09/06/06 *************************************
     C********** XXDT      IFGE XXBGDT
      *** BEG CHG ***** 02/07/07 *************************************
     C********** XXDT      IFGE XXSTDT
     C     XXDT          IFGE      XXBGDT
      *** END CHG ***** 02/07/07 *************************************
      *** END CHG ***** 09/06/06 *************************************
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
      *** BEG CHG ***** 09/06/06 **********************************
     C********** XXDD      IFEQ 1
     C**********           ADD  1         XXMM
     C**********           ENDIF
     C********** XXMM      IFEQ 0
     C**********           Z-ADD1         XXMM
     C**********           ENDIF
     C                   ADD       1             XXMM
      *** END CHG ***** 09/06/06 **********************************
      *
      * Concatenate the efeective month/day (01/16) with the number of
      * installments which fit on the effective date range
     C                   MOVE      *ZEROS        QQPFDT
     C                   MOVEL     XXMDD         XXMDI1            6
     C                   MOVE      XXINCT        XXMDI1
     C                   MOVEL     XXMDI1        QQPFDT            8
      *
      *** END ADD ***** 03/20/06 **************************************
     C                   Z-ADD     *ZEROS        ##RRN
     C*
     C                   CALL      'W2157R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##CALL
     C                   PARM                    ##RRN
     C                   PARM      DBRRN1        ##RRN2            9 0
     C                   PARM                    SCBSTY
     C                   PARM                    QQPFDT
     C*
     C     ##RRN         CHAIN     WMBXP                              60
     C  N60              MOVEL(P)  BXBSCD        SCBSCD
     C*
     C     ##RRN         IFEQ      *ZEROS
     C                   MOVE      *ON           *IN49
     C                   ELSE
     C                   MOVE      *ON           *IN51
     C                   MOVEL(P)  SCBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEL(P)  BSDESC        SCBSDS
      ***** BEG ADD ******* 08/10/99 **********************************
     C   60              MOVEL(P)  *BLANKS       SCBSDS
      ***** END ADD ******* 08/10/99 **********************************
     C                   ENDIF                                                  ##RRN=*ZEROS
     C*
     C                   MOVE      *ON           *IN90
     C*
     C                   ENDIF                                                  X1ERR = *OFF
     C*
     C                   ENDSR                                                  BILAUX
     C*
     C***  Begin Add  ***  11/22/04  **************************************
      ***** BEG ADD ******* 04/07/05 **********************************
      *
      *
      *====================================================================
      *  Create PEO Certificate record
      *====================================================================
     C     CERTRD        BEGSR
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N60EMKY          CHAIN     PEM01                              60
      *
      *  If PEO group
      *
     C     FNPEOF        IFNE      *BLANKS
     C     *IN60         ANDEQ     *OFF
      *
      *  If employer detail record exits for PEO employer
      *
     C                   Z-ADD     ELCO#         SVCO#
     C                   Z-ADD     ELFND         SVFND
     C                   Z-ADD     ELEMP#        SVEMP#
     C                   Z-ADD     ELDIV         SVDIV
     C                   Z-ADD     ELFYR         SVFYR
     C                   Z-ADD     1             K1EMP#
     C                   Z-ADD     0             K1DIV
     C     ELKY4         CHAIN     PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     ELKY5         CHAIN     PEL01                              60
      *
      *  Get next Cert # for PEO employer (1st employer in group)
      *
     C     CEKY          SETGT     PCE01
     C     CEKY          READPE(N) PCE01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     1             ADD       CECER#        XXCER#
     C                   ELSE
     C                   Z-ADD     1             XXCER#
     C                   ENDIF                                                  CEKY
      *
      *  Write new Cert Holder record for PEO employer
      *
     C                   CLEAR                   PCE01
     C                   Z-ADD     ELCO#         CECO#
     C                   Z-ADD     ELFND         CEFND
     C                   Z-ADD     ELFYR         CEFYR
     C                   Z-ADD     1             CEEMP#
     C                   Z-ADD     0             CEDIV
     C                   Z-ADD     XXCER#        CECER#
     C                   MOVEL(P)  EMNAME        CENAME
      *
     C     EMMAD1        IFNE      *BLANKS
     C                   MOVEL(P)  EMMAD1        CEADR1
     C                   MOVEL(P)  EMMAD2        CEADR2
     C                   MOVEL(P)  EMMCTY        CECTY
     C                   MOVEL(P)  EMMST         CEST
     C                   Z-ADD     EMMZIP        CEZIP
      *
     C                   ELSE
      *
     C                   MOVEL(P)  EMPAD1        CEADR1
     C                   MOVEL(P)  EMPAD2        CEADR2
     C                   MOVEL(P)  EMPCTY        CECTY
     C                   MOVEL(P)  EMPST         CEST
     C                   Z-ADD     EMPZIP        CEZIP
     C                   ENDIF                                                  EMMAD1
      *
     C                   MOVE      'Y'           CERLFL
     C                   MOVE      'A'           CESTAT
      *
     C                   MOVEL     Q1USER        CEIUSR
     C                   MOVEL     Q1USER        CEUSER
     C                   MOVEL     Q1PGM         CEIPGM
     C                   MOVEL     Q1PGM         CEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CEIDT
     C                   MOVEL     SRDTO         CEDT
     C                   MOVEL     DSTIME        CEITM
     C                   MOVEL     DSTIME        CETIME
      *
     C                   WRITE     PCE01
     C                   ENDIF                                                  ELKY4
      *
     C                   ENDIF                                                  FNPEOF
      *
     C                   ENDSR                                                  CERTRD
      *
      *
      ***** END ADD ******* 04/07/05 **********************************
     C*====================================================================
     C*  Processing for the change in 'Do Not Renew' Flag
     C*====================================================================
     C     CHGRNW        BEGSR
     C*
     C     *LIKE         DEFINE    SCRLFL        SVRLFL
     C     *LIKE         DEFINE    SCRLFL        S2RLFL
     C*
     C                   MOVE      *OFF          XXUPDA            1
     C*
     C     ADDMOD        IFEQ      *OFF
     C     SC#DIV        ANDGE     2
     C*
     C***** BEG CHG ******* 10/08/07 **********************************
     C****               MOVE      'Y'           SCRNW
     C***** END CHG ******* 10/08/07 **********************************
     C*
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C     *INKC         OREQ      *ON
     C*
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *OFF          *IN73
     C*
     C***** BEG CHG ******* 10/08/07 **********************************
     C     EXUPDA        IFNE      'Y'
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXFMT     WDWRNW
     C*
     C     SCRNW         IFNE      'Y'
     C     SCRNW         ANDNE     'N'
     C                   SETON                                        73  90
     C                   ENDIF
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C*
     C                   ENDDO
     C*
     C     SCRNW         IFEQ      'Y'
     C                   MOVE      *ON           XXUPDA
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      SCRLFL        S2RLFL
     C*
     C                   MOVE      *ON           *IN49                          PC on SCBSTY
     C                   MOVE      *OFF          *IN79                          PC on SCBSTY
     C*
     C                   ENDSR                                                  CHGRNW
     C***  End   Add  ***  11/22/04  **************************************
     C*
     C/EJECT
     C*** BEG ADD ***** 02/03/98 ***********************************************
     C*===============================================================
     C*  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
     C*===============================================================
     C     CKCHGS        BEGSR
     C*
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
B1   C     LDENDA        IFNE      *ZEROS
B1   C     LDENDA        ANDNE     *BLANKS
     C*
B2   C     ELILCD        IFNE      S#ILCD
     C     ELINL2        ORNE      S#INL2
     C     ELEXM1        ORNE      S#EXM1
     C     ELEXM2        ORNE      S#EXM2
     C     ELEXM3        ORNE      S#EXM3
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C*  SETUP FOR NEXT REQUEST NUMBER
     C*
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         XXREQ#
     C                   ELSE
     C                   Z-ADD     ENREQ#        XXREQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PEN01
     C*
     C                   MOVE      EMCO#         ENCO#
     C                   MOVE      EMFND         ENFND
     C                   MOVE      EMEMP#        ENEMP#
     C                   MOVE      EMDIV         ENDIV
     C                   MOVE      LDFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C*
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C*
     C                   MOVEL     EMNAME        ENNAM1
     C                   MOVEL     EMFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     EMNAME        ENENM1
     C                   MOVEL(P)  EMNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      EMNAM2        ENENM2
     C*
     C                   MOVEL     EMFDNM        ENEFN1
     C                   MOVEL(P)  EMFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      EMFDN2        ENEFN2
     C*
     C*** END ADD *****  03/07/05  ************************************
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
     C*
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
     C*
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
     C*
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
     C**** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
     C**** END   ADD **** 10/14/98 *********************************************
     C**** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
     C**** END   ADD **** 11/23/98 *********************************************
     C*
     C*  DETERMINE WHICH TYPE(S) OF CHANGES HAVE OCCURRED
     C*
     C*    INCREASED LIMIT CHANGE
     C*
B3   C     ELILCD        IFNE      S#ILCD
     C     ELINL2        ORNE      S#INL2
     C*  SET INCREASED LIMITS CHANGE FLAG
     C                   MOVEL     'Y'           ENILFL
     C                   MOVEL     DXPINL        ENRMCD
     C*  LOAD OLD VALUES
     C                   MOVEL     S#ILCD        ENIC11
     C                   MOVEL     S#ILDS        ENID11
     C                   MOVEL     S#INL2        ENIC21
     C                   MOVEL     S#I2DS        ENID21
     C*  LOAD NEW VALUES
     C                   MOVEL     ELILCD        ENIC12
     C                   MOVEL     SCILDS        ENID12
     C                   MOVEL     ELINL2        ENIC22
     C                   MOVEL     SCI2DS        ENID22
     C*  CLEAR EXP MOD CHG FILEDS
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
     C*
     C*  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
     C*
     C                   ADD       1             XXREQ#
     C                   Z-ADD     XXREQ#        ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
B4   C     ELILCD        IFNE      S#ILCD
     C     S#ILCD        CAT(P)    S#ILDS:1      ENBLIN
     C     ELILCD        CAT(P)    SCILDS:1      ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
E4   C                   ENDIF                                                  ELILCD <> S#ILCD
     C*
B4   C     ELINL2        IFNE      S#INL2
     C     S#INL2        CAT(P)    S#I2DS:1      ENBLIN
     C     ELINL2        CAT(P)    SCI2DS:1      ENALIN
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
E4   C                   ENDIF                                                  ELINL2 <> S#INL2
     C*
E3   C                   ENDIF                                                  ELILCD <> S#ILCD
     C*
     C*    EXPERIENCE MOD CHANGE
B3   C     ELEXM1        IFNE      S#EXM1
     C     ELEXM2        ORNE      S#EXM2
     C     ELEXM3        ORNE      S#EXM3
     C*  SET EXP MOD CHANGE FLAG
     C                   MOVE      'Y'           ENERFL
     C                   MOVEL     DXPEXM        ENRMCD
     C*  LOAD OLD VALUES
     C                   MOVE      S#EXM1        ENEM11
     C                   MOVE      S#EXM2        ENEM21
     C                   MOVE      S#EXM3        ENEM31
     C                   MOVE      S#EXD1        ENED11
     C                   MOVE      S#EXD2        ENED21
     C                   MOVE      S#EXD3        ENED31
     C*  LOAD NEW VALUES
     C                   MOVE      ELEXM1        ENEM12
     C                   MOVE      ELEXM2        ENEM22
     C                   MOVE      ELEXM3        ENEM32
     C                   MOVE      ELEMD1        ENED12
     C                   MOVE      ELEMD2        ENED22
     C                   MOVE      ELEMD3        ENED32
     C*  CLEAR INCREASED LIMITS CHG FIELDS
     C                   MOVE      *BLANKS       ENILFL
     C                   MOVE      *BLANKS       ENIC11
     C                   MOVE      *BLANKS       ENID11
     C                   MOVE      *BLANKS       ENIC21
     C                   MOVE      *BLANKS       ENID21
     C                   MOVE      *BLANKS       ENIC12
     C                   MOVE      *BLANKS       ENID12
     C                   MOVE      *BLANKS       ENIC22
     C                   MOVE      *BLANKS       ENID22
     C*
     C*  BUILD UP BEFORE/AFTER LINES, & WRITE WMENP RECORDS
     C*
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*  EXP MOD 1
     C     S#EXM1        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVE      S#EXM1        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENBLIN
     C                   CAT       XXEXM4:0      ENBLIN
     C                   MOVE      S#EXD1        XXWKDT
     C                   CAT       '-':1         ENBLIN
     C                   CAT       XXWKMM:1      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKDD:0      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKCY:0      ENBLIN
     C                   ENDIF
     C     ELEXM1        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENALIN
     C                   ELSE
     C                   MOVE      ELEXM1        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENALIN
     C                   CAT       XXEXM4:0      ENALIN
     C                   MOVE      SCEMD1        XXWKDT
     C                   CAT       '-':1         ENALIN
     C                   CAT       XXWKMM:1      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKDD:0      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKCY:0      ENALIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C*  EXP MOD 2
     C     S#EXM2        IFNE      *ZERO
     C     ELEXM2        ORNE      *ZERO
     C     S#EXM2        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVE      S#EXM2        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENBLIN
     C                   CAT       XXEXM4:0      ENBLIN
     C                   MOVE      S#EXD2        XXWKDT
     C                   CAT       '-':1         ENBLIN
     C                   CAT       XXWKMM:1      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKDD:0      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKCY:0      ENBLIN
     C                   ENDIF
     C     ELEXM2        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENALIN
     C                   ELSE
     C                   MOVE      ELEXM2        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENALIN
     C                   CAT       XXEXM4:0      ENALIN
     C                   MOVE      SCEMD2        XXWKDT
     C                   CAT       '-':1         ENALIN
     C                   CAT       XXWKMM:1      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKDD:0      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKCY:0      ENALIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF                                                  S#EXM2<>0
     C*  EXP MOD 3
     C     S#EXM3        IFNE      *ZERO
     C     ELEXM3        ORNE      *ZERO
     C     S#EXM3        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENBLIN
     C                   ELSE
     C                   MOVE      S#EXM3        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENBLIN
     C                   CAT       XXEXM4:0      ENBLIN
     C                   MOVE      S#EXD3        XXWKDT
     C                   CAT       '-':1         ENBLIN
     C                   CAT       XXWKMM:1      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKDD:0      ENBLIN
     C                   CAT       '/':0         ENBLIN
     C                   CAT       XXWKCY:0      ENBLIN
     C                   ENDIF
     C     ELEXM3        IFEQ      *ZERO
     C                   MOVEL     *BLANKS       ENALIN
     C                   ELSE
     C                   MOVE      ELEXM3        XXEXMD
     C     XXEXM1        CAT(P)    '.':0         ENALIN
     C                   CAT       XXEXM4:0      ENALIN
     C                   MOVE      SCEMD3        XXWKDT
     C                   CAT       '-':1         ENALIN
     C                   CAT       XXWKMM:1      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKDD:0      ENALIN
     C                   CAT       '/':0         ENALIN
     C                   CAT       XXWKCY:0      ENALIN
     C                   ENDIF
     C                   WRITE     PEN01
     C                   ADD       1             ENSEQ#
     C                   ENDIF                                                  S#EXM3<>0
     C*
E3   C                   ENDIF                                                  ELEXM1 <> S#EXM1
     C*
E2   C                   ENDIF                                                  ELRTCD <> S#RTCD
     C*
     C                   ENDIF                                                  LDENDA <> *ZERO
     C*
     C                   ENDSR
     C*** END ADD ***** 02/03/98 ***********************************************
     C/EJECT
      ***** BEG ADD ******* 07/20/00 **********************************
      *
      *
      *
      *===============================================================
      * CKPOLD - CHECK IF CLAIMS OUTSIDE NEW POLICY EFF DT ENTERED
      *===============================================================
     C     CKPOLD        BEGSR
      *
     C     XXOFLG        IFEQ      'N'
     C                   MOVE      'N'           XXEFLG            1            ERROR FOUND
      *
     C     @@RRN         CHAIN(N)  WDELP                              60
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
      *
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
      *
      *  Check for claims outside policy eff dt range
      *
     C     XXPFDT        IFGT      ELPFDT
     C                   Z-ADD     ELPFDT        XXDATE
      *
     C     CMKY1         SETLL     PCM01
     C     CMKY2         READE     PCM01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     XXPFDT
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     XXPMTS        IFNE      *ZEROS                                       CLSD CLM W/PMTS
     C     CMSTAT        ORNE      'C'                                          OPN CLM
     C                   SETON                                        90
     C                   MOVE      'Y'           XXEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     CMKY2         READE     PCM01                                  60
     C                   ENDDO                                                  PFDT
      *
     C                   ENDIF                                                  PFDT
      *
      *
      *
     C  N90XXPTDT        IFLT      ELPTDT
     C     XXEFLG        ANDEQ     'N'
     C                   Z-ADD     XXPTDT        XXDATE
     C     CMKY1         SETLL     PCM01
     C     CMKY2         READE     PCM01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     ELPTDT
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     XXPMTS        IFNE      *ZEROS                                       CLSD CLM W/PMTS
     C     CMSTAT        ORNE      'C'                                          OPN CLM
     C                   SETON                                        90
     C                   MOVE      'Y'           XXEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     CMKY2         READE     PCM01                                  60
     C                   ENDDO                                                  PTDT
      *
     C                   ENDIF                                                  PTDT
      *
      *  If claims with payments found outside new effective date range
      *
     C     *IN90         IFEQ      *ON
     C     XXEFLG        ANDEQ     'Y'
      *
      *  Check for user authority to override error msg
      *  and condition display of Y/N override on prompt window
      *
     C     ADKY          CHAIN     PEX02                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      ##TYPE        ##UTYP
     C                   MOVE      ##LVL         ##ULVL
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
     C     ##RES         COMP      *OFF                                   74
     C                   ENDIF
      *
      * Display "Claims outside eff date range" err window
      *
     C                   MOVE      *BLANK        SCOVER
      *
     C     SCOVER        DOWNE     'Y'
     C     SCOVER        ANDNE     'N'
     C     *INKL         ANDEQ     *OFF
     C     *INKC         ANDEQ     *OFF
     C                   EXFMT     WDW03
     C  N74              LEAVE
      *
     C     SCOVER        IFNE      'Y'
     C     SCOVER        ANDNE     'N'
     C                   SETON                                        73
     C                   ENDIF
      *
     C                   ENDDO
      *
     C     SCOVER        IFEQ      'Y'
     C                   MOVE      'Y'           XXOFLG
     C                   ENDIF                                                  SCOVER = Y
      *
     C                   ENDIF                                                  *IN90
      *
     C                   SETOFF                                       7374
     C                   ENDIF                                                  XXOFLG
      *
     C                   ENDSR                                                  CKPOLD
      *
      ***** END ADD ******* 07/20/00 **********************************
     C/EJECT
      *+*+  BEG ADD *+*+*+* 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
      *
      *===============================================================
      * CKRTC1 - "RATE SET CODE" EDITS
      *===============================================================
     C     CKRTC1        BEGSR
      *
      * Info edit if Rate Code corresponds to Phys/Mail state of Emp.
      *
     C     SCRTCD        IFNE      SVRTCD
      *** BEG DEL ***** 08/09/07 **********************************************
     C**********           MOVELSCRTCD    SVRTCD    P
      *** END DEL ***** 08/09/07 **********************************************
     C***  Begin Add  ***  11/10/10  **************************************
     C                   clear                   RSJRST
     C***  End   Add  ***  11/10/10  **************************************
     C     @@RRN2        CHAIN     WMEMP                              60
     C  N60RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     EMMST         ANDNE     RSJRST
     C     EMPST         ANDNE     RSJRST
      *** BEG CHG ***** 08/09/07 **********************************************
     C***********          EXFMTWDW04
     C***KL******          SETON                     52
     C***KL******          SETOF                     KL
     C***********          SETON                     90
     C                   EXFMT     WDW06
     C     *INKL         IFEQ      *ON
     C                   SETON                                        52  90
     C                   SETOFF                                       KL
     C                   ELSE
     C                   MOVEL(P)  SCRTCD        SVRTCD
     C                   ENDIF
      *** END CHG ***** 08/09/07 **********************************************
     C                   ENDIF
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C* Do not allow state to change if HCN program on policy
     C  n90              if        RSJRST <> svRST
     C                              and HCNPgmExists(ELCO#:ELFND
     C                                              :ELEMP#:ELDIV:SCFYR)
     C                              and @@Mode <> 'A'
     C                   seton                                        142690
     C                   endif
     C***  End   Add  ***  11/10/10  **************************************
      *
     C                   ENDIF                                                  SCRTCD
      *
      *  Check against any entered Group Rate Set Codes
      *
     C     *IN90         IFEQ      *OFF
     C     GRKY1         CHAIN     PGR01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     GRSTAT        ANDEQ     'I'
     C     GRKY1         READE     PGR01                                  60
     C                   ENDDO
      *
     C     *IN60         IFEQ      *OFF
     C     GRKY2         CHAIN     PGR01                              60
      *
     C     *IN60         IFEQ      *ON
     C     GRSTAT        OREQ      'I'
     C***  Begin Chg  ***  04/26/07  **************************************
     C**********           SETON                     90  84
     C                   SETON                                        90  14
     C***  End   Chg  ***  04/26/07  **************************************
     C                   ENDIF                                                  GRKY2
      *
     C                   ENDIF                                                  GRKY1
      *
     C                   ENDIF                                                  *IN90
      *
     C                   ENDSR                                                  CKRTC1
      *+*+  END ADD *+*+*+* 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      ***** BEG ADD ******* 05/02/00 **********************************
      *
      *===============================================================
      * CKRTCD - ADDITIONAL "RATE SET CODE" EDITS
      *===============================================================
     C     CKRTCD        BEGSR
      *
      *  If rate code / effective date change
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT2             7 0
     C**** BEGIN ADD **** 02/19/01 ************************************
      *
      *  Get statistical data elements associated with previous
      *  rate code and effective date
      *
     C     RSKY1         CHAIN     PRS01                              60
     C  N60              MOVEL     RSJRST        XXST
     C   60              MOVEL     *BLANKS       XXST
     C**** BEGIN ADD **** 12/22/00 ************************************
     C                   MOVEL     XXST          XXSVST
     C**** END ADD ****** 12/22/00 ************************************
      *
     C                   Z-ADD     ELPFDT        XXDTE
     C**** END ADD ****** 02/19/01 ************************************
      *
     C     ELPFDT        IFNE      XXDT2
     C     ELRTCD        ORNE      SCRTCD
     C**** BEGIN DEL **** 02/19/01 ************************************
      ***********
      *  Get statistical data elements associated with previous
      *  rate code and effective date
      ***********
     C***********RSKY1     CHAINPRS01                60
     C**N60******          MOVELRSJRST    XXST
     C***60******          MOVEL*BLANKS   XXST
     C**** BEGIN ADD **** 12/22/00 ************************************
     C**********           MOVELXXST      XXSVST
     C**** END ADD ****** 12/22/00 ************************************
      **********
     C**********           Z-ADDELPFDT    XXDTE
     C**** END DEL ****** 02/19/01 ************************************
      *
     C  N60RRKY3         SETGT     PRR01
     C  N60RRKY2         READPE    PRR01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  RRDV1H        SVDV1H
     C                   MOVEL(P)  RRDV2H        SVDV2H
     C                   MOVEL(P)  RR1AHD        SV1AHD
     C                   MOVEL(P)  RR1BHD        SV1BHD
     C                   MOVEL(P)  RRCRHD        SVCRHD
     C                   MOVEL(P)  RRDBHD        SVDBHD
     C                   MOVEL(P)  RRWVHD        SVWVHD
      *
     C                   MOVE      RRRDPR        SVRDPR
     C                   MOVE      RRDV1L        SVDV1L
     C                   MOVE      RRDV2L        SVDV2L
     C                   MOVE      RRDSPR        SVDSPR
     C                   MOVE      RRDSIN        SVDSIN
     C                   MOVE      RRDSHD        SVDSHD
     C                   MOVE      RRDISC        SVDISC
     C                   MOVE      RRDSPC        SVDSPC
     C                   MOVE      RRSFAP        SVSFAP
     C                   MOVE      RRSFPC        SVSFPC
     C                   MOVE      RRSFAM        SVSFAM
     C                   MOVE      RRSPFL        SVSPFL
     C                   MOVE      RRYRAD        SVYRAD
     C                   MOVE      RRCXAD        SVCXAD
     C                   Z-ADD     RRTXPC        SVTXPC
     C                   MOVE      RRTDFL        SVTDFL
     C                   MOVE      RRIDTP        SVIDTP
     C                   MOVE      RRTXHD        SVTXHD
     C                   Z-ADD     RR1STP        SV1STP
     C                   MOVE      RR1STL        SV1STL
     C                   MOVE      RR1STH        SV1STH
     C                   MOVE      RR1STF        SV1STF
     C                   MOVE      RR1IDD        SV1IDD
     C                   MOVE      RR1IDV        SV1IDV
     C                   Z-ADD     RR2NDP        SV2NDP
     C                   MOVE      RR2NDL        SV2NDL
     C                   MOVE      RR2NDH        SV2NDH
     C                   MOVE      RR2NDF        SV2NDF
     C                   MOVE      RR2IDD        SV2IDD
     C                   MOVE      RR2IDV        SV2IDV
     C*** BEGIN ADD ***  01/02/01  ************************************
     C                   Z-ADD     RR3RDP        SV3RDP
     C                   MOVE      RR3RDL        SV3RDL
     C                   MOVE      RR3RDH        SV3RDH
     C                   Z-ADD     RR3RDF        SV3RDF
     C                   MOVE      RR3IDD        SV3IDD
     C                   MOVE      RR3IDV        SV3IDV
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
     C                   Z-ADD     RR4THP        SV4THP
     C                   MOVE      RR4THL        SV4THL
     C                   MOVE      RR4THH        SV4THH
     C                   Z-ADD     RR4THF        SV4THF
     C                   MOVE      RR4IDD        SV4IDD
     C                   MOVE      RR4IDV        SV4IDV
     C                   Z-ADD     RR5THP        SV5THP
     C                   MOVE      RR5THL        SV5THL
     C                   MOVE      RR5THH        SV5THH
     C                   Z-ADD     RR5THF        SV5THF
     C                   MOVE      RR5IDD        SV5IDD
     C                   MOVE      RR5IDV        SV5IDV
     C*** END ADD *****  07/02/04  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C                   Z-ADD     RR6THP        SV6THP
     C                   MOVE      RR6THL        SV6THL
     C                   MOVE      RR6THH        SV6THH
     C                   Z-ADD     RR6THF        SV6THF
     C                   MOVE      RR6IDD        SV6IDD
     C                   MOVE      RR6IDV        SV6IDV
     C                   Z-ADD     RR7THP        SV7THP
     C                   MOVE      RR7THL        SV7THL
     C                   MOVE      RR7THH        SV7THH
     C                   Z-ADD     RR7THF        SV7THF
     C                   MOVE      RR7IDD        SV7IDD
     C                   MOVE      RR7IDV        SV7IDV
     C***  End   Add  ***  12/11/08  **************************************
     C                   MOVE      RRASFL        SVASFL
     C                   MOVE      RRWVFL        SVWVFL
     C                   MOVE      RRWDTP        SVWDTP
     C                   MOVE      RRWDPC        SVWDPC
     C                   MOVE      RRWDMN        SVWDMN
     C                   MOVE      RRWDFC        SVWDFC
     C                   Z-ADD     RRACC         SVACC
     C                   Z-ADD     RRDIS         SVDIS
     C                   Z-ADD     RRAGG         SVAGG
     C                   MOVE      RREMLB        SVEMLB
     C*** BEG ADD *****  09/01/04  ************************************
     C                   MOVE(P)   RRVLCH        SVVLCH
     C*** END ADD *****  09/01/04  ************************************
     C                   ENDIF
      *
      *  Get statistical data elements associated with current
      *  rate code and effective date
      *
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL     RSJRST        XXST
     C   60              MOVEL     *BLANKS       XXST
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE
      *
     C  N60RRKY3         SETGT     PRR01
     C  N60RRKY2         READPE    PRR01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
      *  Error check for missing headings
      *
     C  N90SVDV1H        IFNE      *BLANKS
     C     RRDV1H        ANDEQ     *BLANKS
     C     ELASP2        ANDNE     *ZERO
     C                   SETON                                        90
     C                   SETON                                            01
     C                   SETOFF                                       0302
     C                   ENDIF                                                  DV1H
      *
     C  N90SVDV2H        IFNE      *BLANKS
     C     RRDV2H        ANDEQ     *BLANKS
     C     ELDEV2        ANDNE     *ZERO
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         ANDNE     '025'
     C     SVDV2H        ORNE      *BLANKS
     C     RRDV2H        ANDEQ     *BLANKS
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   SETON                                        90
     C                   SETON                                          02
     C                   SETOFF                                       03  01
     C                   ENDIF                                                  DV2H
      *
     C  N90SV1AHD        IFNE      *BLANKS
     C     RR1AHD        ANDEQ     *BLANKS
     C     ELASPM        ANDNE     *ZERO
     C                   SETON                                        90
     C                   SETON                                          0201
     C                   SETOFF                                       03
     C                   ENDIF                                                  1AHD
      *
     C  N90SV1BHD        IFNE      *BLANKS
     C     RR1BHD        ANDEQ     *BLANKS
     C     ELASPS        ANDNE     *ZERO
     C                   SETON                                        90
     C                   SETON                                        03
     C                   SETOFF                                         0201
     C                   ENDIF                                                  1BHD
      *
     C  N90SVCRHD        IFNE      *BLANKS
     C     RRCRHD        ANDEQ     *BLANKS
     C     ELDBCR        ANDNE     *ZERO
     C                   SETON                                        90
     C                   SETON                                        03  01
     C                   SETOFF                                         02
     C                   ENDIF                                                  CRHD
      *
     C  N90SVDBHD        IFNE      *BLANKS
     C     RRDBHD        ANDEQ     *BLANKS
     C     ELDBCR        ANDNE     *ZERO
     C                   SETON                                        90
     C                   SETON                                        0302
     C                   SETOFF                                           01
     C                   ENDIF                                                  DBHD
      *
     C  N90SVWVHD        IFNE      *BLANKS
     C     RRWVHD        ANDEQ     *BLANKS
      *
     C     ELWVTP        IFNE      *BLANK
     C     ELWVPC        ORNE      *ZERO
     C     ELFCHG        ORNE      *ZERO
     C     ELWMIN        ORNE      *ZERO
     C     ELWVFL        ORNE      *BLANK
     C                   SETON                                        90
     C                   SETON                                        030201
     C                   ENDIF                                                  WVHD
      *
     C                   ENDIF                                                  WVHD0
      *
      *** BEG ADD ***** 09/01/04 **************************
     C  N90SVVLCH        IFNE      *BLANKS
     C     RRVLCH        ANDEQ     *BLANKS
      *
     C     ELVLCA        IFNE      *ZERO
     C                   SETON                                        90
     C                   SETON                                        05
     C                   ENDIF                                                  VLCA
      *
     C                   ENDIF                                                  VLCH
      *** END ADD ***** 09/01/04 **************************
      *
      *  Info message if statistical data elements changed
      *
     C     *IN90         IFEQ      *OFF
     C     XXONCE        ANDEQ     'N'
     C                   MOVE      'Y'           XXONCE            1
      *
     C                   Z-ADD     1             I3                3 0
     C                   MOVEL(P)  *BLANKS       LN
      *
      *      Standard Liability Limits
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RRACC         IFNE      SVACC
     C     RRDIS         ORNE      SVDIS
     C     RRAGG         ORNE      SVAGG
     C     RREMLB        ORNE      SVEMLB
     C                   MOVEL     ST(1)         LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
      *
      *      Deviation #1
      *
     C     ELASP2        IFNE      *ZERO
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            ANDLT     11
      ***** END ADD ******* 05/10/06 **********************************
      *
     C     RRDV1L        IFNE      SVDV1L
     C     RRDV1H        ORNE      SVDV1H
     C                   MOVEL     ST(2)         LN(I3)
      *
     C     SVDV1H        IFNE      *BLANKS
     C                   CAT(P)    SVDV1H:0      LN(I3)
      *
     C                   ELSE
      *
     C                   CAT(P)    RRDV1H:0      LN(I3)
     C                   ENDIF                                                  RRDV1H
      *
     C                   CAT(P)    '"':0         LN(I3)
     C                   CAT(P)    'is app':1    LN(I3)
     C                   CAT(P)    'lied':0      LN(I3)
     C                   ADD       1             I3
     C                   ENDIF                                                  RRDV1L
      *
     C                   ENDIF                                                  ELASP2
      *
      *      Deviation #2
      *
     C     ELDEV2        IFNE      *ZERO
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            ANDLT     11
      ***** END ADD ******* 05/10/06 **********************************
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         ANDNE     '025'
     C     ELSTFC        ORNE      *ZERO
     C     I3            ANDLT     11
     C     CLTID         ANDEQ     '025'
     C*** END   ADD ****** 01/14/10 ************************************
      *
     C     RRDV2L        IFNE      SVDV2L
     C     RRDV2H        ORNE      SVDV2H
     C                   MOVEL     ST(3)         LN(I3)
      *
     C     SVDV2H        IFNE      *BLANKS
     C                   CAT(P)    SVDV2H:0      LN(I3)
      *
     C                   ELSE
      *
     C                   CAT(P)    RRDV2H:0      LN(I3)
     C                   ENDIF                                                  RRDV2H
      *
     C                   CAT(P)    '"':0         LN(I3)
     C                   CAT(P)    'is app':1    LN(I3)
     C                   CAT(P)    'lied':0      LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      *
     C                   ENDIF                                                  ELDEV2
      *
      *      Aircraft surcharge
      *
     C     RRASFL        IFNE      SVASFL
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            ANDLT     11
      ***** END ADD ******* 05/10/06 **********************************
     C                   MOVEL     ST(4)         LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      *
      *      Safety fees
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RRSFAP        IFNE      SVSFAP
     C     RRSFPC        ORNE      SVSFPC
     C     RRSFAM        ORNE      SVSFAM
     C     RRSPFL        ORNE      SVSPFL
     C     RRYRAD        ORNE      SVYRAD
     C     RRCXAD        ORNE      SVCXAD
     C                   MOVEL     ST(5)         LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
      *
      *      State tax
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RRTXPC        IFNE      SVTXPC
     C     RRTDFL        ORNE      SVTDFL
     C     RRIDTP        ORNE      SVIDTP
     C     RRTXHD        ORNE      SVTXHD
      *
     C     SVTXHD        IFNE      *BLANKS
     C     '"'           CAT(P)    SVTXHD        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
      *
     C                   ELSE
      *
     C     'STATE'       CAT(P)    'TAX':1       LN(I3)
     C                   ENDIF                                                  RRTXHD
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
      *
      *      Waiver defaults
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RRWVFL        IFNE      SVWVFL
     C     RRWDTP        ORNE      SVWDTP
     C     RRWDPC        ORNE      SVWDPC
     C     RRWDMN        ORNE      SVWDMN
     C     RRWDFC        ORNE      SVWDFC
     C                   MOVEL     ST(7)         LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
      *
      *      First assessment
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RR1STP        IFNE      SV1STP
     C     RR1STL        ORNE      SV1STL
     C     RR1STH        ORNE      SV1STH
     C     RR1STF        ORNE      SV1STF
     C     RR1IDD        ORNE      SV1IDD
     C     RR1IDV        ORNE      SV1IDV
      *
     C     SV1STH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV1STH        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
      *
     C                   ELSE
      *
     C     'FIRST'       CAT(P)    'ASSESS':1    LN(I3)
     C                   CAT(P)    'MENT':0      LN(I3)
     C                   ENDIF                                                  RR1STH
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
      *
      *      Second assessment
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RR2NDP        IFNE      SV2NDP
     C     RR2NDL        ORNE      SV2NDL
     C     RR2NDH        ORNE      SV2NDH
     C     RR2NDF        ORNE      SV2NDF
     C     RR2IDD        ORNE      SV2IDD
     C     RR2IDV        ORNE      SV2IDV
      *
     C     SV2NDH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV2NDH        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
      *
     C                   ELSE
      *
     C     'SECOND'      CAT(P)    'ASSESS':1    LN(I3)
     C                   CAT(P)    'MENT':0      LN(I3)
     C                   ENDIF                                                  RR2NDH
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
     C*** BEGIN ADD ***  01/02/01  ************************************
      *
      *      THIRD ASSESSMENT
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RR3RDP        IFNE      SV3RDP
     C     RR3RDL        ORNE      SV3RDL
     C     RR3RDH        ORNE      SV3RDH
     C     RR3RDF        ORNE      SV3RDF
     C     RR3IDD        ORNE      SV3IDD
     C     RR3IDV        ORNE      SV3IDV
      *
     C     SV3RDH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV3RDH        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
      *
     C                   ELSE
      *
     C     'THIRD'       CAT(P)    'ASSESS':1    LN(I3)
     C                   CAT(P)    'MENT':0      LN(I3)
     C                   ENDIF                                                  RR3RDH
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
     C*** END ADD *****  01/02/01  ************************************
     C*** BEG ADD *****  07/02/04  ************************************
      *
      *      FOURTH ASSESSMENT
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RR4THP        IFNE      SV4THP
     C     RR4THL        ORNE      SV4THL
     C     RR4THH        ORNE      SV4THH
     C     RR4THF        ORNE      SV4THF
     C     RR4IDD        ORNE      SV4IDD
     C     RR4IDV        ORNE      SV4IDV
      *
     C     SV4THH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV4THH        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
      *
     C                   ELSE
      *
     C     'FOURTH'      CAT(P)    'ASSESS':1    LN(I3)
     C                   CAT(P)    'MENT':0      LN(I3)
     C                   ENDIF                                                  RR4THH
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
      *
      *      FIFTH ASSESSMENT
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RR5THP        IFNE      SV5THP
     C     RR5THL        ORNE      SV5THL
     C     RR5THH        ORNE      SV5THH
     C     RR5THF        ORNE      SV5THF
     C     RR5IDD        ORNE      SV5IDD
     C     RR5IDV        ORNE      SV5IDV
      *
     C     SV5THH        IFNE      *BLANKS
     C     '"'           CAT(P)    SV5THH        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
      *
     C                   ELSE
      *
     C     'FIFTH'       CAT(P)    'ASSESS':1    LN(I3)
     C                   CAT(P)    'MENT':0      LN(I3)
     C                   ENDIF                                                  RR5THH
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
     C*** END ADD *****  07/02/04  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
      *
      *      Sixth Assessment
      *
     C     I3            IFLT      11
     C     RR6THP        IFNE      SV6THP
     C     RR6THL        ORNE      SV6THL
     C     RR6THH        ORNE      SV6THH
     C     RR6THF        ORNE      SV6THF
     C     RR6IDD        ORNE      SV6IDD
     C     RR6IDV        ORNE      SV6IDV
      *
     C     SV6THH        IFNE      *BLANKS
     C                   eval      ln(i3) = '"' + %trim(sv6thh) + '"'
     C                   ELSE
     C                   eval      ln(i3) = 'SIXTH ASSESSMENT'
     C                   ENDIF                                                  RR4THH
      *
     C                   eval      ln(i3) = %trim(ln(i3)) + ' information'
     C                   ADD       1             I3
     C                   ENDIF
      *
     C                   ENDIF
      *
      *      SEVENTH ASSESSMENT
      *
     C     I3            IFLT      11
     C     RR7THP        IFNE      SV7THP
     C     RR7THL        ORNE      SV7THL
     C     RR7THH        ORNE      SV7THH
     C     RR7THF        ORNE      SV7THF
     C     RR7IDD        ORNE      SV7IDD
     C     RR7IDV        ORNE      SV7IDV
      *
     C     SV7THH        IFNE      *BLANKS
     C                   eval      ln(i3) = '"' + %trim(sv7thh) + '"'
     C                   ELSE
     C                   eval      ln(i3) = 'SEVENTH ASSESSMENT'
     C                   ENDIF                                                  RR5THH
      *
     C                   eval      ln(i3) = %trim(ln(i3)) + ' information'
     C
     C                   ADD       1             I3
     C                   ENDIF
      *
     C                   ENDIF
     C***  End   Add  ***  12/11/08  **************************************
      *
      *      Deductible/premium discount information
      *
     C     RRRDPR        IFNE      SVRDPR
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            ANDLT     11
      ***** END ADD ******* 05/10/06 **********************************
     C                   MOVEL     ST(10)        LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      *
      *      Stock discount applied
      *
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            IFLT      11
      ***** END ADD ******* 05/10/06 **********************************
     C     RRDSPR        IFNE      SVDSPR
     C     RRDSIN        ORNE      SVDSIN
     C     RRDSHD        ORNE      SVDSHD
     C     RRDISC        ORNE      SVDISC
     C     RRDSPC        ORNE      SVDSPC
      *
     C     SVDSHD        IFNE      *BLANKS
     C     '"'           CAT(P)    SVDSHD        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
      *
     C                   ELSE
      *
     C     'STOCK'       CAT(P)    'DISCOU':1    LN(I3)
     C                   CAT(P)    'NT':0        LN(I3)
     C                   ENDIF                                                  RRDSHD
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
     C                   ENDIF
      ***** BEG ADD ******* 05/10/06 **********************************
     C                   ENDIF
      ***** END ADD ******* 05/10/06 **********************************
      *** BEG ADD ***** 09/01/04 **********************************
      *
      *  Voluntary labor charge
      *
     C     RRVLCH        IFNE      SVVLCH
      ***** BEG ADD ******* 05/10/06 **********************************
     C     I3            ANDLT     11
      ***** END ADD ******* 05/10/06 **********************************
      *
     C     SVVLCH        IFNE      *BLANKS
     C     '"'           CAT(P)    SVVLCH        LN(I3)
     C                   CAT(P)    '"':0         LN(I3)
     C                   ELSE
     C     'Volun'       CAT(P)    'tary':0      LN(I3)
     C                   CAT(P)    'labor':1     LN(I3)
     C                   ENDIF                                                  SVVLCH
      *
     C                   CAT(P)    'infor':1     LN(I3)
     C                   CAT(P)    'mation':0    LN(I3)
     C                   ADD       1             I3
      *
     C                   ENDIF
      *** END ADD ***** 09/01/04 **********************************
      *
     C     I3            IFNE      1
     C                   EXFMT     WDW01
     C                   SETON                                        90
     C                   ENDIF                                                  I3 NE 1
      *
     C                   ENDIF                                                  *IN90
      *
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  PFDT RTCD
      *
     C                   ENDSR                                                  CKRTCD
      *
      *
      ***** END ADD ******* 05/02/00 **********************************
     C/EJECT
     C*
     C*===============================================================
     C* DTERRS - SUBROUTINE FOR DETERMINING FROM/TO DATE ERRORS
     C*===============================================================
     C*
     C     DTERRS        BEGSR
     C*
     C     '0'           CHECK     XXKEY         XXZERO            3 0
     C                   SUB       1             XXZERO
     C     ' '           CHECKR    XXKEY         XXLEN             3 0
     C*
     C     XXZERO        IFEQ      XXLEN
     C                   MOVEL     *ZEROS        XXKEY
     C                   ENDIF                                                  XXZERO=XXLEN
     C*
     C*  IF ONE OF THE DATES IS ENTERED, CODE IS REQUIRED.
     C*
     C*
     C  N90XXKEY         IFEQ      *ZEROS
      **** BEGIN ADD **** 03/02/00 *********************************************
     C     XXCHAR        ANDEQ     *OFF
      **** END   ADD **** 03/02/00 *********************************************
     C     XXKEY         OREQ      *BLANKS
     C*
     C     XXEF          IFNE      *ZERO
     C     XXEX          ORNE      *ZERO
     C                   SETON                                        71  90
     C                   ENDIF                                                  XXEF<>*ZERO<>XXEX
     C*
     C                   ENDIF                                                  *ZEROS=XXKEY=*BLANKS
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C  N90XXEF          IFNE      *ZERO
     C*
     C                   Z-ADD     XXEF          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXEF
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  EFFECTIVE DATE MUST BE BETWEEN POLICY FROM & TO DATES
     C*
     C                   Z-ADD     SCPFDT        XXPF
     C                   Z-ADD     SCPTDT        XXPT
     C*
     C                   Z-ADD     XXEF          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     XXPF          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
     C*
     C                   Z-ADD     XXPT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
     C*
     C  N90XXEFDT        IFLT      XXPFDT
     C     XXEFDT        ORGT      XXPTDT
     C                   SETON                                        73  90
     C                   ENDIF                                                  XXPF>XXEF>XXPT
     C*
     C                   ENDIF                                                  XXEF<>*ZERO
     C*
     C*  IF ENTERED, EXPIRATION DATE MUST BE A VALID DATE
     C*
     C  N90XXEX          IFNE      *ZERO
     C*
     C                   Z-ADD     XXEX          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXEX
     C*
     C  N90SRERR         IFEQ      *ON
     C*** BEGIN CHG ***  07/06/06  *****************************************
     C**************       SETON                     73  90
     C                   SETON                                        75  90
     C*** END CHG *****  07/06/06  *****************************************
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  EFFECTIVE DATE MUST BE LESS THAN EXPIRATION DATE
     C*
     C                   Z-ADD     XXEX          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C  N90XXEXDT        IFLT      XXEFDT
     C                   SETON                                        74  90
     C                   ENDIF                                                  XXEXDT<XXEFDT
     C*
     C*  EXPIRATION DATE MUST BE BETWEEN POLICY FROM & TO DATES
     C*
     C  N90XXEXDT        IFLT      XXPFDT
     C     XXEXDT        ORGT      XXPTDT
     C*** BEGIN CHG ***  07/06/06  *****************************************
     C********             SETON                     75  90
     C                   SETON                                        76  90
     C*** END CHG *****  07/06/06  *****************************************
     C                   ENDIF                                                  XXPFDT>XXEXDT>XXPTDT
     C*
     C                   ENDIF                                                  XXEXDT<>*ZERO
     C*
      **** BEGIN ADD **** 03/02/00 *********************************************
     C                   MOVE      *OFF          XXCHAR
      **** END   ADD **** 03/02/00 *********************************************
     C                   ENDSR                                                  DTERRS
     C*
     C/EJECT
     C*===============================================================
     C* DLTPRE - SUBROUTINE FOR PRIOR TO DELETION PROCESSING
     C*===============================================================
     C*
     C     DLTPRE        BEGSR
     C***  BEGIN ADD  ***  02/12/98  *********************************
     C*
     C*  CALL PROGRAM TO DELETE RELATED FILES
     C*
     C                   CALL      'W0051R01'                           99
     C                   PARM      ELCO#         ##CO#
     C                   PARM      ELFND         ##FND
     C                   PARM      ELEMP#        ##EMP#
     C                   PARM      ELDIV         ##DIV
     C                   PARM      ELFYR         ##FYR
     C***  END   ADD  ***  02/12/98  *********************************
      **** BEGIN ADD **** 12/22/99 *********************************************
     C                   PARM      @@QTE         ##QTE
      **** END   ADD **** 12/22/99 *********************************************
     C*
     C                   ENDSR                                                  DLTPRE
     C*
     C/EJECT
     C*===============================================================
     C* DSPLYR - SUBROUTINE FOR DISPLAYING INFORMATION
     C*===============================================================
     C*
     C     DSPLYR        BEGSR
     C*** BEG ADD ***** 09/13/10 *************************************
     C                   MOVE      I@94          *IN94
     C*** END ADD ***** 09/13/10 *************************************
     C                   ENDSR                                                  DSPLYR
     C*
     C*===============================================================
     C* DSPPRE - SUBROUTINE FOR GETTING INFO PRIOR TO SCREEN DISPLAY
     C*===============================================================
     C*
     C     DSPPRE        BEGSR
     C*
     C*  GET BILLING SCHEDULE DESCRIPTION
     C*
     C     SCBSCD        IFNE      *ZEROS
     C*
     C                   MOVEL     *BLANKS       SCBSDS
     C                   MOVEL(P)  SCBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEL(P)  BSDESC        SCBSDS
     C*
     C*  DETERMINE # OF SCREENS DEPENDING IF THE RETRO/LOSS PLAN IS
     C*  INSTALLED
     C*
     C*** BEGIN DEL ***  09/22/99  ************************************
     C*******    BSLPGM    IFEQ 'P'
     C*******    BSLPGM    OREQ 'I'
     C*******              Z-ADD3         #OFSCR
     C*******              ENDIF                           BSLPGM=P/I
     C*** END DEL *****  09/22/99  ************************************
     C*
     C                   ENDIF                                                  SCBSCD<>*ZEROS
     C***** BEG ADD ******* 09/27/07 **********************************
     C     CLTID         IFNE      '093'
     C                   SETON                                        26
     C                   ENDIF
     C***** END ADD ******* 09/27/07 **********************************
     C*
     C*  GET RATE SET DESCRIPTION
     C*
     C                   MOVEL     *BLANKS       SCRTDS
     C                   MOVEL(P)  SCRTCD        XXKEY
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL(P)  RSDESC        SCRTDS
     C  N60              MOVEL     RSJRST        XXST
     C   60              MOVEL     *BLANKS       SCRTDS
     C   60              MOVEL     *BLANKS       XXST
     C***  BEG ADD ******  01/27/03  *********************************
     C     RSJRST        IFEQ      'MA'
     C                   SETON                                        14
     C                   ELSE
     C                   SETOFF                                       14
     C                   ENDIF
     C***  END ADD ******  01/27/03  *********************************
     C****** Beg Add ******* 01/27/12 **********************************
      *
     C                   IF        XXPFDT = *ZEROS
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
     C                   ENDIF
      *
     C****** End Add ******* 01/27/12 **********************************
     C*
     C*  GET STATE REPORTING/RATING FILE
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*** BEGIN ADD ***  10/28/97  ************************************
     C* SET INDICATORS FOR DISPLAYING CREDITS AND DISCOUNTS.
     C     RRDV1C        IFEQ      *ZERO
     C***  BEG ADD ******  03/11/11  *********************************
     C     @@CFLG        ANDNE     '1'
     C     RRDV1C        OREQ      *ZERO
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDNE     'NM'
     C     RRDV1L        OREQ      *BLANKS
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDEQ     'NM'
     C***  END ADD ******  03/11/11  *********************************
     C                   MOVE      *ON           *IN43
     C                   ELSE
     C                   MOVE      *OFF          *IN43
     C                   ENDIF                                                  CODE IFEQ
     C*** BEG   ADD ***  06/21/10  ************************************
     C*If heading for Dev1 then show Heading for Dev1 Split2
     C     *IN43         IFEQ      *OFF
     C     RRDV1S        ANDEQ     'Y'
     C     SCANDT        ANDNE     *ZEROS
     C                   MOVE      *OFF          *IN13
     C                   ELSE
     C*
     C     *IN43         IFEQ      *OFF
     C     ELD1S2        ANDNE     0
     C                   MOVE      *OFF          *IN13
     C                   ELSE
     C                   MOVE      *ON           *IN13
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD ***  06/21/10  ************************************
     C     RRDV2C        IFEQ      *ZERO
      *** BEG ADD ***** 01/14/10 **************************************
     C     CLTID         ANDNE     '025'
      *** END ADD ***** 01/14/10 **************************************
     C                   MOVE      *ON           *IN44
     C                   ELSE
     C                   MOVE      *OFF          *IN44
     C                   ENDIF                                                  CODE IFEQ
     C*** BEG   ADD ***  06/21/10  ************************************
     C*If heading for Dev2 then show Heading for Dev2 Split2
     C     *IN44         IFEQ      *OFF
     C     RRDV2S        ANDEQ     'Y'
     C     SCANDT        ANDNE     *ZEROS
     C                   MOVE      *OFF          *IN16
     C                   ELSE
     C*
     C     *IN44         IFEQ      *OFF
     C     ELD2S2        ANDNE     0
     C                   MOVE      *OFF          *IN16
     C                   ELSE
     C                   MOVE      *ON           *IN16
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD ***  06/21/10  ************************************
     C     RR1ACD        IFEQ      *ZERO
     C                   MOVE      *ON           *IN63
     C                   ELSE
     C                   MOVE      *OFF          *IN63
     C                   ENDIF                                                  CODE IFEQ
     C*
     C     RR1BCD        IFEQ      *ZERO
     C                   MOVE      *ON           *IN64
     C                   ELSE
     C                   MOVE      *OFF          *IN64
     C                   ENDIF                                                  CODE IFEQ
     C*
     C     RRDBCD        IFEQ      *ZERO
     C     RRCRCD        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN65
     C                   ELSE
     C                   MOVE      *OFF          *IN65
     C                   ENDIF                                                  CODE IFEQ
     C*
     C*** BEGIN ADD ***  07/06/06  ************************************
     C     RRARCC        IFEQ      *ZERO
     C                   MOVE      *ON           *IN91
     C                   ELSE
     C                   MOVE      *OFF          *IN91
     C                   ENDIF                                                  CODE IFEQ
     C*** END ADD *****  07/06/06  ************************************
     C*** END ADD *****  10/28/97  ************************************
     C*
     C*  DEVIATION HEADINGS
     C*
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEL     '1st'         RRDV1H
     C     RRDV1H        CAT       DEV:1         RRDV1H
     C                   ENDIF                                                  RRDV1H=*BLANKS
     C*
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEL     '2nd'         RRDV2H
     C     RRDV2H        CAT       DEV:1         RRDV2H
     C                   ENDIF                                                  RRDV2H=*BLANKS
     C*
     C                   MOVEL     *BLANKS       XXDVHD
     C     RRDV1H        CAT       '%':1         XXDVHD
     C                   MOVEL(P)  XXDVHD        SCDVH1
     C*
     C                   MOVEL     *BLANKS       XXDVHD
     c*** BEG   ADD ***  06/21/10  **************************************************
     c     @2NPER        CAT       SCDVH1:1      XXDVHD
     C                   MOVEL(P)  XXDVHD        SCD1H2
     C                   MOVEL     *BLANKS       XXDVHD
     c*** END   ADD ***  06/21/10  **************************************************
     c*** begin chg ***  02/25/10  **************************************************
     C*****RRDV2H        CAT       '%':1         XXDVHD
     c                   if        cltid <> '025'
     C     RRDV2H        CAT       '%':1         XXDVHD
     c                   else
     c                   movel     rrdv2h        xxdvhd
     c                   endif
     c*** end chg *****  02/25/10  **************************************************
     C                   MOVEL(P)  XXDVHD        SCDVH2
     C*
     c*** BEG   ADD ***  06/21/10  **************************************************
     C                   MOVEL     *BLANKS       XXDVHD
     C     @2NPER        CAT       SCDVH2:1      XXDVHD
     C                   MOVEL(P)  XXDVHD        SCD2H2
     c*** END   ADD ***  06/21/10  **************************************************
     C     ' '           CHECKR    SCDVH1        XXLEN             3 0
      /Free
       Select;
       When not useDots;
       // Change length so that dots will not be added;
          xxLen = 22;
       Endsl;
      /End-free
     C*
     C     XXLEN         IFLE      21
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     SCDVH1        CAT       DOTS:1        SCDVH1
     C                   ELSE
     C     SCDVH1        CAT       DOTS:0        SCDVH1
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<=21
     C*
     C     ' '           CHECKR    SCDVH2        XXLEN             3 0
      /Free
       Select;
       When not useDots;
       // Change length so that dots will not be added;
          xxLen = 22;
       Endsl;
      /End-free
     C*
     C     XXLEN         IFLE      21
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     SCDVH2        CAT       DOTS:1        SCDVH2
     C                   ELSE
     C     SCDVH2        CAT       DOTS:0        SCDVH2
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<=21
     C*
     C*  SUBJECT PREMIUM CREDIT 1A HEADING
     C*
     C     RR1AHD        IFEQ      *BLANKS
     C     SPC           CAT       '1A':1        RR1AHD
     C                   ENDIF                                                  RR1AHD=*BLANKS
     C*
     C     ' '           CHECKR    RR1AHD        XXLEN             3 0
     C*
     C     XXLEN         IFLE      21
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     RR1AHD        CAT       DOTS:1        SCSH1A
     C                   ELSE
     C     RR1AHD        CAT       DOTS:0        SCSH1A
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          scSh1A = RR1aHd;
       Endsl;
      /End-free
     C*
     C                   ENDIF                                                  XXLEN<=21
     C*
     C*  SUBJECT PREMIUM CREDIT 1B HEADING
     C*
     C     RR1BHD        IFEQ      *BLANKS
     C     SPC           CAT       '1B':1        RR1BHD
     C                   ENDIF                                                  RR1BHD=*BLANKS
     C*
     C     ' '           CHECKR    RR1BHD        XXLEN             3 0
     C*
     C     XXLEN         IFLE      21
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     RR1BHD        CAT       DOTS:1        SCSH1B
     C                   ELSE
     C     RR1BHD        CAT       DOTS:0        SCSH1B
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          scSh1b = RR1bHd;
       Endsl;
      /End-free
     C*
     C                   ENDIF                                                  XXLEN<=21
     C*
     C*** BEG ADD *****  09/01/04  ************************************
     C*
     C*  VOLUNTARY LABOR CHARGE
     C*
     C     RRVLCC        IFEQ      *ZERO
     C     RRVLCH        OREQ      *BLANK
     C                   MOVE      *OFF          *IN42
     C                   ELSE
     C                   MOVE      *ON           *IN42
     C     ' '           CHECKR    RRVLCH        XXLEN
     C*
     C     XXLEN         IFLE      21
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     RRVLCH        CAT       DOTS:1        SCVLCH
     C                   ELSE
     C     RRVLCH        CAT       DOTS:0        SCVLCH
     C                   ENDIF                                                  XXLEN=*ZERO
      /Free
       Select;
       When not useDots;
          scVlCh = RRvlCh;
       Endsl;
      /End-free
     C*
     C                   ENDIF                                                  XXLEN<=21
     C*
     C                   ENDIF                                                  RRVLCC = 0
     C*** END ADD *****  09/01/04  ************************************
     C*** BEGIN ADD ***  11/20/01  ************************************
     C*
     C*  SAFETY FEE HEADING
     C*
     C     RRSFHD        IFEQ      *BLANKS
     C                   MOVEL     SAF           RRSFHD
     C                   ENDIF                                                              KS
     C*
     C                   MOVEL     *BLANKS       XXDVHD
     C     RRSFHD        CAT       '%':1         XXDVHD
     C                   MOVEL(P)  XXDVHD        SCSFHD
     C*
     C     ' '           CHECKR    SCSFHD        XXLEN             3 0
      /Free
       Select;
       When not useDots;
       // Change length so that dots will not be added;
          xxLen = 22;
       Endsl;
      /End-free
     C*
     C     XXLEN         IFLE      21
     C*
     C                   DIV       2             XXLEN
     C                   MVR                     XXLEN
     C*
     C     XXLEN         IFEQ      *ZERO
     C     SCSFHD        CAT       DOTS:1        SCSFHD
     C                   ELSE
     C     SCSFHD        CAT       DOTS:0        SCSFHD
     C                   ENDIF                                                  XXLEN=*ZERO
     C*
     C                   ENDIF                                                  XXLEN<=21
     C*** END ADD *****  11/20/01  ************************************
     C*
     C*  GET INCREASED LIMIT #1 DESCRIPTION
     C*
     C     SCILCD        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCILCD        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C  N60              MOVEL(P)  LMDESC        SCILDS
     C   60              MOVEL     *BLANKS       SCILDS
     C*
     C                   ENDIF                                                  SCILCD<>*ZEROS
     C*
     C*  GET INCREASED LIMIT #2 DESCRIPTION
     C*
     C     SCINL2        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCINL2        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C  N60              MOVEL(P)  LMDESC        SCI2DS
     C   60              MOVEL     *BLANKS       SCI2DS
     C*
     C                   ENDIF                                                  SCINL2<>*ZEROS
     C*
     C*  GET DEDUCTIBLE DISCOUNT DESCRIPTION
     C*
     C     SCDCDE        IFNE      *ZEROS
     C*
     C***  BEGIN CHG  ***  04/07/05-2 ***********************************
     C     *LIKE         DEFINE    DDCODE        XXDDCD
     C*
     C                   MOVEL(P)  SCDCDE        XXDDCD
     C***  END   CHG  ***  04/07/05-2 ***********************************
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C  N60              MOVEL(P)  DDDESC        SCDCDS
     C   60              MOVEL     *BLANKS       SCDCDS
     C*
     C                   ENDIF                                                  SCDCDE<>*ZEROS
     C*
     C***  BEGIN CHG  ***  04/07/05-2 ***********************************
     C     SCDCD2        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCDCD2        XXDDCD
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C*
      ***** BEG ADD ***** 06/22/05 **************************************
     C   60DDKY1         CHAIN     PDD01                              60
      ***** END ADD ***** 06/22/05 **************************************
     C  N60              MOVEL(P)  DDDESC        SC2DDS
     C   60              MOVEL     *BLANKS       SC2DDS
     C*
     C                   ENDIF                                                  SCDCD2<>*ZEROS
     C***  END   CHG  ***  04/07/05-2 ***********************************
     C*
     C*  GET MCO TYPE DESCRIPTION
     C*
     C                   MOVEL     *BLANKS       SCMCOD
     C     SCMCCD        IFNE      *BLANKS
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVEL(P)  SCMCCD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCMCOD
     C                   ENDIF                                                  SCMCCD<>*BLANKS
     C*
     C*  USE DEFAULT STOCK DISCOUNT, NECESSARY
     C*
     C     SCASP3        IFEQ      *ZEROS
     C**  GET DISCOUNT
     C***  BEGIN ADD  ***  12/30/97  *********************************
     C     SCDISC        ANDEQ     *BLANKS
     C***  BEGIN ADD  ***  06/12/98  *********************************
     C     @@MODE        ANDEQ     'A'
     C     Q2RECF        ANDEQ     'SCR01'
     C***  END   ADD  ***  06/12/98  *********************************
     C*
     C     RRDISC        IFNE      *BLANKS
     C                   MOVEL     RRDISC        SCDISC
     C                   ELSE
     C                   Z-ADD     RRDSPC        SCASP3
     C                   ENDIF                                                  RRDISC<>*BLANKS
     C*
     C***  END   ADD  ***  12/30/97  *********************************
     C                   ENDIF                                                  SCASP3=*ZEROS
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFEQ      '025'
     C     Q2RECF        ANDEQ     'SCR01'
     C                   MOVE      '1'           *IN47
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C*** beg del ***** 08/22/11 ***************************************
     C****begin*Add ****** 03/01/11 ************************************
     C**********         if        xxflg3c = *OFF
     C****END***Add ****** 03/01/11 ************************************
     C****begin*Add ****** 12/31/10 ************************************
     C*Execute*Load3c once to populate default value for new policy year.
     C**********         if        q2recf = 'SCR03' and @@ftim = 'Y'
     C**********         eval      *in97 = *on
     C**********         move      X'22'         AT16F
     C**********         move      X'22'         AT17F
     C**********         exsr      load3c
     C**********         eval      @@ftim = ' '
     c**********         endif
     C**********
     C**********         if        SCFFLA = 'Y'  and *inkl = *on
     C**********         eval      *in97 = *on
     C**********         move      X'22'         AT16F
     C**********         move      X'22'         AT17F
     C**********         else
     C**********         if         *inkl  = *off and scffla = *blanks and
     C**********                             *in09 = *off
     C**********         eval      *in97= *off
     C**********         move      X'24'         AT16F
     C**********         move      X'24'         AT17F
     C**********         endif                                                  scffla = *blanks
     C**********         endif                                                  scffla = 'Y'
     C****end***add ****** 12/31/10 ************************************
     C****begin*Add ****** 03/01/11 ************************************
     C**********         ENDIF
     C**********         if        xxflg3c = *On
     C**********         move      *blanks       scffla
     C**********         move      X'A7'         AT392
     C**********         move      X'A7'         AT3966
     C**********         ENDIF
     C****END***Add ******N03/01/11 ************************************
     C*** end del ***** 08/22/11 ***************************************
     C*
     C                   ENDSR                                                  DSPPRE
     C*
     C/EJECT
     C***  BEGIN ADD  ***  12/01/99  *********************************
     C*===============================================================
     C* DUPCHK - BE SURE THE RECORD IS NOT DUPLICATED
     C*===============================================================
     C*
     C     DUPCHK        BEGSR
     C*
B02  C     ELKY1         CHAIN     PEL01                              60
     C  N60              SETON                                        79  90
     C*
     C                   ENDSR                                                  DUPCHK
     C***  END   ADD  ***  12/01/99  *********************************
     C/EJECT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE FOR ERROR CHECKING SCR01
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C***  Begin Add  ***  11/22/04  **************************************
     C*  The CHECK(ER) is on SCRLFL, therefore, this needs to be checked
     C*   first so that the error checking on previous fields does not get
     C*   implemented.
     C     SCRLFL        IFNE      SVRLFL
     C     SCRLFL        ANDNE     S2RLFL
     C                   EXSR      CHGRNW
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*****              ENDIF
     C***  End   Add  ***  11/22/04  **************************************
     C*
     C  N90              Z-ADD     SCFYR         XXFYR
     C*
     C*  POLICY YEAR MUST BE VALID FOR FUND (IF IN ADD MODE)
     C*
B02  C     F1KY1         CHAIN     PF101                              60
     C   40
     CANN90
     CAN 60              SETON                                        20  90
     C*
     C  N90F1CLMS        COMP      'N'                                6767
     C*
     C*  CHECK TO BE SURE POLICY YEAR HAS NOT BEEN USED PREVIOUSLY
     C*
     C   40
B02  CANN90ELKY1         CHAIN     PEL01                              60
     C   40
     CANN90
     CANN60              SETON                                        21  90
      *
     C*
     C*  EDIT POLICY FROM DATE
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPFDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        28  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  POLICY FROM DATE CANNOT BE LESS THAN INITIAL EFFECTIVE DATE
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
     C*
     C  N90XXPFDT        IFLT      EMEFD1
     C                   SETON                                        29  90
     C                   ENDIF                                                  XXPF<EMEFD1
     C*
     C*  POLICY FROM DATE MUST BE LESS THAN OR EQUAL TO ALL EXISTING
     C*  PAYROLL RECORDS
     C*
     C  N90EPKY1         CHAIN     PEP01                              60
     C*
     C  N90*IN60         DOWEQ     *OFF
     C*
     C     EPEFDT        IFLT      XXPFDT
     C     EPSTAT        ANDNE     'I'
     C                   SETON                                        306090
     C                   ENDIF                                                  EPEFDT<XXPF/EPSTAT<>
     C*
     C  N90EPKY1         READE     PEP01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C*  EDIT POLICY TO DATE
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        33  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        31  90
     C                   ENDIF                                                  XXDAT2<XXDAT1
     C*
     C*  BOTH DATES MUST BE BETWEEN FUND POLICY START & END DATES
     C*
     C  N90FNCGFL        IFNE      'Y'
     C*
     C     XXDAT1        IFLT      F1STPO
     C     XXDAT1        ORGE      F1ENPO
     C     XXDAT2        ORLT      F1STPO
     C     XXDAT2        ORGT      F1ENPO
     C                   SETON                                        32  90
     C                   ENDIF                                                  XXDAT1<F1STPO
     C                   ELSE
     C     EMSTRP        IFEQ      'Y'
     C     XXDAT1        IFLT      F1STPO
     C     XXDAT1        ORGE      F1ENPO
     C                   SETON                                        32  90
     C                   ENDIF                                                  XXDAT1<F1STPO
     C                   ENDIF                                                  EMSTRP = Y
     C*
     C                   ENDIF                                                  FNCGFL<>Y
     C*
     C* DATES CANNOT OVERLAP DATE RANGES FOR THAT EMPLOYER
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     ELFYR         IFNE      SCFYR
     C     XXDAT1        IFGE      ELPFDT
     C     XXDAT1        ANDLT     ELPTDT
     C                   SETON                                        18  90
     C                   ENDIF                                                  ELPFDT
     C*
     C     XXDAT2        IFGT      ELPFDT
     C     XXDAT2        ANDLT     ELPTDT
     C                   SETON                                        19  90
     C                   ENDIF                                                  ELPTDT
     C                   ENDIF                                                  ELFYR
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C                   MOVE      *OFF          DTLESS
     C                   MOVE      ' '           XXSTAR
     C                   MOVE      ' '           XXOTAR
     C                   Z-ADD     *ZEROS        SCSRAD
     C*
     C*  POLICY END DATE CANNOT BE > POLICY END DATE OF THE STATE
     C*   REPORTING DIVISION  OR < = Policy FROM Date (01/17/02)
     C*
      *
      *   if not claims only client
      *
     C                   MOVEL     'WC'          XSKEY             2
     C     XSKEY         CHAIN     SWC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     WCFL08        ANDNE     'C'
      *
     C  N90EMKY1         CHAIN(N)  PEM01                              60
     C  N90*IN60         DOWEQ     *OFF
     C*
     C     ELKY2         CHAIN(N)  PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDNE     'Y'
     C     ELARSM        ANDEQ     'Y'
     C                   MOVE      'Y'           XXOTAR            1
     C                   ENDIF
     C*
     C     EMSTRP        IFEQ      'Y'
     C*
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSRAD
     C     EMDIV         IFNE      SCDIV
     C*
     C     ELKY2         CHAIN(N)  PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     ELARSM        ANDEQ     'Y'
     C                   MOVE      ELBSCD        SVBSCD
     C                   MOVE      ELBSDT        SVBSDT
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     ELARSM        ANDEQ     'Y'
     C                   MOVE      'Y'           XXSTAR            1
     C                   ENDIF
     C*
      *** BEG ADD ***** 01/25/13 ******************************************
     C     XXDAT1        IFNE      XXDAT2
      *** END ADD ***** 01/25/13 ******************************************
     C     XXDAT2        IFGT      ELPTDT                                        > TO DATE
     C     XXDAT2        ORLE      ELPFDT                                        < FROM DATE
     C                   SETON                                        34  90
     C                   LEAVE
     C                   ENDIF                                                  XXDAT2>ELPTDT
      *** BEG ADD ***** 01/25/13 ******************************************
     C                   ENDIF
      *** END ADD ***** 01/25/13 ******************************************
     C*
     C                   MOVE      *OFF          DTLESS
     C  N90XXDAT2        IFLT      ELPTDT
     C                   MOVE      *ON           DTLESS            1
     C                   ENDIF                                                  XXDAT2<ELPTDT
     C*
     C                   ENDIF                                                  EMDIV<>SCDIV
     C*
     C                   ENDIF                                                  EMSTRP=Y
     C*
     C  N90EMKY1         READE(N)  PEM01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   ENDIF                                                  WCFL08
     C*
     C     @@RRN2        CHAIN(N)  WMEMP                              60
     C  N40@@RRN         CHAIN(N)  WDELP                              60
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      'Y'           XXSTRP            1
     C                   ELSE
     C                   MOVE      ' '           XXSTRP
     C                   ENDIF
     C*
     C*  TO DATE MUST EQUAL CANCEL DATE IN EMPLOYER MASTER FOR EACH DIV
     C*
     C  N90DTLESS        IFEQ      *ON
     C*
     C     EMKY          CHAIN     PEM01                              60
     C*
     C     XXDAT2        IFNE      EMCAD1
     C     XXDAT2        ANDNE     EMCAD2
     C     XXDAT2        ANDNE     EMCAD3
     C     XXDAT2        ANDNE     EMCAD4
     C     XXDAT2        ANDNE     EMCAD5
     C                   SETON                                        36  90
     C                   ENDIF                                                  XXDAT2<>EMCAD1/2/3/4
     C*
     C                   ENDIF                                                  DTLES=*ON
     C*
     C*  POLICY END DATE CANNOT BE MORE THAN 365(366) DAYS PAST POLICY
     C*   START DATE
     C*
     C  N90FNCGFL        IFNE      'Y'
     C                   CALL      'SRDDIF8'
     C                   PARM                    SCPFDT
     C                   PARM                    SCPTDT
     C                   PARM                    XXDAYS            5 0
     C                   PARM                    XXERR             1
     C*
     C                   MOVE      *OFF          LEAPYR            1
     C*    TO DATE
     C*
     C                   MOVE      XXDAT2        XXDT
     C     XXYY          DIV       4             XXLPYR            2 1
     C                   MVR                     XXREM             1 0
     C*
     C     XXREM         IFEQ      *ZERO
     C     XXMMDD        ANDGT     0228
     C                   MOVE      *ON           LEAPYR
     C                   ENDIF                                                  XXREM=*ZERO
     C*
     C*    FROM DATE
     C*
     C                   MOVE      XXDAT1        XXDT
     C     XXYY          DIV       4             XXLPYR
     C                   MVR                     XXREM
     C*
     C     XXREM         IFEQ      *ZERO
     C     XXMMDD        ANDLT     0301
     C                   MOVE      *ON           LEAPYR
     C                   ENDIF                                                  XXREM=*ZERO
     C*
     C  N90LEAPYR        IFEQ      *OFF
     C*
     C     XXDAYS        IFGT      365
     C                   SETON                                        35  90
     C                   ENDIF                                                  XXDAYS>365
     C*
     C                   ELSE
     C*
     C     XXDAYS        IFGT      366
     C                   SETON                                        35  90
     C                   ENDIF                                                  XXDAYS>366
     C*
     C                   ENDIF                                                  LEAPYR=*OFF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     XXDAT1        X7MXDT                         FROM DATE
     C                   ADD       XXMXEX        X7MXMM
     C                   ADD       XXMXCY        X7MXCY
     C*
     C     X7MXMM        DOWGT     12
     C                   ADD       1             X7MXCY
     C                   SUB       12            X7MXMM
     C                   ENDDO                                                  X7MXMM>12
     C*
     C     XXDAT2        IFGT      X7MXDT
     C                   SETON                                        83  90
     C                   ENDIF                                                  XXDAT2>X7MXDT
     C*
     C                   ENDIF                                                  FNCGFL IFNE
      *
      * If policy eff date range change, check for claims out of new
      * policy eff date range.
      *
     C  N90@@MODE        IFEQ      'C'
     C  N90              EXSR      CKPOLD
     C                   ENDIF
      *
     C*
     C*  EDIT ANNIVERSARY RATING DATE
     C*
     C  N90SCANDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SCANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCANDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        37  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SCANDT<>*ZERO
     C*
     C*  ANNIV DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C*
     C                   Z-ADD     SCANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXANDT
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
     C*
     C  N90XXANDT        IFGT      XXPFDT
     C                   SETON                                        38  90
     C                   ENDIF                                                  XXANDT>XXPF
     C*
     C*  ANNIV DATE ON EACH DIVISION SHOULD BE CONSISTENT WITH THE
     C*  STATE REPORTING DIVISION (IGNORE INFO MSG IF CLAIMS ONLY)
     C*
     C     SCANDT        IFNE      SCSRAD
     C     XXSTRP        ANDNE     'Y'
     C     F1CLMS        ANDEQ     'N'
     C                   EXFMT     WDW02
     C     *INKL         IFEQ      *ON
     C                   SETON                                        90
     C                   END
     C                   END
     C*
     C*  DO NOT PERFORM THESE EDIT IF CLAIMS ONLY GROUP
     C*
      *** BEG CHG ***** 08//08/03 ****************************************
     C**N90******F1CLMS****IFEQ 'N'
     C  N90F1CLMS        IFNE      'Y'
      *** END CHG ***** 08//08/03 ****************************************
     C*
     C*  VALIDATE THE A/R SUMMARY FLAG
     C*
     C  N90SCARSM        IFNE      'Y'
     C     SCARSM        ANDNE     ' '
     C                   SETON                                        17  90
     C                   ENDIF                                                  SCARSM IFNE
     C*
     C*  DO NOT ALLOW SUMMARY IF STATE RPT DIV IS NOT SUMMARY
     C*
     C  N90SCARSM        IFEQ      'Y'
     C     XXSTAR        ANDNE     'Y'
     C     XXSTRP        ANDNE     'Y'
     C                   SETON                                        16  90
     C                   ENDIF                                                  SCARSM IFEQ
     C*
     C*  DO NOT ALLOW STATE RPT TO BE CHANGED IF OTHERS ARE SUMMARY
     C*
     C  N90SCARSM        IFEQ      ' '
     C     XXOTAR        ANDEQ     'Y'
     C     XXSTRP        ANDEQ     'Y'
     C                   SETON                                        13  90
     C                   ENDIF                                                  SCARSM IFEQ
     C*
     C*  DEFAULT BILLING SCHEDULE CODE AND START DATE IN ADD MODE
     C*
     C   40
     CANN90SCARSM        IFEQ      'Y'
     C     XXSTRP        ANDNE     'Y'
     C*
     C     SCBSCD        IFEQ      *BLANKS
     C     SCBSCD        OREQ      *ZEROS
     C                   MOVE      SVBSCD        SCBSCD
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCBSCD IFEQ
     C*
     C     SCBSDT        IFEQ      *ZERO
     C                   Z-ADD     SVBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCBSDT
     C                   MOVE      *ON           *IN90
     C                   ENDIF                                                  SCBSDT IFEQ
     C*
     C                   ENDIF                                                  SVPFDT IFEQ
     C*
     C*  BILLING SCHEDULE CODE AND START DATE MUST EQUAL STATE RPT DIV
     C*  IF SUMMARY AND EFFECTIVE DATE IS THE SAME.
     C*
     C  N90SCARSM        IFEQ      'Y'
     C     XXSTRP        ANDNE     'Y'
     C*
     C                   Z-ADD     SCBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBSDT
     C*
     C                   Z-ADD     SVBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXBSDT            8 0
     C                   MOVE      SVBSCD        XXBSCD            3
     C*
     C     SCBSCD        IFNE      XXBSCD
     C     SCBSDT        ORNE      XXBSDT
     C                   SETON                                        12  90
     C                   ENDIF                                                  SCBSCD IFNE
     C*
     C                   ENDIF                                                  SCARSM IFEQ
     C*
      ***** BEG ADD ***** 03/03/05 ***********************************
     C  N90SCIFFL        IFNE      'Y'
     C     SCIFFL        ANDNE     *BLANKS
     C                   SETON                                        66  90
     C                   ENDIF                                                  SCIFFL<>Y/
      ***** END ADD ***** 03/03/05 ***********************************
     C*
     C*** Begin add *** 07/09/04 **************************************
     C*  Validate the "Adverse acct" flag
     C*
     C  N90SCADVR        IFNE      *BLANKS
7    C     SCADVR        ANDNE     'Y'
     C                   SETON                                        161790
     C                   ENDIF                                                  SCADVR
     C*** End add ***** 07/09/04 **************************************
     C*
     C*  VALIDATE THE POLICY TYPE
     C*
     C*** Beg Add *** 10/25/10 ***********************************************
     C  N90              if        XXRQD6 = 'Y' and SCQTYP = *BLANKS
     C                   seton                                        90  98
     C                   endif
     C*** End Add *** 10/25/10 ***********************************************
     C     SCQTYP        IFNE      *BLANKS
     C*** Beg Add *** 07/28/10 ***********************************************
     C     FHF6TB        ANDNE     *Blanks
     C*** End Add *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C     FLG132        ANDEQ     'Y'
     c     FLG132        OREQ      'N'
     C     SCQTYP        ANDNE     *BLANKS
     C*** End Add *** 12/30/10 ***********************************************
     c*** begin add ***  02/05/10  *************************************
     c     CLTID         OREQ      '025'
     c     F1CLMS        ANDEQ     'N'
     c*** end add *****  02/05/10  *************************************
     C*** Beg Chg *** 07/28/10 ***********************************************
     C*****              MOVEL(P)  'PTYP'        XXTBID
     C*** Beg Chg *** 12/30/10 ***********************************************
     C*****              MOVEL(P)  FHF6TB        XXTBID
     C     FLG132        IFEQ      'N'
     C                   MOVEL(P)  'PTYP'        XXTBID
     C                   ELSE
     C                   MOVEL(P)  FHF6TB        XXTBID
     C                   ENDIF
     C*** End Chg *** 12/30/10 ***********************************************
     C*** End Chg *** 07/28/10 ***********************************************
     C                   MOVEL(P)  SCQTYP        XXKEY
     C*
     C                   MOVEL     *BLANKS       SCQTYD
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCQTYD
     C*
     C  N90*IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        78  90
      ***** BEG ADD ******* 10/13/03 **********************************
     C                   SETOFF                                       22
      ***** END ADD ******* 10/13/03 **********************************
     C                   ENDIF                                                  *IN60=*ON/MASTAT<>A
     C                   ENDIF                                                  SCQTYP IFNE
      ***** BEG ADD ******* 10/13/03 **********************************
      *
      *  "Do not renew (Y/ )"
      *
     C  N90SCRLFL        IFNE      *BLANKS
     C     SCRLFL        ANDNE     'Y'
     C                   SETON                                        227890
     C                   ENDIF                                                  SCRLFL
      ***** END ADD ******* 10/13/03 **********************************
     C*
     C*  VALIDATE THE BILLING SCHEDULE TYPE IF ENTERED
     C*
     C  N90SCBSTY        IFNE      *BLANKS
      *** BEG CHG ***** 04/11/03 ***************************************
     C*********  SCBSTY    ANDNE'Q'
     C*********  SCBSTY    ANDNE'M'
     C*********  SCBSTY    ANDNE'A'
     C*********  SCBSTY    ANDNE'O'
     C**********           SETON                     22  90
     C                   SELECT
     C     VNDRFL        WHENEQ    *OFF
     C     ACHFLG        ANDEQ     *OFF
     C     SCBSTY        ANDNE     'Q'
     C     SCBSTY        ANDNE     'M'
     C     SCBSTY        ANDNE     'A'
     C     SCBSTY        ANDNE     'O'
     C                   SETON                                        22  90
      ***** BEG ADD ******* 10/13/03 **********************************
     C                   SETOFF                                       78
      ***** END ADD ******* 10/13/03 **********************************
     C     VNDRFL        WHENEQ    *ON
     C     ACHFLG        ANDEQ     *OFF
     C     SCBSTY        ANDNE     'Q'
     C     SCBSTY        ANDNE     'M'
     C     SCBSTY        ANDNE     'A'
     C     SCBSTY        ANDNE     'O'
     C     SCBSTY        ANDNE     'W'
     C                   SETON                                        88  90
     C     ACHFLG        WHENEQ    *ON
     C     SCBSTY        ANDNE     'Q'
     C     SCBSTY        ANDNE     'M'
     C     SCBSTY        ANDNE     'A'
     C     SCBSTY        ANDNE     'O'
     C     SCBSTY        ANDNE     'W'
     C     SCBSTY        ANDNE     'T'
     C                   SETON                                        89  90
     C                   ENDSL
      *** END CHG ***** 04/11/03 ***************************************
     C                   ENDIF                                                  XXBSTY<>*BLANKS
      *
     C*
     C*  THE BILLING SCHEDULE CODE
     C*
     C  N90SCBSCD        IFEQ      *BLANKS
     C     SCBSCD        OREQ      *ZEROS
      *** BEG ADD ***** 03/20/06 ***************************************
     C     SCBSTY        ORNE      *BLANK
      *** END ADD ***** 03/20/06 ***************************************
     C                   EXSR      BILAUX
     C                   ENDIF                                                  SCBSCD=*BLANKS
     C*
      *** BEG ADD ***** 03/20/06 ***************************************
     C                   MOVE      *BLANKS       SCBSTY
      *** END ADD ***** 03/20/06 ***************************************
     C                   MOVEL(P)  SCBSCD        XXKEY
     C*
     C  N90TBKY          CHAIN     PBS01                              60
     C*
     C  N90*IN60         IFEQ      *ON
     C     BSSTAT        ORNE      'A'
     C                   SETON                                        23  90
     C                   ENDIF                                                  *IN60=*ON/BSSTAT<>A
      *
      *
      * Billings must not be posted after policy effective date
      *
     C  N90              EXSR      SRBILL
      *
      *
      *** BEG DEL ***** 04/08/05 ***********************************************
     C**N90***** BSSEFL    IFEQ 'S'
     C********** SCARSM    ANDEQ'Y'
     C**********           SETON                     08  90
     C**********           ENDIF
      *** END DEL ***** 04/08/05 ***********************************************
     C*
     C*  DETERMINE # OF SCREENS DEPENDING IF THE RETRO/LOSS PLAN IS
     C*  INSTALLED
     C*
     C*  EDIT BILLING START DATE
     C*
     C                   Z-ADD     SCBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCBSDT
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        24  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C* BILLING START DATE CANNOT BE GREATER THAN POLICY EFFECTIVE DATE
     C*
     C                   Z-ADD     SCBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT1             7 0
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT2             7 0
     C*
     C  N90XXDT1         IFGT      XXDT2
     C                   SETON                                        68  90
     C                   ENDIF
      *** BEGIN ADD *** 11/23/04 ***********************************************
     C*
     C* DO NOT ALLOW BILLING START DATE TO BE PRIOR TO CURRENT DATE IF
     C* WE ARE CHANGING THE VALUE OF THE FIELD OR ADDING A RECORD.
      ***** BEG ADD ***** 02/16/05 ********************************************
     C* UNLESS THE CHANGE IS THE BILLING START DATE TO = POLICY FROM DATE
      ***** END ADD ***** 02/16/05 ********************************************
     C*
     C  N90*IN40         IFEQ      *ON
     C     XXDT2         ANDGT     SYSDT
     C     *IN40         OREQ      *OFF
     C     XXDT1         ANDNE     ELBSDT
     C*
     C     XXDT1         IFLT      SYSDT
      ***** BEG ADD ***** 02/16/05 ********************************************
     C     XXDT1         ANDNE     XXDT2
      ***** END ADD ***** 02/16/05 ********************************************
      ***** BEG ADD ***** 05/18/05 ********************************************
     C     SCARSM        ANDNE     'Y'
      ***** END ADD ***** 05/18/05 ********************************************
     C                   SETON                                        95  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40 IFEQ
      *** END ADD ***** 11/23/04 ***********************************************
     c*** begin add ***  03/28/08  *********************************************
     C*
     C*  Verify ACH setup if online s/r and billing schedule code changes when not
     C*  in Add mode.
     C*
     C  N90              if        scbscd <> s2bscd and *in40 = *off
     c*
     c                   if        bsonli = 'Y'
     c*
     c                   move      elco#         q3co#
     c                   move      elfnd         q3fnd
     c                   move      elemp#        q3emp#
     c                   move      eldiv         q3div
     c                   move      elfyr         q3fyr
     C                   z-add     elpfdt        srdti
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   movel     srdto         q3efdt
     c                   move      '1'           q3mode
     c                   move      *blank        q3err
     c                   movel     scbscd        q3data
     c*
     c                   call      'SRONLI'
     C                   PARM                    Q3CO#             3
     C                   PARM                    Q3FND             3
     C                   PARM                    Q3EMP#            9
     C                   PARM                    Q3DIV             5
     C                   PARM                    Q3FYR             3
     C                   PARM                    Q3EFDT            8
     C                   PARM                    Q3MODE            1
     C                   PARM                    Q3ERR             1
     C                   PARM                    Q3DATA          100
     c*
     c                   if        q3err <> '0'
     C                   SETON                                        96  90
     c                   endif
     c*
     c                   endif                                                  if bsonli =
     c*
     c                   endif                                                  if scbscd <>
     c*** end add *****  03/28/08  *********************************************
     C*
     C*  RESET RATE SET CODE CHANGE INDICATOR
     C*
     C                   MOVE      'N'           XXRCCG            1
      *
      *
     C*
     C*  VERIFY THAT THE RATE SET CODE EXISTS IN THE PAYROLL CLASS FILE
     C*
     C  N90PRKY          CHAIN     PPR01                              60
     C*
     C  N90*IN60         IFEQ      *ON
      *+*+  BEG CHG *+*+*+  06/06/07 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+ *
     C*********            SETON                     25  90
     C*** beg chg ***** 01/12/12 *********************************************
     C***********        SETON                                        41  90
     C                   SETON                                        867190
     C*** end chg ***** 01/12/12 *********************************************
      *+*+  END CHG *+*+*+  06/06/07 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+ *
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  CHAIN TO STATISTICAL RPT/RATING FILE TO FIND STATE INFO
     C*  IF THERE IS NO POLICY INFO FOR THAT STATE, GIVE ERROR
     C*  MESSAGE.
     C*
     C                   CLEAR                   XXST
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL     RSJRST        XXST
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C  N90*IN60         IFEQ      *ON
     C                   SETON                                        26  90
     C*
     C*  IF THE RATE SET CODE HAS BEEN CHANGED, THEN RELOAD THE STOCK
     C*  DISCOUNT DEFAULTS
     C*
     C                   ELSE
     C     ELRTCD        IFNE      SCRTCD
     C     XXSVST        ANDNE     XXST
     C     ELASP3        ANDEQ     *ZEROS
     C                   MOVE      'Y'           XXRCCG
     C                   MOVE      *BLANKS       SCDISC
     C                   Z-ADD     *ZEROS        SCASP3
     C     RRDISC        IFNE      *BLANKS
     C                   MOVEL     RRDISC        SCDISC
     C                   ELSE
     C                   Z-ADD     RRDSPC        SCASP3
     C                   ENDIF                                                  RRDISC<>*BLANKS
     C                   ENDIF                                                  ELRTCD<>SCRTCD
     C                   ENDIF                                                  *IN60=*ON
      *+*+  BEG ADD *+*+*+  08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+ *
      *
      *  Rate Set Code edits
      *
     C  N90              EXSR      CKRTC1
      *
      *+*+  END ADD *+*+*+  08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+ *
      *
      *  Additional Rate Set Code edits
      *
     C     @@MODE        IFEQ      'C'
     C  N90              EXSR      CKRTCD
     C                   ENDIF                                                  @@MODE=C
     C*
     C                   ENDIF                                                  F1CLMS=N
     c*** begin del ***  03/12/10  *************************************
     c*
     c* Policy type must be valid if entered or client 25
     c*
     C  n90SCQTYP        IFNE      *BLANKS
     C*** Beg Add *** 07/28/10 ***********************************************
     c     FHF6TB        ANDNE     *Blanks
     C*** End Add *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C     FLG132        ANDEQ     'Y'
     c     FLG132        OREQ      'N'
     C     SCQTYP        ANDNE     *BLANKS
     C*** End Add *** 12/30/10 ***********************************************
     c     CLTID         OREQ      '025'
     C*** Beg Add *** 07/28/10 ***********************************************
     C     FHF6TB        ANDNE     *Blanks
     C*** End Add *** 07/28/10 ***********************************************
     C*** Beg Chg *** 07/28/10 ***********************************************
     C*****              MOVEL(P)  'PTYP'        XXTBID
     C*** Beg Chg *** 12/30/10 ***********************************************
     C*****              MOVEL(P)  FHF6TB        XXTBID
     C     FLG132        IFEQ      'N'
     C                   MOVEL(P)  'PTYP'        XXTBID
     C                   ELSE
     C                   MOVEL(P)  FHF6TB        XXTBID
     C                   ENDIF
     C*** End Chg *** 12/30/10 ***********************************************
     C*** End Chg *** 07/28/10 ***********************************************
     C                   MOVEL(P)  SCQTYP        XXKEY
     C*
     C                   MOVEL     *BLANKS       SCQTYD
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCQTYD
     C*
     C  N90*IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        78  90
     C                   SETOFF                                       22
     C                   ENDIF                                                  *IN60=*ON/MASTAT<>A
     C                   ENDIF                                                  SCQTYP IFNE
     c*** end   del ***  03/12/10  *************************************
      *
      ****** Begin Add 08/22/02 ***************************************
     C*** BEGIN ADD ***  01/04/10  ************************************
     C*** BEGIN DEL ***  03/01/10  ************************************
     C*****SCMBCD        IFNE      ' '
     C*****SCMBCD        ANDNE     'F'
     C*****SCMBCD        ANDNE     'T'
     C*****SCMBCD        ANDNE     'M'
     C*****SCMBCD        ANDNE     'A'
     C*****              SETON                                        44  90
     C*****              ENDIF                                                  SCMBCD IFNE
     C*** END DEL *****  03/01/10  ************************************
     C*** END   ADD ***  01/04/10  ************************************
      *
      *  CANNOT ENTER BLANKS IF CLARENDON PREPROCESSOR
      *
     C                   OPEN      SWCPF
     C  N90              READ      SWC02                                  60
      *
     C  N90*IN60         IFEQ      *OFF
      ****** BEGIN ADD 01/28/03 ***************************************
     C     WCFL20        ANDNE     *BLANK
      ****** END   ADD 01/28/03 ***************************************
     C  N90RSKY          CHAIN     WMRSL                              60
      *
     C  N90*IN60         IFEQ      *ON
     C     SCRTCD        OREQ      *BLANK
     C                   SETON                                        25  90
     C                   ENDIF                                                              *ON
      *
     C                   ENDIF
      *
     C                   CLOSE     SWCPF
      *
      ****** End   Add 08/22/02 ***************************************
     C*
     C*** BEG ADD ***** 12/30/09 **************************************
     C***  Begin Chg  ***  02/27/10  **************************************
     C*****@@MODE        IFEQ      'C'
     C**********         MOVEL     ELBSCD        X2BSCD            3
     C**********         SETOFF                                       5061
     C**N90SCBSCD        IFNE      *BLANKS
     C*****SCBSCD        ORNE      *ZEROS
     C*****SCBSCD        IFNE      X2BSCD
     C**********         MOVEL(P)  ELBSCD        XXKEY
     C*****TBKY*         CHAIN     PBS01                              60
     C******IN60         IFEQ      *OFF
     C*****BSLPGM        IFEQ      'L'
     C*****LDKY*         CHAIN     PLD01                              60
     C******IN60         IFEQ      *OFF
     C**********         SETON                                        506190
     C**********         ENDIF
     C**********         ENDIF
     C**********         ENDIF
     C**********         ENDIF
     C**********         ENDIF
     C**********         ENDIF
     C  n90              if        @@Mode = 'C'
     C                              and scBsCd <> saveScBsCD
     C                              and
     C                               (scBsCd <> *blanks or scBsCd <> *zeros)
     C                   if        saveBSLPGM = 'L'
     C                   eval      clearWMLDP = *off
     C                   endif
     C                   clear                   BSLPGM
     C                   eval      xxKey = scBsCd
     C     TBKY          CHAIN     PBS01
     C     LDKY          CHAIN(n)  PLD01
     C     dDDKY         CHAIN     DDD01
     C                   select
     C* When change from Large Deductible, deductible detail can not exist
     C                   when      (saveBSLPGM = 'L' and BSLPGM <> 'L'
     C                              and %found(WDDDL1))
     C                   seton                                        61  90
     C* When change from Large Deductible, remove deductible setup info
     C                   when      (saveBSLPGM = 'L' and BSLPGM <> 'L'
     C                              and not %found(WDDDL1))
     C                   eval      clearWMLDP = *on
     C                   endsl
     C                   eval      saveScBsCD = scBsCd
     C                   endif
     C***  End   Chg  ***  02/27/10  **************************************
     C*** END ADD ***** 12/30/09 **************************************
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C                   MOVEL(P)  SCBSCD        XXKEY
     C                   SETOFF                                       8291
     C     TBKY          CHAIN     PBS01                              60
     C*
     C  N90SCDCDE        IFNE      *BLANKS
     C     BSLPGM        ANDEQ     'L'
     C                   SETON                                        829190
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C                   SETOFF                                       8494
     C  N90SCDCD2        IFNE      *BLANKS
     C     BSLPGM        ANDEQ     'L'
     C                   SETON                                        849490
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
     C*
     C*
     C*  EDIT "A" RATING FLAG
     C*
     C  N90SCAFLG        IFNE      'Y'
     C     SCAFLG        ANDNE     'N'
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCAFLG<>Y/N
     C*
     C*** Begin dlt *** 01/27/06 **************************************
     C*  AN ANNIVERSARY RATING DATE IS NOT ALLOWED IF EMPLOYER "A" RATING
     C*  IS BEING USED
     C*********
     C**N90****  SCAFLG    IFEQ 'Y'
     C*********  SCANDT    ANDNE*ZEROS
     C*********            SETON                     48  90
     C*********            ENDIF                           SCAFLG=Y
     C*** End dlt ***** 01/27/06 **************************************
     C*
     C*
     C*  CHECK THE INCREASED LIMITS FACTOR 1
     C*
     C  N90              MOVE      *ON           *IN80
     C*
     C                   MOVEL(P)  SCILCD        XXKEY
     C*
     C  N90SCILCD        IFNE      *BLANK
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        39  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDIF                                                  SCILCD<>*BLANKS
     C*
     C*  CHECK THE INCREASED LIMITS 1 DATES
     C*
     C                   Z-ADD     SCILEF        XXEF
     C                   Z-ADD     SCILEX        XXEX
     C                   MOVE      *ON           XXCHAR            1
     C*
     C  N90              EXSR      DTERRS
     C*
     C  N90              MOVE      *OFF          *IN80
     C*
     C*  CHECK THE INCREASED LIMITS FACTOR 2
     C*
     C  N90              MOVE      *ON           *IN81
     C*
     C                   MOVEL(P)  SCINL2        XXKEY
     C*
     C  N90SCINL2        IFNE      *BLANK
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        45  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  FACTOR 1 EXPIRATION DATE MUST BE ENTERED
     C*
     C  N90SCILEX        IFEQ      *ZERO
     C                   SETON                                        86  90
     C                   ENDIF                                                  SCILEX=*ZERO
     C*
     C*  IF FACTOR 2 EFF DT NOT ENTERED DEFAULT IT TO FACTOR 1 EXP DT
     C*
     C  N90SCI2EF        IFEQ      *ZERO
     C                   Z-ADD     SCILEX        SCI2EF
     C                   ENDIF                                                  SCI2EF=*ZERO
     C*
     C*
     C*  CHECK THE INCREASED LIMITS 2 DATES
     C*
     C                   Z-ADD     SCI2EF        XXEF
     C                   Z-ADD     SCI2EX        XXEX
     C*
     C                   MOVE      *ON           XXCHAR
     C  N90              EXSR      DTERRS
     C*
     C*  INCREASED LIMITS 2 EFF DT MUST BE >= LIMIT 1 EXP DT
     C*
     C                   Z-ADD     SCI2EF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCI2EF
     C*
     C                   Z-ADD     SCILEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C  N90XXEFDT        IFLT      XXEXDT
     C                   SETON                                        87  90
     C                   ENDIF                                                  XXEFDT<XXEXDT
     C*
     C                   ENDIF                                                  SCINL2<>*BLANKS
     C*
     C  N90              MOVE      *OFF          *IN81
     C*
     C*  VALIDATE THE DEDUCTIBLE DISCOUNT CODE, IF ENTERED
     C*
     C  N90              MOVE      *ON           *IN82
     C*
     C***  Begin Chg  ***  07/06/11  **************************************
     C******IN69         IFEQ      *ON
     C                   if        deductInstalled
     C***  End   Chg  ***  07/06/11  **************************************
     C*
     C     SCDCDE        IFNE      *BLANKS
     C*
     C***  BEGIN CHG  ***  04/07/05-2 ***********************************
     C                   MOVEL(P)  SCDCDE        XXDDCD
     C***  END   CHG  ***  04/07/05-2 ***********************************
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C*
     C  N90*IN60         IFEQ      *ON
     C***  BEG   DEL  ***  07/05/09 *************************************
     C*****DDSTAT        ORNE      'A'
     C***  END   DEL  ***  07/05/09 *************************************
     C                   SETON                                        46  90
     C                   ENDIF                                                  *IN60=*ON
     C***  BEG   ADD  ***  07/05/09 *************************************
     C     *IN60         IFEQ      *OFF
     C     DDSTAT        ANDNE     'A'
     C  N90              DOW       *IN60 = *OFF
     C     DDSTAT        IFNE      'A'
     C     DDKY1         READPE    PDD01                                  60
     C                   IF        *IN60 = *OFF
     C                   LEAVE
     C                   ELSE
     C                   SETON                                        46  90
     C                   ENDIF
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDDO                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*OFF
     C***  END   ADD  ***  07/05/09 *************************************
     C*
     C                   ENDIF                                                  SCCDDE<>*BLANKS
     C*
     C*  CHECK THE DEDUCTIBLE DATES
     C*
     C                   MOVEL(P)  SCDCDE        XXKEY
     C                   Z-ADD     SCDCEF        XXEF
     C***  Begin Chg  ***  04/26/07  **************************************
     C**********           Z-ADDSCDCEF    XXEX
     C                   Z-ADD     SCDCEX        XXEX
     C***  End   Chg  ***  04/26/07  **************************************
     C*
     C  N90              EXSR      DTERRS
     C*
     C***  BEGIN CHG  ***  04/07/05-2 ***********************************
     C  N90              MOVE      *OFF          *IN82
     C*
     C*  VALIDATE THE 2nd DEDUCTIBLE DISCOUNT CODE, IF ENTERED
     C*
     C  N90              MOVE      *ON           *IN84
     C*
     C     SCDCD2        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCDCD2        XXDDCD
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C*
      ***** BEG ADD ***** 06/22/05 **************************************
     C   60
     CANN90DDKY1         CHAIN     PDD01                              60
      ***** END ADD ***** 06/22/05 **************************************
     C  N90*IN60         IFEQ      *ON
     C     DDSTAT        ORNE      'A'
     C                   SETON                                        46  90
     C                   ENDIF                                                  *IN60=*ON
     C*
     C*  1ST DEDUCTIBLE DATE MUST BE ENTERED
     C*
     C  N90SCDCEX        IFEQ      *ZERO
     C                   SETON                                        86  90
     C                   ENDIF                                                  SCDCEX=*ZERO
     C*
     C*  IF #2 EFF DT NOT ENTERED DEFAULT IT TO #1 EXP DT
     C*
     C  N90SC2DEF        IFEQ      *ZERO
     C                   Z-ADD     SCDCEX        SC2DEF
     C                   ENDIF                                                  SC2DEF=*ZERO
     C*
     C***  Begin Del  ***  04/26/07  **************************************
     C**********           ENDIF                           SCDCD2<>*BLANKS
     C***  End   Del  ***  04/26/07  **************************************
     C*
     C*  CHECK THE DEDUCTIBLE DATES
     C*
     C                   MOVEL(P)  SCDCD2        XXKEY
     C                   Z-ADD     SC2DEF        XXEF
     C                   Z-ADD     SC2DEX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C*  DEDUCTIBLE #2 EFF DT MUST BE >= DEDUCTBLE #1 EXP DT
     C*
     C                   Z-ADD     SC2DEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SC2DEF
     C*
     C                   Z-ADD     SCDCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
     C*
     C  N90XXEFDT        IFLT      XXEXDT
     C                   SETON                                        87  90
     C                   ENDIF                                                  XXEFDT<XXEXDT
     C*
     C***  Begin Add  ***  04/26/07  **************************************
     C                   ENDIF                                                  SCDCD2<>*BLANKS
     C***  End   Add  ***  04/26/07  **************************************
     C*
     C  N90              MOVE      *OFF          *IN84
     C  N90              MOVE      *ON           *IN82
     C***  END   CHG  ***  04/07/05-2 ***********************************
     C*
     C*  IF THE DEDUCTIBLE DISCOUNT CODE IS NOT BLANK, THE BILL DED.
     C*  FLAG MUST BE 'Y' OR 'N'.
     C*
     C  N90SCCXBL        IFNE      'Y'
     C     SCCXBL        ANDNE     'N'
     C     SCDCDE        ANDNE     *BLANKS
     C                   SETON                                        47  90
     C                   ENDIF                                                  SCCXBL<>Y/N
     C*
     C***  BEGIN CHG  ***  04/07/05-2 ***********************************
     C********** SCDCDE    IFEQ *BLANKS
     C  N90SCDCDE        IFEQ      *BLANKS
     C***  END   CHG  ***  04/07/05-2 ***********************************
     C     SCCXBL        ANDNE     *BLANKS
     C     SCDCDE        ORNE      *BLANKS
     C     SCCXBL        ANDEQ     *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCCDDE=*BLANKS
     C*
     C                   ENDIF                                                  *IN69=*ON
     C*
     C  N90              MOVE      *OFF          *IN82
     C*
      *** BEG ADD ***** 07/16/07 **************************************
     C***
     C*  EDIT BILL INSTALLMENTS ON 15TH FLAG
     C***
     C***** BEG ADD ******* 09/27/07 **********************************
     C     CLTID         IFEQ      '093'
     C***** END ADD ******* 09/27/07 **********************************
     C  N90SCFFL2        IFNE      *BLANK
     C     CLTID         ANDEQ     '093'
     C     SCFFL2        ANDNE     'Y'
     C                   SETON                                        62  90
     C                   ENDIF
     F***** BEG ADD ******* 09/27/07 **********************************
     C                   ENDIF
     F***** END ADD ******* 09/27/07 **********************************
     C***** BEG ADD ******* 10/11/07 **********************************
     C*
     C*  VALIDATE THE BILLING ON EFF DAY FLAG
     C*
     C  N90SCBLFL        IFNE      'Y'
     C     SCBLFL        ANDNE     *BLANK
     C                   SETON                                        64  90
     C                   ENDIF                                                  SCBLFL <> 'Y'
     C*
     C                   MOVEL(P)  SCBSCD        XXKEY
     C*
     C  N90TBKY          CHAIN     PBS01                              60
     C*
     C  N90
     CANN60SCBLFL        IFEQ      'Y'
     C*** beg add ***** 07/10/08 **************************************
     c                   if        vndrfl = *on or
     c                             achflg = *on
     C*** end add ***** 07/10/08 **************************************
     C     BSFREQ        IFNE      'M'
     C     BSSEFL        ORNE      'E'
     C                   SETON                                        65  90
     C                   ENDIF                                                  BSFREQ NE 'M'
     C*** beg add ***** 07/10/08 **************************************
     c                   else
     c                   if        BSSEFL <> 'E'
     c                   seton                                        65  90
     c                   endif
     c                   endif
     C*** end add ***** 07/10/08 **************************************
     C                   ENDIF                                                  SCBLFL = 'Y'
     C***** END ADD ******* 10/11/07 **********************************
      *
     F***** BEG ADD ******* 09/27/07 **********************************
     C     CLTID         IFEQ      '093'
     F***** END ADD ******* 09/27/07 **********************************
     C  N90SCFFL2        IFEQ      'Y'
     C     CLTID         ANDEQ     '093'
     C     BSSEFL        ANDNE     'E'
     C                   SETON                                        63  90
     C                   ENDIF
      *** END ADD ***** 07/16/07 **************************************
     F***** BEG ADD ******* 09/27/07 **********************************
     C                   ENDIF
     F***** END ADD ******* 09/27/07 **********************************
      *
     C*  EDIT CONTRACTOR CREDIT DATES                                  RED
     C*
     C  N90              MOVE      *ON           *IN85
     C*
     C* IF CODE IS NOT SET UP, DO NOT ALLOW CREDIT
     C  N90RRCCCD        IFEQ      *ZERO
     C     SCASP1        ANDNE     *ZERO
     C                   SETON                                        42  90
     C                   ENDIF                                                  CODE IFEQ
     C  N90SCASP1        IFGT      100
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCASP1>100
     C*
     C                   MOVEL(P)  SCASP1        XXKEY
     C                   Z-ADD     SCCREF        XXEF
     C                   Z-ADD     SCCREX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C  N90              MOVE      *OFF          *IN85
     C*
      *** BEG ADD ***** 08/02/05 **********************************
      *
      * IF INCREASED LIMITS CODE OR DEDUCTIBLE DISCOUNT CODE HAS CHANGED, OFFER
      * OPTION TO UPDATE ALL DIVISIONS
      *
     C  N90@@MODE        IFEQ      'C'
     C     FNPEOF        ANDNE     *BLANK
     C     XXSTRP        ANDEQ     'Y'
     C     SC#DIV        ANDGT     1
      *
     C     SCILCD        IFNE      ELILCD
     C     SCINL2        ORNE      ELINL2
     C     SCDCDE        ORNE      ELDCDE
     C     SCDCD2        ORNE      ELDCD2
      *
     C                   MOVE      *OFF          XXCHEK
     C                   MOVE      'N'           SCUPDT
     C                   MOVE      *ON           *IN90
     C     *IN90         DOWEQ     *ON
     C                   EXFMT     WDW04
      *
     C     SCUPDT        IFNE      'Y'
     C     SCUPDT        ANDNE     'N'
     C                   MOVE      *ON           *IN71
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      *OFF          *IN90
     C                   ENDIF
      *
      *  ENSURE THAT INC LIMITS AND DED DISC CODES ARE VALID IN OTHER DIVS
      *
     C     SCUPDT        IFEQ      'Y'
      *
     C     ELKYL1        CHAIN(N)  PELL1                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMKY3         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMCAD1        IFNE      0
     C     EMCAD2        ORNE      0
     C     EMCAD3        ORNE      0
     C     EMCAD4        ORNE      0
     C     EMCAD5        ORNE      0
      *
     C     XXCHEK        IFEQ      *OFF
     C                   SETON                                        72  90
     C                   MOVE      *ON           XXCHEK            1
     C                   LEAVE
     C                   ENDIF                                                  XXCHEK
      *
     C                   ENDIF                                                  EMCAD1
      *
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO                                                  PEM01
      *
     C     RSKY1         CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXPFDT
      *
     C  N90SCILCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCILCD        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCILCD <> *BLANKS
      *
     C  N90SCINL2        IFNE      *BLANKS
     C                   MOVEL(P)  SCINL2        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *ON
     C                   SETON                                        73  90
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCINL2 <> *BLANKS
      *
     C  N90SCDCDE        IFNE      *BLANKS
     C                   MOVEL(P)  SCDCDE        XXDDCD
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C     *IN60         IFEQ      *ON
     C     DDSTAT        ORNE      'A'
     C                   SETON                                        74  90
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDCDE <> *BLANKS
     C*
     C  N90SCDCD2        IFNE      *BLANKS
     C                   MOVEL(P)  SCDCD2        XXDDCD
     C     DDKY          SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C     *IN60         IFEQ      *ON
     C     DDSTAT        ORNE      'A'
     C                   SETON                                        74  90
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDCD2 <> *BLANKS
     C*
     C     ELKYL1        READE(N)  PELL1                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  SCUPDT
      *
     C                   ENDDO                                                  *IN90 = *ON
      *
      *  RESET EMP MASTER AND EMP DETAIL INFO
      *
     C     ELKYL2        CHAIN(N)  PELL1                              60
     C     EMKY          CHAIN(N)  PEM01                              60
      *
     C                   ENDIF                                                  SCILCD <> ELILCD
      *
     C                   ENDIF                                                  @@MODE = 'C'
      *** END ADD ***** 08/02/05 **********************************
      *** BEG ADD ***** 04/06/06 **********************************
      *
      * POP UP WINDOW INFORMING USER THAT BSCD'S WILL BE CHANGED ON ALL
      * SUMMARIZED DIV IF ST RPT DIV IS CHG'D
      *
     C  N90@@MODE        IFEQ      'C'
     C     XXSTRP        ANDEQ     'Y'
     C     XXOTAR        ANDEQ     'Y'
      *
     C                   MOVE      SCBSCD        X1BSCD            3 0
     C     X1BSCD        IFNE      ELBSCD
     C                   EXFMT     WDBSCH
     C     *INKL         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   ELSE
     C                   MOVE      'Y'           XXUPDB            1
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF                                                  @@MODE = 'C'
      *** END ADD ***** 04/06/06 **********************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ELDT          IFGT      SAVEDT
     C     ELDT          OREQ      SAVEDT
     C     ELTIME        ANDGT     SAVETM
     C                   SETON                                        9093
     C                   ENDIF                                                  ELDT>SAVEDT
     C*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*++*+*+*+*+*+*+*+*+*+*+*+*
     C*
     C* OVERRIDE PRO-RATA FACTOR FLAG MUST BE BLANK OR "Y"
     C*
     C  N90SCPRFT        IFNE      'Y'
     C     SCPRFT        ANDNE     ' '
     C                   SETON                                        97  90
     C                   ENDIF
     C*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*++*+*+*+*+*+*+*+*+*+*+*+*
      *** beg add ***** 08/22/11 *****************************************
      *
      * Load 3c fields in Add mode
      *
     c  n90              if        @@mode = 'A'
     c                   exsr      load3c
     c                   endif
      *** end add ***** 08/22/11 *****************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
      ****** BEG CHG ***** 10/28/05 **************************************
      * If *IN90 = *ON, and is ADD (*IN40), then don't re-display
     C***40*********       EXSR DSPPRE
     C   40
     CANN90              EXSR      DSPPRE
      ****** END CHG ***** 10/28/05 **************************************
     C*
     C  N90              MOVE      'N'           XXOFLG            1
     C                   ENDSR                                                  ERRCK1
     C*
     C*
     C/EJECT
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE FOR ERROR CHECKING SCR02
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C*  DEVIATION 1 CANNOT BE ENTERED WITHOUT A HEADING
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C     *IN43         IFEQ      *OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C  N90SCDVH1        IFEQ      *BLANKS
     C     SCASP2        ANDNE     *ZEROS
     C                   SETON                                        20  90
     C                   ENDIF                                                  SCDVH1=*BLANKS
     C*
     C*  EDIT DEVIATION 1 DATES
     C*
     C  N90              MOVE      *ON           *IN80
     C*
     C  N90SCASP2        IFGT      100
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCASP2>100
     C*
     C                   MOVEL(P)  SCASP2        XXKEY
     C                   Z-ADD     SCDVEF        XXEF
     C                   Z-ADD     SCDVEX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C  N90              MOVE      *OFF          *IN80
     C*
     C***  BEG   ADD  ***  06/21/10  *********************************
     C  N90SCD1H2        IFEQ      *BLANKS
     C     SCD1S2        ANDNE     *ZEROS
     C                   SETON                                        48  90
     C                   ENDIF                                                  SCD1H2  =*BLANKS
     C*  EDIT DEVIATION 1 SPLIT 2 DATES
     C*
     C  N90              MOVE      *ON           *IN67
     C  N90SCD1S2        IFGT      100
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCASP2>100
     C*
     C                   MOVEL(P)  SCD1S2        XXKEY
     C                   Z-ADD     SCD1D2        XXEF
     C                   Z-ADD     SCD1X2        XXEX
     C  N90              EXSR      DTERRS
     C*
     C     SCD1S2        IFNE      0
     C     SCD1D2        ANDEQ     0
     C                   SETON                                        46  90
     C                   ENDIF
     C*
     C     SCD1D2        IFNE      0
     C     SCDVEX        ANDEQ     0
     C                   SETON                                        62  90
     C                   ENDIF
     C*Check 2nd Prd Eff. Date < 1st Prd Exp. Date
     C                   Z-ADD     SCD1D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEF
     C*
     C                   Z-ADD     SCDVEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEX
     C     XXEF          IFNE      0
     C     XXEX          ANDGT     XXEF
     C                   SETON                                        17  90
     C                   ENDIF
     C*
     C  N90              MOVE      *OFF          *IN67
     C***  END   ADD  ***  06/21/10  *********************************
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C                   ENDIF                                                  *IN43=*OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C*  DEVIATION 2 CANNOT BE ENTERED WITHOUT A HEADING
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C     *IN44         IFEQ      *OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C  N90SCDVH2        IFEQ      *BLANKS
     C     SCDEV2        ANDNE     *ZEROS
     C                   SETON                                        21  90
     C                   ENDIF                                                  SCDVH2=*BLANKS
     C*
     C*  EDIT DEVIATION 2 DATES
     C*
     C  N90              MOVE      *ON           *IN81
     C*
     C  N90SCDEV2        IFGT      100
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCDEV2>100
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL(P)  SCDEV2        XXKEY
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL(P)  SCSTFC        XXKEY
     C                   ENDIF                                                  CLTID <> 025
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     SCD2EF        XXEF
     C                   Z-ADD     SCD2EX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C  N90              MOVE      *OFF          *IN81
     C*
     C***  BEG   ADD  ***  06/21/10  *********************************
     C  N90SCD2H2        IFEQ      *BLANKS
     C     SCD2S2        ANDNE     *ZEROS
     C                   SETON                                        97  90
     C                   ENDIF                                                  SCD2H2  =*BLANKS
     C*  EDIT DEVIATION 2 DATES
     C  N90              MOVE      *ON           *IN69
     C  N90SCD2S2        IFGT      100
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCDEV2>100
     C*
     C                   MOVEL(P)  SCD2S2        XXKEY
     C                   Z-ADD     SCD2D2        XXEF
     C                   Z-ADD     SCD2X2        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C     SCD2S2        IFNE      0
     C     SCD2D2        ANDEQ     0
     C                   SETON                                        98  90
     C                   ENDIF
     C*
     C     SCD2D2        IFNE      0
     C     SCD2EX        ANDEQ     0
     C                   SETON                                        49  90
     C                   ENDIF
     C*Check 2nd Prd Eff. Date < 1st Prd Exp. Date
     C                   Z-ADD     SCD2D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEF
     C*
     C                   Z-ADD     SCD2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEX
     C*
     C     XXEF          IFNE      0
     C     XXEX          ANDGT     XXEF
     C                   SETON                                        50  90
     C                   ENDIF
     C*
     C  N90              MOVE      *OFF          *IN69
     C***  END   ADD  ***  06/21/10  *********************************
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C                   ENDIF                                                  *IN44=*OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C*  EDIT SUBJECT PREM CREDIT 1A DATES
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C     *IN63         IFEQ      *OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C  N90              MOVE      *ON           *IN86
     C*
     C  N90SCSH1A        IFNE      *BLANKS
     C*
     C  N90SCASPM        IFGT      100
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCASPM>100
     C*
     C                   MOVEL(P)  SCASPM        XXKEY
     C                   Z-ADD     SCMCEF        XXEF
     C                   Z-ADD     SCMCEX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C                   ENDIF                                                  SCSH1A<>*BLANKS
     C*
     C  N90              MOVE      *OFF          *IN86
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C                   ENDIF                                                  *IN63=*OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C*  EDIT SUBJECT PREM CREDIT 1B DATES
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C     *IN64         IFEQ      *OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C  N90              MOVE      *ON           *IN87
     C*
     C  N90SCSH1B        IFNE      *BLANKS
     C*
     C  N90SCASPS        IFGT      100
     C                   SETON                                        77  90
     C                   ENDIF                                                  SCASPS>100
     C*
     C                   MOVEL(P)  SCASPS        XXKEY
     C                   Z-ADD     SCSCEF        XXEF
     C                   Z-ADD     SCSCEX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C                   ENDIF                                                  SCSH1B<>*BLANKS
     C*
     C  N90              MOVE      *OFF          *IN87
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C                   ENDIF                                                  *IN64=*OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C***  BEGIN ADD  ***  04/23/09  *********************************
     C*
     C*  EDIT to check schedule debit/credit factor is in min/max range(FMIC only)
     C*
      ******* beg del ****** 03/19/10 *********************************
     C*****CLTID         IFEQ      '102'
     C*****SCDBCR        ANDNE     *Zeros
     C*******            IF        SCDBCR < RRCMAX OR SCDBCR > RRDMAX
      ******* end del ****** 03/19/10 *********************************
      ******* beg add ****** 03/19/10 *********************************

     c                   if        scdbcr <> 0

      ******* beg del ****** 05/18/10 *********************************
     c*******            if        (scdbcr < rrcmax)
     c*******                      or (scdbcr > rrdmax)
      ******* end del ****** 05/18/10 *********************************
      ******* beg add ****** 05/18/10 *********************************
     c                   if        ((scdbcr < rrcmax) and (rrcmax <> 0))
     c                              or
     c                             ((scdbcr > rrdmax) and (rrdmax <> 0))
      ******* end add ****** 05/18/10 *********************************
      ******* end add ****** 03/19/10 *********************************
     C                   SETON                                        66  90
     C                   Endif                                                 Min/Max Debit/Credit
     C                   Endif                                                 FMIC
     C***  END   ADD  ***  04/23/09  *********************************
     C*
     C*  EDIT MCO TYPE CODE
     C*
     C  N90SCMCCD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVEL(P)  SCMCCD        XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        28  90
     C                   ENDIF                                                  *IN60=*ON/MASTAT<>A
     C*
     C                   ENDIF                                                  SCMCCD<>*BLANKS
     C*
     C  N90XXST          IFEQ      'FL'
     C*
     C*  IF MANAGED CARE CREDIT % IS ENTERED THEN THE CODE IS REQUIRED
     C*
     C  N90SCASPM        IFNE      *ZEROS
     C     SCMCCD        ANDEQ     *BLANKS
     C                   SETON                                        29  90
     C                   ENDIF                                                  SCASPM<>*ZEROS
     C*
     C*  IF A MANAGED CARE ORGANIZATIONAL CODE IS ENTERED, THEN
     C*  THERE MUST BE A A MANAGED CARE CREDIT EFFECTIVE DATE SPECIFIED
     C*
     C  N90SCMCCD        IFNE      *BLANKS
     C     SCMCEF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCMCEF
     C                   ENDIF                                                  SCMCCD<>*BLANKS
     C*
     C  N90SCMCCD        IFNE      *BLANKS
     C     SCMCEF        ANDEQ     *ZEROS
     C                   SETON                                        30  90
     C                   ENDIF                                                  SCMCCD<>*BLANKS
     C*
     C                   ENDIF                                                  XXST=FL
      ***** BEG ADD ******* 11/30/99 **********************************
      *
      * MCO Code must be valid and effective as of ELPFDT, if entered.
      *
     C     *IN90         IFEQ      *OFF
     C     SCMCO         ANDNE     *BLANKS
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##PFDT
      *
     C     MGKY1         SETLL     PMG01
     C     MGKY          READE     PMG01                                  60
      *
     C     ##PFDT        IFGE      MGEFDT
     C     *IN60         ANDEQ     *OFF
      *
     C     MGEXDT        IFNE      *ZERO
     C     ##PFDT        ANDGE     MGEXDT
     C                   SETON                                        90  78
     C                   ENDIF                                                  MGEXDT
      *
     C                   ELSE
      *
     C                   SETON                                        90  78
     C                   ENDIF                                                  *IN60 MGEFDT
      *
     C                   ENDIF                                                  *IN90
      *
      ***** END ADD ******* 11/30/99 **********************************
     C*
     C*  THE FIRST EXPERIENCE MOD MUST BE ENTERED
     C*
     C     SCEXM1        IFNE      *ZERO
     C     SCEMD1        ANDEQ     *ZERO
     C                   Z-ADD     SCPFDT        SCEMD1
     C                   ENDIF                                                  SCEXM1=*ZERO
     C*
     C*** BEG ADD ***  05/25/07  ************************************
     C*  EXPERIENCE MODIFIER 1 MUST BE ENTERED FIRST
     C*
     C  N90SCEXM1        IFEQ      *ZEROS
     C     SCEMD2        ANDNE     *ZEROS
     C     SCEXM2        ANDNE     *ZEROS
     C                   SETON                                        19  90
     C                   ELSE
     C  N90SCEXM1        IFEQ      *ZEROS
     C     SCEMD2        ANDEQ     *ZEROS
     C     SCEXM2        ANDNE     *ZEROS
     C                   SETON                                        19  90
     C                   ENDIF                                                  SCEMD1=*ZERO
     C                   ENDIF                                                  SCEMD1=*ZERO
     C*
     C  N90SCEXM1        IFNE      *ZEROS
     C     SCEXM2        ANDEQ     *ZEROS
     C     SCEXM3        ANDNE     *ZEROS
     C                   SETON                                        18  90
     C                   ENDIF                                                  SCEMD1=*ZERO
     C**
     C     SCEXM1        IFEQ      *ZEROS
     C     SCEXM2        ANDEQ     *ZEROS
     C     SCEXM3        ANDNE     *ZEROS
     C                   SETON                                        25  90
     C                   ENDIF                                                  SCEMD1=*ZERO
     C***** END ADD ****** 04/06/99 **********************************
     C*** END ADD ***  05/25/07  ************************************
     C*
     C*  DO NOT PERFORM THIS EDIT IF CLAIMS ONLY GROUP
     C*
     C*** BEGIN DEL ***  08/12/99  ************************************
     C**N90*     F1CLMS    IFEQ 'N'
     C*
     C**N90*     SCEXM1    IFEQ *ZERO
     C******     SCEMD1    ANDEQ*ZERO
     C******               SETON                     22  90
     C******               ENDIF                           SCEXM1=*ZERO
     C*
     C******               ENDIF                           F1CLMS=N
     C*** END DEL *****  08/12/99  ************************************
     C*
     C*  EDIT EXPERIENCE MOD 1 DATES
     C*
     C  N90              MOVE      *ON           *IN82
     C*
     C                   MOVEL(P)  SCEXM1        XXKEY
     C                   Z-ADD     SCEMD1        XXEF
     C                   Z-ADD     *ZEROS        XXEX
     C*
     C*  IF THE CODE IS ENTERED, DATE IS REQUIRED.
     C*
     C***** BEGIN CHG **** 04/06/99 **********************************
     C**N90***** XXKEY     IFNE *ZERO
     C********** XXKEY     ORNE *BLANKS
     C  N90SCEXM1        IFNE      *ZEROS
     C***** END CHG ****** 04/06/99 **********************************
     C     XXEF          IFEQ      *ZERO
     C                   SETON                                        71  90
     C                   ENDIF                                                  XXEF<>*ZERO
     C*
     C                   ENDIF                                                  *ZEROS=XXKEY=*BLANKS
     C*
     C*** BEGIN CHG ***  02/02/98  ************************************
     C**N90*****           EXSR DTERRS
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C  N90XXEF          IFNE      *ZERO
     C*
     C                   Z-ADD     XXEF          SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXEF
     C*
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        72  90
     C**** ADD SUPP #15751 ****
     C                   ELSE
     C                   Z-ADD     XXEF          SCEMD1
     C**** END ADD #15751
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  XXEF
     C*** END CHG *****  02/02/98  ************************************
     C*
     C  N90              MOVE      *OFF          *IN82
     C*
     C*  FIRST EFFECT DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C*
     C                   Z-ADD     SCEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
     C*
      *** Begin Add ***** 07/29/11 *****************************************
      ***** FIRST EFF DATE MUST BE WITHIN TERM *****************************
      **
     C     CLTID         IFEQ      '139'
      *
      *** Begin Chng **** 12/21/11 *****************************************
     C**   XXEFDT        IFLT      XXPFDT
     C**   XXEFDT        ORGT      XXPTDT
     C**                 SETON                                        23  90
     C**                 ENDIF                                                  XXEFDT>XXPFDT
     C     XXEFDT        IFGT      XXPTDT
     C                   SETON                                        23  90
     C                   ENDIF
      *** End   Chng **** 12/21/11 *****************************************
      *
     C                   ELSE
      *** End   Add ***** 07/29/11 *****************************************
     C     XXEFDT        IFGT      XXPFDT
     C                   SETON                                        23  90
     C                   ENDIF                                                  XXEFDT>XXPFDT
     C*
      *** Begin Add ***** 07/29/11 *****************************************
     C                   ENDIF                                                  XXEFDT>XXPFDT
      *** End   Add ***** 07/29/11 *****************************************
     C*  EDIT EXPERIENCE MOD 2 DATES
     C*
     C  N90              MOVE      *ON           *IN83
     C*
     C                   MOVEL(P)  SCEXM2        XXKEY
     C                   Z-ADD     SCEMD2        XXEF
     C                   Z-ADD     *ZEROS        XXEX
     C*
     C  N90SCEXM2        IFNE      *ZEROS
     C     SCEMD2        ANDEQ     *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCEXM2<>*ZERO=SCEMD2
     C*
     C  N90              EXSR      DTERRS
     C*** ADD 8/27/98 ****** SUPPORT #'S 12830/13329
     C                   Z-ADD     XXEF          SCEMD2
     C*** END ADD 8/27/98 ****** SUPPORT #'S 12830/13329
     C*
     C  N90              MOVE      *OFF          *IN83
     C*
     C*  EDIT EXPERIENCE MOD 3 DATES
     C*
     C  N90              MOVE      *ON           *IN84
     C*
     C                   MOVEL(P)  SCEXM3        XXKEY
     C                   Z-ADD     SCEMD3        XXEF
     C                   Z-ADD     *ZEROS        XXEX
     C*
     C  N90SCEXM3        IFNE      *ZEROS
     C     SCEMD3        ANDEQ     *ZEROS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCEXM2<>*ZERO=SCEMD2
     C*
     C  N90              EXSR      DTERRS
     C*** ADD 8/27/98 ****** SUPPORT #'S 12830/13329
     C                   Z-ADD     XXEF          SCEMD3
     C*** END ADD 8/27/98 ****** SUPPORT #'S 12830/13329
     C*
     C  N90              MOVE      *OFF          *IN84
     C***** BEGIN ADD **** 04/06/99 **********************************
     C*  EXPERIENCE MODIFIER 1 MUST BE ENTERED FIRST
     C*
     C  N90SCEMD1        IFEQ      *ZEROS
     C     SCEXM1        ANDEQ     *ZEROS
     C     SCEMD2        ANDNE     *ZEROS
     C     SCEXM2        ANDNE     *ZEROS
     C                   SETON                                        96  90
     C                   ENDIF                                                  SCEMD1=*ZERO
     C*
     C  N90SCEMD1        IFEQ      *ZEROS
     C     SCEXM1        ANDEQ     *ZEROS
     C     SCEMD3        ANDNE     *ZEROS
     C     SCEXM3        ANDNE     *ZEROS
     C***** BEG CHG **** 05/25/07 **********************************
     C*****                SETON                     96  90
     C                   SETON                                        25  90
     C***** END CHG **** 05/25/07 **********************************
     C                   ENDIF                                                  SCEMD1=*ZERO
     C***** END ADD ****** 04/06/99 **********************************
     C* EXPERIENCE MODIFIER 2 MUST HAVE BEEN ENTERED FIRST
     C*
     C***** BEG DLT **** 05/25/07 **********************************
     C* EXPERIENCE MODIFIER 2 MUST HAVE BEEN ENTERED FIRST
     C*
     C**N90***   SCEMD2    IFEQ *ZEROS
     C********   SCEXM2    ANDEQ*ZEROS
     C********   SCEMD3    ANDNE*ZEROS
     C********   SCEXM3    ANDNE*ZEROS
     C********             SETON                     25  90
     C********             ENDIF                           SCEMD2=*ZEROS
     C***** END DLT **** 05/25/07 **********************************
      ***** BEG ADD ******* 03/24/00 **********************************
      *
      *  Experience Modifier Dates must be in ascending order, if entered
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   Z-ADD     SCEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEMD1
      *
     C                   Z-ADD     SCEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1EMD1
      *
     C                   Z-ADD     SCEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEMD2
      *
     C                   Z-ADD     SCEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1EMD2
      *
     C                   Z-ADD     SCEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEMD3
      *
     C                   Z-ADD     SCEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1EMD3
      *
     C     X1EMD1        IFNE      *ZERO
     C     X1EMD2        ANDNE     *ZERO
     C     X1EMD1        ANDGE     X1EMD2
     C                   SETON                                        90  36
     C                   ENDIF
      *
     C     X1EMD2        IFNE      *ZERO
     C     X1EMD3        ANDNE     *ZERO
     C     X1EMD2        ANDGE     X1EMD3
     C                   SETON                                        90  37
     C                   ENDIF
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
      ***** END ADD ******* 03/24/00 **********************************
      *** BEG ADD **** 01/27/03 ************************************************
      *
      *  IF RATING STATE = MASS, PERFORM FOLLOWING CHECKS
      *  ARP FACTOR NOT VALID IF SCHEDULED DB/CR FACTOR EXISTS
      *
     C  N90XXST          IFEQ      'MA'
      *
     C     SCARP1        IFNE      *ZERO
     C     SCEXM1        ANDEQ     *ZERO
     C                   SETON                                        31  90
     C                   ENDIF
      *
     C     SCARP1        IFNE      *ZERO
     C     SCARP1        IFLT      1.0
     C     SCARP1        ORGT      1.49
     C                   SETON                                        56  90
     C                   ENDIF
     C                   ENDIF
      *
     C     SCARP2        IFNE      *ZERO
     C     SCEXM2        ANDEQ     *ZERO
     C                   SETON                                        38  90
     C                   ENDIF
      *
     C  N90SCARP2        IFNE      *ZERO
     C     SCARP2        IFLT      1.0
     C     SCARP2        ORGT      1.49
     C                   SETON                                        57  90
     C                   ENDIF
     C                   ENDIF
      *
     C     SCARP3        IFNE      *ZERO
     C     SCEXM3        ANDEQ     *ZERO
     C                   SETON                                        39  90
     C                   ENDIF
      *
     C  N90SCARP3        IFNE      *ZERO
     C     SCARP3        IFLT      1.0
     C     SCARP3        ORGT      1.49
     C                   SETON                                        58  90
     C                   ENDIF
     C                   ENDIF
      *
     C  N90SCARP1        IFNE      *ZERO
     C     SCDBCR        ANDNE     *ZERO
     C     SCARP2        ORNE      *ZERO
     C     SCDBCR        ANDNE     *ZERO
     C     SCARP3        ORNE      *ZERO
     C     SCDBCR        ANDNE     *ZERO
     C                   SETON                                        55  90
     C                   ENDIF
      *
     C                   ENDIF                                                  ST = 'MA'
      *** END ADD **** 01/27/03 ************************************************
     C*
     C*  INTERNALLY PROMULGATED FLAG NOT 'Y' OR BLANK
     C*
     C  N90SCPROM        IFNE      'Y'
     C     SCPROM        ANDNE     *BLANKS
     C*** BEGIN ADD ***  02/21/01  ************************************
     C     SCPROM        ANDNE     'M'
     C*** END ADD *****  02/21/01  ************************************
     C                   SETON                                        24  90
     C                   ENDIF                                                  SCPROM<>Y/*BLANKS
     C*
     C*  SCHEDULE DB/CR CANNOT BE < *ZERO
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C     *IN65         IFEQ      *OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C  N90SCDBCR        IFLT      *ZERO
     C                   SETON                                        34  90
     C                   ENDIF                                                  SCDBCR<*ZERO
     C*
     C*  EDIT SCHEDULE DB/CR FACTOR DATES
     C*
     C  N90              MOVE      *ON           *IN85
     C*
     C                   MOVEL(P)  SCDBCR        XXKEY
     C                   Z-ADD     SCSDEF        XXEF
     C                   Z-ADD     SCSDEX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C  N90              MOVE      *OFF          *IN85
     C*
     C***  BEGIN ADD  ***  05/07/98  *********************************
     C                   ENDIF                                                  *IN65=*OFF
     C***  END   ADD  ***  05/07/98  *********************************
     C*
     C*  STOCK DISCOUNT % MUST EXIST > 100
     C*
     C  N90SCASP3        IFGT      100
     C                   SETON                                        778890
     C                   ENDIF                                                  SCASP3>100
     C*
     C*  DISCOUNT MUST EXIST IN DISCOUNT FILE
     C*
     C  N90SCDISC        IFNE      *BLANK
     C*
     C     DSKY          SETGT     PDS01
     C     DSKY1         READPE    PDS01                                  26
     C*
     C  N90*IN26         COMP      *ON                                    90
     C*
     C                   ENDIF                                                  SCDISC<>*BLANK
     C***  BEGIN ADD  ***  12/30/97  *********************************
     C*
     C*  DISCOUNT % AND TYPE CANNOT BOTH BE USED
     C*
     C  N90SCDISC        IFNE      *BLANKS
     C     SCASP3        ANDNE     *ZEROS
     C                   SETON                                        27  90
     C                   ENDIF                                                  SCDISC<>*BLANKS
     C***  END   ADD  ***  12/30/97  *********************************
     C*** BEGIN DEL ***  09/22/99  ************************************
     C*
     C*  CONTESTED AUDIT FLAG MUST BE 'Y' OR BLANK (NOT IN QUOTE)
     C*
     C**N90**    @@QTE     IFNE 'Q'
     C*******    SCCTST    ANDNE'Y'
     C*******    SCCTST    ANDNE*BLANK
     C*******              SETON                     31  90
     C*******              ENDIF                           QQTE<>Q/SCCTST<>Y
     C*** END DEL *****  09/22/99  ************************************
     C*
     C*** BEG DEL *****  06/21/10  ************************************
     C*  DO NOT PERFORM THESE EDIT IF CLAIMS ONLY GROUP
     C*
     C************  N90F1CLMS        IFEQ      'N'
     C************
     C************  SAFETY FEE % < ZERO OR > 100
     C************
     C***********  N90SCSFPC        IFLT      *ZERO
     C***********                   SETON                                        32  90
     C***********                ENDIF                                                  SCSFPC<*ZERO
     C***********
     C***********  N90SCSFPC        IFGT      100
     C***********        SETON                                        778990
     C***********        ENDIF                                                  SCSFPC>100
     C***********
     C***********  SAFETY FEE MINIMUM AMOUNT < ZERO
     C***********
     C***********  N90SCSFMN        IFLT      *ZERO
     C***********                   SETON                                        33  90
     C***********                ENDIF                                                  SCSFMN<*ZERO
     C*
     C******* BEGIN DEL ****** 12/29/98 *******************************
     C*
     C*  IF THERE IS A MINIMUM, THERE SHOULD BE A %
     C*
     C********** N90      SCSFMN    IFGT *ZERO
     C********** SCSFPC    ANDEQ*ZERO
     C**********           SETON                     35  90
     C**********           ENDIF                           SCSFMN<=*ZERO
     C*
     C******* END   DEL ****** 12/29/98 *******************************
     C**********         ENDIF                                                  F1CLMS=N
     C*** END DEL *****  06/21/10  ************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ELDT          IFGT      SAVEDT
     C     ELDT          OREQ      SAVEDT
     C     ELTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  ELDT>SAVEDT
     C*
     C***  BEGIN ADD  ***  12/01/99  *********************************
     C  N90*IN40         IFEQ      *ON
B02  C     #OFSCR        ANDEQ     2
     C                   EXSR      DUPCHK
     C                   ENDIF                                                  *IN40=*ON
     C***  END   ADD  ***  12/01/99  *********************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C   40              MOVE      F1MFEE        SCMFEE
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C*** BEG ADD ***** 09/13/10 *************************************
     C                   MOVE      I@94          *IN94
     C*** END ADD ***** 09/13/10 *************************************
     C                   ENDSR                                                  ERRCK2
     C*
     C/EJECT
     C*===============================================================
     C* ERRCK3 - SUBROUTINE FOR ERROR CHECKING SCR03
     C*===============================================================
     C*
     C     ERRCK3        BEGSR
     C****** BEG ADD ***** 07/07/05 **************************************
     C*
     C*  Validate activity status code if not blank
     C  N90SCASCD        IFNE      *BLANKS
     C                   MOVEL(P)  SCASCD        XXKEY
     C     TBKY          CHAIN     PAV01                              60
     C*
     C     *IN60         IFEQ      *ON
     C****** BEG CHG ***** 07/06/06 **************************************
     C**********           SETON                     357690
     C                   SETON                                        357890
     C****** END CHG ***** 07/06/06 **************************************
     C                   ELSE
     C                   MOVEL(P)  AVDESC        SCACTD
     C*
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCASCD<>*BLANKS
     C*
     C*  If activity Status Code is Blanks, be sure that Desc is blank too.
     C  N90SCASCD        IFEQ      *BLANKS
     C                   MOVEL(P)  *BLANKS       SCACTD
     C                   ENDIF                                                  SCASCD = *BLANKS
     C*
     C****** END ADD ***** 07/07/05 **************************************
     C*
     C*** BEGIN ADD ***  09/22/99  ************************************
     C*  Validate cancellation notice days
     C  N90SCCXDY        IFLT      *ZERO
     C                   SETON                                        88  90
     C                   ENDIF
     C*
     C  N90SCSRPR        IFNE      'S'
     C     SCSRPR        ANDNE     'P'
     C                   SETON                                        17  90
     C                   ENDIF                                                  SCSRPR<>S/P
     C*** BEGIN ADD ***  04/17/00  ************************************
     C*
     C  N90SCCOFL        IFNE      'Y'
     C     SCCOFL        ANDNE     ' '
     C                   SETON                                        32  90
     C                   ENDIF                                                  SCCOFL IFNE
     C*** END ADD *****  04/17/00  ************************************
     C*
     C*  Additional days.. cannot be less than zero
     C  N90SCADDM        IFLT      *ZERO
     C                   SETON                                        89  90
     C                   ENDIF
     C***  Begin Add  ***  06/24/05  **************************************
     C*
     C*  MEMBERSHIP FEES.. cannot be less than zero
     C  N90SCMFEE        IFLT      *ZERO
     C                   SETON                                        87  90
     C                   ENDIF
     C***  End   Add  ***  06/24/05  **************************************
     C***  BEG   ADD  ***  06/21/10  **************************************
     C*  DO NOT PERFORM THESE EDIT IF CLAIMS ONLY GROUP
     C  N90F1CLMS        IFEQ      'N'
     C*  SAFETY FEE % < ZERO OR > 100
     C  N90SCSFPC        IFLT      *ZERO
     C                   SETON                                        56  90
     C                   ENDIF                                                  SCSFPC<*ZERO
     C*
     C  N90SCSFPC        IFGT      100
     C                   SETON                                        656890
     C                   ENDIF                                                  SCSFPC>100
     C*  SAFETY FEE MINIMUM AMOUNT < ZERO
     C  N90SCSFMN        IFLT      *ZERO
     C***  Begin Chg  ***  12/09/10  **************************************
     C**********         SETON                                        64  90
     C                   SETON                                        57  90
     C***  End   Chg  ***  12/09/10  **************************************
     C                   ENDIF                                                  SCSFMN<*ZERO
     C*
     C                   ENDIF                                                  F1CLMS='N'
     C***  END   ADD  ***  06/21/10  **************************************
     C*  If WCFL14='Y' check that number of employees is > 0
     C***  BEG   CHG  ***  09/27/07  **************************************
     C**N90*94** SCEMPY    IFLE *ZERO
     C**********           SETON                     95  90
     C**********           ENDIF
      *
     C***  BEG   Add  ***  07/16/07  **************************************
     C     CLTID         IFNE      '093'
     C  N90
     CAN 94SCEMPY        IFLE      *ZERO
     C                   SETON                                        95  90
     C                   ENDIF
     C                   ELSE
     C     CLTID         IFEQ      '093'
     C                   MOVE      *OFF          *IN95
     C                   ENDIF
     C                   ENDIF
     C***  END   Add  ***  07/16/07  **************************************
     C***  END   CHG  ***  09/27/07  **************************************
      ***** BEG ADD *******  0/05/04 **********************************
      *
      *  Entered type of building code must NOT exist for ADD, and
      *  must exist on CHANGE if entered.
      *
     C     *IN09         IFEQ      *OFF
     C     SCTYPB        ANDNE     *BLANKS
     C     BUKY          CHAIN     PTB01                              60
     C   60              SETON                                        33  90
     C                   ENDIF
      ***** END ADD ******* 10/05/04 **********************************
      *
     C***** BEG ADD *******  04/17/07 *********************************
     C*  Check that maximum # of employees per shift > 0
     C***** BEG ADD *******  05/10/07 *********************************
     C     CLTID         IFEQ      '071'
     C***** END ADD *******  05/10/07 *********************************
     C  N90
     CAN 92SCM#EM        IFLE      *ZERO
     C                   SETON                                        93  90
     C                   ENDIF
     C***** END ADD *******  05/10/07 *********************************
     C                   ENDIF                                                  CLTID = 071
     C***** BEG ADD *******  05/10/07 *********************************
     C*
     C***** END ADD *******  04/17/07 *********************************
     C*
     C*** BEG ADD *** 12/12/01 ************************************
     C  N90SCEXMF        IFNE      'Y'
     C     SCEXMF        ANDNE     *BLANK
     C*** BEGIN CHG ***  07/11/06  ***********************************
     C*********            SETON                     97  90
     C                   SETON                                        96  90
     C*** END CHG *****  07/11/06  ***********************************
     C                   ENDIF                                                              ST<>Y
     C*** END ADD *** 12/12/01 ************************************
     C*
     C*  CONTESTED AUDIT FLAG MUST BE 'Y' OR BLANK (NOT IN QUOTE)
     C*
     C  N90@@QTE         IFNE      'Q'
     C     SCCTST        ANDNE     'Y'
     C     SCCTST        ANDNE     *BLANK
     C                   SETON                                        31  90
     C                   ENDIF                                                  QQTE<>Q/SCCTST<>Y
     C*** END ADD *****  09/22/99  ************************************
     C*** BEGIN ADD ***  07/11/06  *******************************************
     C*
     C*  SPREAD ADJUSTMENTS FLAG MUST BE 'Y' OR BLANK.
     C*
     C  N90SCFFL1        IFNE      'Y'
     C     SCFFL1        ANDNE     ' '
     C                   SETON                                        81  90
     C                   ENDIF                                                  SCFFL1 IFNE
      *
      * Spread billing adjustments can't be changed if A/R exists in prior month
      * for any division that also has A/R summarized
      *
     C  N90
     CANN40SCFFL1        IFNE      ELFFL1
     C     SCARSM        ANDEQ     'Y'
      *
     C                   MOVEL     SYSDT         XXXDTE
     C     EAKYC         CHAIN     PEA0C                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     ELKYC         CHAIN     PEL01                              60
     C     ELARSM        IFEQ      'Y'
      *
     C     XXTRYM        IFLT      XXCUYM
     C     XXPOYM        ANDLT     XXCUYM
      *
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C                   SETON                                        90  85
     C                   LEAVE
     C                   ENDIF                                                  EAARTY = 'A'
      *
     C                   ENDIF                                                  XXTRYM LT XXCUYM
      *
     C                   ENDIF                                                  ELARSM = 'Y'
      *
     C     EAKYC         READE     PEA0C                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  SCFFL1 <> ELFFL1
      *
      * Spread billing adjustments flag must match state reporting division if
      * A/R is summarized
      *
     C  N90SCARSM        IFEQ      'Y'
      *
      * If not state reporting division, get state reporting division's
      * Sprd Adj flag and compare to make sure they are the same.
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C     EMSTRP        ANDNE     'Y'
      *
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   LEAVE
     C                   ENDIF                                                  EMSTRP = 'Y'
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
     C     ELKY3         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     SCFFL1        ANDNE     ELFFL1
     C                   SETON                                        82  90
     C                   ENDIF                                                  SCFFL1 <> ELFFL1
      *
     C                   ELSE
      *
      * If state reporting division, check other division's (with summarized
      * A/R) Sprd Adj flag. Pop up window informing that other divisions
      * will be changed to match if they are different.
      *
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFNE      'Y'
      *
     C     ELKY2         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     SCFFL1        ANDNE     ELFFL1
     C                   SETON                                        82  90
     C                   LEAVE
     C                   ENDIF                                                  SCFFL1 = 'Y'
      *
     C                   ENDIF                                                  EMSTRP = 'Y'
      *
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
      *
      ***** BEGIN CHANGE ******* 02/07/07 ****************************
     C******     *IN72     IFEQ *ON
     C     *IN82         IFEQ      *ON
      ***** END   CHANGE ******* 02/07/07 ****************************
     C                   CLEAR                   WDSAFL
     C                   EXFMT     WDW05
      *
     C     WDSAFL        IFEQ      'Y'
      *
     C                   SETOFF                                       82
     C     ELKYD         CHAIN     PELL1                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE(P)   SCFFL1        ELFFL1
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   MOVEL     DSTIME        ELTIME
     C                   MOVEL     SRDTO         SAVEDT
     C                   MOVEL     DSTIME        SAVETM
     C                   UPDATE    PELL1
     C     ELKYD         READE     PELL1                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  WDSAFL = 'Y'
      *
     C     WDSAFL        IFNE      'Y'
     C     *INKL         OREQ      *ON
     C                   SETON                                            90
     C                   ENDIF                                                  WDSAFL <> 'Y'
      *
     C                   ENDIF                                                  *IN80 = *ON
      *
     C                   ENDIF                                                  EMSTRP <> 'Y'
      *
     C                   ENDIF                                                  SCARSM = 'Y'
     C*** END ADD *****  07/11/06  *******************************************
     C***** BEGIN ADD **** 07/06/06 ***********************************
      *
      * Percentage can not be less than *zeros
      *
     C  N90SCARCP        IFLT      *ZEROS
     C                   SETON                                        37  90
     C                   ENDIF                                                              ST<>Y
      *
      * Flat amount can not be less than *zeros
      *
     C  N90SCARCA        IFLT      *ZEROS
     C                   SETON                                        36  90
     C                   ENDIF                                                              ST<>Y
      *
      * Validates the date range entered
      *
     C     SCARCP        IFNE      *ZERO
     C                   MOVEL(P)  SCARCP        XXKEY
     C                   ELSE
     C                   MOVEL(P)  SCARCA        XXKEY
     C                   ENDIF
     C*
     C                   Z-ADD     SCAREF        XXEF
     C                   Z-ADD     SCAREX        XXEX
     C*
     C  N90              EXSR      DTERRS
     C*
     C                   Z-ADD     SCAREF        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAREF
     C*
     C                   Z-ADD     SCAREX        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCAREX
      *
     C***** END   ADD **** 07/06/06 ***********************************
     C***  Begin Del  ***  10/02/06  **************************************
     C******BEGIN ADD **** 09/29/99 ***********************************
     C**********
     C***PREMIUM FINANCE COMPANY MUST BE EFFECTIVE FOR POLICY EFF DATE
     C*********
     C**N90****  SCFCOD    IFNE *BLANKS
     C*********
     C**********           Z-ADDELPFDT    KKPFDT
     C**********
     C********** PFKY2     SETGTPPF01
     C**********
     C********** PFKY      REDPEPPF01                    60
     C**********
     C********** *IN60     IFEQ *OFF
     C**********
     C********** PFEXDT    IFNE *ZEROS
     C********** ELPFDT    ANDGTPFEXDT
     C**********           SETON                     98  90
     C**********           ENDIF                           PFDT<EFDT
     C**********
     C**********           ELSE
     C**********
     C**********           SETON                     98  90
     C**********           ENDIF                           *IN60=OFF
     C**********
     C**********           ENDIF                           SCFCOD<>BLKS
     C**********  ADD **** 09/29/99 ***********************************
     C***  End   Del  ***  10/02/06  **************************************
     C*
     C*** BEG DEL ******** 01/09/04 ***********************************
     C*** BEG ADD ******** 02/18/03 ***********************************
     C* VALIDATE THE VENDOR NUMBER
     C********** *IN90     IFEQ *OFF
     C********** SCVDR#    ANDNE*ZERO
     C********** VPKY      CHAINPVP02                60
     C********** *IN60     IFEQ *ON
     C**********           SETON                     72  90
     C**********           ENDIF
     C**********           ENDIF
     C*** END ADD ******** 02/18/03 ***********************************
     C*** END DEL ******** 01/09/04 ***********************************
     C***  Begin Add  ***  09/08/07  **************************************
     C*****Begin Add  ***  10/02/06  **************************************
     C**N90***** SCAAFL    IFNE 'Y'
     C********** SCAAFL    ANDNE*BLANK
     C**********           SETON                     98  90
     C**********           ENDIF
     C*****End** Add  ***  10/02/06  **************************************
     C***  End   Add  ***  09/08/07  **************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ELDT          IFGT      SAVEDT
     C     ELDT          OREQ      SAVEDT
     C     ELTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  ELDT>SAVEDT
     C*
     C***  BEGIN ADD  ***  12/01/99  *********************************
     C  N90*IN40         IFEQ      *ON
B02  C     #OFSCR        ANDEQ     3
     C                   EXSR      DUPCHK
     C                   ENDIF                                                  *IN40=*ON
     C***  END   ADD  ***  12/01/99  *********************************
     C*
     C*** BEGIN ADD ****** 10/20/09 ************************************
     C* Passed all edits : Write DARTS remark if
     C**********       - Program is in change mode AND
     C**********       - DARTS remark is setup AND
     C**********       - Activity status changed
     C*
     C                   IF        *IN90 = *OFF
     C                             AND @@MODE = 'C'
     C                             AND SVASRM <> *blanks
     C                             AND SCASCD <> ELASCD
     C* process DARTS
     C                   EXSR      SRPrcDARTS
     C                   ENDIF                                                  @@MODE = 'C'
     C*** END   ADD ****** 10/20/09 ************************************
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
      ******* BEG ADD ****** 11/03/09 *********************************
     C*
     C                   if        EXEHD1  = *BLANKS
     C                   SETON                                        45
     C                   ENDIF
     C*
     C                   if        EXEHD2  = *BLANKS
     C                   SETON                                        46
     C                   ENDIF
      ******* END ADD ****** 11/03/09 *********************************
     C   40              EXSR      DSPPRE
     C*
     C*** Beg Add *** 11/16/09 ***********************************************
     C*** Beg Dlt *** 02/17/10 ***********************************************
     C*******            setoff                                       4748
     C*******            if        XXRQD1 = 'Y'
     C*******            move      *on           *in47                          HI int on SCFFD1
     C*******            endif
     C*
     C*******            if        XXRQD2 = 'Y'
     C*******            move      *on           *in48                          HI int on SCFFD2
     C*******            endif
     C*** End dlt *** 02/17/10 ***********************************************
     C*** End Add *** 11/16/09 ***********************************************
     C*
     C***  Begin Del  ***  02/11/11  **************************************
     C*** BEG ADD *** 01/25/10 ***********************************************
     C**********         MOVEL     'WC'          XSKEY
     C*****XSKEY         CHAIN     SWC01                              60
      **********
     C******IN60         IFEQ      *OFF
     C*****WCFL44        IFEQ      'Y'
     C***********        MOVEL     *ON           *IN81
     C***********        ELSE
     C***********        MOVEL     *OFF          *IN81
     C***********        ENDIF
     C***********        ENDIF
     C*** END ADD *** 01/25/10 ***********************************************
     C***  End   Del  ***  02/11/11  **************************************
     C*
     C                   ENDSR                                                  ERRCK3
     C*
     C*
     C***  Begin Add  ***  10/02/06  **************************************
     C*===============================================================
     C* ERRCK4 - Subroutine for error checking SCR04
     C*===============================================================
     C*
     C     ERRCK4        BEGSR
     C*** beg chg ***** 08/22/11 ***************************************
     C****begin*Add ****** 03/01/11 ************************************
     C************       if        xxflg3c = *OFF
     C****END***Add ****** 03/01/11 ************************************
     C************
     C****begin*add ****** 12/31/10 ************************************
     C************       if        scffla <> *blanks and scffla <> 'Y'
     C************       eval      *in83 = *on
     C************       eval      *in90 = *on
     C************       endif
     C************
     C************       if        xxffla <> scffla
     C************
     C************       if        scffla = 'Y'
     C*Protect*the fields
     C************       eval      *in97 = *on
     C************       move      X'22'         AT16F
     C************       move      X'22'         AT17F
     C************       endif                                                  scffla = 'Y'
     C************       if        scffla = *blanks
     C*Unprotect*the fields
     C************       eval      *in97 = *off
     C************       move      X'24'         AT16F
     C************       move      X'24'         AT17F
     C************       endif                                                  scffla = *blanks
     C*Store*the*changed value
     C************       eval      xxffla = scffla
     C************       eval      *in90 = *on
     C************       endif
     C****end***add ****** 12/31/10 ************************************
     C****begin*Add ****** 03/01/11 ************************************
     C************       ENDIF
     C****END***Add ****** 03/01/11 ************************************

     c  n90              if        WC1918instald
     C                   if        scffla <> *blanks and scffla <> 'Y'
     C                   eval      *in83 = *on
     C                   eval      *in90 = *on
     C                   endif
      *
      * - Make 3c fields editable if Overwrite field changes from blank to 'Y'
      * - Protect 3c fields and reload from group default if Overwrite field
      *   changes from 'Y' to blank
      *
     c  n90              if        svffla <> scffla
     c                   select
     c                   when      scffla = 'Y'
     c                   eval      at16f = editLowLine
     c                   eval      at17f = editLowLine
     c                   when      scffla = *blanks
     c                   exsr      load3c
     c                   endsl
     c                   eval      svffla = scffla
     C                   eval      *in90 = *on
     c                   endif                                                  svffla <> scffla
     c                   endif                                                  WC1918  = installed
      *** end chg ***** 08/22/11 ***************************************
     C*  PREMIUM FINANCE COMPANY MUST BE EFFECTIVE FOR POLICY EFF DATE
     C*
     C  N90SCFCOD        IFNE      *BLANKS
     C*
     C                   Z-ADD     ELPFDT        KKPFDT
     C*
     C     PFKY2         SETGT     PPF01
     C*
     C     PFKY          READPE    PPF01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     PFEXDT        IFNE      *ZEROS
     C     ELPFDT        ANDGT     PFEXDT
     C                   SETON                                        71  90
     C                   ENDIF                                                  PFDT<EFDT
     C*
     C                   ELSE
     C*
     C***** Beg Chg ***** 11/25/11 ****************************************
     C******             SETON                                        98  90
     C                   SETON                                        71  90
     C***** End Chg ***** 11/25/11 ****************************************
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  SCFCOD<>BLKS
     C*
     C***  Begin Add  ***  09/08/07  **************************************
     C  N90SCAPFL        IFNE      'Y'
     C     SCAPFL        ANDNE     *BLANK
     C                   SETON                                        74  90
     C                   ENDIF
     C*
     C  N90SCAAFL        IFNE      'Y'
     C     SCAAFL        ANDNE     *BLANK
     C                   SETON                                        73  90
     C                   ENDIF
     C***  End   Add  ***  09/08/07  **************************************
     C*
     C*** BEG ADD ****** 01/25/10 *************************************
     C***  Begin Chg  ***  02/11/11  **************************************
     C*******            MOVEL     'WC'          XSKEY
     C*****XSKEY         CHAIN     SWC01                              60
      **********
     C******IN60         IFEQ      *OFF
     C*****WCFL44        ANDEQ     'Y'
      ***********
     C***********        MOVEL     *ON           *IN81
     C                   if        WCFL44 = 'Y'
     C***  End   Chg  ***  02/11/11  **************************************
      *
      ** beg del ***** 04/01/10 ***********************************************
     C**N90SCCGCF        IFNE      'Y'
     C*****SCCGCF        ANDNE     'N'
     C***********        SETON                                        80  90
     C***********        ENDIF
      ***********
     C**If*Group*setup is not completed, then throw the error..
      ***********
     C**N90SCCGCF        IFEQ      'Y'
      ***********
     C**GET*THE*RATING STATE TO ACESS THE WTFIP RECORDS APPLICABLE TO E
      ***********
     C*****RSKY**        CHAIN     PRS01                              60
     C**N60******        MOVEL     RSJRST        XXST
      ***********
     C**ACCESS*THE GROUP LEVEL CHARGE SETUP FILE  & VALIDATE EXPIRY DATE
     C***********
     C*****CIKY**        SETGT     PFI01
     C*****CIKY1*        READPE    PFI01                                  60
     C***********        IF        *IN60 = *ON
     C***********        SETON                                        84  90
     C***********        ELSE
     C***********        DOW       *IN60 =*OFF
     C***********
     C***********        IF        FIEXDT = 0  OR  FIEXDT>ELPFDT
     C***********        LEAVE
     C***********        ELSE
     C***********
     C*****CIKY1*        READPE    PFI01                                  60
     C***********        ENDIF
     C***********        ENDDO
     C***********        IF        *IN60 = *ON
     C***********        SETON                                        84  90
     C***********        ENDIF
     C***********
     C***********        ENDIF
     C***********
     C***********        ENDIF
      *** end del ***** 04/01/10 ***********************************************
     C***  Begin Add  ***  02/11/11  **************************************
     C*
     C* Validate stop collateral date
     C                   if        scScDt <> *zero
     C*
     C                   Z-ADD     SCSCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSCDT
     C*
     C                   if        srErr = *on
     C                   seton                                        88  90
     C                   leavesr
     C                   endif
     C*
     C                   endif
     C***  End   Add  ***  02/11/11  **************************************
      *
     C  N90SCSTCO        IFNE      'Y'
     C     SCSTCO        ANDNE     *BLANKS
     C                   SETON                                        82  90
     C                   ENDIF
      *
      *** beg del ***** 04/01/10 ***********************************************
     C**N90SCOGDF        IFNE      'Y'
     C*****SCOGDF        ANDNE     'N'
     C***********        SETON                                        83  90
     C***********        ENDIF
      *** end del ***** 04/01/10 ***********************************************
      *
     C                   ENDIF
     C*** END ADD ****** 01/25/10 *************************************
     C***  Beg   Add  ***  04/21/11  **************************************
      *
     C  n90scaeft        ifne      'A'
     C     scaeft        andne     *blanks
     C                   seton                                        86  90
     C                   endif
     c*
     c* If Auto EFT = 'A' then call WA023R3 to validate active EFT.
     c                   if        scaeft = 'A'

     C                   time                    dsdttm
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         q@dt              7 0
     C                   move      q@dt          q3dt

     C                   move      elco#         q3co#
     C                   movel(p)  elemp#        q3emp#            9
     C                   move      elfnd         q3fnd
     C                   movel(p)  eldiv         q3div             5
     c     q3emp#        cat(p)    q3div:0       xxwk30           30
     c     xxwk30        cat(p)    'AG':0        xxwk30
     c*
     C                   call      'WA023R3'
     C                   PARM                    Q3CO#             3
     C                   PARM      'E'           Q3TYPE            1
     C                   PARM                    Q3FND             3
     C                   PARM      XXWK30        Q3KEY            30
     C                   PARM                    Q3DT              7
     C                   PARM      ' '           Q3EFT             1
     C                   PARM      ' '           Q3PRE             1
     c                   if        q3eft <> *on
     C                   seton                                        85  90
     c                   endif                                                  q3eft <> *on

     c                   endif                                                  scaeft = 'A'
     C***  End   Add  ***  04/21/11  **************************************
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ELDT          IFGT      SAVEDT
     C     ELDT          OREQ      SAVEDT
     C     ELTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  ELDT>SAVEDT
     C*
     C  N90*IN40         IFEQ      *ON
B02  C     #OFSCR        ANDEQ     4
     C                   EXSR      DUPCHK
     C                   ENDIF                                                  *IN40=*ON
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C   40              EXSR      DSPPRE
     C*
      ***** BEGIN ADD ************** 03/14/08 ******************************
     C                   IF        *IN90= *OFF
     C                   IF        CLTID = '136'
     C                   IF        (SCFFL2 <> ' ' AND SCFFL2 <> 'Y')
     C                   MOVE      *ON           *IN98
     C                   MOVE      *ON           *IN90
     C                   MOVE      X'25'         AT8
     C                   ELSE
     C                   MOVE      *OFF          *IN98
     C                   MOVE      X'24'         AT8
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ***** END ADD **************** 03/14/08 ******************************
      ******* beg add ****** 04/29/08 *********************************

      * clt071 - Exclude from Retro Calc (Y/ )

     c                   if        CLTID = '071'
     c                   eval      *in76 = *on
     c                   endif

     c  n90              if        scexfr <> 'Y'
     c                             and scexfr <> ' '
     c                   eval      *in75 = *on
     c                   eval      *in90 = *on
     c                   endif

      ******* end add ****** 04/29/08 *********************************
     C*** BEG ADD ****** 08/21/08 ************************************
     C* Validate exclusion flag for min premium
     C  N90              IF        SCLATE <> 'Y' AND SCLATE <> ' '
     C                   SETON                                        78  90
     C                   ENDIF                                                  SCLATE<>'Y' ' '
     C*** END ADD ****** 08/21/08 ************************************
     C*** BEG ADD ****** 06/06/09 ************************************
     C* Validate flexible field 1
     C*** Beg Add *** 11/16/09 ***********************************************
     C*** Beg Del *** 07/28/10 ***********************************************
     C******N90            if        XXRQD1 = 'Y' and SCFFD1 = ' '
     C******             seton                                        90  49
     C******             endif
     C******
     C*** End Add *** 11/16/09 ***********************************************
     C******N90              IF        SCFFD1 <> *BLANKS
     C******             MOVEL     EXTBD1        ##TBID
     C*** Beg Chg *** 01/19/10 ***********************************************
     C**** GIKY          CHAIN     PGI01                              99
     C*****GIKY1         CHAIN     PGI01                              99
     C*** End Chg *** 01/19/10 ***********************************************
     C******IN99         IFEQ      *OFF
     C******
     C******             MOVEL     SCFFD1        XXKEY
     C******             MOVEL     EXTBD1        XXTBID
     C*****GTKY          CHAIN     PGT01                              99
     C******IN99         IFEQ      *ON
     C******             SETON                                        32  90
     C******             ENDIF
     C******             ENDIF
     C******             ENDIF
     C******
     C* Validate flexible field 2
     C******
     C*** Beg Add *** 11/16/09 ***********************************************
     C******N90              if        XXRQD2 = 'Y' and SCFFD2 = ' '
     C******             seton                                        90  50
     C******             endif
     C******
     C*** End Add *** 11/16/09 ***********************************************
     C******N90              IF        SCFFD2 <> *BLANKS
     C******             MOVEL     EXTBD2        ##TBID
     C*** Beg Chg *** 01/19/10 ***********************************************
     C**** GIKY          CHAIN     PGI01                              99
     C******GIKY1         CHAIN     PGI01                              99
     C*** End Chg *** 01/19/10 ***********************************************
     C******IN99         IFEQ      *OFF
     C******
     C******             MOVEL     SCFFD2        XXKEY
     C******             MOVEL     EXTBD2        XXTBID
     C******GTKY          CHAIN     PGT01                              99
     C******IN99         IFEQ      *ON
     C******             SETON                                        33  90
     C******             ENDIF
     C******             ENDIF
     C******             ENDIF
     C******
     C*** End Del *** 07/28/10 ***********************************************
     C*** END ADD ****** 06/06/09 ************************************
     C*
     C*** BEG ADD ****** 01/25/10 ************************************
     C*** beg del ****** 04/01/10 ************************************
     C*** In change mode if flag is set to N then clear policy level
     C*** claim fee fields from WDEFP (if set)
     C***********
     C******IN90*        IFEQ      *OFF
     C***********        IF        (@@MODE = 'C' AND SCCGCF = 'N')
     C***********                   OR (@@MODE = 'C' AND SCOGDF = 'N')
     C*****EFKY**        CHAIN     PEF01
     C***********        IF        %FOUND(WDEFL1)
     C***********        DELETE    PEF01
     C***********        ENDIF
     C***********        ENDIF
     C***********        ENDIF
     C*** end del ****** 04/01/10 ************************************
     C*** END ADD ****** 01/25/10 ************************************
     C*
     C                   ENDSR                                                  ERRCK4
     C*
     C*===============================================================
     C* ERRCK5 - Subroutine for error checking SCR05
     C*===============================================================
     C*
     C     ERRCK5        BEGSR
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90@@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C  N90ELDT          IFGT      SAVEDT
     C     ELDT          OREQ      SAVEDT
     C     ELTIME        ANDGT     SAVETM
     C                   SETON                                        93  90
     C                   ENDIF                                                  ELDT>SAVEDT
     C*
     C  N90*IN40         IFEQ      *ON
B02  C     #OFSCR        ANDEQ     5
     C                   EXSR      DUPCHK
     C                   ENDIF                                                  *IN40=*ON
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C   40              EXSR      DSPPRE
     C*
     C*** Beg Add *** 07/28/10 ***********************************************
     C  N90              if        XXRQD1 = 'Y' and SCFFD1 = ' '
     C                   seton                                        90  49
     C                   endif
     C*
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'N'
     C  N90              IF        SCFFD1 <> *BLANKS
     C                   MOVEL     EXTBD1        ##TBID
     C     GIKY1         CHAIN     PGI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCFFD1        XXKEY
     C                   MOVEL     EXTBD1        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        32  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* Validate flexible field 2
     C  N90              if        XXRQD2 = 'Y' and SCFFD2 = ' '
     C                   seton                                        90  50
     C                   endif
     C*
     C  N90              IF        SCFFD2 <> *BLANKS
     C                   MOVEL     EXTBD2        ##TBID
     C     GIKY1         CHAIN     PGI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCFFD2        XXKEY
     C                   MOVEL     EXTBD2        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        33  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  93 = *ON && 94= *OFF
     C                   if        flg132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C  N90              IF        SCFFD1 <> *BLANKS
     C                   MOVEL     FHF1TB        ##TBID
     C     GIKY1         CHAIN     PGI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCFFD1        XXKEY
     C                   MOVEL     FHF1TB        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        32  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* Validate flexible field 2
     C  N90              if        XXRQD2 = 'Y' and SCFFD2 = ' '
     C                   seton                                        90  50
     C                   endif
     C*
     C  N90              IF        SCFFD2 <> *BLANKS
     C                   MOVEL     FHF2TB        ##TBID
     C     GIKY1         CHAIN     PGI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCFFD2        XXKEY
     C                   MOVEL     FHF2TB        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        33  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* Validate flexible field 3
     C  N90              if        XXRQD3 = 'Y' and SCFFD3 = ' '
     C                   seton                                        90  52
     C                   endif
     C*
     C  N90              IF        SCFFD3 <> *BLANKS
     C                   MOVEL     FHF3TB        ##TBID
     C     GIKY1         CHAIN     PGI01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     SCFFD3        XXKEY
     C                   MOVEL     FHF3TB        XXTBID
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        51  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* Validate flexible field 4
     C  N90              if        XXRQD4 = 'Y' and SCFFD4 = ' '
     C                   seton                                        90  54
     C                   endif
     C* Validate flexible field 5
     C  N90              if        XXRQD5 = 'Y' and SCFFD5 = 0
     C                   seton                                        90  56
     C                   endif
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   ENDIF                                                  93= *on && 94 = *on
     C*** End Add *** 12/30/10 ***********************************************
     C*** End Add *** 07/28/10 ***********************************************
     C                   ENDSR                                                  ERRCK5
     C***  End   Add  ***  10/02/06  **************************************
     C*
     C/EJECT
     C***  BEGIN ADD  ***  02/10/00  *********************************
     C*===============================================================
     C* GETRCD - GET THE RATE SET CODE FOR THE STATISTICAL/RATING FILE
     C*===============================================================
     C*
     C     GETRCD        BEGSR
     C*
     C                   Z-ADD     EMEFD1        XXDATE
     C*
      *** BEG CHG ***** 12/06/06A ************************************
     C********** EMMST     IFNE *BLANKS
     C**********           MOVELEMMST     XXST
     C**********           ELSE
     C**********           MOVELEMPST     XXST
     C**********           ENDIF                           EMMST<>*BLANKS
     C     EMPST         IFNE      *BLANKS
     C                   MOVEL     EMPST         XXST
     C                   ELSE
     C                   MOVEL     EMMST         XXST
     C                   ENDIF                                                  EMMST<>*BLANKS
      *** END CHG ***** 12/06/06A ************************************
     C*
     C     RRKY1         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   RRDRAT
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDSR                                                  GETRCD
     C***  END   ADD  ***  02/10/00  *********************************
     C/EJECT
     C*
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C*  F10=TABLE SEARCH
     C*
     C     KJDO          BEGSR
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCPFDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCPTDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C                   Z-ADD     SCANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCANDT
     C                   ENDIF                                                  SRERR=*OFF
     C*
     C*** BEGIN ADD ***  04/28/98  ************************************
     C     SCRTCD        IFNE      *BLANKS
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        ##ST
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  SCRTCD IFNE
     C*** END ADD *****  04/28/98  ************************************
     C                   SELECT
     C*
     C*  BILLING SCHEDULE CODE
     C*
     C     HFLD          WHENEQ    'SCBSCD'
     C*** BEGIN ADD ***  12/05/01  ************************************
     C     *IN06         ANDEQ     *OFF
     C*** END   ADD ***  12/05/01  ************************************
     C*
     C     SCBSTY        IFEQ      *BLANK
     C*
      *** BEG ADD ***** 03/20/06 **************************************
      *
      *  GET THE NUMBER OF POSSIBLE MONTHLY INSTALLMENTS FOR THE POLICY WHICH
      *  CURRENTLY BILLS THE FIRST OF THE MONTH. SUBTRACT THIS NUMBER FROM
      *  12 TO GET THE MONTH VALUE FOR THE BILLING AUXILIARY EFFECTIVE DATE.
      *  USE THE DAY VALUE OF THE BSDT OR PFDT FOR THE DAY VALUE. CALL W2155R
      *  WITH THE ##RRN PARM EQUAL TO FFF00MMDD WHERE FFF IS THE FUND; MM THE
      *  NUMBER OF MONTHS; DD THE DAY VALUE.  THESE VALUES WILL BE USED TO
      *  FILTER THE BILLING SCHEDULE CODE RECORD SELECTION IN W2155R.
      *
     C                   CLEAR                   ##RRN
     C                   MOVEL     SCFND         ##RRN
     C     SCBSDT        IFNE      *ZERO
     C                   Z-ADD     SCBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0
     C                   ELSE
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT
     C                   ENDIF
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
     C                   Z-ADD     XXBGDT        XXDT
      *** BEG ADD ***** 10/19/06 *************************************
     C                   MOVE      XXBGDT        SVDD              2 0
      *** END ADD ***** 10/19/06 *************************************
     C                   Z-ADD     1             XXDD
      *** BEG ADD ***** 06/12/06 *************************************
      *** BEG CHG ***** 02/07/07 *************************************
     C*****************    Z-ADDXXDT      XXBGDT
     C                   Z-ADD     XXDT          XXSTDT            7 0
      *** END CHG ***** 02/07/07 *************************************
      *** END ADD ***** 06/12/06 *************************************
     C                   Z-ADD     0             XXINCT            2 0
      *** BEG ADD ***** 02/07/07 *************************************
     C                   Z-ADD     XXPTDT        SVPTDT            7 0
     C                   MOVE      '01'          SVPTDT
     C     XXSTDT        IFLT      XXBGDT
     C     XXPTDT        ANDEQ     SVPTDT
     C                   ADD       1             XXINCT
     C                   ENDIF
      *** END ADD ***** 02/07/07 *************************************
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
     C                   Z-ADD     XXINCT        XXMM
     C                   MOVE      XXMMDD        ##RRN
      *** END ADD ***** 03/20/06 **************************************
      *** BEG ADD ***** 10/19/06 *************************************
     C                   MOVE      SVDD          ##RRN
      *** END ADD ***** 10/19/06 *************************************
     C*
     C                   CALL      'W2155R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C*
     C     ##RRN         CHAIN     WTBSP                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     *BLANKS       SCBSTY
     C                   MOVEL(P)  BSKEY         SCBSCD
     C                   MOVEL(P)  BSDESC        SCBSDS
     C                   MOVE      *ON           *IN51
     C                   ELSE
     C                   MOVE      *ON           *IN49
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ELSE
     C*
     C                   EXSR      BILAUX
      *** BEG ADD ***** 02/07/07 ***************************************
     C                   MOVE      *BLANKS       SCBSTY
      *** END ADD ***** 02/07/07 ***************************************
     C*
     C                   ENDIF                                                  SCBSTY=*BLANKS
     C*
     C*  BILLING SCHEDULE TYPE
     C*
     C     HFLD          WHENEQ    'SCBSTY'
      *** BEG ADD **** 04/03/02 ************************************************
     C     *IN06         ANDEQ     *OFF
      *** END ADD **** 04/03/02 ************************************************
     C*
     C                   EXSR      BILAUX
      *** BEG ADD ***** 02/07/07 **********************************
     C                   MOVE      *BLANKS       SCBSTY
      *** END ADD ***** 02/07/07 **********************************
     C*
     C*** Beg Del *** 07/28/10 ***********************************************
     C*  POLICY TYPE
     C*****
     C*****HFLD          WHENEQ    'SCQTYP'
     C*****
     C*****              CALL      'W2493R'
     C*****              PARM                    QQPRDS           40
     C*****              PARM      '2'           QQCALL            1
     C*****              PARM      'PTYP'        QQID              4
     C*****              PARM      *BLANKS       QQKEY            10
     C*****              PARM                    QQRTCD            1
     C*****
     C*****QQKEY         IFNE      *BLANKS
     C*****
      ***** BEG CHG ******* 10/13/03 **********************************
     C*********            MOVE *ON       *IN49
     C*****              SETON                                        79  49
      ***** END CHG ******* 10/13/03 **********************************
     C*****              MOVEL     QQKEY         SCQTYP
     C*****              MOVEL     QQID          XXTBID
     C*****              MOVEL(P)  QQKEY         XXKEY
     C*****
     C*****GTKY          CHAIN     PGT01                              60
     C*****N60              MOVEL(P)  GTDESC        SCQTYD
     C*****
     C*****
     C*****              ELSE
     C*****
     C*****              MOVE      *ON           *IN79
      ***** BEG ADD ******* 10/13/03 **********************************
     C*****              MOVE      *OFF          *IN49
      ***** END ADD ******* 10/13/03 **********************************
     C*****
     C*****              ENDIF                                                  QQKEY<>*BLANKS
     C*** End Del *** 07/28/10 ***********************************************
     C*** Beg Add *** 07/28/10 ***********************************************
     C*  Policy Type (Field 6 @ Grp level)
     C     HFLD          WHENEQ    'SCQTYP'
     C     FHF6TB        ANDNE     *BLANKS
     C*** Beg Add *** 12/30/10 ***********************************************
     C     FLG132        ANDEQ     'Y'
     c     FLG132        OREQ      'N'
     C*** End Add *** 12/30/10 ***********************************************
     C*** Beg Chg *** 12/30/10 ***********************************************
     C*********          MOVE      FHF6TB        QQID
     C                   IF        FLG132 = 'Y'
     C                   MOVE      FHF6TB        QQID
     C                   ELSE
     C                   MOVE      'PTYP'        QQID
     C                   ENDIF
     C*** End Chg *** 12/30/10 ***********************************************
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C
     C     QQKEY         IFNE      *BLANKS
     C                   SETON                                        79  49
     C                   MOVEL     QQKEY         SCQTYP
     C                   MOVEL     QQID          XXTBID
     C                   MOVEL(P)  QQKEY         XXKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL(P)  GTDESC        SCQTYD
     C                   ELSE
     C                   MOVE      *ON           *IN79
     C                   MOVE      *OFF          *IN49
     C                   ENDIF                                                  QQKEY<>*BLANKS
     C*** End Add *** 07/28/10 ***********************************************
     C*
     C*  RATE SET CODE
     C*
     C     HFLD          WHENEQ    'SCRTCD'
     C*
     C                   CALL      'W2471R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@KEY            10
     C*
     C     @@KEY         IFEQ      *BLANKS
     C                   MOVE      *ON           *IN52
     C                   ELSE
     C                   MOVE      *ON           *IN53
     C*
     C                   MOVEL     @@KEY         SCRTCD
     C                   MOVE      *BLANKS       SCRTDS
     C                   MOVEL(P)  SCRTCD        XXKEY
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL(P)  RSDESC        SCRTDS
     C                   ENDIF                                                  SCBSCD=*BLANKS
     C*
     C*  INCREASED LIMITS #1 & #2
     C*
     C     HFLD          WHENEQ    'SCILCD'
     C     HFLD          OREQ      'SCINL2'
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCILCD'
     C                   Z-ADD     54            PC                3 0
     C     HFLD          WHENEQ    'SCINL2'
     C                   Z-ADD     56            PC                3 0
     C                   ENDSL
     C*
     C                   Z-ADD     DBRRN1        ##RRN
     C                   CALL      'W2185R'      SRCHPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WTLMP                              60
     C                   ADD       1             PC
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCILCD'
     C                   MOVEL     LMKEY         SCILCD
     C                   MOVEL     LMDESC        SCILDS
     C     HFLD          WHENEQ    'SCINL2'
     C                   MOVEL     LMKEY         SCINL2
     C                   MOVEL     LMDESC        SCI2DS
     C                   ENDSL
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
     C                   MOVE      *ON           *IN(PC)
     C*
     C*  DEDUCTIBLE DISCOUNT CODE
     C*
     C***  Begin Chg  ***  07/06/11  **************************************
     C*****HFLD*         WHENEQ    'SCDCDE'
     C******IN69         ANDEQ     *ON
     C                   when      hfld = 'SCDCDE'
     C                              and deductInstalled
     C***  End   Chg  ***  07/06/11  **************************************
     C*
     C***  BEGIN ADD  ***  04/07/05-2 ***********************************
     C                   MOVE      *ON           *IN82
     C***  END   ADD  ***  04/07/05-2 ***********************************
     C*
     C                   Z-ADD     DBRRN1        ##RRN
     C                   CALL      'W0155R'      SRCHPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WTDDP                              60
     C*
     C                   MOVEL     DDCODE        SCDCDE
     C                   MOVEL     DDDESC        SCDCDS
     C*
     C                   MOVE      *ON           *IN59
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN58
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C*
     C***  BEGIN ADD  ***  04/07/05-2 ***********************************
     C***  Begin Chg  ***  07/06/11  **************************************
     C*****HFLD*         WHENEQ    'SCDCD2'
     C******IN69         ANDEQ     *ON
     C                   when      hfld = 'SCDCD2'
     C                              and deductInstalled
     C***  End   Chg  ***  07/06/11  **************************************
     C*
     C                   MOVE      *ON           *IN84
     C*
     C                   Z-ADD     DBRRN1        ##RRN
     C                   CALL      'W0155R'      SRCHPL
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     WTDDP                              60
     C*
     C                   MOVEL     DDCODE        SCDCD2
     C                   MOVEL     DDDESC        SC2DDS
     C*
     C                   MOVE      *ON           *IN59
     C*
     C                   ELSE
     C*
     C                   MOVE      *ON           *IN58
     C*
     C                   ENDIF                                                  ##RRN<>*ZEROS
     C***  END   ADD  ***  04/07/05-2 ***********************************
     C*
     C*  MCO TYPE CODE
     C*
     C     HFLD          WHENEQ    'SCMCCD'
     C*
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVE(P)   *BLANK        XXKEY
     C*
     C                   CALL      'W2493R'
     C                   PARM                    ##DATA
     C                   PARM                    @@MODE
     C                   PARM                    XXTBID
     C                   PARM                    XXKEY
     C                   PARM                    QQRTC             1
     C*
     C     XXKEY         IFNE      *BLANKS
      ***** BEG CHG ******* 11/30/99 **********************************
     C***********          MOVE *ON       *IN52
     C                   MOVE      *ON           *IN95
      ***** END CHG ******* 11/30/99 **********************************
     C                   MOVEL(P)  XXKEY         SCMCCD
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCMCOD
     C                   ELSE
     C                   MOVE      *ON           *IN51
     C                   ENDIF                                                  XXKEY<>*BLANKS
      ***** BEG ADD ******* 11/30/99 **********************************
      *
      *  MCO GROUP CODE
      *
     C     HFLD          WHENEQ    'SCMCO'
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##PFDT
      *
     C                   CALL      'W2295R'
     C                   PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN1            9 0          WMMGP
     C                   PARM      DBRRN1        ##RRN2            9 0          WMFNP
     C                   PARM                    ##PFDT            7 0
      *
     C     ##RRN1        IFEQ      *ZERO
     C                   SETON                                        95
      *
     C                   ELSE
      *
     C     ##RRN1        CHAIN     PMG02                              60
     C                   MOVEL(P)  MGMCO         SCMCO
     C                   MOVEL(P)  MGNAME        SCMCDS
     C                   SETON                                        52
     C                   ENDIF
      *
      ***** END ADD ******* 11/30/99 **********************************
     C*
     C*
     C     HFLD          WHENEQ    'SCDISC'
     C*
     C                   MOVE      *BLANK        QQCODE            1
     C                   Z-ADD     0             QQYR              2 0
     C                   Z-ADD     *ZERO         QQLMT            11 2
     C                   MOVE      *BLANK        QQEQTP            1
     C                   Z-ADD     DBRRN1        ##RRN
     C                   CALL      'W0150R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM                    XXST
     C*
     C*
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PDS02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DSCODE        SCDISC
     C                   MOVE      *ON           *IN62
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  ##RRN IFNE
     C*
     C     *IN62         IFEQ      *OFF
     C*
     C*** BEGIN CHG *** 03/09/98 **************************************
     C**********           MOVE *ON       *IN61
     C*** BEGIN CHG *** 07/06/06 **************************************
     C**********           MOVE *ON       *IN91
     C                   MOVE      *ON           *IN54
     C*** END CHG ***** 07/06/06 **************************************
     C*** END   CHG *** 03/09/98 **************************************
     C*
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C*
     C*  SEARCH ON PREMIUM FINANCE CO. CODE
     C*
     C     HFLD          WHENEQ    'SCFCOD'
     C*
     C                   CALL      'W0045R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRN1        ##RRN             9 0          WMFNP
     C                   PARM                    ##RRN2            9 0          WMPFP
     C*
     C     ##RRN2        IFNE      *ZEROS
     C     ##RRN2        CHAIN     PPF02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PFFCOD        SCFCOD
     C                   MOVEL     PFNAME        SCPFNM
     C***** BEGIN DEL **** 01/09/04 ***********************************
     C***** BEGIN ADD **** 02/18/03 ***********************************
     C***************      SETON                     71
     C***** END   ADD **** 02/18/03 ***********************************
     C***** END   DEL **** 01/09/04 ***********************************
     C                   ENDIF                                                  *IN60=OFF
     C                   ENDIF                                                  ##RRN2<>0
     C*
     C***** END   ADD **** 09/29/99 ***********************************
     C***** BEGIN DEL **** 01/09/04 ***********************************
     C***** BEGIN ADD **** 02/18/03 ***********************************
     C*  SEARCH ON VENDOR NUMBER
     C********** HFLD      WHEQ 'SCVDR#'
     C**********           CALL 'W0003R'
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM '2'       ##CALL  1
     C**********           PARM *ZEROS    ##RRN
     C********** ##RRN     IFNE *ZEROS
     C********** ##RRN     CHAINPVP01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVE VPVDR#    SCVDR#
     C**********           MOVELVPNAME    SCVDRN
     C**********           ENDIF                           *IN60=OFF
     C**********           ELSE
     C**********           SETON                     71
     C**********           ENDIF                           ##RRN<>0
     C***** END   ADD **** 02/18/03 ***********************************
     C***** END   DEL **** 01/09/04 ***********************************
     C*
     C***** BEG ADD ***** 10/05/04 ***********************************
     C*
     C*  SEARCH ON PREMIUM FINANCE CO. CODE
     C*
     C     HFLD          WHENEQ    'SCTYPB'
     C*
     C                   MOVE      *BLANKS       @@TYPB
      *
     C                   CALL      'W2275R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@TYPB            3
     C*
     C     @@TYPB        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN34
     C                   ELSE
     C                   MOVE      *ON           *IN35
     C                   MOVEL     @@TYPB        SCTYPB
     C                   ENDIF
     C*
     C***** END ADD ***** 10/05/04 ***********************************
     C****** BEG ADD ***** 07/07/05 **************************************
     C*
     C*      ACTIVITY STATUS
     C     HFLD          WHENEQ    'SCASCD'
     C*
     C                   CLEAR                   ##RRN
     C                   CALL      'W2164R'      ACDPL                  99
     C*
     C     ##RRN         IFNE      *ZEROS
     C                   MOVE      *ON           *IN77
     C     ##RRN         CHAIN     WTAVP                              60
     C                   MOVEL(P)  AVKEY         SCASCD
     C                   MOVEL     AVDESC        SCACTD
     C                   ELSE
     C**** BEGIN CHG ***** 07/06/06 **************************************
     C***********          MOVE *ON       *IN76
     C                   MOVE      *ON           *IN78
     C*
     C**** END CHG ******* 07/06/06 **************************************
     C                   ENDIF                                                  XXKEY<>*BLANKS
     C****** END ADD ***** 07/07/05 **************************************
      *
      ***** BEG ADD ******* 06/06/09 **********************************
      *
     C*
     C*** Beg Del *** 07/28/10 ***********************************************
     C*  Flexible field 1
     C*****
     C*****HFLD          WHENEQ    'SCFFD1'
     C*****EXTBD1        ANDNE     *BLANKS
     C*****
     C*****              MOVE      EXTBD1        QQID
     C*****
     C*****              CALL      'W2493R'
     C*****              PARM                    QQPRDS           40
     C*****              PARM      '2'           QQCALL            1
     C*****              PARM                    QQID              4
     C*****              PARM      *BLANKS       QQKEY            10
     C*****              PARM                    QQRTCD            1
     C*****
     C*****QQKEY         IFNE      *BLANKS
     C*****              MOVEL     QQKEY         SCFFD1
     C*****              MOVEL     *ON           *IN35
     C*****              ELSE
     C*****              MOVEL     *ON           *IN34
     C*****              ENDIF
     C*****
     C*****              IF        EXEHD1  = *BLANKS
     C*****              EVAL      *IN45 = *ON
     C*****              ENDIF
     C*****
     C*  Flexible field 2
     C*****
     C*****HFLD          WHENEQ    'SCFFD2'
     C*****EXTBD2        ANDNE     *BLANKS
     C*****
     C*****              MOVE      EXTBD2        QQID
     C*****
     C*****              CALL      'W2493R'
     C*****              PARM                    QQPRDS           40
     C*****              PARM      '2'           QQCALL            1
     C*****              PARM                    QQID              4
     C*****              PARM      *BLANKS       QQKEY            10
     C*****              PARM                    QQRTCD            1
     C*****
     C*****QQKEY         IFNE      *BLANKS
     C*****              MOVEL     QQKEY         SCFFD2
     C*****              MOVEL     *ON           *IN35
     C*****              ELSE
     C*****              MOVEL     *ON           *IN35
     C*****              ENDIF
     C*****
     C*****              IF        EXEHD2  = *BLANKS
     C*****              EVAL      *IN46 = *ON
     C*****              ENDIF
     C*** End Del *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   endsl
     C*** End Add *** 12/30/10 ***********************************************
     C*
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'N'
     C                   select
     C*  Flexible field 1
     C     HFLD          WHENEQ    'SCFFD1'
     C     extbd1        ANDNE     *BLANKS
     C                   MOVE      extbd1        QQID
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCFFD1
     C                   MOVEL     *ON           *IN35
     C                   ELSE
     C                   MOVEL     *ON           *IN34
     C                   ENDIF
     C*Field#1 column 3
     C                   If        extbd1 <> *Blanks
     C                   Move      X'20'         AT10S
     C                   Movel     extbd1        XXFTB
     C                   Movel     SCFFD1        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS1
     C                   Else
     C                   Move      X'27'         AT10S
     C                   EndIf
     C*  Flexible field 2
     C
     C     HFLD          WHENEQ    'SCFFD2'
     C     extbd2        ANDNE     *BLANKS
     C                   MOVE      extbd2        QQID
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCFFD2
     C                   MOVEL     *ON           *IN36
     C                   ELSE
     C                   MOVEL     *ON           *IN35
     C                   ENDIF
     C*Field#2 column 3
     C                   If        extbd2 <> *Blanks
     C                   Move      X'20'         AT11S
     C                   Movel     extbd2        XXFTB
     C                   Movel     SCFFD2        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS2
     C                   Else
     C                   Move      X'27'         AT11S
     C                   EndIf
     C*
     C                   endsl
     C                   endif
     C*** End Add *** 12/30/10 ***********************************************
     C*** Beg Add *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'Y'
     C                   SELECT
     C*** End Add *** 12/30/10 ***********************************************
     C*  Flexible field 1
     C
     C     HFLD          WHENEQ    'SCFFD1'
     C     FHF1TB        ANDNE     *BLANKS
     C                   MOVE      FHF1TB        QQID
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCFFD1
     C                   MOVEL     *ON           *IN35
     C                   ELSE
     C                   MOVEL     *ON           *IN34
     C                   ENDIF
     C*Field#1 column 3
     C                   If        FHF1TB <> *Blanks
     C                   Move      X'20'         AT10S
     C                   Movel     FHF1TB        XXFTB
     C                   Movel     SCFFD1        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS1
     C                   Else
     C                   Move      X'27'         AT10S
     C                   EndIf
     C*  Flexible field 2
     C
     C     HFLD          WHENEQ    'SCFFD2'
     C     FHF2TB        ANDNE     *BLANKS
     C                   MOVE      FHF2TB        QQID
     C*
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCFFD2
     C                   MOVEL     *ON           *IN36
     C                   ELSE
     C                   MOVEL     *ON           *IN35
     C                   ENDIF
     C*Field#2 column 3
     C                   If        FHF2TB <> *Blanks
     C                   Move      X'20'         AT11S
     C                   Movel     FHF2TB        XXFTB
     C                   Movel     SCFFD2        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS2
     C                   Else
     C                   Move      X'27'         AT11S
     C                   EndIf
     C*  Flexible field 3
     C     HFLD          WHENEQ    'SCFFD3'
     C     FHF3TB        ANDNE     *BLANKS
     C                   MOVE      FHF3TB        QQID
     C
     C                   CALL      'W2493R'
     C                   PARM                    QQPRDS           40
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QQID              4
     C                   PARM      *BLANKS       QQKEY            10
     C                   PARM                    QQRTCD            1
     C*
     C     QQKEY         IFNE      *BLANKS
     C                   MOVEL     QQKEY         SCFFD3
     C                   MOVEL     *ON           *IN37
     C                   ELSE
     C                   MOVEL     *ON           *IN36
     C                   ENDIF
     C*Field#3 column 3
     C                   If        FHF3TB <> *Blanks
     C                   Move      X'20'         AT12S
     C                   Movel     FHF3TB        XXFTB
     C                   Movel     SCFFD3        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS3
     C                   Else
     C                   Move      X'27'         AT12S
     C                   EndIf
     C*** End Add *** 07/28/10 ***********************************************
      ***** END ADD ******* 06/06/09 **********************************
     C*
     C                   OTHER
     C*
     C                   ENDSL
     C*
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   EndIf
     C*** End Add *** 12/30/10 ***********************************************
     C                   ENDSR                                                  KJDO
     C*
     C/EJECT
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14
     C*===============================================================
     C*
     C*  F14=DARTS
     C*
     C     KNDO          BEGSR
     C*
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C   41              MOVE      'Q'           QCQTE
     C  N41              MOVE      *BLANKS       QCQTE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  KNDO
     C/EJECT
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE FOR MOVING DATA BASE FIELDS TO SCREEN
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
      *** BEG ADD ***** 08/09/07 **************************************
     C                   MOVE      ELRTCD        SVRTCD
      *** END ADD ***** 08/09/07 **************************************
     C  N40              Z-ADD     ELFYR         SCFYR
     C*
     C*** beg chg ***** 01/12/12 **************************************
     C************       Z-ADD     ELPFDT        SRDTI
     C****BEGIN*ADD ***  11/09/01  ************************************
     C******IN40**       IFEQ      *ON
     C*****YRFLG**       ANDEQ     'N'
     C************       Z-ADD     EMEFD1        SRDTI
     C************       ENDIF
     C****END*ADD******  11/09/01  ************************************
      ******BEG*ADD ******* 01/05/05 **********************************
     C************       Z-ADD     SRDTI         X1PFDT            7 0
      ******END*ADD ******* 01/05/05 **********************************
     C************       CALL      'SRCVTDAT'    DTMDYY
     C************       MOVEL     SRDTO         SCPFDT
     C************       Z-ADD     ELPFDT        XXPFDT

     c                   if        @@mode <> 'A'
     c     *cymd         move      ELPFDT        workDate
     c     *usa          move      workDate      scpfdt
     c     *cymd         move      ELPTDT        workDate
     c     *usa          move      workDate      scptdt
     C*** END ADD ***** 05/07/12 **************************************
     C*** BEG CHG ***** 05/07/12 **************************************
     c**** *cymd         move      ELBSDT        workDate
     c**** *usa          move      workDate      scbsdt
      /Free
       Eval ##datein = ELBSDT ;
       Eval ##fromfmt = '*CYMD' ;
       Eval ##tofmt = *Blanks;
       Eval @@dateout = *Blanks ;
       Eval @@err = *Blanks ;

       SRCVTDAT(##datein : ##fromfmt : ##tofmt : @@dateout : @@err) ;

       If @@err = '0' ;
          workDate = %Date(ELBSDT : *cymd) ;
          scbsdt = %Dec(workDate : *usa) ;
       Endif ;
      /End-Free
     C*** END CHG ***** 05/07/12 **************************************
     c                   eval      scaafl = ELAAFL
     c                   eval      scapfl = ELAPFL
     C                   ENDIF
     C*** end chg ***** 01/12/12 **************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*** BEG CHG ***** 06/13/07 **************************************
     C***********          MOVE ELARSM    SCARSM
     C                   MOVE      ELARSM        SVARSM            1
     C     ELARSM        IFEQ      '@'
     C                   MOVE      *BLANK        SCARSM
     C                   ELSE
     C                   MOVE      ELARSM        SCARSM
     C                   ENDIF
     C*** END CHG ***** 06/13/07 **************************************
     C*
     C*** BEGIN ADD ***  09/16/99  ************************************
     C                   MOVE      ELSRPR        SCSRPR
     C   40              MOVE      'P'           SCSRPR
     C*** END ADD *****  09/16/99  ************************************
     C*** BEGIN ADD ***  04/17/00  ************************************
     C                   MOVE      ELCOFL        SCCOFL
     C*** END ADD *****  04/17/00  ************************************
     C*** beg del ***** 01/12/12 **************************************
     C***********        Z-ADD     ELPTDT        SRDTI
     C****BEGIN*ADD ***  11/09/01  ************************************
      ***********
     C******IN40*        IFEQ      *ON
     C*****YRFLG*        ANDEQ     'N'
      ***********
     C*****EMSTRP        IFEQ      'Y'
     C*****EMEFD1        ADD       10000         SRDTI
     C***********        ELSE
      ***********
     C*****EMEFD1        ADD       10000         SRDTI
     C*****EMKY2*        SETLL     PEM01
     C******IN60*        DOUEQ     *ON
     C*****EMKY2*        READE     PEM01                                  60
      ***********
     C******IN60*        IFEQ      *ON
     C***********        LEAVE
     C***********        ENDIF
      ***********
     C*****EMSTRP        IFEQ      'Y'
     C*****ELKY3*        CHAIN     PEL01                              60
     C******IN60*        IFEQ      *OFF
     C***********        Z-ADD     ELPTDT        SRDTI
     C*****Begin*Add  ***  07/20/07  **************************************
     C******LIKE*        DEFINE    ELAAFL        SVAAFL
     C***********        MOVEL(P)  ELAAFL        SVAAFL
     C*****End***Add  ***  07/20/07  **************************************
     C*****Begin*Add  ***  09/08/07  **************************************
     C******LIKE*        DEFINE    ELAPFL        SVAPFL
     C***********        MOVEL(P)  ELAPFL        SVAPFL
     C*****End***Add  ***  09/08/07  **************************************
     C****BEGIN*ADD ***  09/05/02  ************************************
     C***********        CLEAR                   DBRCDF
     C****END*ADD *****  09/05/02  ************************************
     C***********        LEAVE
     C***********        ENDIF
     C***********        ENDIF
     C***********        ENDDO
      ***********
     C***********        ENDIF
      ******BEG*ADD ******* 04/23/03 **********************************
      ***********
     C*****FNCGFL        IFEQ      'Y'
     C*****EMEFD1        ADD       10000         SRDTI
      ******BEG*ADD ******* 01/05/05 **********************************
      ***********
     C*****SRDTI*        DOWLE     X1PFDT
     C*****SRDTI*        ANDLT     1990000
     C***********        ADD       10000         SRDTI
     C***********        ENDDO
      ******END*ADD ******* 01/05/05 **********************************
      ***********
     C***********        ELSE
      ******BEG*ADD ***** 04/07/05 ************************************
     C*****FNPEOF        IFEQ      *BLANK
     C*****##ENPO        ANDNE     *ZEROS
     C***********        Z-ADD     ##ENPO        SRDTI
     C***********        ELSE
      ******END*ADD ***** 04/07/05 ************************************
      ***********
     C***********        Z-ADD     F1ENPO        SRDTI
      ******BEG*ADD ***** 04/07/05 ************************************
     C***********        ENDIF
      ******END*ADD ***** 04/07/05 ************************************
     C***********        ENDIF
      ***********
      ******END*ADD ******* 04/23/03 **********************************
     C***********        ENDIF
     C*****@@RRN2        CHAIN(N)  WMEMP                              60
     C****END*ADD *****  11/09/01  ************************************
     C***********        CALL      'SRCVTDAT'    DTMDYY
     C***********        MOVEL     SRDTO         SCPTDT

     c     @@rrn2        chain(n)  WMEMP
     C*** end del ***** 01/12/12 **************************************
     C*
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCANDT
     C*
     C                   MOVEL     ELILCD        SCILCD
     C*
     C                   Z-ADD     ELILEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCILEF
     C*
     C                   Z-ADD     ELILEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCILEX
     C*
     C                   MOVEL     ELINL2        SCINL2
     C*
     C                   Z-ADD     ELI2EF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCI2EF
     C*
     C                   Z-ADD     ELI2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCI2EX
     C*
     C                   MOVEL     ELDCDE        SCDCDE
     C*
     C                   Z-ADD     ELDCEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDCEF
     C*
     C                   Z-ADD     ELDCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDCEX
     C*
     C***  BEGIN ADD  ***  04/07/05-2 ***********************************
     C                   MOVEL     ELDCD2        SCDCD2
     C*
     C                   Z-ADD     EL2DEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SC2DEF
     C*
     C                   Z-ADD     EL2DEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SC2DEX
     C***  END   ADD  ***  04/07/05-2 ***********************************
     C*
     C                   MOVEL     ELCXBL        SCCXBL
     C                   MOVEL     ELASPM        SCASPM
     C*
     C                   Z-ADD     ELMCEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMCEF
     C*
     C                   Z-ADD     ELMCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMCEX
     C*
     C                   MOVEL     ELASPS        SCASPS
     C*
     C                   Z-ADD     ELSCEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCEF
     C*
     C                   Z-ADD     ELSCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCEX
     C*
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C                   Z-ADD     ELEMPY        SCEMPY
     C***  END   ADD  ***  02/05/99  **********************************
     C*
     C***  BEGIN ADD  ***  04/17/07  **********************************
     C                   Z-ADD     ELM#EM        SCM#EM
     C***  END   ADD  ***  04/17/07  **********************************
     C*
     C***** BEG ADD ***** 10/05/04 **************************************
     C                   MOVEL     ELTYPB        SCTYPB
     C***** END ADD ***** 10/05/04 **************************************
     C*
     C                   MOVEL     ELASP1        SCASP1
     C*
     C                   Z-ADD     ELCREF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCREF
     C*
     C                   Z-ADD     ELCREX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCREX
     C*
     C                   MOVEL     *BLANKS       SCBSTY
     C                   MOVEL     ELBSCD        SCBSCD
     C***  Begin Add  ***  02/27/10  **************************************
     C                   eval      saveScBsCD = scBsCD
     C***  End   Add  ***  02/27/10  **************************************
     c*** begin add ***  03/28/08  **********************************
     c                   move      elbscd        s2bscd            3
     c*** end add *****  03/28/08  **********************************
     C*
      ***** BEG ADD ***** 03/03/05 ***********************************
     C                   MOVE      ELIFFL        SCIFFL
      ***** END ADD ***** 03/03/05 ***********************************
     C*
      ***** BEG ADD ******* 08/10/99 **********************************
      *
     C***  Begin Add  ***  02/27/10  **************************************
     C                   clear                   BSLPGM
     C***  End   Add  ***  02/27/10  **************************************
     C                   MOVEL(P)  SCBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEL(P)  BSDESC        SCBSDS
     C   60              MOVEL(P)  *BLANKS       SCBSDS
     C***  Begin Add  ***  02/27/10  **************************************
     C                   if        first
     C                   eval      saveBSLPGM = BSLPGM
     C                   endif
     C***  End   Add  ***  02/27/10  **************************************
      *
      ***** END ADD ******* 08/10/99 **********************************
      **** beg del ***** 01/12/12 *************************************
      ************
     C************       Z-ADD     ELBSDT        SRDTI
     C****BEGIN*ADD ***  11/09/01  ************************************
     C******IN40**       IFEQ      *ON
     C*****YRFLG**       ANDEQ     'N'
     C************       Z-ADD     EMEFD1        SRDTI
     C************       ENDIF
     C****END*ADD******  11/09/01  ************************************
     C************       CALL      'SRCVTDAT'    DTMDYY
     C************       MOVEL     SRDTO         SCBSDT
      **** end del ***** 01/12/12 *************************************
     C*
     C**
     C*** Beg Chg *** 07/28/10 ***********************************************
     C*****              MOVEL     ELQTYP        SCQTYP
     C*** Beg Chg *** 12/30/10 ***********************************************
     C*****              if        *in40 = *on
     C                   if        *in40 = *on and flg132 = 'Y'
     C*** End Chg *** 12/30/10 ***********************************************
     C                   MOVEL     FHF6DF        SCQTYP
     C                   else
     C                   MOVEL     ELQTYP        SCQTYP
     C                   endif
     C*** End Chg *** 07/28/10 ***********************************************
      ***** BEG ADD ******* 10/13/03 **********************************
     C                   MOVEL     ELRLFL        SCRLFL
      ***** END ADD ******* 10/13/03 **********************************
     C***  Begin Add  ***  11/22/04  **************************************
     C                   MOVE      SCRLFL        SVRLFL
     C                   MOVE      SCRLFL        S2RLFL
     C***  End   Add  ***  11/22/04  **************************************
     C*** Begin add *** 07/09/04 **************************************
     C                   MOVEL     ELADVR        SCADVR
     C*** End add ***** 07/09/04 **************************************
     C                   CLEAR                   SCQTYD
     C*
     C*** Beg Chg *** 07/28/10 ***********************************************
     C*****              MOVEL     'PTYP'        XXTBID
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C                   If        FHF6TB <> *Blanks
     C                             AND FHFH#6 <> *Blanks
     C                   Move      X'20'         QTYD
     C                   MOVEL     FHF6TB        XXTBID
     C*** End Chg *** 07/28/10 ***********************************************
     C                   MOVEL(P)  SCQTYP        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCQTYD
     C*** Beg Add *** 07/28/10 ***********************************************
     C                   ELSE
     C                   Move      X'27'         QTYD
     C                   EndIf
     C*** End Add *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   EndIf                                                  flg132 = 'Y'
     C*
     C                   if        flg132 = 'N'
     C                   Move      X'20'         QTYD
     C                   MOVEL     'PTYP'        XXTBID
     C                   MOVEL(P)  SCQTYP        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C  N60              MOVEL     GTDESC        SCQTYD
     C                   EndIf                                                  flg132 = 'N'
     C*** End Add *** 12/30/10 ***********************************************
     C**
     C                   MOVEL     ELRTCD        SCRTCD
     C     SCRTCD        IFEQ      *BLANKS
     C***  BEGIN CHG  ***  02/10/00  *********************************
     C**********           MOVELF1RTCD    SCRTCD
     C                   EXSR      GETRCD
     C                   MOVEL     RRDRAT        SCRTCD
     C***  END   CHG  ***  02/10/00  *********************************
     C                   ENDIF                                                  SCRTCD=*BLANK
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   CLEAR                   RSJRST
     C                   MOVEL(P)  SCRTCD        XXKEY
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL     RSJRST        SVRST
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   Z-ADD     ELCXDY        SCCXDY
     C                   Z-ADD     ELADDM        SCADDM
     C***  END   ADD  ***  11/10/98  **********************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   Z-ADD     ELMFEE        SCMFEE
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C                   MOVEL     ELSTAT        SCAFLG
     C*
     C                   Z-ADD     ELASP2        SCASP2
     C*** BEGIN DEL ***  07/31/98  ************************************
     C*****      SCASP2    IFEQ *ZEROS
     C*****                Z-ADDF1ADP2    SCASP2
     C*****                ENDIF                           SCASP2=*ZEROS
     C*** END DEL *****  07/31/98  ************************************
     C*
     C                   Z-ADD     ELDVEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDVEF
     C*
     C                   Z-ADD     ELDVEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDVEX
     C*
     C                   MOVEL     ELDEV2        SCDEV2
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   MOVEL     ELSTFC        SCSTFC
     C*** END   ADD ****** 01/14/10 ************************************
     C*
     C                   Z-ADD     ELD2EF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCD2EF
     C*
     C                   Z-ADD     ELD2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCD2EX
     C*
     C                   MOVEL     ELEXM1        SCEXM1
     C*** BEG   ADD ****** 06/21/10 ************************************
     C                   Z-ADD     ELD1S2        SCD1S2
     C                   Z-ADD     ELD1D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCD1D2
     C*
     C                   Z-ADD     ELD1X2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCD1X2
     C*
     C                   MOVEL     ELD2S2        SCD2S2
     C*
     C                   Z-ADD     ELD2D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCD2D2
     C*
     C                   Z-ADD     ELD2X2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCD2X2
     C*** BEG   END ****** 06/21/10 ************************************
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEMD1
     C*
     C                   MOVEL     ELPROM        SCPROM
     C                   MOVEL     ELEXM2        SCEXM2
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEMD2
     C*
     C                   MOVEL     ELEXM3        SCEXM3
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEMD3
     C*
     C*
     C*
      *** BEG ADD ***** 12/18/05 **************************************
     C     CLTID         IFEQ      '136'
     C                   MOVE      *ON           *IN68
     C     *IN09         IFEQ      *ON
     C                   Z-ADD     ELCMUL        SCCMUL
     C                   ELSE
     C                   Z-ADD     *ZERO         XXFCTR            5 4
     C                   Z-ADD     DBRRN         ##RRN             9 0
     C                   CALL      'WA060R'
     C                   PARM                    ##RRN
     C     XXFCTR        PARM                    ##FCTR            5 4
     C     XXFCTR        IFNE      *ZERO
     C                   Z-ADD(H)  XXFCTR        SCCMUL
     C                   ELSE
     C                   Z-ADD     1             SCCMUL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *** END ADD ***** 12/18/05 **************************************
     C*
     C                   MOVEL     ELDBCR        SCDBCR
     C*
     C                   Z-ADD     ELSDEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSDEF
     C*
     C                   Z-ADD     ELSDEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSDEX
     C*
     C                   Z-ADD     ELASP3        SCASP3
     C                   MOVEL     ELDISC        SCDISC
     C                   MOVEL     ELMCCD        SCMCCD
     C*** BEGIN ADD ***  07/14/98  ************************************
     C                   Z-ADD     ELDISP        SCDISP
     C*** END ADD *****  07/14/98  ************************************
     C*
     C                   MOVEL(P)  'MCO '        XXTBID
     C                   MOVEL(P)  SCMCCD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C                   MOVEL(P)  GTDESC        SCMCOD
      ***** BEG ADD ******* 11/30/99 **********************************
      *
      * MCO group code and description
      *
     C                   MOVEL(P)  ELMCGP        SCMCO
      *
     C     MGKY          CHAIN     PMG01                              60
     C  N60              MOVEL(P)  MGNAME        SCMCDS
     C   60              MOVEL(P)  *BLANKS       SCMCDS
      *
      ***** END ADD ******* 11/30/99 **********************************
     C*
     C                   MOVEL     ELCTST        SCCTST
      *
      ***** BEGIN  ADD  *** 07/06/06 **********************************
      *
     C                   Z-ADD     ELARCA        SCARCA
     C                   Z-ADD     ELARCP        SCARCP
      *
     C                   Z-ADD     ELAREF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAREF
     C*
     C                   Z-ADD     ELAREX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCAREX
      *
      ***** END ADD ******* 07/06/06 **********************************
      *
     C                   MOVEL     ELEXMF        SCEXMF                         ADD 12/12/01
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     RRSFPC        SCSFPC
     C                   Z-ADD     RRSFAM        SCSFMN
     C*
     C                   ELSE
     C*
     C                   Z-ADD     ELSFPC        SCSFPC
     C                   Z-ADD     ELSFMN        SCSFMN
     C*
     C                   ENDIF                                                  *IN40=*ON
     C*
     C** *****             MOVELELDPPC    SCDPPC
     C** *****             MOVELELDPSN    SCDPSN
     C                   MOVEL     ELMNPR        SCMNPR
     C** *****             MOVELELMNSN    SCMNSN
     C                   MOVEL     ELMXPR        SCMXPR
     C** *****             MOVELELMXSN    SCMXSN
     C** *****             MOVELELLCVR    SCLCVR
     C*
     C                   Z-ADD     ELSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   MOVEL     ELSTAM        SCSTAM
     C                   MOVEL     ELRETN        SCRETN
     C                   MOVEL     ELHZGR        SCHZGR
     C                   MOVEL     ELTAX         SCTAX
     C                   MOVEL     ELBSFC        SCBSFC
     C                   MOVEL     ELBSPR        SCBSPR
     C                   MOVEL     ELRIFC        SCRIFC
     C                   MOVEL     ELRIPR        SCRIPR
     C*
     C**** BEGIN ADD **** 03/06/98 *********************************************
     C                   MOVE      *OFF          *IN70
     C                   Z-ADD     0             SCCXDT
HS01 C************       Z-ADD     1             I1
     C                   Z-ADD     1             I1                3 0
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C*
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN70
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C**** END   ADD **** 03/06/98 *********************************************
     C                   EXSR      DSPPRE
     C*
     C**** BEGIN ADD **** 09/03/98 ************************************
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'N'           SCAFLG
     C                   ENDIF                                                  @@MODE = A
     C**** END   ADD **** 09/03/98 ************************************
     C**** BEGIN ADD **** 02/04/99 ************************************
     C                   CLEAR                   SCPP3C
     C                   CLEAR                   SCPPC3
     C**** END   ADD **** 02/04/99 ************************************
     C**** BEGIN ADD **** 10/30/98 ************************************
     C*** beg chg ***** 08/22/11 **************************************
     C*****ELPP3C        IFNE      *BLANK
     C***********        MOVE      ELPP3C        SCPP3C
     C*****BEGIN*ADD **** 01/12/98 ************************************
     C***********        MOVE      ELPPC3        SCPPC3
     C*****END***ADD **** 01/12/98 ************************************
     C***********        ELSE
     C****begin chg ****** 12/31/10 ************************************
     C*****$2PP3C**      IFNE      *BLANK
     C*************      MOVE      F2PP3C        SCPP3C
     C**** BEGIN ADD **** 01/12/98 ************************************
     C*************      MOVE      F2PPC3        SCPPC3
     C**** END   ADD **** 01/12/98 ************************************
     C*************      ENDIF                                                  F2PP3C
     C*** BEGIN ADD ***** 03/01/11 ************************************
     C***********        IF        xxflg3c = *ON
     C*****F2PP3C        IFNE      *BLANK
     C***********        MOVE      F2PP3C        SCPP3C
     C***********        MOVE      F2PPC3        SCPPC3
     C***********        ENDIF                                                  F2PP3C
     C***********        ENDIF
      ***********
     C***********        IF        xxflg3c = *OFF
     C****END***ADD ***** 03/01/11 ************************************
     C***********        if        f4txt1 <> *blanks
     C***********        move      f4txt1        scpp3c
     C***********        move      f4txt2        scppc3
     C***********        endif                                                  f4txt1
     C****end***chg ****** 12/31/10 ************************************
     C***********        ENDIF                                                  ELPP3C
     C****BEGIN*ADD ***** 03/01/11 ************************************
     C***********        ENDIF                                                  f4txt1
     C*** END   ADD ***** 03/01/11 ************************************
      *
      * -Store Overwrite field from d/b for later comparison in case value
      *  is changed.
      * -Load 3c field in Change and Display mode.
      *
     c                   eval      svffla = ELFFLA
     c                   eval      scffla = ELFFLA
     c                   exsr      load3c
     C*** end chg ***** 08/22/11 **************************************
     C**** END   ADD **** 10/30/98 ************************************
     C***** BEGIN ADD **** 09/29/99 ***********************************
     C                   MOVE      ELPFIN        SCFCOD
     C                   Z-ADD     ELPFDT        KKPFDT
     C                   MOVE      *BLANKS       SCPFNM
     C*
     C     PFKY2         SETGT     PPF01
     C*
     C     PFKY          READPE    PPF01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PFNAME        SCPFNM
     C                   ENDIF                                                  *IN60=OFF
     C*
     C***** END   ADD **** 09/29/99 ***********************************
     C***** BEG ADD ****** 01/27/03 ***********************************
     C                   Z-ADD     ELARP1        SCARP1
     C                   Z-ADD     ELARP2        SCARP2
     C                   Z-ADD     ELARP3        SCARP3
     C***** END ADD ****** 01/27/03 ***********************************
     C***** BEG ADD ****** 09/01/04 ***********************************
     C                   Z-ADD     ELVLCA        SCVLCA
     C***** END ADD ****** 09/01/04 ***********************************
     C***  BEG   DEL  ***  01/09/04  *********************************
     C***  BEG   ADD  ***  02/18/03  *********************************
      *** DISPLAY VENDOR FIELD IF VENDOR FLAG IS ON
     C**********           SETOF                       68
     C********** VNDRFL    IFEQ *ON
     C**********           SETON                       68
     C**********           Z-ADDELVDR#    SCVDR#
     C**********
     C********** VPKY      CHAINPVP02                60
     C**N60*****           MOVELVPNAME    SCVDRN
     C***60*****           MOVEL*BLANKS   SCVDRN
     C**********
     C**********           ENDIF
     C***  END   ADD  ***  02/18/03  *********************************
     C***  END   DEL  ***  01/09/04  *********************************
     C****** BEG ADD ***** 07/07/05 **************************************
     C*
     C                   MOVEL     ELASCD        SCASCD
     C*
     C                   CLEAR                   AVDESC
     C                   MOVEL     ELASCD        XXKEY
     C     TBKY          CHAIN     PAV01                              60
     C                   MOVEL(P)  AVDESC        SCACTD
     C****** END ADD ***** 07/07/05 **************************************
     C*** BEGIN ADD ***  07/11/06  *******************************************
     C                   MOVE      ELFFL1        SCFFL1
     C*** END ADD *****  07/11/06  *******************************************
      ***** BEG ADD ******* 03/06/07 **********************************
      *
     C   40XX1ST         IFEQ      'N'
     C                   MOVE      SVAJFL        SCFFL1
     C                   MOVE      'Y'           XX1ST             1
     C                   ENDIF
      *
      ***** END ADD ******* 03/06/07 **********************************
     C*
     C*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   MOVE      ELFFL9        SCPRFT
     C*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C***  Begin Add  ***  10/02/06  **************************************
     C***  Begin Chg  ***  07/20/07  **************************************
     C***40*****           MOVELF1APFL    ELAAFL
      *** beg del ***** 01/12/12 ******************************************
     C***40******        MOVEL     SVAAFL        ELAAFL
     C*****End***Chg  ***  07/20/07  **************************************
     C*****Begin*Add  ***  09/08/07  **************************************
     C***40******        MOVEL     SVAPFL        ELAPFL
     C***********        MOVEL     ELAPFL        SCAPFL
     C*****End***Add  ***  09/08/07  **************************************
     C***********        MOVEL     ELAAFL        SCAAFL
      *** end del ***** 01/12/12 ******************************************
     C***  End   Add  ***  10/02/06  **************************************
     C***  BEG   Add  ***  07/16/07  **************************************
     C***  Begin Add  ***  12/03/08  **************************************
     C                   select
     C                   when      clt102
     C                   MOVE      ELFFL2        SCLATE
     C                   other
     C***  End   Add  ***  12/03/08  **************************************
     C                   MOVE      ELFFL2        SCFFL2
     C***  Begin Add  ***  12/03/08  **************************************
     C                   endsl
     C***  End   Add  ***  12/03/08  **************************************
     C***  END   Add  ***  07/16/07  **************************************
      *
     C***** BEG ADD ******* 07/27/07 **********************************
      *
     C     XXFLG         IFEQ      '0'
     C                   MOVEL     ELPFDT        S@PFDT
     C                   MOVEL     ELPTDT        S@PTDT
     C                   MOVEL     ELARSM        S@ARSM
     C                   MOVEL     ELIFFL        S@IFFL
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     ELQTYP        S@QTYP
     C*************        MOVELELMBCD    S@MBCD
     C***** END CHG ******* 10/08/07 **********************************
     C                   MOVEL     ELADVR        S@ADVR
     C                   MOVEL     ELRLFL        S@RLFL
     C                   MOVEL     ELBSDT        S@BSDT
     C                   MOVEL     ELSTAT        S@STAT
     C                   MOVEL     ELILCD        S@ILCD
     C                   MOVEL     ELILEF        S@ILEF
     C                   MOVEL     ELILEX        S@ILEX
     C                   MOVEL     ELDCDE        S@DCDE
     C                   MOVEL     ELDCEF        S@DCEF
     C                   MOVEL     ELDCEX        S@DCEX
     C                   MOVEL     ELCXBL        S@CXBL
     C                   MOVEL     ELASP2        S@ASP2
     C                   MOVEL     ELDVEF        S@DVEF
     C                   MOVEL     ELDVEX        S@DVEX
     C                   MOVEL     ELASP1        S@ASP1
     C                   MOVEL     ELCREF        S@CREF
     C                   MOVEL     ELCREX        S@CREX
     C                   MOVEL     ELASPM        S@ASPM
     C                   MOVEL     ELMCEF        S@MCEF
     C                   MOVEL     ELMCEX        S@MCEX
     C                   MOVEL     ELMCCD        S@MCCD
     C                   MOVEL     ELMCGP        S@MCGP
     C                   MOVEL     ELDEV2        S@DEV2
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   MOVEL     ELSTFC        S@STFC
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     ELD2EF        S@D2EF
     C                   MOVEL     ELD2EX        S@D2EX
     C                   MOVEL     ELEXM1        S@EXM1
     C                   MOVEL     ELEMD1        S@EMD1
     C***** BEG ADD ******* 10/18/07 **********************************
     C                   MOVEL     ELEXM2        S@EXM2
     C                   MOVEL     ELEMD2        S@EMD2
     C                   MOVEL     ELEXM3        S@EXM3
     C                   MOVEL     ELEMD3        S@EMD3
     C***** END ADD ******* 10/18/07 **********************************
     C                   MOVEL     ELASPS        S@ASPS
     C                   MOVEL     ELSCEF        S@SCEF
     C                   MOVEL     ELSCEX        S@SCEX
     C                   MOVEL     ELSRPR        S@SRPR
     C                   MOVEL     ELASP3        S@ASP3
     C                   MOVEL     ELDISC        S@DISC
     C                   MOVEL     ELPROM        S@PROM
     C                   MOVEL     ELDBCR        S@DBCR
     C                   MOVEL     ELSDEF        S@SDEF
     C                   MOVEL     ELSDEX        S@SDEX
     C                   MOVEL     ELCXDY        S@CXDY
     C                   MOVEL     ELPP3C        S@PP3C
     C                   MOVEL     ELPPC3        S@PPC3
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     ELMFEE        S@MFEE
     C****************     MOVELELMFAM    S@MFAM
     C***** END CHG ******* 10/08/07 **********************************
     C                   MOVEL     ELSFPC        S@SFPC
     C                   MOVEL     ELSFAM        S@SFAM
     C                   MOVEL     ELSFMN        S@SFMN
     C                   MOVEL     ELADDM        S@ADDM
     C                   MOVEL     ELANDT        S@ANDT
     C                   MOVEL     ELDCD2        S@DCD2
     C                   MOVEL     EL2DEF        S@2DEF
     C                   MOVEL     EL2DEX        S@2DEX
     C                   MOVEL     ELINL2        S@INL2
     C                   MOVEL     ELI2EF        S@I2EF
     C                   MOVEL     ELI2EX        S@I2EX
     C                   MOVEL     ELEXMF        S@EXMF
     C                   MOVEL     ELWVTP        S@WVTP
     C                   MOVEL     ELWVFL        S@WVFL
     C                   MOVEL     ELWVPC        S@WVPC
     C                   MOVEL     ELWMIN        S@WMIN
     C                   MOVEL     ELFCHG        S@FCHG
     C                   MOVEL     ELWMAX        S@WMAX
     C***** BEG CHG ******* 10/18/07 **********************************
     C                   MOVEL     ELASCD        S@ASTS
     C************         MOVELELASTS    S@ASTS
     C***** BEG CHG ******* 10/18/07 **********************************
     C                   MOVEL     ELCOFL        S@COFL
     C                   MOVEL     ELTYPB        S@TYPB
     C                   MOVEL     ELPFIN        S@PFIN
     C                   MOVEL     ELCTST        S@CTST
     C                   MOVEL     *ON           XXFLG
     C                   ENDIF
     C***** END ADD ******* 07/27/07 ************************************
     C***** BEG ADD ***** 10/11/07 ***********************************
     C*
     C  N40              MOVE      ELBLFL        SCBLFL
     C*
     C* Assign a default value for the new flag from F1BLFL in add mode.
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C*  POLICY YEAR MUST BE VALID FOR FUND (IF IN ADD MODE)
     C*
B02  C     F1KY1         CHAIN     PF101                              60
     C*
     C   40
     CANN60              MOVE      F1BLFL        SCBLFL
     C***** END ADD ***** 10/11/07 ***********************************
      ******* beg add ****** 04/29/08 *********************************
     C                   if        CLTID = '071'
     C                   eval      *in76 = *on
     C                   else
     C                   eval      *in76 = *off
     c                   endif

     c                   eval      scexfr = elexfr
      ******* end add ****** 04/29/08 *********************************
      ******* BEG ADD ****** 06/06/09 *********************************
     C*
     C*** Beg Del *** 07/28/10 ***********************************************
     C*****              if        EXEHD1  = *BLANKS
     C*****              EVAL      *IN45 = *ON
     C*****              ENDIF
     C*****
     C*****              if        EXEHD2  = *BLANKS
     C*****              EVAL      *IN46 = *ON
     C*****              ENDIF
     C*** END Del *** 07/28/10 ***********************************************
     C*
     C*
     C*** Beg Add *** 11/16/09 ****************************************
     C                   if        *in40 = *on
     C*** Beg Chg *** 07/28/10 ***********************************************
     C*****              MOVEL     EXFDF1        SCFFD1
     C*****              MOVEL     EXFDF2        SCFFD2
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'N'
     C                   MOVEL     EXFDF1        SCFFD1
     C                   MOVEL     EXFDF2        SCFFD2
     C                   endif
     C*
     C                   if        flg132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C                   MOVEL     FHF1DF        SCFFD1
     C                   MOVEL     FHF2DF        SCFFD2
     C                   MOVEL     FHF3DF        SCFFD3
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   endif
     C*** End Add *** 12/30/10 ***********************************************
     C*** End Chg *** 07/28/10 ***********************************************
     C                   else
     C*** End Add *** 11/16/09 ****************************************
     C                   MOVEL     ELFFD1        SCFFD1
     C                   MOVEL     ELFFD2        SCFFD2
     C*** Beg Add *** 07/28/10 ***********************************************
     C                   MOVEL     ELFFD3        SCFFD3
     C                   MOVEL     ELFFD4        SCFFD4
     C                   MOVE      ELFFD5        SCFFD5
     C*** End Add *** 07/28/10 ***********************************************
     C*** Beg Add *** 11/16/09 ****************************************
     C                   endif
     C*** End Add *** 11/16/09 ****************************************
     C*** Beg Del *** 07/28/10 ***********************************************
     C*****              MOVEL     EXEHD1        SCEHD1
     C*****              MOVEL     EXEHD2        SCEHD2
     C*** End Del *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'N'
     C*Field#1 column 3
     C                   If        extbd1 <> *Blanks
     C                   Move      X'20'         AT10S
     C                   Movel     extbd1        XXFTB
     C                   Movel     SCFFD1        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS1
     C                   Else
     C                   Move      X'27'         AT10S
     C                   EndIf
     C*Field#2 column 3
     C                   If        EXTBD2 <> *Blanks
     C                   Move      X'20'         AT11S
     C                   Movel     EXTBD2        XXFTB
     C                   Movel     SCFFD2        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS2
     C                   Else
     C                   Move      X'27'         AT11S
     C                   EndIf
     C*
     C                   EndIf                                                  FLG132= 'N'
     C*** End Add *** 12/30/10 ***********************************************
     C*** Beg Add *** 07/28/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C*Field#1 column 3
     C                   If        FHF1TB <> *Blanks
     C                   Move      X'20'         AT10S
     C                   Movel     FHF1TB        XXFTB
     C                   Movel     SCFFD1        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS1
     C                   Else
     C                   Move      X'27'         AT10S
     C                   EndIf
     C*Field#2 column 3
     C                   If        FHF2TB <> *Blanks
     C                   Move      X'20'         AT11S
     C                   Movel     FHF2TB        XXFTB
     C                   Movel     SCFFD2        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS2
     C                   Else
     C                   Move      X'27'         AT11S
     C                   EndIf
     C*Field#3 column 3
     C                   If        FHF3TB <> *Blanks
     C                   Move      X'20'         AT12S
     C                   Movel     FHF3TB        XXFTB
     C                   Movel     SCFFD3        XXFFD
     C     TBKEY         Chain     PGT01                              60
     C  N60              Movel     GTDESC        SCFDS3
     C                   Else
     C                   Move      X'27'         AT12S
     C                   EndIf
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   endif                                                  FLG132= 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C*** End Add *** 07/28/10 ***********************************************
      ******* END ADD ****** 06/06/09 *********************************
     C*** BEGIN ADD ***  01/04/10  ************************************
     C*** BEGIN DEL ***  03/01/10  ************************************
     C*************      MOVEL     ELMBCD        SCMBCD
     C*** END DEL *****  03/01/10  ************************************
     C*** END   ADD ***  01/04/10  ************************************
     C*** BEG ADD ****** 01/25/10 *************************************
     C***  Begin Del  ***  02/11/11  **************************************
     C**********         MOVEL     'WC'          XSKEY
     C*****XSKEY         CHAIN     SWC01                              60
      **********
     C******IN60         IFEQ      *OFF
     C*****WCFL44        IFEQ      'Y'
     C**********         MOVEL     *ON           *IN81
     C**********         ELSE
     C**********         MOVEL     *OFF          *IN81
     C**********         ENDIF
     C**********         ENDIF
     C***  End   Del  ***  02/11/11  **************************************
      *** beg chg ***** 04/01/10 **************************************
     C***********        MOVEL     ELCGCF        SCCGCF
     C***********        MOVEL     ELSTCO        SCSTCO
     C***********        MOVEL     ELOGDF        SCOGDF
     C                   MOVEL     ELSTCO        SCSTCO
      *** end chg ***** 04/01/10 **************************************
     C***  Beg   Add  ***  04/21/11  **************************************
     C                   movel     elaeft        scaeft
     C***  End   Add  ***  04/21/11  **************************************
     C***  Begin Add  ***  02/11/11  **************************************
     C                   Z-ADD     ELSCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCDT
     C***  End   Add  ***  02/11/11  **************************************
     C*** END ADD ****** 01/25/10 *************************************
     C*
     C***  Begin Add  ***  02/27/10  **************************************
     C                   eval      first = *off
     C***  End   Add  ***  02/27/10  **************************************
     C*** BEGIN ADD ****** 03/01/11 ************************************
     C*** beg del ***** 08/22/11 ***************************************
     C***********        IF        XXFLG3C = *OFF
     C****End***ddd ****** 03/01/11 ************************************
     C***********        move      elffla        scffla
     C*save*value of flag in temp variable
     C***********        move      elffla        xxffla            1
     C****end***add ****** 12/31/10 ************************************
     C****BEGIN*ADD ****** 03/01/11 ************************************
     C***********        ENDIF
     C*** end   ddd ****** 03/01/11 ************************************
     C*** end del ***** 08/22/11 ***************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C/EJECT
     C*===============================================================
     C* MVSCDB - SUBROUTINE FOR MOVING SCREEN FIELDS TO DATA BASE
     C*===============================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCQTYP        ELQTYP
     C***** BEG ADD ***** 10/11/07 ***********************************
     C                   MOVE      SCBLFL        ELBLFL
     C***** END ADD ***** 10/11/07 ***********************************
      ***** BEG ADD ******* 10/13/03 **********************************
     C                   MOVEL     SCRLFL        ELRLFL
      ***** END ADD ******* 10/13/03 **********************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*** BEG CHG ***** 06/13/07 **************************************
     C***********          MOVE SCARSM    ELARSM
     C     SVARSM        IFEQ      'Y'
     C     SCARSM        ANDEQ     *BLANK
     C     SVARSM        OREQ      '@'
     C     SCARSM        ANDEQ     *BLANK
     C                   MOVE      '@'           ELARSM
     C                   ELSE
     C                   MOVE      SCARSM        ELARSM
     C                   ENDIF
     C*** END CHG ***** 06/13/07 **************************************
     C*** END ADD *****  11/15/99  ************************************
     C*** Begin add *** 07/09/04 **************************************
     C                   MOVEL     SCADVR        ELADVR
     C*** End add ***** 07/09/04 **************************************
     C*
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELPFDT
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELPFDT        X@PFDT            7 0
     C***** END ADD ******* 07/27/07 ************************************
     C*** BEGIN ADD ***  09/16/99  ************************************
     C                   MOVE      SCSRPR        ELSRPR
     C*** END ADD *****  09/16/99  ************************************
     C*** BEGIN ADD ***  04/17/00  ************************************
     C                   MOVE      SCCOFL        ELCOFL
     C*** END ADD *****  04/17/00  ************************************
     C*
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELPTDT
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELPTDT        X@PTDT            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELANDT
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELANDT        X@ANDT            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C*
      ****  BEG ADD 03/06/06 ******************************************
      *
      * Sets the 'Renewal to Quote' flag to be the same as the one in
      * the State Reporting Division
      *
     C                   MOVEL     XXRQFL        ELRQFL
      ****  END ADD 03/06/06 ******************************************
      *
     C                   MOVEL     SCILCD        ELILCD
     C*
     C     SCILCD        IFNE      *BLANKS
     C     SCILEF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCILEF
     C                   ENDIF                                                  SCILCD<>*BLANKS
     C*
     C                   Z-ADD     SCILEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELILEF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELILEF        X@ILEF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCILEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELILEX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELILEX        X@ILEX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   MOVEL     SCINL2        ELINL2
     C*
     C     SCINL2        IFNE      *BLANKS
     C     SCI2EF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCI2EF
     C                   ENDIF                                                  SCINL2<>*BLANKS
     C*
     C                   Z-ADD     SCI2EF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELI2EF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELI2EF        X@I2EF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCI2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELI2EX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELI2EX        X@I2EX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   MOVEL     SCDCDE        ELDCDE
     C*
     C     SCDCDE        IFNE      *BLANKS
     C     SCDCEF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCDCEF
     C                   ENDIF                                                  SCDCDE<>*BLANKS
     C*
     C                   Z-ADD     SCDCEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDCEF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELDCEF        X@DCEF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCDCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDCEX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELDCEX        X@DCEX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C***  BEG   ADD  ***  09/22/09 *************************************
     C* GET RATING DATE BASED ON ARD LOGIC
     C                   MOVE      ELDCEF        Q#DATE
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
     C                   PARM                    Q#DATE            7
     C
     C                   MOVE      Q#DATE        XXDDT
     C*
     C* READ WTDDL BASED ON ARD LOGIC
     C     DDKY3         SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C                   DOW       *IN60 = *OFF
     C                   IF        DDSTAT = 'A'
     C                   LEAVE
     C                   ELSE
     C     DDKY1         READPE    PDD01                                  60
     C                   ENDIF
     C                   ENDDO
     C*
     C* POPULATE DEDUCTIBLE INFO FIELDS..
     C                   IF        SCDCDE <> SVDCDE
     C                             AND @@MODE = 'C'
     C                   EVAL      ELDEDP = DDTYPE
     C                   EVAL      ELAMPO = DDAMPO
     C                   EVAL      ELAMAG = DDAMAG
     C                   ENDIF
     C                   IF        @@MODE = 'A'
     C                   EVAL      ELDEDP = DDTYPE
     C                   EVAL      ELAMPO = DDAMPO
     C                   EVAL      ELAMAG = DDAMAG
     C                   ENDIF
     C                   IF        SCDCDE = *BLANKS
     C                   CLEAR                   ELDEDP
     C                   CLEAR                   ELAMPO
     C                   CLEAR                   ELAMAG
     C                   ENDIF
     C***  END   ADD  ***  09/22/09 *************************************
     C*
     C***  BEGIN CHG  ***  04/07/05-2 *********************************
     C                   MOVEL     SCDCD2        ELDCD2
     C*
     C                   Z-ADD     SC2DEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EL2DEF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     EL2DEF        X@2DEF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SC2DEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EL2DEX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     EL2DEX        X@2DEX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C***  END   CHG  ***  04/07/05-2 *********************************
     C*
     C***  BEG   ADD  ***  09/22/09 *************************************
     C* GET RATING DATE BASED ON ARD LOGIC
     C                   MOVE      EL2DEF        Q#DATE
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
     C                   PARM                    Q#DATE            7
     C
     C                   MOVE      Q#DATE        XXDDT
     C*
     C* READ WTDDL BASED ON ARD LOGIC
     C     DDKY3         SETGT     PDD01
     C     DDKY1         READPE    PDD01                                  60
     C                   DOW       *IN60 = *OFF
     C                   IF        DDSTAT = 'A'
     C                   LEAVE
     C                   ELSE
     C     DDKY1         READPE    PDD01                                  60
     C                   ENDIF
     C                   ENDDO
     C*
     C* POPULATE DEDUCTIBLE INFO FIELDS..
     C                   IF        SCDCD2 <> SVDCD2
     C                             AND @@MODE = 'C'
     C                   EVAL      EL2DDP = DDTYPE
     C                   EVAL      EL2APO = DDAMPO
     C                   EVAL      EL2AAG = DDAMAG
     C                   ENDIF
     C                   IF        @@MODE = 'A'
     C                   EVAL      EL2DDP = DDTYPE
     C                   EVAL      EL2APO = DDAMPO
     C                   EVAL      EL2AAG = DDAMAG
     C                   ENDIF
     C                   IF        SCDCD2 = *BLANKS
     C                   CLEAR                   EL2DDP
     C                   CLEAR                   EL2APO
     C                   CLEAR                   EL2AAG
     C                   ENDIF
     C***  END   ADD  ***  09/22/09 *************************************
     C                   MOVEL     SCCXBL        ELCXBL
     C                   MOVEL     SCASPM        ELASPM
     C*
     C     SCASPM        IFNE      *ZEROS
     C     SCMCEF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCMCEF
     C                   ENDIF                                                  SCASPM<>*ZEROS=SCMCE
     C*
     C                   Z-ADD     SCMCEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELMCEF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELMCEF        X@MCEF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCMCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELMCEX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELMCEX        X@MCEX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   MOVEL     SCASPS        ELASPS
     C*
     C     SCASPS        IFNE      *ZEROS
     C     SCSCEF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCSCEF
     C                   ENDIF                                                  SCASPS<>*ZEROS=SCSCE
     C*
     C                   Z-ADD     SCSCEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELSCEF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELSCEF        X@SCEF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCSCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELSCEX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELSCEX        X@SCEX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C                   Z-ADD     SCEMPY        ELEMPY
     C***  END   ADD  ***  02/05/99  **********************************
     C*
     C***  BEGIN ADD  ***  04/17/07  **********************************
     C                   Z-ADD     SCM#EM        ELM#EM
     C***  END   ADD  ***  04/17/07  **********************************
     C*
     C***** BEG ADD ***** 10/05/04 **************************************
     C                   MOVEL     SCTYPB        ELTYPB
     C***** END ADD ***** 10/05/04 **************************************
     C*
     C                   MOVEL     SCASP1        ELASP1
     C*
     C     SCASP1        IFNE      *ZEROS
     C     SCCREF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCCREF
     C                   ENDIF                                                  SCASP1<>*ZEROS=SCCRE
     C*
     C                   Z-ADD     SCCREF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELCREF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELCREF        X@CREF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCCREX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELCREX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELCREX        X@CREX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   MOVEL     SCBSCD        ELBSCD
     C*
     C                   Z-ADD     SCBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELBSDT
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELBSDT        X@BSDT            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   MOVEL     SCRTCD        ELRTCD
     C*
      ***** BEG ADD ***** 03/03/05 ***********************************
     C                   MOVE      SCIFFL        ELIFFL
      ***** END ADD ***** 03/03/05 ***********************************
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C                   CLEAR                   RSJRST
     C                   MOVEL(P)  SCRTCD        XXKEY
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL     RSJRST        XXRST
     C***  END   ADD  ***  07/19/99  *********************************
     C*
     C*********            MOVELSCSTAT    ELAFLG
     C                   MOVEL     SCASP2        ELASP2
     C*
     C***  BEGIN ADD  ***  11/10/98  **********************************
     C                   Z-ADD     SCCXDY        ELCXDY
     C                   Z-ADD     SCADDM        ELADDM
     C***  END   ADD  ***  11/10/98  **********************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   Z-ADD     SCMFEE        ELMFEE
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C     SCASP2        IFNE      *ZEROS
     C     SCDVEF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCDVEF
     C                   ENDIF                                                  SCASP2<>*ZEROS=SCDVE
     C*
     C                   Z-ADD     SCDVEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDVEF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELDVEF        X@DVEF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCDVEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDVEX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELDVEX        X@DVEX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*** BEG   ADD ****** 06/21/10 ************************************
     C                   MOVEL     SCD1S2        ELD1S2
     C                   MOVEL     SCD2S2        ELD2S2
      *
     C                   Z-ADD     SCD1D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELD1D2
     C                   MOVEL     ELD1D2        X@D1D2            7 0
      *
     C                   Z-ADD     SCD1X2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELD1X2
     C                   MOVEL     ELD1X2        X@D1X2            7 0
     C*** END   ADD ****** 06/21/10 ************************************
     C                   MOVEL     SCDEV2        ELDEV2
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   MOVEL     SCSTFC        ELSTFC
     C*** END   ADD ****** 01/14/10 ************************************
     C*
     C     SCDEV2        IFNE      *ZEROS
     C     SCD2EF        ANDEQ     *ZEROS
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         ANDNE     '025'
     C     SCSTFC        ORNE      *ZEROS
     C     SCD2EF        ANDEQ     *ZEROS
     C     CLTID         ANDEQ     '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   Z-ADD     SCPFDT        SCD2EF
     C                   ENDIF                                                  SCDEV2<>*ZEROS=SCD2E
     C*
     C                   Z-ADD     SCD2EF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELD2EF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELD2EF        X@D2EF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCD2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELD2EX
     C***** BEG ADD ******* 06/21/10 **********************************
     C                   Z-ADD     SCD2D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELD2D2
     C                   MOVEL     ELD2D2        X@D2D2            7 0
     C*
     C                   Z-ADD     SCD2X2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELD2X2
     C*
     C                   MOVEL     ELD2X2        X@D2X2            7 0
     C                   MOVEL     ELD2EX        X@D2EX            7 0
     C*** END   ADD ****** 06/21/10 ************************************
     C                   MOVEL     SCEXM1        ELEXM1
     C*
     C     SCEXM1        IFNE      *ZEROS
     C     SCEMD1        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCEMD1
     C                   ENDIF                                                  SCEXM1<>*ZEROS=SCEMD
     C*
     C                   Z-ADD     SCEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELEMD1
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELEMD1        X@EMD1            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   MOVEL     SCPROM        ELPROM
     C                   MOVEL     SCEXM2        ELEXM2
     C*
     C     SCEXM2        IFNE      *ZEROS
     C     SCEMD2        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCEMD2
     C                   ENDIF                                                  SCEXM2<>*ZEROS=SCEMD
     C*
     C                   Z-ADD     SCEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELEMD2
     C***** BEG ADD ******* 10/18/07 **********************************
     C                   MOVEL     ELEMD2        X@EMD2            7 0
     C***** END ADD ******* 10/18/07 **********************************
     C*
     C                   MOVEL     SCEXM3        ELEXM3
     C*
     C     SCEXM3        IFNE      *ZEROS
     C     SCEMD3        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCEMD3
     C                   ENDIF                                                  SCEXM3<>*ZEROS=SCEMD
     C*
     C                   Z-ADD     SCEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELEMD3
     C***** BEG ADD ******* 10/18/07 **********************************
     C                   MOVEL     ELEMD3        X@EMD3            7 0
     C***** END ADD ******* 10/18/07 **********************************
     C*
      *** BEG ADD ***** 12/18/05 **************************************
     C     CLTID         IFEQ      '136'
     C                   Z-ADD     SCCMUL        ELCMUL
     C                   ENDIF
      *** END ADD ***** 12/18/05 **************************************
     C*
     C                   MOVEL     SCDBCR        ELDBCR
     C*
     C     SCDBCR        IFNE      *ZEROS
     C     SCSDEF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCSDEF
     C                   ENDIF                                                  SCDBCR<>*ZEROS=SCSDE
     C*
     C                   Z-ADD     SCSDEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELSDEF
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELSDEF        X@SDEF            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCSDEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELSDEX
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVEL     ELSDEX        X@SDEX            7 0
     C***** END ADD ******* 07/27/07 **********************************
     C*
     C                   Z-ADD     SCASP3        ELASP3
     C                   MOVEL     SCDISC        ELDISC
     C                   MOVEL     SCMCCD        ELMCCD
      ***** BEG ADD ******* 11/30/99 **********************************
     C                   MOVEL     SCMCO         ELMCGP
      ***** END ADD ******* 11/30/99 **********************************
     C                   MOVEL     SCCTST        ELCTST
     C*** BEG ADD ****** 08/30/10 ************************************
     C*** BEG CHG ****** 07/07/11 ************************************
     c*****AUKY**        CHAIN     PAU01                              99
     c***********        IF        *IN99 = *OFF  AND ELCTST <> AUCNFL
     c     AUKY          CHAIN(N)  PAU01                              99
     c                   IF        *IN99 = *OFF  AND ELCTST <> AUCNFL
     c                             AND *IN90 =*OFF
     c     AUKY          CHAIN     PAU01
     C*** END CHG ****** 07/07/11 ************************************
      *
     C                   MOVEL     ELCTST        AUCNFL
      *
     C                   IF        AUCNFL = 'Y'
     C                   Z-ADD     SYSDT         AUCNTD
     C                   ELSE
     C                   Z-ADD     0             AUCNTD
     C                   ENDIF
     C
     C                   UPDATE    PAU01
     C                   ENDIF
     C*** END ADD ****** 08/30/10 ************************************
      *
      ***** beg ADD ******* 07/06/06 **********************************
      *
     C                   Z-ADD     SCARCA        ELARCA
     C                   Z-ADD     SCARCP        ELARCP
     C*
     C     SCARCA        IFNE      *ZEROS
     C     SCAREF        ANDEQ     *ZEROS
     C     SCARCP        ORNE      *ZEROS
     C     SCAREF        ANDEQ     *ZEROS
     C                   Z-ADD     SCPFDT        SCAREF
     C                   ENDIF                                                  SCARCA IFNE
      *
     C                   Z-ADD     SCAREF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELAREF
      *
     C                   Z-ADD     SCAREX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELAREX
      *
      ***** END ADD ******* 07/06/06 **********************************
     C                   MOVEL     SCEXMF        ELEXMF                         ADD 12/12/01
     C                   Z-ADD     SCSFPC        ELSFPC
     C                   Z-ADD     SCSFMN        ELSFMN
     C** *****             MOVELSCDPPC    ELDPPC
     C** *****             MOVELSCDPSN    ELDPSN
     C                   MOVEL     SCMNPR        ELMNPR
     C** *****             MOVELSCMNSN    ELMNSN
     C                   MOVEL     SCMXPR        ELMXPR
     C** *****             MOVELSCMXSN    ELMXSN
     C** *****             MOVELSCLCVR    ELLCVR
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELSTDT
     C*
     C                   MOVEL     SCSTAM        ELSTAM
     C                   MOVEL     SCRETN        ELRETN
     C                   MOVEL     SCHZGR        ELHZGR
     C                   MOVEL     SCTAX         ELTAX
     C                   MOVEL     SCBSFC        ELBSFC
     C                   MOVEL     SCBSPR        ELBSPR
     C                   MOVEL     SCRIFC        ELRIFC
     C                   MOVEL     SCRIPR        ELRIPR
     C                   MOVEL     SCAFLG        ELSTAT
     C**** BEGIN ADD **** 10/30/98 ************************************
     C*** beg chg ***** 08/22/11 **************************************
     C***********        MOVEL     SCPP3C        ELPP3C
     C*****END***ADD **** 10/30/98 ************************************
     C*****BEGIN*ADD **** 01/12/98 ************************************
     C***********        MOVEL     SCPPC3        ELPPC3
     C*****END***ADD **** 01/12/98 ************************************
      *
      * -Clear d/b 3c fields if Overwrite flag is blank and WC1918 is
      *  installed.
      * -Populate 3c fields if Overwrite flag is not blank.
      *
     c                   eval      ELFFLA = scffla
     c                   select
     c                   when      WC1918instald and ELFFLA = *blank
     c                   eval      ELPP3C = *blanks
     c                   eval      ELPPC3 = *blanks
     c                   other
     c                   eval      ELPP3C = scpp3c
     c                   eval      ELPPC3 = scppc3
     c                   endsl
     C*** end chg ***** 08/22/11 **************************************
     C******BEGIN ADD **** 09/29/99 ***********************************
     C**** Beg Chg **** 11/25/11 **************************************
     C***********        MOVEL     SCFCOD        ELPFIN
     C                   MOVEL     SCFCOD        ELPFIN
     C**** End Chg **** 11/25/11 **************************************
     C******END** ADD **** 09/29/99 ***********************************
     C***** BEG ADD ****** 01/27/03 ***********************************
     C                   Z-ADD     SCARP1        ELARP1
     C                   Z-ADD     SCARP2        ELARP2
     C                   Z-ADD     SCARP3        ELARP3
     C***** END ADD ****** 01/27/03 ***********************************
      *** BEG DEL ***** 01/09/04 **************************************
      *** BEG ADD ***** 02/18/03 **************************************
     C*****************    Z-ADDSCVDR#    ELVDR#
     C*** END ADD ***** 02/18/03 *************************************
      *** END DEL ***** 01/09/04 **************************************
     C***** BEG ADD ****** 09/01/04 ***********************************
     C                   Z-ADD     SCVLCA        ELVLCA
     C***** END ADD ****** 09/01/04 ***********************************
     C****** BEG ADD ***** 07/07/05 **************************************
     C                   MOVEL     SCASCD        ELASCD
     C****** END ADD ***** 07/07/05 **************************************
     C*** BEGIN ADD ***  07/11/06  *******************************************
     C                   MOVE      SCFFL1        ELFFL1
     C*** END ADD *****  07/11/06  *******************************************
     C*                                                                ***
     C*+*+ BEGIN ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   MOVE      SCPRFT        ELFFL9
     C*+*+ END   ADD *+*+ 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C***  Begin Add  ***  10/02/06  **************************************
     C                   MOVEL     SCAAFL        ELAAFL
     C***  End   Add  ***  10/02/06  **************************************
     C***  Begin Add  ***  09/08/07  **************************************
     C                   MOVEL     SCAPFL        ELAPFL
     C***  End   Add  ***  09/08/07  **************************************
     C***  BEG   Add  ***  07/16/07  **************************************
     C***  Begin Add  ***  12/03/08  **************************************
     C                   select
     C                   when      clt102
     C                   MOVE      SCLATE        ELFFL2
     C                   other
     C***  End   Add  ***  12/03/08  **************************************
     C                   MOVE      SCFFL2        ELFFL2
     C***  Begin Add  ***  12/03/08  **************************************
     C                   endsl
     C***  End   Add  ***  12/03/08  **************************************
     C***  END   Add  ***  07/16/07  **************************************
      ******* beg add ****** 04/29/08 *********************************
     C                   MOVE      SCEXFR        ELEXFR
      ******* end add ****** 04/29/08 *********************************
      *** beg add ***** 01/14/09 **************************************
     c                   if        @@mode = 'A'
     c                   call      'WA069R'
     C                   parm      ELCO#         #1co#             3 0
     C                   parm      ELFND         #1fnd             3 0
     C                   parm      ELEMP#        #1emp#            9 0
     C                   parm      ELFYR         #1fyr             3 0
     C     ELNWRW        parm      *blanks       #1nwrw            1
     c                   endif
      *** end add ***** 01/14/09 **************************************
      ******* BEG ADD ****** 06/06/09 *********************************
     C                   MOVEL     SCFFD1        ELFFD1
     C                   MOVEL     SCFFD2        ELFFD2
     C*** Beg Add *** 07/28/10 ***********************************************
     C                   MOVEL     SCFFD3        ELFFD3
     C                   MOVEL     SCFFD4        ELFFD4
     C                   MOVE      SCFFD5        ELFFD5
     C*** End Add *** 07/28/10 ***********************************************
      ******* END ADD ****** 06/06/09 *********************************
     C*** BEGIN ADD ***  01/04/10  ************************************
     C*** BEGIN DEL ***  03/01/10  ************************************
     C*************      MOVEL     SCMBCD        ELMBCD
     C*** END DEL *****  03/01/10  ************************************
     C*** END   ADD ***  01/04/10  ************************************
     C*** BEG ADD ****** 01/25/10 *************************************
     C*** beg chg ****** 04/01/10 *************************************
     C***********        MOVEL     SCCGCF        ELCGCF
     C***********        MOVEL     SCSTCO        ELSTCO
     C***********        MOVEL     SCOGDF        ELOGDF
     C                   MOVEL     SCSTCO        ELSTCO
     C*** end chg ****** 04/01/10 *************************************
     C***  Beg   Add  ***  04/21/11  **************************************
     C                   movel     scaeft        elaeft
     C***  End   Add  ***  04/21/11  **************************************
     C***  Begin Add  ***  02/11/11  **************************************
     C                   Z-ADD     SCSCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELSCDT
     C***  End   Add  ***  02/11/11  **************************************
     C*** END ADD ****** 01/25/10 *************************************
     C*** beg del ***** 08/22/11 ***************************************
     C*** begin add ****** 03/01/11 ************************************
     C***********        IF        XXFLG3C = *OFF
     C****END***add ****** 03/01/11 ************************************
     C****begin*add ****** 12/31/10 ************************************
     C***********        move      scffla        elffla
     C****end***add ****** 12/31/10 ************************************
     C****begin*add ****** 03/01/11 ************************************
     C***********        ENDIF
     C*** END   add ****** 03/01/11 ************************************
     C*** end del ***** 08/22/11 ***************************************
     C                   ENDSR                                                  MVSCDB
     C*
     C/EJECT
     C*===============================================================
     C* OTHERR - SUBROUTINE FOR PROCESSING OTHER MODES
     C*===============================================================
     C     OTHERR        BEGSR
      *** BEG DEL ***** 01/09/04 *************************************
      *** BEG ADD ***** 06/12/03 *************************************
      * DISPLAY VENDOR FIELD (SCVDR#) ON SCR03 IN ADD MODE
     C********** VNDRFL    IFEQ *ON
     C********** *IN40     ANDEQ*ON
     C**********           SETON                     68
     C**********           ENDIF
      *** END ADD ***** 06/12/03 *************************************
      *** END DEL ***** 01/09/04 *************************************
     C                   ENDSR                                                   OTHERR
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C     *INKN         CASEQ     *ON           KNDO                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE FOR ENDING THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   CLOSE     WDELP
     C                   CLOSE     WDELL
     C                   CLOSE     WMEPL
     C*
     *** BEGIN CHG *** 09/29/99 ***********************************************
      *** BEGIN CHG *** 09/15/99 ***********************************************
     C*** BEGIN ADD *** 05/12/99 **************************************
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
      ***** BEG DEL ******* 06/22/99 **********************************
     C*********  RRWVAC    IFNE *BLANKS
      ***** END DEL ******* 06/22/99 **********************************
      ***** BEG ADD ******* 06/22/99 **********************************
     C     RRWVAC        IFNE      *ZERO
      ***** END ADD ******* 06/22/99 **********************************
     C*
     C*** BEGIN ADD ***  07/05/00  ************************************
     C*
     C*  PASS SPECIAL MODE VALUE TO INDICATE A RATE SET CODE CHANGE
     C*  HAS OCCURRED, IF SO WE NEED TO RELOAD WAIVER DEFAULTS
     C*
     C*** END ADD *****  07/05/00  ************************************
     C                   CALL      'W0051R58'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
    C**********           PARM 'A'       ##CALL
     C                   PARM      @@MODE        ##CALL
     C                   PARM      DBRRN         ##RRN
     C                   PARM      @@RRN2        ##RRN2
     C*** BEGIN ADD ***  07/05/00  ************************************
     C                   PARM      XXRCCG        ##RCCG            1
     C*** END ADD *****  07/05/00  ************************************
     C*
     C***** BEG ADD ******* 07/27/07 **********************************
     C***** BEG CHG ******* 10/18/07 **********************************
     C*********  DKKY9     CHAINPELL1               N60
     C*********            MOVELELWVTP    E@WVTP
     C*********            MOVELELWVFL    E@WVFL
     C*********            MOVELELWVPC    E@WVPC
     C*********            MOVELELWMIN    E@WMIN
     C*********            MOVELELFCHG    E@FCHG
     C*********            MOVELELWMAX    E@WMAX
     C                   OPEN      WDELL5
     C     DKKY9         CHAIN     PELL5                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ZZWVTP        E@WVTP
     C                   MOVEL     ZZWVFL        E@WVFL
     C                   MOVEL     ZZWVPC        E@WVPC
     C                   MOVEL     ZZWMIN        E@WMIN
     C                   MOVEL     ZZFCHG        E@FCHG
     C                   MOVEL     ZZWMAX        E@WMAX
     C                   CLOSE     WDELL5
     C                   ENDIF                                                  *IN60=*OFF
     C***** END CHG ******* 10/18/07 **********************************
     C***** END ADD ******* 07/27/07 **********************************
     C                   ENDIF                                                  RRWACD!=BLAN
      *
     C                   ENDIF                                                  *INKC<>*ON
     C*** END   ADD *** 05/12/99 **************************************
      *** END   CHG *** 09/15/99 ***********************************************
     *** END   CHG *** 09/29/99 ***********************************************
     C*** BEGIN ADD ***  06/24/04  ************************************
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C***  Begin Chg  ***  02/26/10  **************************************
     C*****BSLPGM        IFNE      *BLANK
     C***  Beg   Chg  ***  11/23/10  **************************************
     C***********        if        (BSLPGM = 'P' or BSLPGM = 'I')
     C                   if        (BSLPGM = 'P' or BSLPGM = 'I' or
     C                                BSLPGM='R')
     C***  End   Chg  ***  11/23/10  **************************************
     C***  End   Chg  ***  02/26/10  **************************************
     C*
     C                   CALL      'W0051R48'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@MODE        ##CALL
     C                   PARM      DBRRN         ##RRN
     C                   PARM      @@RRN2        ##RRN2
     C*
     C                   ENDIF                                                  BSLGM IFNE
     C*
     C                   ENDIF                                                  *INKC<>*ON
     C*** END ADD *****  06/24/04  ************************************
     C*
     C*** BEG ADD ***** 12/30/09 *************************************
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
     C***** BEG ADD ******* 01/28/11 **********************************
     C     @@MODE        OREQ      'D'
     C***** END ADD ******* 01/28/11 **********************************
     C*
     C***  Begin Add  ***  02/27/10  **************************************
     C                   if        clearWMLDP
     C     LDrrn         delete    wmldp                              99
     C                   endif
     C***  End   Add  ***  02/27/10  **************************************
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C     BSLPGM        IFEQ      'L'
     C*
     C***** BEG ADD ******* 01/28/11 **********************************
     C     LDKY          CHAIN(n)  PLD01
     C                   if        not %found(WMLDL1)
     C                   eval      ##call = 'A'
     C                   else
     C                   eval      ##call = 'C'
     C                   endif
      *
     c                   if        @@mode = 'D'
     c                   eval      ##call = 'D'
     c                   endif
     C***** END ADD ******* 01/28/11 **********************************
     C*
     C                   CALL      'W0051R38'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C***** BEG CHG ******* 01/28/11 **********************************
     C*************      PARM      @@MODE        ##CALL
     C*************      PARM                    ##RRN
     C                   PARM                    ##CALL
     C                   PARM      LDrrn         ##RRN
     C***** END CHG ******* 01/28/11 **********************************
     C                   PARM      DBRRN         ##RRN2                         WDELP
     C***** BEG ADD ******* 01/28/11 **********************************
     C                   PARM      ' '           #@mode            1            Call in emp mode
     C                   PARM      @@qte         #@qte             1            Call in emp mode
     C***** END ADD ******* 01/28/11 **********************************
     C*
     C                   ENDIF                                                  BSLGM IFNE
     C*
     C                   ENDIF                                                  *INKC<>*ON
     C*
     C                   ENDIF                                                  *INKC<>*ON
     C*** END ADD ***** 12/30/09 *************************************
     C*** BEG ADD ***** 01/25/10 *************************************
     c*** begin add ***  05/06/11  ***********************************
     c                   if        wcfl44 = 'Y'
     c*** end add *****  05/06/11  ***********************************
     C     @@MODE        IFEQ      'A'
     C     @@MODE        OREQ      'C'
      *
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     c*** begin add ***  08/19/11  ***********************************
     C* ACCESS THE GROUP LEVEL CHARGE SETUP FILE  & VALIDATE EXPIRY DATE
     C
     C     CIKY          SETGT     PFI01
     C     CIKY1         READPE    PFI01                                  60
     c*
     C                   DOW       *IN60 =*OFF
     C                   IF        FIEXDT = 0  OR  FIEXDT>XXPFDT
     C                   LEAVE
     C                   ELSE
     C     CIKY1         READPE    PFI01                                  60
     C                   ENDIF
     C                   ENDDO
     c*
     C                   IF        *IN60 = *OFF
     c*** end add *****  08/19/11  ***********************************
     C***  Begin Chg  ***  02/11/11  **************************************
     C*****ELCGCF        IFEQ      'Y'
     C**********
     C**********         MOVEL     @@MODE        ##CALL
     C*****ELOGDF        IFEQ      'N'
     C**********         MOVEL     'D'           ##CALL
     C**********         ENDIF
     C**********
     C**********         CALL      'W0051R63'
     C******INKC         PARM                    ##KC
     C******INKL         PARM                    ##KL
     C**********         PARM                    ##CALL
     C**********         PARM                    ##RRN
     C**********         PARM      DBRRN         ##RRN2                         WDELP
     C**********
     C**********         ENDIF                                                  BSLGM IFNE
     C  N09              MOVE      'C'           ##Mode
     c*** begin add ***  05/06/11  ***********************************
     C     @@MODE        IFEQ      'A'
     C                   MOVE      'A'           ##Mode
     c                   ENDIF
     c*** end add *****  05/06/11  ***********************************
     C   09              MOVE      'D'           ##Mode
     C*
     C                   CALL      'W0051R63'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##Mode
     C                   PARM                    ##RRN
     C                   PARM      DBRRN         ##RRN2                         WDELP
     C***  End   Chg  ***  02/11/11  **************************************
     C*
     c*** begin add ***  08/19/11  ***********************************
     C                   ENDIF                                                  *INKC<>*ON
     c*** end add *****  08/19/11  ***********************************
     C                   ENDIF                                                  *INKC<>*ON
     C*
     C                   ENDIF                                                  *INKC<>*ON
     c*** begin add ***  05/06/11  ***********************************
     C                   ENDIF                                                  WCFL44 = 'Y'
     c*** end add *****  05/06/11  ***********************************
     C*** END ADD ***** 01/25/10 *************************************
     C*
     C     @@MODE        IFEQ      'A'
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C                   if        not *inkc
     C*
     C                   if        cltId = '139'
      *
      *** beg add ***** 12/28/10 ******************************************
     c                   if        ELPFDT > sysDt
     c                   eval      xxDate = ELPFDT
     c                   else
     c                   eval      xxDate = sysDt
     c                   endif
     c     hcky1         setgt     phc01
     c     hcky          readpe    phc01
     c                   if        not(%eof) and
     c                             (hcexdt = 0 or hcexdt > sysDt)
      *** end add ***** 12/28/10 ******************************************
      *
     C* HCN Program
      *
     C                   call      'W1191R'                             99
     C     *inkc         parm                    ##kc
     C     *inkl         parm                    ##kl
     C                   parm      ' '           ##call
     C                   parm      DBRRN         ##rrn
     C*
      *** beg add ***** 12/28/10 ******************************************
     c                   endif
      *** end add ***** 12/28/10 ******************************************
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   if        not *inkc
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C***  BEGIN CHG  ***  10/30/97  *********************************
     C**********           CALL 'W0060R02'
     C********** *INKC     PARM           ##KC    1
     C********** *INKL     PARM           ##KL    1
     C**********           PARM @@MODE    ##MODE  1
     C**********           PARM           ##RRN   90
     C**********           PARM DBRRN     ##RRN2  90
     C                   CALL      'W0060R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      @@QTE         ##QTE             1
     C***  END   CHG  ***  10/30/97  *********************************
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C                   endif
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C***  BEGIN ADD  ***  10/30/97  *********************************
     C     *INKC         IFNE      *ON
     C*
     C                   CALL      'W0553R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      ' '           ##CALL
     C                   PARM      DBRRN         ##RRN
     C                   PARM      @@QTE         ##QTE
     C                   PARM      'Y'           ##MODE            1
     C*
     C                   ENDIF                                                  *INKC<>*ON
     C***  END   ADD  ***  10/30/97  *********************************
     C*
     C*
     C                   ENDIF                                                  @@MODE=A
     C*
     C***  Begin Add  ***  12/17/10  **************************************
     C                   if        @@mode = 'A' and *inkc = *off and
     C                             *inkl = *off and cltid = '071' and
     C                             fn3ppe = 'Y'
     C                   CALL      'W0096R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      ' '           ##MODE            1
     C                   PARM      DBRRN         ##RRN
     C                   endif                                                  cltid and fn3ppe
     C***  End   Add  ***  12/17/10  **************************************
     C*** BEG ADD ***** 02/03/98 ***********************************************
     C*
     C*  CHECK FOR REPORTABLE CHANGE AS MID TERM ENDORSEMENT
     C*
     C     @@MODE        IFEQ      'C'                                          CHANGE MODE
     C                   EXSR      CKCHGS
     C                   ENDIF
     C*** END ADD ***** 02/03/98 ***********************************************
     C*
     C*** BEG ADD ***** 04/11/03 ***********************************************
     C                   CLEAR                   ACHFLG
     C                   CLEAR                   VNDRFL
     C*** END ADD ***** 04/11/03 ***********************************************
      ***** BEG ADD ******* 06/20/03 **********************************
     C*+*+ BEGIN DLT *+*+  08/24/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
     C*+*+*+*+*+*+*+*      SETON                     LR
     C*+*+ END   DLT *+*+  08/24/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
      ***** END ADD ******* 06/20/03 **********************************
     C*
     C*+*+ BEGIN ADD  *+*+ 08/24/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
      **  Display screen to capture individual percentages that add up
      **  to Schedule DB/CR factor.
      **
     C     SCDBCR        IFGT      *ZERO
     C     CLTID         IFEQ      '114'
     C*
     C                   Z-ADD     SCSDEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDEF
     C*
     C                   Z-ADD     SCSDEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSDEX
      *
     C                   MOVE      F2CAR#        ENCAR1
      ***** BEG ADD   ******   04/04/07 *******************************
     C                   MOVE      @@QTE         ##QTE
      ***** END ADD   ******   04/04/07 *******************************
     C*
     C                   CALL      'W1551R02'
     C                   PARM                    ELCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    ELDIV
     C                   PARM                    ELFYR
     C                   PARM      ENCAR1        ENCAR             5 0
     C                   PARM                    SCDBCR
     C                   PARM                    XXSDEF
     C                   PARM                    XXSDEX
     C                   PARM                    @@MODE
     C                   PARM                    SCCONM
     C                   PARM                    SCFND
     C                   PARM                    SCNAME
     C                   PARM                    SCEDN
     C     *IN90         PARM      '0'           ERROR
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
      ***** BEG ADD   ******   04/04/07 *******************************
     C                   PARM                    ##QTE
      ***** END ADD   ******   04/04/07 *******************************
     C*
     C                   ENDIF                                                  SCDBCR >ZERO
     C                   ENDIF                                                  CLTID = 114
     C*
     C                   ENDIF                                                  *INKC<>*ON
     C***** BEG ADD ******* 07/27/07 **********************************
     C***** BEG CHG ******* 10/18/07 **********************************
     C  N90@@MODE        IFEQ      'C'
     C     *INKC         ANDEQ     *OFF
     C     *INKC         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
     C******             MOVE      'Y'           EXUPDA
     C***** END CHG ******* 10/18/07 **********************************
     C     EXUPDA        IFEQ      'Y'
      *
     C                   MOVE      '0'           @ARSMFLG          1
     C     S@ARSM        IFEQ      '@'
     C                   MOVE      *Blanks       S@ARSM
     C                   MOVE      '1'           @ARSMFLG          1
     C                   Endif
      *
     C***  Begin Add  ***  08/25/10  **************************************
     C                   clear                   EFEXCF
     C                   clear                   EFEXCO
     C                   clear                   EFSCBD
     C     efKy          chain     pef01
     C***  End   Add  ***  08/25/10  **************************************
      *
     C     X@PFDT        IFNE      S@PFDT
     C     X@PTDT        ORNE      S@PTDT
     C     SCARSM        ORNE      S@ARSM
     C     SCIFFL        ORNE      S@IFFL
     C     SCADVR        ORNE      S@ADVR
     C     SVRLFL        ORNE      S@RLFL
     C     X@BSDT        ORNE      S@BSDT
     C     SCAFLG        ORNE      S@STAT
     C     SCILCD        ORNE      S@ILCD
     C     X@ILEF        ORNE      S@ILEF
     C     X@ILEX        ORNE      S@ILEX
     C     SCDCDE        ORNE      S@DCDE
     C     X@DCEF        ORNE      S@DCEF
     C     X@DCEX        ORNE      S@DCEX
     C     SCCXBL        ORNE      S@CXBL
     C     SCASP2        ORNE      S@ASP2
     C     X@DVEF        ORNE      S@DVEF
     C     X@DVEX        ORNE      S@DVEX
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     SCD1S2        ORNE      S@D1S2
     C     X@D1D2        ORNE      S@D1D2
     C     X@D1X2        ORNE      S@D1X2
     C*** END   ADD ****** 06/21/10 ************************************
     C     SCASP1        ORNE      S@ASP1
     C     X@CREF        ORNE      S@CREF
     C     X@CREX        ORNE      S@CREX
     C     SCASPM        ORNE      S@ASPM
     C     X@MCEF        ORNE      S@MCEF
     C     X@MCEX        ORNE      S@MCEX
     C     SCMCCD        ORNE      S@MCCD
     C     SCMCOD        ORNE      S@MCGP
     C     SCDEV2        ORNE      S@DEV2
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     SCSTFC        ORNE      S@STFC
     C*** END   ADD ****** 01/14/10 ************************************
     C     X@D2EF        ORNE      S@D2EF
     C     X@D2EX        ORNE      S@D2EX
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     SCD2S2        ORNE      S@D2S2
     C     X@D2D2        ORNE      S@D2D2
     C     X@D2X2        ORNE      S@D2X2
     C*** END   ADD ****** 06/21/10 ************************************
     C     SCEXM1        ORNE      S@EXM1
     C     X@EMD1        ORNE      S@EMD1
     C***** BEG CHG ******* 10/18/07 **********************************
     C     X@EMD2        ORNE      S@EMD2
     C     X@EMD3        ORNE      S@EMD3
     C***** END CHG ******* 10/18/07 **********************************
     C     SCASPS        ORNE      S@ASPS
     C     X@SCEF        ORNE      S@SCEF
     C     X@SCEX        ORNE      S@SCEX
     C     SCSRPR        ORNE      S@SRPR
     C     SCASP3        ORNE      S@ASP3
     C     SCDISC        ORNE      S@DISC
     C     SCPROM        ORNE      S@PROM
     C     SCDBCR        ORNE      S@DBCR
     C     X@SDEF        ORNE      S@SDEF
     C     X@SDEX        ORNE      S@SDEX
     C     SCCXDY        ORNE      S@CXDY
     C     SCPP3C        ORNE      S@PP3C
     C     SCPPC3        ORNE      S@PPC3
     C     SCSFPC        ORNE      S@SFPC
     C     SCSFMN        ORNE      S@SFMN
     C     SCADDM        ORNE      S@ADDM
     C     X@ANDT        ORNE      S@ANDT
     C     SCDCD2        ORNE      S@DCD2
     C     X@2DEF        ORNE      S@2DEF
     C     X@2DEX        ORNE      S@2DEX
     C     SCINL2        ORNE      S@INL2
     C     X@I2EF        ORNE      S@I2EF
     C     X@I2EX        ORNE      S@I2EX
     C     SCEXMF        ORNE      S@EXMF
     C     E@WVTP        ORNE      S@WVTP
     C     E@WVFL        ORNE      S@WVFL
     C     E@WVPC        ORNE      S@WVPC
     C     E@WMIN        ORNE      S@WMIN
     C     E@FCHG        ORNE      S@FCHG
     C     E@WMAX        ORNE      S@WMAX
     C     SCASCD        ORNE      S@ASTS
     C     SCCOFL        ORNE      S@COFL
     C     SCTYPB        ORNE      S@TYPB
     C     SCFCOD        ORNE      S@PFIN
     C     SCCTST        ORNE      S@CTST
     C***** BEG CHG ******* 10/08/07 **********************************
     C     SCQTYP        ORNE      S@QTYP
     C     SCMFEE        ORNE      S@MFEE
     C***  Begin Add  ***  08/25/10  **************************************
     C     EFEXCF        ORNE      saveEFEXCF
     C     EFEXCO        ORNE      saveEFEXCO
     C     EFSCBD        ORNE      saveEFSCBD
     C***  End   Add  ***  08/25/10  **************************************
     C*********  SCQTYP    ORNE S@MBCD
     C*********  SCMFEE    ORNE S@MFAM
     C***** END CHG ******* 10/08/07 **********************************
     C** set indicators to display correct prompt
     C*****      FNPEOF    IFEQ 'M'
     C*****      SC#DIV    ANDEQ1
     C*****                SETON                     72
     C*****                SETOF                     71
     C*****                ELSE
     C*****                SETON                     71
     C*****                SETOF                     72
     C*****                ENDIF                            FNPEOF = 'M'
     C***** BEG CHG ******* 10/08/07 **********************************
      *
     C     X@PFDT        IFNE      S@PFDT
     C     X@PTDT        ORNE      S@PTDT
     C     SCARSM        ORNE      S@ARSM
     C     SCIFFL        ORNE      S@IFFL
     C     SCADVR        ORNE      S@ADVR
     C     SVRLFL        ORNE      S@RLFL
     C     X@BSDT        ORNE      S@BSDT
     C     SCAFLG        ORNE      S@STAT
     C     SCILCD        ORNE      S@ILCD
     C     X@ILEF        ORNE      S@ILEF
     C     X@ILEX        ORNE      S@ILEX
     C     SCDCDE        ORNE      S@DCDE
     C     X@DCEF        ORNE      S@DCEF
     C     X@DCEX        ORNE      S@DCEX
     C     SCCXBL        ORNE      S@CXBL
     C     SCASP2        ORNE      S@ASP2
     C     X@DVEF        ORNE      S@DVEF
     C     X@DVEX        ORNE      S@DVEX
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     SCD1S2        ORNE      S@D1S2
     C     X@D1D2        ORNE      S@D1D2
     C     X@D1X2        ORNE      S@D1X2
     C*** END   ADD ****** 06/21/10 ************************************
     C     SCASP1        ORNE      S@ASP1
     C     X@CREF        ORNE      S@CREF
     C     X@CREX        ORNE      S@CREX
     C     SCASPM        ORNE      S@ASPM
     C     X@MCEF        ORNE      S@MCEF
     C     X@MCEX        ORNE      S@MCEX
     C     SCMCCD        ORNE      S@MCCD
     C     SCMCOD        ORNE      S@MCGP
     C     SCDEV2        ORNE      S@DEV2
     C*** BEGIN CHG ****** 01/14/10 ************************************
     C     SCSTFC        ORNE      S@STFC
     C*** END   CHG ****** 01/14/10 ************************************
     C     X@D2EF        ORNE      S@D2EF
     C     X@D2EX        ORNE      S@D2EX
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     SCD2S2        ORNE      S@D2S2
     C     X@D2D2        ORNE      S@D2D2
     C     X@D2X2        ORNE      S@D2X2
     C*** END   ADD ****** 06/21/10 ************************************
     C     SCEXM1        ORNE      S@EXM1
     C     X@EMD1        ORNE      S@EMD1
     C     X@EMD2        ORNE      S@EMD2
     C     X@EMD3        ORNE      S@EMD3
     C     SCASPS        ORNE      S@ASPS
     C     X@SCEF        ORNE      S@SCEF
     C     X@SCEX        ORNE      S@SCEX
     C     SCSRPR        ORNE      S@SRPR
     C     SCASP3        ORNE      S@ASP3
     C     SCDISC        ORNE      S@DISC
     C     SCPROM        ORNE      S@PROM
     C     SCDBCR        ORNE      S@DBCR
     C     X@SDEF        ORNE      S@SDEF
     C     X@SDEX        ORNE      S@SDEX
     C     SCCXDY        ORNE      S@CXDY
     C     SCPP3C        ORNE      S@PP3C
     C     SCPPC3        ORNE      S@PPC3
     C     SCSFPC        ORNE      S@SFPC
     C     SCSFMN        ORNE      S@SFMN
     C     SCADDM        ORNE      S@ADDM
     C     X@ANDT        ORNE      S@ANDT
     C     SCDCD2        ORNE      S@DCD2
     C     X@2DEF        ORNE      S@2DEF
     C     X@2DEX        ORNE      S@2DEX
     C     SCINL2        ORNE      S@INL2
     C     X@I2EF        ORNE      S@I2EF
     C     X@I2EX        ORNE      S@I2EX
     C     SCEXMF        ORNE      S@EXMF
     C     E@WVTP        ORNE      S@WVTP
     C     E@WVFL        ORNE      S@WVFL
     C     E@WVPC        ORNE      S@WVPC
     C     E@WMIN        ORNE      S@WMIN
     C     E@FCHG        ORNE      S@FCHG
     C     E@WMAX        ORNE      S@WMAX
     C     SCQTYP        ORNE      S@QTYP
     C     SCMFEE        ORNE      S@MFEE
     C***  Begin Add  ***  08/25/10  **************************************
     C     EFEXCF        ORNE      saveEFEXCF
     C     EFEXCO        ORNE      saveEFEXCO
     C     EFSCBD        ORNE      saveEFSCBD
     C***  End   Add  ***  08/25/10  **************************************
     C                   MOVE      *ON           XXFL6                           XXFL6
     C                   ENDIF
     C*
     C                   MOVE      *OFF          XXFL5
     C     SCASCD        IFNE      S@ASTS
     C     SCCOFL        ORNE      S@COFL
     C     SCTYPB        ORNE      S@TYPB
     C     SCFCOD        ORNE      S@PFIN
     C     SCCTST        ORNE      S@CTST
     C                   MOVE      *ON           XXFL5
     C                   ENDIF
      *
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     FNPEOF        IFEQ      'M'
     C***** BEG CHG ******* 10/08/07 **********************************
     C     XXFL5         ANDEQ     *ON
     C     XXFL6         ANDEQ     *ON
     C***** END CHG ******* 10/08/07 **********************************
     C                   SETON                                        72
     C                   SETOFF                                       71
     C                   ELSE
     C     FNPEOF        IFEQ      'S'
     C     SC#DIV        ANDGT     1
     C***** BEG CHG ******* 10/08/07 **********************************
     C**   XXFL5         ANDEQ     *OFF
     C***** END CHG ******* 10/08/07 **********************************
     C     FNPEOF        OREQ      ' '
     C     SC#DIV        ANDGT     1
     C***** BEG CHG ******* 10/08/07 **********************************
     C***  XXFL5         ANDEQ     *OFF
     C***** END CHG ******* 10/08/07 **********************************
     C                   SETON                                        71
     C                   SETOFF                                       72
     C                   ENDIF
     C                   ENDIF
      *
     C     FNPEOF        IFEQ      'M'
     C     SC#DIV        ANDGT     1
     C     XXFL5         ANDEQ     *ON
     C     XXFL6         ANDEQ     *OFF
     C                   SETON                                        71
     C**                 SETON                                        72
     C                   ENDIF
      *
     C     FNPEOF        IFEQ      'M'
     C*    SC#DIV        ANDGT     1
     C     XXFL5         ANDEQ     *OFF
     C     XXFL6         ANDEQ     *ON
     C                   SETON                                        72
     C**                 SETON                                        71
     C                   ENDIF
      *
     C     *IN90         DOUEQ     *OFF
     C     *IN71         IFEQ      *ON
     C     *IN72         OREQ      *ON
     C                   EXFMT     UPDAW
     C                   MOVE      *OFF          *IN90
     C                   MOVE      *OFF          *IN66
     C                   MOVE      *OFF          *IN68
     C     SCUPDA        IFNE      'Y'
     C***  Begin Chg  ***  08/25/10  *************************************
     C*****SCUPDA        ANDNE     ' '
     C     SCUPDA        ANDNE     'N'
     C***  End   Chg  ***  08/25/10  *************************************
     C                   SETON                                        66  90
     C                   ENDIF
     C     SCCXFL        IFNE      'Y'
     C***  Begin Chg  ***  08/25/10  *************************************
     C*****SCCXFL        ANDNE     ' '
     C     SCCXFL        ANDNE     'N'
     C***  End   Chg  ***  08/25/10  *************************************
     C                   SETON                                        68  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO
      *
     C********** SCUPDA    IFEQ 'Y'
     C********** FNPEOF    ANDEQ'M'
     C********** SC#DIV    ANDGT1
     C**********
     C********** DKKY10    CHAINWMEML                60
     C********** *IN60     DOWEQ*OFF
     C**********           CLEAR@@DIVA
     C**********           Z-ADDEMDIV     ELDIV
     C*** determine if division is active
     C********** EMCAD1    IFEQ *ZERO
     C********** EMCAD2    ANDEQ*ZERO
     C********** EMCAD3    ANDEQ*ZERO
     C********** EMCAD4    ANDEQ*ZERO
     C********** EMCAD5    ANDEQ*ZERO
     C**********           MOVE 'Y'       @@DIVA  1
     C**********           ELSE
     C**********           MOVE 'N'       @@DIVA  1
     C**********           ENDIF                           EMCAD1 <> 0
     C*** UPDATErecord if DIVISION is active or update canceled div flag Is 'Y'
     C********** @@DIVA    IFEQ 'Y'
     C********** SCCXFL    OREQ 'Y'
     C********** DKKY11    CHAINPELL2                60
     C**********           MOVELSCASCD    ELASTS
     C**********           MOVELSCCOFL    ELCOFL
     C**********           MOVELSCTYPB    ELTYPB
     C**********           MOVELSCFCOD    ELPFIN
     C**********           MOVELSCCTST    ELCTST
     C**********           UPDATPELL2
     C**********           ENDIF
     C********** EMKY2     READEWMEML                    60
     C**********           ENDDO
     C**********           ELSE
     C     SCUPDA        IFEQ      'Y'
     C                   EXSR      CHKFA
     C                   EXSR      UPKFA
     C                   ENDIF
     C                   ENDIF                                                   EXUPDA = 'Y'
     C****                 ENDIF                            EXUPDA = 'AND'
     C                   ENDIF                                                   *INKL  = *OFF
     C                   ENDIF
      *
     C                   SETON                                        LR
      *
     C*+*+ END   ADD  *+*+ 08/24/06  *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*
     C                   ENDSR                                                  PGMEND
      *
      *
      *
     C/EJECT
     C*===============================================================
     C* PGMSTR - SUBROUTINE FOR STARTING THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
      /Free
      /If Not Defined (WEB)
       useDots = *on;
      /Endif
       If useDots;
          @@per   = @#per;
          @@per25 = @#per25;
       Endif;
      /End-free
      *
      *** BEG ADD **** 12/18/05 **********************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *** END ADD **** 12/18/05 **********************************************
     C***  Begin Add  ***  12/03/08  **************************************
     C                   SETOFF                                       07
     C                   select
     C                   when      CLTID = '102'
     C                   SETON                                        07
     C                   endsl                                                  CLTID = '102'
     C                   eval      clt102 = *in07
     C***  End   Add  ***  12/03/08  **************************************
     C*
     C***  Begin Add  ***  07/20/07  **************************************
      *** beg del ***** 01/12/12 ******************************************
     C***********        CLEAR                   SVAAFL
     C*****End***Add  ***  07/20/07  **************************************
     C*****Begin*Add  ***  09/08/07  **************************************
     C***********        CLEAR                   SVAPFL
      *** end del ***** 01/12/12 ******************************************
     C***  End   Add  ***  09/08/07  **************************************
      *
     C*** BEGIN ADD ***  07/11/06  *******************************************
      ***** BEGIN DELETE ******** 03/01/07 **************************
     C*****                MOVE *OFF      *IN97
     C***
     C**DISPLAY SPREAD ADJUSTMENTS INTO FUTURE FLAG FOR THIS CLIENT.
     C***
     C*****      CLTID     IFEQ '132'
     C*******              MOVE *ON       *IN97
     C*****                ENDIF                           CLTID IFEQ
      ***** END   DELETE ******** 03/01/07 **************************
     C*** END ADD *****  07/11/06  *******************************************
      ***** BEG ADD *+*+*+* 03/01/07 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      ***** BEG DEL ******* 03/06/07 **********************************
      *********
     C**DISPLAY Spread-Adjustments-Into_Future flag FOR THIS CLIENT.
      *********
     C*********  CLTID     IFEQ '132'
     C*********            MOVE X'20'     AT3              NORMAL
     C*********            MOVE X'26'     AT4              UNDER HI
      *********
     C*********  *IN09     IFEQ *ON                        Inquiry mode
     C*********            MOVE X'A6'     AT4              UNDER HI PROT
     C*********            ENDIF
      *********
     C*********            ELSE
     C*********            MOVE X'A7'     AT3              NON-DSP PROT
     C*********            MOVE X'A7'     AT4
     C*********            ENDIF
      *********
      ***** END DEL ******* 03/06/07 **********************************
      ***** END ADD *+*+*+* 03/01/07 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
      *
      *** BEG ADD **** 02/25/02 **********************************************
     C                   MOVE      *OFF          BSCDFL            1            PROTECT SCBSCD
     C     @@MODE        IFEQ      'R'
     C                   MOVE      'C'           @@MODE
     C                   MOVE      *ON           BSCDFL                         UNPROTCT SCBSCD
     C                   ENDIF
      *** END ADD **** 02/25/02 **********************************************
      *** BEG ADD **** 06/06/02 **********************************************
     C     @@QTE         IFEQ      'Q'
     C                   MOVE      *ON           BSCDFL                         UNPROTCT SCBSCD
     C                   ENDIF
      *** END ADD **** 06/06/02 **********************************************
      ***** BEG ADD ******* 11/09/01 **********************************
      *** beg del ****** 01/12/12 *************************************
     C***********        MOVE      'Y'           YRFLG             1
      *** end del ****** 01/12/12 *************************************
      ***** END ADD ******* 11/09/01 **********************************
      *
      ***** BEG ADD ******* 05/02/00 **********************************
     C                   MOVE      'N'           XXONCE            1
      ***** END ADD ******* 05/02/00 **********************************
     C***  BEGIN ADD  ***  08/13/98  *********************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C***  END   ADD  ***  08/13/98  *********************************
     C*
     C*** BEGIN CHG ***  09/22/99  ************************************
     C********             Z-ADD2         #OFSCR  10
     C***  Begin Chg  ***  10/02/06  **************************************
     C**********           Z-ADD3         #OFSCR  10
     C*** Beg Chg *** 07/28/10 ***********************************************
     C**********         Z-ADD     4             #OFSCR            1 0
     C*** Beg Chg *** 12/30/10 ***********************************************
     C**********         Z-ADD     5             #OFSCR            1 0
     C                   Z-ADD     4             #OFSCR            1 0
     C*** End Chg *** 12/30/10 ***********************************************
     C*** End Chg *** 07/28/10 ***********************************************
     C***  End   Chg  ***  10/02/06  **************************************
     C*** END CHG *****  09/22/99  ************************************
     C*
     C                   OPEN      WDELP
     C                   OPEN      WDELL
     C                   OPEN      WMEPL
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C     @@QTE         COMP      'Q'                                    41    QUOTE/ADD
     C*
     c*** begin add ***  03/12/10  *************************************
     c                   if        *in09
     c                   eval      qtya=x'A2'
     c                   else
     c                   if        cltid='025'
     c                   eval      qtya=x'26'
     c                   else
     c                   eval      qtya=x'24'
     c                   endif
     c                   endif
     c*** end   add ***  03/12/10  *************************************
     C*
     C***  Begin Add  ***  11/22/04  **************************************
     C                   MOVE      *IN40         ADDMOD            1
     C                   MOVE      *IN09         INQMOD            1
     C***  End   Add  ***  11/22/04  **************************************
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C***** BEG ADD ******* 01/12/99 **********************************
     C*
     C*   Get Number of Employer Divisions to display in Screen Header
     C*
     C     @@RRN2        CHAIN     WMEMP                              60
     C                   Z-ADD     EMCO#         KKCO#
     C                   Z-ADD     EMFND         KKFND
     C                   Z-ADD     EMEMP#        KKEMP#
     C*
     C     EMKY2         CHAIN     PEM01                              60
     C                   Z-ADD     0             SC#DIV
     C     *IN60         DOWEQ     *OFF
     C***** Beg Add ******* 12/08/11 **********************************
      *** beg chg ***** 01/12/12 **************************************
     C***********        IF        EMSTRP = 'Y'
     C***********        EVAL      savDiv = EMDIV
     C***********        ENDIF
     C                   IF        EMSTRP = 'Y'
     c                   eval      stRptDiv = EMDIV
     C                   ENDIF
      *** end chg ***** 01/12/12 **************************************
     C***** End Add ******* 12/08/11 **********************************
     C                   ADD       1             SC#DIV
     C     EMKY2         READE     PEM01                                  60
     C                   ENDDO
     C*
     C***** END ADD ******* 01/12/99 **********************************
     C***** BEG ADD ******* 03/06/06 **********************************
      *
      * Finds the ELRQFL for the division record which is the State
      * Reporting Division.  There is a EMRQFL at the master (WMEMP)
      * level, but it might be out of synch, that's why we are getting
      * it from the detail (WDELP) level
      *
     C                   MOVEL     *BLANKS       XXRQFL            1
     C  N40@@RRN         CHAIN     WDELP                              60
     C  N40              Z-ADD     ELFYR         X#FYR             3 0
     C   40              MOVE      *HIVAL        X#FYR
      *
     C     EMKY2         SETLL     WMEML
     C     EMKY2         READE     WMEML                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     EMSTRP        IFEQ      'Y'
     C                   MOVEL     EMRQFL        XXRQFL
      *
     C     EMKY4         CHAIN     WDELL                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVEL     ELRQFL        XXRQFL
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
     C     EMKY2         READE     WMEML                                  60
     C                   ENDDO
      *
      * Resets the file to the top for chain below, it should not be
      * necesary, but I have seen strange things happen, it won't hurt.
     C     *LOVAL        SETLL     WMEML
      *
     C***** END ADD ******* 03/06/06 **********************************
      *
     C*
     C*  GET EMPLOYER RECORD
     C*
     C     @@RRN2        CHAIN     WMEMP                              60
     C  N40@@RRN         CHAIN     WDELP                              60
     C*
     C*** BEG ADD ***** 02/03/98 ***********************************************
     C*
     C*  SAVE THE OLD DATA BASE FIELDS FOR LATER COMPARISON
     C*
     C     @@MODE        IFEQ      'C'                                          CHANGE MODE
     C     *IN60         ANDEQ     *OFF                                         WDELP REC FOUND
     C                   MOVEL     ELILCD        S#ILCD
     C                   MOVEL     ELINL2        S#INL2
     C                   MOVE      ELEXM1        S#EXM1
     C                   MOVE      ELEXM2        S#EXM2
     C                   MOVE      ELEXM3        S#EXM3
     C*
     C*** BEG ADD ***** 09/22/09 ***********************************************
     C                   MOVE      SCDCDE        SVDCDE
     C                   MOVE      SCDCD2        SVDCD2
     C*** END ADD ***** 09/22/09 ***********************************************
     C*
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         S#EXD1
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         S#EXD2
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         S#EXD3
     C*
     C*  GET JURISDICTION STATE FROM RATE SET FILE
     C*
     C                   MOVE      ELFND         SCFND
     C                   MOVEL     ELRTCD        SCRTCD
     C     RSKY          CHAIN     PRS01                              60
     C  N60              MOVEL     RSJRST        XXST
     C*
     C*  SAVE THE OLD INCREASED LIMIT DESCRIPTIONS
     C*
     C                   MOVE      ELRTCD        SCRTCD
     C                   MOVE      ELPFDT        XXPFDT
     C                   MOVEL     *BLANKS       S#ILDS
     C                   MOVEL     *BLANKS       S#I2DS
     C*
     C     ELILCD        IFNE      *ZEROS
     C                   MOVEL(P)  ELILCD        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C  N60              MOVEL(P)  LMDESC        S#ILDS
     C                   ENDIF                                                  ELILCD<>*ZEROS
     C*
     C     ELINL2        IFNE      *ZEROS
     C                   MOVEL(P)  ELINL2        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C  N60              MOVEL(P)  LMDESC        S#I2DS
     C                   ENDIF                                                  ELINL2<>*ZEROS
     C*
     C                   ENDIF                                                  @@MODE='C'/*IN60=*OF
     C*** END ADD ***** 02/03/98 ***********************************************
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*** BEG ADD ****** 01/21/09 **********************************************
     C                   move      *off          assignVendor      1
     C*** END ADD ****** 01/21/09 **********************************************
     C*** BEG ADD ****** 04/07/08 **********************************************
     C     CLTID         IFEQ      '139'
     C     ADDMOD        ANDEQ     *ON
     C     CLTID         OREQ      '146'
     C     ADDMOD        ANDEQ     *ON
     C                   MOVE(P)   *BLANKS       GW001
     C                   MOVE(P)   *BLANKS       GW002
     C                   MOVE(P)   *BLANKS       GW003
     C                   MOVE(P)   *BLANKS       GW004
     C                   Z-ADD     EMFND         KKVDR#
     C     VPKY          CHAIN     PVP01                              60
     C     *IN60         IFEQ      *ON
     C*** BEG ADD ****** 01/21/09 **********************************************
     C                   if        cltID = '146'
     C*** end ADD ****** 01/21/09 **********************************************
     C                   MOVEL(P)  STR(1)        STRTIT
     C                   MOVEL(P)  STR(3)        GW001
     C                   MOVEL(P)  STR(4)        GW002
     C                   EXFMT     STRWDW
     C                   SETON                                        LR
     C                   RETURN
     C*** BEG ADD ****** 01/21/09 **********************************************
     C                   endif
     C                   else
     c                   move      *on           assignVendor
     C*** END ADD ****** 01/21/09 **********************************************
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ****** 04/07/08 **********************************************
     C*
     C*  LOAD PHYSICAL STATE IF NOT BLANK ELSE LOAD MAILING STATE
     C*
     C     EMPST         IFNE      *BLANKS
     C*
     C                   MOVEL     EMPST         ##ST
     C*
     C                   ELSE
     C*
     C                   MOVEL     EMMST         ##ST
     C*
     C                   ENDIF                                                  EMPST NE ' '
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*** Beg Add *** 07/28/10 ***********************************************
     C     FNKY          CHAIN     PFH01                              60
     C*** End Add *** 07/28/10 ***********************************************
     C**** BEGIN ADD **** 10/14/98 *********************************************
     C     F2KY          CHAIN     PF201                              60
     C**** END   ADD **** 10/14/98 *********************************************
     C**** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
     C**** END   ADD **** 11/23/98 *********************************************
     C*** Beg Add *** 12/01/10 ***********************************************
     C*
     C*** Begin add ***  12/30/10  ***************************************************
     C     exky1         CHAIN     PEX02                              60
     C                   call      'SRCHKOBJ'
     C                   parm      'W0010R64'    objnam           10
     C                   parm      '*PGM'        xxtyp             7
     C                   parm                    xxobj             1
     C*
     C                   if        xxobj = '0'                                  if found
     C                   move      'Y'           flg132            1
     C*** End Add *** 12/30/10 ***********************************************
     C* If all heading fields are blank then do not show screen 5
     C*
     C                   If        FHFH#1 = *Blanks and FHFH#2 = *Blanks and
     C                             FHFH#3 = *Blanks and FHFH#4 = *Blanks and
     C                             FHFH#5 = *Blanks
     C*
     C                   Z-ADD     4             #OFSCR            1 0
     C*** Beg Del *** 12/30/10 ***********************************************
     C*********          endif
     C*** End Del *** 12/30/10 ***********************************************
     C*** End Add *** 12/01/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   else
     C                   z-add     5             #ofscr
     C                   endif
     C*
     C                   else
     C                   if        xxobj = '1' and (exehd1 <> ' ' or
     C                                             exehd2 <> ' ')
     C                   eval      flg132= 'N'
     C                   Move      X'27'         AT12P
     C                   Move      X'27'         AT12F
     C                   Move      X'27'         AT12S
     C                   Move      X'27'         AT13P
     C                   Move      X'27'         AT13F
     C                   Move      X'27'         AT14P
     C                   Move      X'27'         AT14F
     C                   z-add     5             #ofscr
     C                   endif
     C                   endif                                                  xxobj = '0'
     C
     C*** end   add ***  12/30/10  ***************************************************
     C*** Beg Add *** 07/28/10 ***********************************************
     C
     C
     C                   MoveA     @@per         @@ARR2
     C                   z-add     1             i                 2 0
     C*** Beg Add *** 12/30/10 ***********************************************
     C*Field#1
     C                   if        flg132 = 'N'
     C                   If        exehd1<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT10P
     C                   Eval      @@len =%Len(%trim(exehd1))
     C                   If        @@len > 13
     C                   MoveL     exehd1        SCEHD1
     C                   Move      ':'           SCEHD1
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Even length
     C                   If        @@rem <> 0
     C                   Movea     exehd1        @@ARR1
     C                   dow       i<=@@len
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Odd length
     C                   Movea     exehd1        @@ARR1
     C                   dow       i<=@@len+1
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf
     C                   movea     @@arr2        SCEHD1
     C                   EndIf                                                  @@len>13
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT10P
     C                   EndIf                                                  exehd1 <> *blanks
     C*Second column
     C                   If        exehd1<>*Blanks
     C                             OR ELFFD1<> *Blanks
     C  N09              Move      X'24'         AT10F
     C   09              Move      X'20'         AT10F
     C                   Else
     C                   Move      X'A7'         AT10F
     C                   EndIf
     C*Field#2
     C                   Movea     *blanks       @@ARR1
     C                   Movea     @@per         @@ARR2
     C                   z-add     1             i
     C                   If        exehd2<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT11P
     C                   Eval      @@len =%Len(%trim(exehd2))
     C                   If        @@len > 13
     C                   MoveL     exehd2        SCEHD2
     C                   Move      ':'           SCEHD2
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Odd length
     C                   If        @@rem <> 0
     C                   Movea     exehd2        @@ARR1
     C                   dow       i<=@@len
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Even length
     C                   Movea     exehd2        @@ARR1
     C                   dow       i<=@@len+1
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf                                                  Odd/Even length
     C                   movea     @@arr2        SCEHD2
     C                   EndIf                                                  @@len>13
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT11P
     C                   EndIf                                                  FHFH#2 <> *blanks
     C*Field#2 Second column
     C                   If        exehd2<>*Blanks
     C                             OR ELFFD2<> *Blanks
     C  N09              Move      X'24'         AT11F
     C   09              Move      X'20'         AT11F
     C                   Else
     C                   Move      X'A7'         AT11F
     C                   EndIf
     C                   EndIf                                                  FLG132 = 'N'
     C*
     C                   if        flg132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C*Field#1
     C                   If        FHFH#1<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT10P
     C                   Eval      @@len =%Len(%trim(FHFH#1))
     C                   If        @@len > 13
     C                   MoveL     FHFH#1        SCEHD1
     C                   Move      ':'           SCEHD1
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Even length
     C                   If        @@rem <> 0
     C                   Movea     FHFH#1        @@ARR1
     C                   dow       i<=@@len
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Odd length
     C                   Movea     FHFH#1        @@ARR1
     C                   dow       i<=@@len+1
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf
     C                   movea     @@arr2        SCEHD1
     C                   EndIf                                                  @@len>13
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT10P
     C                   EndIf                                                  FHFH#1 <> *blanks
     C*Second column
     C                   If        FHFH#1<>*Blanks
     C                             OR ELFFD1<> *Blanks
     C  N09              Move      X'24'         AT10F
     C   09              Move      X'20'         AT10F
     C                   Else
     C                   Move      X'A7'         AT10F
     C                   EndIf
     C*Field#2
     C                   Movea     *blanks       @@ARR1
     C                   Movea     @@per         @@ARR2
     C                   z-add     1             i
     C                   If        FHFH#2<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT11P
     C                   Eval      @@len =%Len(%trim(FHFH#2))
     C                   If        @@len > 13
     C                   MoveL     FHFH#2        SCEHD2
     C                   Move      ':'           SCEHD2
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Odd length
     C                   If        @@rem <> 0
     C                   Movea     FHFH#2        @@ARR1
     C                   dow       i<=@@len
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Even length
     C                   Movea     FHFH#2        @@ARR1
     C                   dow       i<=@@len+1
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf                                                  Odd/Even length
     C                   movea     @@arr2        SCEHD2
     C                   EndIf                                                  @@len>13
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT11P
     C                   EndIf                                                  FHFH#2 <> *blanks
     C*Field#2 Second column
     C                   If        FHFH#2<>*Blanks
     C                             OR ELFFD2<> *Blanks
     C  N09              Move      X'24'         AT11F
     C   09              Move      X'20'         AT11F
     C                   Else
     C                   Move      X'A7'         AT11F
     C                   EndIf
     C*Field#3
     C                   Movea     *blanks       @@ARR1
     C                   Movea     @@per         @@ARR2
     C                   z-add     1             i
     C                   If        FHFH#3<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT12P
     C                   Eval      @@len =%Len(%trim(FHFH#3))
     C                   If        @@len > 13
     C                   MoveL     FHFH#3        SCEHD3
     C                   Move      ':'           SCEHD3
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Odd length
     C                   If        @@rem <> 0
     C                   Movea     FHFH#3        @@ARR1
     C                   dow       i<=@@len
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For even length
     C                   Movea     FHFH#3        @@ARR1
     C                   dow       i<=@@len+1
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf                                                  Odd/Even length
     C                   movea     @@arr2        SCEHD3
     C                   EndIf                                                  @@len>13
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT12P
     C                   EndIf                                                  FHFH#3 <> *blanks
     C*Field#3 Second column
     C                   If        FHFH#3<>*Blanks
     C                             OR ELFFD3<> *Blanks
     C  N09              Move      X'24'         AT12F
     C   09              Move      X'20'         AT12F
     C                   Else
     C                   Move      X'A7'         AT12F
     C                   EndIf
     C*Field#4
     C                   Movea     *blanks       @@ARR1
     C                   Movea     @@per         @@ARR2
     C                   z-add     1             i
     C                   If        FHFH#4<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT13P
     C                   Eval      @@len =%Len(%trim(FHFH#4))
     C                   If        @@len > 13
     C                   MoveL     FHFH#4        SCEHD4
     C                   Move      ':'           SCEHD4
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For odd length
     C                   If        @@rem <> 0
     C                   Movea     FHFH#4        @@ARR1
     C                   dow       i<=@@len
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Evenlength
     C                   Movea     FHFH#4        @@ARR1
     C                   dow       i<=@@len+1
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf                                                  Odd/Even length
     C                   movea     @@arr2        SCEHD4
     C                   EndIf                                                  @@len>13
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT13P
     C                   EndIf                                                  FHFH#4 <> *blanks
     C*Field#4 Second column
     C                   If        FHFH#4<>*Blanks
     C                             OR ELFFD4<> *Blanks
     C  N09              Move      X'24'         AT13F
     C   09              Move      X'20'         AT13F
     C                   Else
     C                   Move      X'A7'         AT13F
     C                   EndIf
     C*Field#5
     C                   Movea     *blanks       @@ARR1
     C                   Movea     @@per         @@ARR2
     C                   z-add     1             i
     C                   If        FHFH#5<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT14P
     C                   Eval      @@len =%Len(%trim(FHFH#5))
     C                   If        @@len > 13
     C                   MoveL     FHFH#5        SCEHD5
     C                   Move      ':'           SCEHD5
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Odd length
     C                   If        @@rem <> 0
     C                   Movea     FHFH#5        @@ARR1
     C                   dow       i<=@@len
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Even length
     C                   Movea     FHFH#5        @@ARR1
     C                   dow       i<=@@len+1
     C                   move      @@arr1(i)     @@arr2(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf                                                  Odd/Even length
     C                   movea     @@arr2        SCEHD5
     C                   EndIf                                                  @@len>13
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT14P
     C                   EndIf                                                  FHFH#5 <> *blanks
     C*Field#5 Second column
     C                   If        FHFH#5<>*Blanks
     C                             OR ELFFD5<> 0
     C  N09              Move      X'24'         AT14F
     C   09              Move      X'20'         AT14F
     C                   Else
     C                   Move      X'A7'         AT14F
     C                   EndIf
     C*Field#6
     C                   Movea     *blanks       @@ARR3
     C                   Movea     @@per25       @@ARR4
     C                   z-add     1             i
     C                   If        FHFH#6<> *Blanks
     C*Display  the field
     C                   Move      X'20'         AT15P
     C                   Eval      @@len =%Len(%trim(FHFH#6))
     C                   If        @@len > 19
     C                   MoveL     FHFH#6        SCEHD6
     C                   Move      ':'           SCEHD6
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Odd length
     C                   If        @@rem <> 0
     C                   Movea     FHFH#6        @@ARR3
     C                   dow       i<=@@len
     C                   move      @@arr3(i)     @@arr4(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Even length
     C                   Movea     FHFH#6        @@ARR3
     C                   dow       i<=@@len+1
     C                   move      @@arr3(i)     @@arr4(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf                                                  Odd/Even length
     C                   movea     @@arr4        SCEHD6
     C                   EndIf                                                  @@len>19
     C*Not display the field
     C                   Else
     C                   Move      X'A7'         AT15P
     C                   EndIf                                                  FHFH#6 <> *blanks
     C*Second column
     C                   If        FHFH#6<>*Blanks
     C                             OR ELQTYP <> *Blanks
     C  N09              Move      X'24'         QTYA
     C   09              Move      X'20'         QTYA
     C                   Else
     C                   Move      X'A7'         QTYA
     C                   EndIf
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   endif                                                  FLG132= 'Y'
     C*For field6 (Policy type at SCR01) when flg132 = 'N'
     C                   if        flg132 = 'N'
     C                   Movea     *blanks       @@ARR3
     C                   Movea     @@per25       @@ARR4
     C                   z-add     1             i
     C*Display  the field
     C                   Move      X'20'         AT15P
     C                   Eval      @@len =%Len(%trim(FHFH#6))
     C                   If        @@len > 19
     C                   MoveL     @@ptyp        SCEHD6
     C                   Move      ':'           SCEHD6
     C                   Else
     C                   Eval      @@rem =%Rem(@@len:2)
     C*For Odd length
     C                   If        @@rem <> 0
     C                   Movea     @@ptyp        @@ARR3
     C                   dow       i<=@@len
     C                   move      @@arr3(i)     @@arr4(i)
     c                   eval      i=i+1
     C                   enddo
     C                   Else
     C*For Even length
     C                   Movea     @@ptyp        @@ARR3
     C                   dow       i<=@@len+1
     C                   move      @@arr3(i)     @@arr4(i)
     c                   eval      i=i+1
     C                   enddo
     C                   EndIf                                                  Odd/Even length
     C                   movea     @@arr4        SCEHD6
     C                   EndIf                                                  @@len>19
     C*Second column
     C  N09              Move      X'24'         QTYA
     C   09              Move      X'20'         QTYA
     C                   endif                                                  flg132= 'N'
     C*** End Add *** 12/30/10 ***********************************************
     C*** End Add *** 07/28/10 ***********************************************
     C***  BEGIN ADD  ***  06/01/98  *********************************
     C     FNCGFL        IFEQ      'Y'
     C*
     C     FNMXEX        IFEQ      *ZERO
     C                   Z-ADD     12            FNMXEX
     C                   ENDIF                                                  FNMXEX=*ZERO
     C*
     C                   Z-ADD     FNMXEX        XXMXEX
     C                   Z-ADD     *ZEROS        XXMXCY
     C*
     C     XXMXEX        DOWGE     12
     C                   ADD       1             XXMXCY
     C                   SUB       12            XXMXEX
     C                   ENDDO                                                  XXMXEX>=12
     C*
     C                   ENDIF                                                  FNCGFL=Y
     C***  END   ADD  ***  06/01/98  *********************************
     C*
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET AGENT RECORDS
     C*
     C  N40              Z-ADD     ELFYR         XXFYR
     C   40              MOVE      *HIVAL        XXFYR
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     WCFL07        COMP      'Y'                                    66
     C**** BEGIN CHG ***** 07/22/98 ********
     C********** WCFL08    COMP 'Y'                      67
     C     WCFL08        COMP      'C'                                    67
     C**** END   CHG ***** 07/22/98 ********
     C***  Begin Chg  ***  07/06/11  **************************************
     C*****WCFL06        COMP      'Y'                                    69
     C                   eval      deductInstalled = WCFL06 = 'Y'
     C* Deductible display attributes
     C                   select
     C                   when      deductInstalled and *in09
     C                   eval      at30  = displayHigh
     C                   eval      at30p = normal
     C                   when      deductInstalled
     C                   eval      at30  = editLowLine
     C                   eval      at30p = normal
     C                   other
     C                   eval      at30  = hideProtect
     C                   eval      at30p = nonDisplay
     C                   endsl
     C***  End   Chg  ***  07/06/11  **************************************
     C***  BEGIN ADD  ***  02/05/99  **********************************
     C     WCFL14        COMP      'Y'                                    94
     C***  END   ADD  ***  02/05/99  **********************************
     C**** Begin Add *** 09/13/10 ***************************************
     C                   MOVE      *IN94         I@94
     C**** Begin Add *** 09/13/10 ***************************************
     C***  BEG ADD ******  02/18/03  **********************************
     C                   MOVE      *OFF          VNDRFL            1
     C     WCFL23        IFEQ      'Y'
     C                   MOVE      *ON           VNDRFL
     C                   ENDIF
     C***  END   ADD  ***  02/18/03  **********************************
     C***  BEG ADD ******  04/11/03  **********************************
     C                   MOVE      *OFF          ACHFLG            1
     C     WCFL24        IFEQ      'Y'
     C                   MOVE      *ON           ACHFLG
     C                   ENDIF
     C***  END   ADD  ***  04/11/03  **********************************
     C*
     C*  IF IN ADD MODE, DEFAULT THE NEXT FYR FOR DEFAULTS
     C*
      *** beg chg ****** 01/12/12 *************************************
     C******IN40*        IFEQ      *ON
      ***** BEG ADD ******* 11/09/01 **********************************
      **SET*FLAG*FOR "NO OTHER POLICY YEARS EXIST" TO BE USED IN
      **MOVING*DEFAULT DATES TO SCREEN
     C*****ELKY**        CHAIN     PEL01                              60
     C******IN60*        IFEQ      *ON
     C***********        MOVE      'N'           YRFLG
     C***********        ENDIF
      ***** END ADD ******* 11/09/01 **********************************
     C*****ELKY1***      SETGT     PEL01
     C*****ELKY****      READPE    PEL01                                  60
     C******IN60***      IFEQ      *OFF
     C*****ELFYR***      ADD       1             SCFYR
     C*****F1KY****      CHAIN     PF101                              60
     C*************      ELSE
      *************
     C*****BEGIN*CHG  ***  08/13/98  *********************************
     C***********F1KY1     SETGTPF101
     C***********F1KY      REDPEPF101                    60
     C*****F1KY2***      SETGT     PF102
     C*****F1KY****      READPE    PF102                                  60
     C*****END***CHG  ***  08/13/98  *********************************
     C*************
      ******BEG*DEL ******* 02/28/00 **********************************
     C*************        Z-ADDF1FYR     SCFYR
      ******END*DEL ******* 02/28/00 **********************************
      ******BEG*ADD ******* 02/28/00 **********************************
     C**N60********      Z-ADD     F1FYR         SCFYR
     C**N60********      SETON                                        15
      ******BEG*ADD ***** 04/07/05 ************************************
      **If*have*record, move common policy end date to work field
     C**N60********      Z-ADD     F1ENPO        ##ENPO            7 0
     C***60********      Z-ADD     *ZEROS        ##ENPO            7 0
     C*****FNPEOF**      IFEQ      *BLANK
     C*****##ENPO**      ANDNE     *ZEROS
     C*************      Z-ADD     ##ENPO        SCPTDT
     C*************      ELSE
     C*************      Z-ADD     F1ENPO        SCPTDT
     C*************      ENDIF
      ******END*ADD ***** 04/07/05 ************************************
      *************
      ******END*ADD ******* 02/28/00 **********************************
     C*************
     C*************      ENDIF                                                  *IN60=*OFF
     C*************
     C*************      ELSE
     C*************
     C*****F1KY1***      CHAIN     PF101                              60
     C*************
     C*************      ENDIF                                                  *IN40=*ON
     C*************
     C******Beg*Add ******* 12/08/11 **************************************
     C*************      EXSR      POLFYR
     C******End*Add ******* 12/08/11 **************************************
      *************
     C*****Begin*Add  ***  09/08/07  **************************************
     C*************      MOVEL     F1AAFL        SVAAFL
     C*************      MOVEL     F1APFL        SVAPFL
     C*****End***Add  ***  09/08/07  **************************************

     c                   if        @@mode = 'A'                                 add mode
      *
      *  If prior year exists for division, base the policy date range on the
      *  exp date of the prior year
      *
     C     ELKY1         SETGT     PEL01
     c     elky          readpe    pel01
     c                   if        not(%eof)
     c     *cymd         move      ELPTDT        workDate
     c     *usa          move      workDate      scpfdt
     c                   eval      scbsdt = scbsdt
     c     *cymd         move      ELPTDT        workDate
     c                   adddur    1:*years      workDate
     c     *usa          move      workDate      scptdt
     c                   eval      scfyr  = ELFYR + 1
     c                   eval      xxfyr  = scfyr
     c     f1ky1         chain     pf101
     c                   eval      scaafl = F1AAFL
     c                   eval      scaafl = F1APFL

     C                   ELSE
      *
      *  For new business, base state rpt div date range on the inception date. For PEO's
      *  and non-commerical business, the policy exp date is the fund year exp date. For
      *  commercial, the policy exp date is defaulted one year after the inception date.
      *  Base other divisions date range on state rpt div year.
      *
     c     *cymd         move      EMEFD1        workDate
     c     *usa          move      workDate      scpfdt
     c                   eval      scbsdt = scpfdt
     c                   select
     c                   when      stRptDiv = scdiv

     C     F1KY2         SETGT     PF102
     c     f1ky          readpe    pf102
     c                   if        not(%eof) and F1ENPO > EMEFD1
     c                   eval      scfyr = F1FYR
     c                   eval      xxfyr  = scfyr
     c                   eval      scaafl = F1AAFL
     c                   eval      scaafl = F1APFL
     c                   select
     c                   when      FNPEOF <> *blank or
     c                             FNCGFL <> 'Y'
     c     *cymd         move      F1ENPO        workDate
     c     *usa          move      workDate      scptdt
     c                   other
     c     *cymd         move      EMEFD1        workDate
     c                   adddur    1:*years      workDate
     c     *usa          move      workDate      scptdt
     c                   endsl
     C                   ENDIF

     c                   other

     c                   eval      kkdiv  = stRptDiv
     c                   eval      kkefdt = EMEFD1
     c     elky2l1       setgt     pel02
     c     elky2l        readpe    pel02
     c                   if        %found and l2ELPTDT > EMEFD1
     c                   eval      scfyr = l2ELFYR
     c     *cymd         move      l2ELPTDT      workDate
     c     *usa          move      workDate      scptdt
     c                   eval      scaafl = l2ELAAFL
     c                   eval      scapfl = l2ELAPFL
     C                   ENDIF                                                  *IN60=*OFF

     c                   endsl
     c                   endif                                                  not (%eof)
     C                   ENDIF                                                  *IN40=*ON
      *** end chg ****** 01/12/12 *************************************
     C*
     C     F1CLMS        COMP      'N'                                6767
     C*
     C*  GET SECURITY LEVEL OF THE USER
     C*
     C*** BEGIN DEL *** 09/14/04 **************************************
     C**********           MOVE *OFF      *IN41
     C*** END   DEL *** 09/14/04 **************************************
     C*
     C*** BEGIN ADD ***  10/28/97  ************************************
     C* PREPARE INDICATORS FOR DISPLAYING DISCOUNTS
     C                   MOVEA     '00'          *IN(43)
     C                   MOVEA     '000'         *IN(63)
     C*** END ADD *****  10/28/97  ************************************
     C*                                       2
     C***** BEG ADD ******* 08/10/98 **********************************
     C*
     C*  Get Employer Cross Reference info for header
     C*
     C     EXKY          CHAIN     PEX01                              92
      ***** BEG ADD ******* 06/22/99 **********************************
     C  N92              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/22/99 **********************************
     C  N92              Z-ADD     EXOEMP        SCOEMP
     C  N92              Z-ADD     EXODIV        SCODIV
     C*
     C***** END ADD ******* 08/10/98 **********************************
     F***  BEGIN ADD  ***  10/29/01  *********************************
      *
     C                   MOVE      *OFF          *IN06
     C     @@MODE        IFEQ      'C'
      *** BEG ADD **** 02/25/02 **********************************************
     C     BSCDFL        ANDEQ     *OFF
      *** END ADD **** 02/25/02 **********************************************
     C                   TIME                    DSDTTX
     C                   Z-ADD     DSDATX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXXDTE
     C     EAKY          CHAIN     PEA01                              60
     C*
      *** BEG CHG **** 02/25/02 **********************************************
     C*********  *IN60     IFEQ *OFF
     C*********  XXTRYM    ANDLTXXCUYM
     C*********  XXPOYM    ANDLTXXCUYM
     C     *IN60         DOWEQ     *OFF
     C     BSCDFL        ANDEQ     *OFF
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'H'
     C     XXTRYM        IFLT      XXCUYM
      *** END CHG **** 02/25/02 **********************************************
     C     XXPOYM        ANDLT     XXCUYM
     C                   MOVE      *ON           *IN06
      *** BEG ADD **** 02/25/02 **********************************************
     C                   LEAVE
      *** END ADD **** 02/25/02 **********************************************
     C                   ENDIF                                                  XXTRYM LT XXCUYM
     C                   ENDIF                                                  EAARTY EQ 'A'
      *** BEG ADD **** 02/25/02 **********************************************
     C     EAKY          READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 = *OFF
     C                   ENDIF                                                  @@MODE = 'C'
      *** END ADD **** 02/25/02 **********************************************
     F***  END   ADD  ***  10/29/01  *********************************
      *
      ***** BEG ADD ******* 07/20/00 **********************************
     C                   MOVE      'N'           XXOFLG
      ***** END ADD ******* 07/20/00 **********************************
      *+*+  BEG ADD *+*+*+* 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C                   MOVE      *BLANKS       SVRTCD
      * SET DISPLAY ATTRIBUTES
      *
      ***** BEGIN DEL ***** 04/13/07 **********************************
     C*********  CLTID     IFEQ '114'
     C*********            MOVE X'20'     AT1              NORMAL
     C*********            MOVE X'26'     AT2              UNDER HI
      *
     C*********  *IN09     IFEQ *ON
     C*********            MOVE X'A6'     AT2              UNDER HI PROT
     C*********            ENDIF
      *
     C*********            ELSE
     C*********            MOVE X'A7'     AT1              NON-DSP PROT
     C*********            MOVE X'A7'     AT2
     C*********            ENDIF
      ***** END DEL ******* 04/13/07 **********************************
      *
      *+*+  END ADD *+*+*+* 08/24/06 *+*+*+*+*+*+*+*+*+*+*+*+*+*+*+*+
     C*
      ***** BEG ADD ******* 04/17/07 **********************************
     C     CLTID         IFEQ      '071'
     C                   MOVE      X'20'         AT3                            NORMAL
     C                   MOVE      X'24'         AT4                            UNDER HI
      *
     C     *IN09         IFEQ      *ON
     C                   MOVE      X'A2'         AT4                            HI PROTECT
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      X'A7'         AT3                            NON-DSP PROT
     C                   MOVE      X'A7'         AT4
     C                   ENDIF
      ***** BEG ADD ******* 04/17/07 **********************************
     C***  BEG   Add  ***  07/16/07  **************************************
     C****
     C     CLTID         IFEQ      '093'
     C                   MOVE      X'20'         AT5                            NORMAL
     C                   MOVE      X'24'         AT6                            UNDER HI
     C     *IN09         IFEQ      *ON
     C                   MOVE      X'A2'         AT6                            HI PROTECT
     C                   ENDIF
     C                   ELSE
     C                   MOVE      X'A7'         AT5                            NON-DSP PROT
     C                   MOVE      X'A7'         AT6
     C                   ENDIF                                                  CLTID IFEQ
     C***  END   Add  ***  07/16/07  **************************************
      ***** BEG ADD ******* 03/06/07 **********************************
     C     FNKY          CHAIN     PEX02                              60
     C  N60              MOVE      EXAJFL        SVAJFL
     C*** BEGIN ADD ****** 10/20/09 ************************************
     C  N60              MOVE      EXASRM        SVASRM
     C*** END   ADD ****** 10/20/09 ************************************
     C                   MOVE      'N'           XX1ST             1
      ***** END ADD ******* 03/06/07 **********************************
     C***** BEG ADD ******* 07/27/07 **********************************
     C                   MOVE      *OFF          XXFLG
     C***** END ADD ******* 07/27/07 **********************************
     ****** BEGIN ADD ****** 03/14/08 *********************************
     C     CLTID         IFEQ      '136'
     C                   MOVE      X'20'         AT7                            NORMAL
     C                   MOVE      X'24'         AT8                            UNDER GREEN
      *
     C     *IN09         IFEQ      *ON
     C                   MOVE      X'A2'         AT8                            HI PROTECT
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      X'A7'         AT7                            NON-DSP PROT
     C                   MOVE      X'A7'         AT8
     C                   ENDIF
     ****** END ADD ******** 03/14/08 *********************************
      *** BEG ADD ***** 05/22/08 *********************************************
     C     CLTID         IFEQ      '093'
     C     EMVIP         IFEQ      'Y'
     C                   MOVE      X'21'         AT1
     C                   ELSE
     C                   MOVE      X'27'         AT1
     C                   ENDIF
     C                   ELSE
     C                   MOVE      X'A7'         AT1                            NON-DSP
     C                   ENDIF                                                  CLTID='093'
      *** END ADD ***** 05/22/08 *********************************************
     C*** Beg Add *** 11/16/09 ***********************************************
     C                   select
     C                   when      @@QTE = 'Q'
     C*** Beg Chg *** 07/28/10 ***********************************************
     C*****              eval      XXRQD1 = EXERQ1
     C*****              eval      XXRQD2 = EXERQ2
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'N'
     C                   eval      XXRQD1 = EXERQ1
     C                   eval      XXRQD2 = EXERQ2
     C                   endif
     C*
     C                   if        flg132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C                   eval      XXRQD1 = FHFH1Q
     C                   eval      XXRQD2 = FHFH2Q
     C                   eval      XXRQD3 = FHFH3Q
     C                   eval      XXRQD4 = FHFH4Q
     C                   eval      XXRQD5 = FHFH5Q
      *** Beg Add *** 10/25/10 ***********************************************
     C                   eval      XXRQD6 = FHFH6Q
      *** End Add *** 10/25/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   endif                                                  FLG132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
      *** End Chg *** 07/28/10 ***********************************************
     C                   other
     C*** Beg Chg *** 07/28/10 ***********************************************
     C*****              eval      XXRQD1 = EXERW1
     C*****              eval      XXRQD2 = EXERW2
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   if        flg132 = 'N'
     C                   eval      XXRQD1 = EXERW1
     C                   eval      XXRQD2 = EXERW2
     C                   endif
     C*
     C                   if        flg132 = 'Y'
     C*** End Add *** 12/30/10 ***********************************************
     C                   eval      XXRQD1 = FHFH1W
     C                   eval      XXRQD2 = FHFH2W
     C                   eval      XXRQD3 = FHFH3W
     C                   eval      XXRQD4 = FHFH4W
     C                   eval      XXRQD5 = FHFH5W
      *** Beg Add *** 10/25/10 ***********************************************
     C                   eval      XXRQD6 = FHFH6W
      *** End Add *** 10/25/10 ***********************************************
     C*** Beg Add *** 12/30/10 ***********************************************
     C                   endif                                                  FLG132 ='Y'
     C*** End Add *** 12/30/10 ***********************************************
      *** End Chg *** 07/28/10 ***********************************************
     C                   endsl
     C*
     C*** Beg Del *** 07/28/10 ***********************************************
     C******             if        XXRQD1 = 'Y'
     C*** Beg Chg *** 02/17/10 ***********************************************
     C******             move      editHighLine  AT9                            UNDER HI

     C******in09         ifeq      *on
     C******             move      displayHigh   AT9                            HI PROTECT
     C******             endif
      *
     C**********         move      *on           *in47                          HI int on SCFFD1
     C*** End Chg *** 02/17/10 ***********************************************
     C******             endif
     C*
     C******             if        XXRQD2 = 'Y'
     C*** Beg Chg *** 02/17/10 ***********************************************
     C******             move      editHighLine  AT10                           UNDER HI

     C******in09         ifeq      *on
     C******             move      displayHigh   AT10                           HI PROTECT
     C******             endif
      *
     C***********        move      *on           *in48                          HI int on SCFFD2
     C*** End Chg *** 02/17/10 ***********************************************
     C******             endif
     C*** End Del *** 07/28/10 ***********************************************
     C*** End Add *** 11/16/09 ***********************************************
     C***  Begin Add  ***  02/11/11  **************************************
     C* Display attributes for SCSCDT
     C                   select
     C                   when      WCFL44 = 'Y' and *in09
     C                   eval      at20  = displayHigh
     C                   eval      at20p = normal
     C                   when      WCFL44 = 'Y'
     C                   eval      at20  = editLowLine
     C                   eval      at20p = normal
     C                   other
     C                   eval      at20  = hideProtect
     C                   eval      at20p = nonDisplay
     C                   endsl
     C* Display attributes for SCSTCO
     C                   select
     C                   when      EMSTRP = 'Y'
     C                   eval      at21  = at20
     C                   eval      at21p = at20p
     C                   other
     C                   eval      at21  = hideProtect
     C                   eval      at21p = nonDisplay
     C                   endsl
     C***  End   Add  ***  02/11/11  **************************************
     C*
     C***  Begin Add  ***  08/25/10  **************************************
     C                   if        first
     C                   clear                   EFEXCF
     C                   clear                   EFEXCO
     C                   clear                   EFSCBD
     C     efKy          chain     pef01
     C                   eval      saveEFEXCF = EFEXCF
     C                   eval      saveEFEXCO = EFEXCO
     C                   eval      saveEFSCBD = EFSCBD
     C                   endif
     C***  End   Add  ***  08/25/10  **************************************
     C*
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   Z-ADD     1             XXSCRN            1 0
     C*** END   ADD ****** 01/14/10 ************************************
     C* Begin add ********** 03/01/11 **************************************
     C                   call      'SRCHKOBJ'
     C                   parm      'W0010R65'    objnam           10
     C                   parm      '*PGM'        xxtyp             7
     C                   parm                    objnotfnd         1
     C* End   add ********** 03/01/11 **************************************
     C*** beg add ***** 08/22/11 *******************************************
      *
      * Hide Overwrite 3c flag and label if WC1918 is not installed or clients
      * have modified code that interferes with WC1918
      *
     c                   if        cltid = '065' or
     c                             cltid = '093'
     c                   eval      objnotfnd = *on
     c                   endif

     c                   if        objnotfnd = *on
     c                   eval      at18f = hideProtect
     c                   eval      at19f = hideProtect
     c                   else
     c                   eval      WC1918instald = *on
     c                   endif
     C*** end add ***** 08/22/11 *******************************************
     C* Begin add ********** 03/11/11 **************************************
     c                   move      *zeros        @@cflg            1
     c                   if        cltid = '132' or cltid = '932'
     c                   eval      @@cflg ='1'
     c                   endif
     C* End   add ********** 03/11/11 **************************************
     c*** Begin delete ***** 02/03/12 **************************************
      *** BEG ADD ***** 05/18/11 *********************************************
     C* In Quote, If policy is in Renew to Quote status , Protect the Policy Exp date
     C* Used EMRQFL to know the status , could have used XXRQFL(Renew to Quote status
     C* of main reporting division) but assume both will be in sync
     C**%  @@QTE         IFEQ      'Q'
     C**%  EMRQFL        ANDEQ     'Y'
     C**%  *IN09         OREQ      *ON
     C**%                MOVE      X'A2'         AT9
     C**%                ENDIF                                                  protect exp date
      *** END ADD ***** 05/18/11 *********************************************
     c*** End   delete ***** 02/03/12 **************************************
     C                   ENDSR                                                  PGMSTR
     C/EJECT
     C*===============================================================
     C* PREOTH - SUBROUTINE FOR OTHER MODE PRE-PROCESSING
     C*===============================================================
     C*
     C     PREOTH        BEGSR
      *** BEG ADD ***** 12/18/05 **************************************
     C     CLTID         IFEQ      '136'
     C                   MOVE      *ON           *IN68
     C                   ENDIF
      *** END ADD ***** 12/18/05 **************************************
     C                   ENDSR
     C*
      ***** BEG ADD ******* 05/02/00 **********************************
      *
      *
      *===============================================================
      * SRBILL - CHECK NO BILLING AFTER POL EFF DT
      *===============================================================
      *
     C     SRBILL        BEGSR
      ***** BEG ADD ******* 08/14/00 **********************************
      *
      *  Only perform edit if employer employer not cancelled as of
      *  policy TO date.
      *
     C                   Z-ADD     SCCXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1CXDT
      *
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT3             7 0
      *
     C     SCCXDT        IFNE      *ZERO
     C     SRERR         ANDEQ     *OFF
     C     X1CXDT        ANDLE     XXDT3
      *                    *** DON'T PERFORM EDIT
      *
     C                   ELSE
      *
      ***** END ADD ******* 08/14/00 **********************************
      ***** BEG DEL ***** 03/20/06 ************************************
      **********
      ***Get*number of billing periods
      **********
     C**********           SELEC
     C********** BSAB12    WHNE *ZERO
     C**********           Z-ADD11        ##MTH
     C********** BSAB11    WHNE *ZERO
     C**********           Z-ADD10        ##MTH
     C********** BSAB10    WHNE *ZERO
     C**********           Z-ADD9         ##MTH
     C********** BSAB09    WHNE *ZERO
     C**********           Z-ADD8         ##MTH
     C********** BSAB08    WHNE *ZERO
     C**********           Z-ADD7         ##MTH
     C********** BSAB07    WHNE *ZERO
     C**********           Z-ADD6         ##MTH
     C********** BSAB06    WHNE *ZERO
     C**********           Z-ADD5         ##MTH
     C********** BSAB05    WHNE *ZERO
     C**********           Z-ADD4         ##MTH
     C********** BSAB04    WHNE *ZERO
     C**********           Z-ADD3         ##MTH
     C********** BSAB03    WHNE *ZERO
     C**********           Z-ADD2         ##MTH
     C********** BSAB02    WHNE *ZERO
     C**********           Z-ADD1         ##MTH
     C**********           OTHER
     C**********           Z-ADD0         ##MTH
     C**********           ENDSL
      **********
      ***Get*Bill start date (1st of month)
      **********
     C**********           Z-ADDSCBSDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDT1   70
      **********
     C**********           MOVELXXDT1     XXTMP1  50
     C**********           Z-ADD*ZERO     XXSTDT  70
     C**********           MOVELXXTMP1    XXSTDT
      **********
      *******BEG  ADD/CHG 03/20/06 ************************************
      **********
      **If*the*billing scheduling code is for the 15th, then add 15 to
      **the*billing start date
     C********** BSON15    IFEQ 'Y'
     C**********           ADD  15        XXSTDT
     C**********           ELSE
     C**********           ADD  1         XXSTDT
     C**********           ENDIF
      *******BEG  ADD/CHG 03/20/06 ************************************
      **********
     C**********           Z-ADDXXSTDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     ##DATE
      **********
      ***Calc*last acceptable policy effective date
      **********
     C**********           CALL 'SRFMTH8'
     C**********           PARM           ##DATE  80
     C**********           PARM           ##MTH   30
     C**********           PARM           ##ERR   1
      **********
     C**********           Z-ADD##DATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXLADT  70
      **********
      ***Compare pol TO date with date of last billing
      ***for*billing schedule code
      **********
     C**********           Z-ADDSCPTDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXDT3   70
      **********
     C********** ##ERR     IFEQ *OFF
     C********** SRERR     ANDEQ*OFF
     C********** XXLADT    ANDGEXXDT3
     C**********           SETON                     90  04
     C**********           ENDIF                           ##ERR
      ***** END DEL ***** 03/20/06 ************************************
      ***** BEG ADD ***** 03/20/06 ************************************
     C                   Z-ADD     12            J                 2 0
     C                   DO        12
     C     BS(J)         IFNE      *ZERO
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             J
     C                   ENDDO
     C                   Z-ADD     J             XXHIBS            2 0
      *
     C     SCBSDT        IFNE      *ZERO
     C                   Z-ADD     SCBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0
     C                   ELSE
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT
     C                   ENDIF
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
      *
      * Checks how many billings for the 1st will fall within the
      * effective dates
      *
     C                   Z-ADD     XXBGDT        XXDT
      *
     C     BSON15        IFEQ      'Y'
     C                   Z-ADD     15            XXDD
     C                   ELSE
     C                   Z-ADD     1             XXDD
     C                   ENDIF
      *
     C***  BEG   Add  ***  07/16/07  **************************************
     C*
     C     SCFFL2        IFEQ      'Y'
     C     CLTID         ANDEQ     '093'
     C                   MOVE      15            XXSTDT
     C                   ENDIF
     C***  END   Add  ***  07/16/07  **************************************
      *
      *** BEG ADD ***** 06/12/06 *************************************
     C                   Z-ADD     XXDT          XXBGDT
      *** END ADD ***** 06/12/06 *************************************
     C                   Z-ADD     0             XXINCT            2 0
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
      *
     C     XXHIBS        IFGT      XXINCT
     C                   SETON                                        90  04
     C                   ENDIF
      ***** END ADD ***** 03/20/06 ************************************
      *
      ***** BEG ADD ******* 08/14/00 **********************************
     C                   ENDIF                                                  CXDT
      ***** END ADD ******* 08/14/00 **********************************
      *
     C                   ENDSR                                                  SRBILL
      *
      *
      *
      ***** BEG ADD ******* 08/12/04 **********************************
      *===============================================================
      * SRRNTK - Create Renewal Tracking Record
      *===============================================================
      *
     C     SRRNTK        BEGSR
      *
     C                   MOVEL     'WC'          XSKEY             2
     C     XSKEY         CHAIN     SWC01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     WCFL26        ANDEQ     'Y'
      *
     C     RTKY          CHAIN     PRT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PRT01
     C                   ENDIF
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
     C                   ENDIF                                                  WCFL26
      *
     C                   ENDSR                                                  SRRNTK
      *
      *
      ***** END ADD ******* 08/12/04 **********************************
      *
     C/EJECT
      *
      *
      ***** END ADD ******* 05/02/00 **********************************
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C*===============================================================
     C* UPDTAB - SUBROUTINE FOR UPDATING DATA BASE RECORDS (WDABP)
     C*===============================================================
     C*
     C     UPDTAB        BEGSR
     C*
     C     ABKY          CHAIN     PAB01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  XXRST         ABRST
     C                   MOVEL(P)  Q1PGM         ABPGM
     C                   MOVEL(P)  Q1USER        ABUSER
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ABDT
     C*
     C                   MOVEL     DSTIME        ABTIME
     C*
     C                   UPDATE    PAB01
     C*
     C     ABKY          READE     PAB01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  UPDTAB
     C***  END   ADD  ***  07/19/99  *********************************
     C/EJECT
     C*
     C***  Begin Add  ***  11/22/04  **************************************
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
     C                   Z-ADD     ELFYR         XXFYR
     C                   MOVEL     ELUSER        SVUSER
     C                   MOVEL     ELPGM         SVPGM
     C                   Z-ADD     ELDT          SVDT
     C     1             ADD       ELTIME        SVTIME
     C*
     C                   MOVE      *IN60         SVIN60
     C                   MOVE      *IN61         SVIN61
     C*
     C     EMKY1         SETLL     PEM01                              60
     C*
     C     *IN60         DOUEQ     *ON
     C*
     C     EMKY1         READE     PEM01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C*
     C     ELKY2         CHAIN     PEL01                              61
     C*
     C     *IN61         IFEQ      *OFF
     C     ELDIV         ANDNE     SCDIV
     C*
     C***** BEG CHG ******* 12/18/09 **********************************
     C*****ELRRN         CHAIN     DBRCDF                             61
     C     ELKY6         CHAIN     PELL1                              61
     C***** END CHG ******* 12/18/09 **********************************
     C***** BEG ADD ******* 10/08/07 **********************************
     C**   *IN61         IFEQ      *ON
     C**                 LEAVE
     C**                 ENDIF
     C***** END ADD ******* 10/08/07 **********************************
     C*
     C                   MOVEL     SVRLFL        ELRLFL
     C                   MOVEL     SVUSER        ELUSER
     C                   MOVEL     SVPGM         ELPGM
     C                   Z-ADD     SVDT          ELDT
     C                   Z-ADD     SVTIME        ELTIME
     C*
     C***** BEG CHG ******* 12/18/09 **********************************
     C*******            UPDATE    DBRCDF
     C                   UPDATE    PELL1
     C***** END CHG ******* 12/18/09 **********************************
     C*
     C                   ADD       1             SVTIME
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   MOVE      *OFF          XXUPDA
     C*
     C                   MOVE      SVIN60        *IN60
     C                   MOVE      SVIN61        *IN61
     C*
     C                   ENDSR                                                  UPDALL
     C***  End   Add  ***  11/22/04  **************************************
     C*
     C/EJECT
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B02  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   MOVEL     DSTIME        ELTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E02  C                   ENDIF                                                  *IN99=*OFF
     C*
     C***  beg add ***** 04/21/09 *****************************************
      *
      * Move S/R billings off of state reporting division to originating division.
      *
     c                   if        svarsm = 'Y' and ELARSM <> 'Y'
     c                   exsr      srmvar
     c                   endif
     C***  end add ***** 04/21/09 *****************************************
     C***  Begin Add  ***  11/22/04  **************************************
     C* Update all divisions
     C     XXUPDA        IFEQ      *ON
     C                   EXSR      UPDALL
     C                   ENDIF
     C***  End   Add  ***  11/22/04  **************************************
     C*
     C***  BEGIN ADD  ***  07/19/99  *********************************
     C     SVRST         IFNE      XXRST
     C                   EXSR      UPDTAB
     C                   ENDIF                                                  SVRST<>XXRST
     C***  END   ADD  ***  07/19/99  *********************************
     C*** BEG ADD ***** 08/02/05 *************************************
     C*
     C* UPDATE ALL DIVISIONS WITH NEW INC LIMITS OR DED DISCOUNT CODE
     C*
     C     SCUPDT        IFEQ      'Y'
     C                   EXSR      SRUPDT
     C                   ENDIF
     C*** END ADD ***** 08/02/05 *************************************
     C*** BEG ADD ***** 04/06/06 *************************************
     C*
     C* UPDATE ALL DIVISIONS WITH NEW INC LIMITS OR DED DISCOUNT CODE
     C*
     C     XXUPDB        IFEQ      'Y'
     C                   EXSR      SRUPDB
     C                   ENDIF
     C*** END ADD ***** 04/06/06 *************************************
      *
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C/EJECT
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*** BEG ADD ***** 07/11/06 *************************************
     C     @@RRN2        CHAIN     WMEMP                              60
     C*** END ADD ***** 07/11/06 *************************************
     C*
     C                   Z-ADD     EMCO#         ELCO#
     C                   Z-ADD     EMFND         ELFND
     C                   Z-ADD     EMEMP#        ELEMP#
     C                   Z-ADD     EMDIV         ELDIV
     C                   Z-ADD     SCFYR         ELFYR
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        ELIUSR
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELIPGM
     C                   MOVEL     Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELIDT
     C                   MOVEL     SRDTO         ELDT
     C                   MOVEL     DSTIME        ELITM
     C                   MOVEL     DSTIME        ELTIME
     C*
     C                   WRITE     DBRCDF
      ***** BEG ADD ******* 12/06/06 **********************************
      *
      *  Create new Carrier #
      *
     C     @@QTE         IFNE      'Q'
      *
     C                   CALL      'WA200R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM      Q1PGM         #1PGM            10
      *
     C                   ENDIF
      *
      ***** END ADD ******* 12/06/06 **********************************
      ***** BEG ADD ******* 04/07/05 **********************************
      *
      *    Create PEO Certificate Record
      *
     C                   EXSR      CERTRD
      *
      ***** END ADD ******* 04/07/05 **********************************
      ***** BEG ADD ******* 08/12/04 **********************************
      *
      *    Create Renewal Tracking Record
      *
     C                   EXSR      SRRNTK
      *
      ***** END ADD ******* 08/12/04 **********************************
     C*
     C***  Begin Add  ***  12/21/07  **************************************
     C                   eval      saveDBRRN = DBRRN
     C***  End   Add  ***  12/21/07  **************************************
     C                   FEOD      WDELP
     C***  Begin Add  ***  12/21/07  **************************************
     C                   eval      DBRRN = saveDBRRN
     C***  End   Add  ***  12/21/07  **************************************
     C*
     C*** beg chg ***** 01/21/09 ******************************************
     C***  Begin Add  ***  01/20/06  **************************************
     C*****CLTID         IFEQ      '139'
     C*****BEGIN ADD  ***  11/02/07  **************************************
     C*****CLTID         OREQ      '146'
     C***  END ADD ******  11/02/07  **************************************
     c                   if        assignVendor = *on
     C*** end chg ***** 01/21/09 ******************************************
     C                   EXSR      WRTVDR
     C                   ENDIF
     C***  End   Add  ***  01/20/06  **************************************
     C*
     C*** BEGIN ADD ***  10/14/05  ************************************
     C*
     C                   Z-ADD     0             X1EMP#
     C                   Z-ADD     0             X1FYR
     C*
     C     *IN41         IFEQ      *OFF
     C*
     C     XSKY          CHAIN     PXS01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     XSKY1         CHAIN     PXS01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     EMEMP#        XSEMP#
     C                   Z-ADD     SCFYR         XSFYR
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
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XSKY          READE     PXS01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *IN41 IFEQ
     C*** END ADD *****  10/14/05  ************************************
     C                   MOVE      *ON           OUTFLG
     C*
     C                   ENDSR                                                  WRITER
     C*
     C***  Begin Add  ***  01/20/06  **************************************
     C*===============================================================
     C* WRTVDR - Write a Vendor record (WDVHP)
     C*===============================================================
     C     WRTVDR        BEGSR
     C*
     C                   CLEAR                   PVH01
     C*
     C                   Z-ADD     ELCO#         VHCO#
     C                   Z-ADD     ELFND         VHGRP
     C                   Z-ADD     ELEMP#        VHEMP#
     C                   Z-ADD     ELDIV         VHDIV
     C                   Z-ADD     ELFYR         VHFYR
     C                   Z-ADD     ELFND         VHVDR#
     C                   Z-ADD     ELPFDT        VHEFDT
     C*
     C                   MOVEL     Q1USER        VHUSER
     C                   MOVEL     Q1PGM         VHPGM
     C                   Z-ADD     ELDT          VHDT
     C                   Z-ADD     ELTIME        VHTIME
     C*
     C                   MOVEL     Q1USER        VHIUSR
     C                   MOVEL     Q1PGM         VHIPGM
     C                   Z-ADD     ELDT          VHIDT
     C                   Z-ADD     ELTIME        VHITIM
     C*
     C                   WRITE     PVH01
     C*
     C                   ENDSR
     C***  End   Add  ***  01/20/06  **************************************
     C*
      *** BEG ADD ***** 08/02/05 *************************************
     C*===============================================================
     C* SRUPDT - SUBROUTINE TO UPDATE ALL DIVISIONS WITH NEW INCREASED
     C*          LIMITS OR DEDUCTIBLE DISCOUNT CODE
     C*===============================================================
     C*
     C     SRUPDT        BEGSR
      *
     C     ELKYL1        CHAIN     PELL1                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  SCILCD        ELILCD
     C                   MOVEL(P)  SCINL2        ELINL2
     C                   MOVEL(P)  SCDCDE        ELDCDE
     C                   MOVEL(P)  SCDCD2        ELDCD2
      *
     C     SCILCD        IFNE      *BLANKS
      *
     C     SCILEF        IFNE      *ZERO
     C                   Z-ADD     SCILEF        X8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EFDT            7 0
     C                   ENDIF
      *
     C     SCILEX        IFNE      *ZERO
     C                   Z-ADD     SCILEX        X8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     X7EFDT        ELILEF
     C                   Z-ADD     0             ELILEX
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C                   Z-ADD     X7EFDT        ELILEF
     C                   Z-ADD     X7EXDT        ELILEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     0             ELILEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDLT     ELPFDT
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
     C                   MOVE      *BLANKS       ELILCD
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     X7EXDT        ELILEX
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
      *
     C                   ENDIF                                                  SCILCD <> *BLANKS
      *
     C     SCINL2        IFNE      *BLANKS
      *
     C     SCI2EF        IFNE      *ZERO
     C                   Z-ADD     SCI2EF        X8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EFDT            7 0
     C                   ENDIF
      *
     C     SCI2EX        IFNE      *ZERO
     C                   Z-ADD     SCI2EX        X8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     X7EFDT        ELI2EF
     C                   Z-ADD     0             ELI2EX
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     X7EFDT        ELI2EF
     C                   Z-ADD     X7EXDT        ELI2EX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   MOVE      *BLANKS       ELINL2
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      SCINL2        ELILCD
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     0             ELILEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDLT     ELPFDT
     C                   MOVE      *ZERO         ELILEF
     C                   MOVE      *ZERO         ELILEX
     C                   MOVE      *BLANKS       ELILCD
     C                   MOVE      *ZERO         ELI2EF
     C                   MOVE      *ZERO         ELI2EX
     C                   MOVE      *BLANKS       ELINL2
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDGT     ELPFDT
     C                   MOVE      SCINL2        ELILCD
     C                   Z-ADD     ELPFDT        ELILEF
     C                   Z-ADD     X7EXDT        ELILEX
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
     C     SCDCDE        IFNE      *BLANKS
      *
     C     SCDCEF        IFNE      *ZERO
     C                   Z-ADD     SCDCEF        X8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EFDT            7 0
     C                   ENDIF
      *
     C     SCDCEX        IFNE      *ZERO
     C                   Z-ADD     SCDCEX        X8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     X7EFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C                   Z-ADD     X7EFDT        ELDCEF
     C                   Z-ADD     X7EXDT        ELDCEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDLT     ELPFDT
     C                   MOVE      *BLANKS       ELDCDE
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     X7EXDT        ELDCEX
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
      *
     C                   ENDIF                                                  SCDCDE <> *BLANKS
      *
     C     SCDCD2        IFNE      *BLANKS
      *
     C     SC2DEF        IFNE      *ZERO
     C                   Z-ADD     SC2DEF        X8EFDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EFDT            7 0
     C                   ENDIF
      *
     C     SC2DEX        IFNE      *ZERO
     C                   Z-ADD     SC2DEX        X8EXDT            8 0
     C                   EXSR      SRDCVT
     C                   ELSE
     C                   MOVE      *ZERO         X7EXDT            7 0
     C                   ENDIF
      *
     C                   SELECT
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   Z-ADD     X7EFDT        EL2DEF
     C                   Z-ADD     0             EL2DEX
     C     X7EFDT        WHENGT    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDGT     ELPFDT
     C                   Z-ADD     X7EFDT        EL2DEF
     C                   Z-ADD     X7EXDT        EL2DEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDEQ     *ZERO
     C                   MOVE      *BLANKS       ELDCD2
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      SCINL2        ELDCDE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     0             ELDCEX
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDLT     ELPFDT
     C                   MOVE      *ZERO         ELDCEF
     C                   MOVE      *ZERO         ELDCEX
     C                   MOVE      *BLANKS       ELDCDE
     C                   MOVE      *ZERO         EL2DEF
     C                   MOVE      *ZERO         EL2DEX
     C                   MOVE      *BLANKS       ELDCD2
     C     X7EFDT        WHENLE    ELPFDT
     C     X7EXDT        ANDNE     *ZERO
     C     X7EXDT        ANDGT     ELPFDT
     C                   MOVE      SCDCD2        ELDCDE
     C                   Z-ADD     ELPFDT        ELDCEF
     C                   Z-ADD     X7EXDT        ELDCEX
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
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   MOVEL     DSTIME        ELTIME
     C                   UPDATE    PELL1
     C     ELKYL1        READE     PELL1                                  60
     C                   ENDDO
      *
     C                   ENDSR
      *===============================================================
      * SRDCVT - DATE CONVERSION SUBROUTINE FOR SRUPDT
      *===============================================================
     C     SRDCVT        BEGSR
      *
     C                   Z-ADD     X8EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7EFDT
      *
     C                   Z-ADD     X8EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7EXDT
      *
     C                   ENDSR
      *** END ADD ***** 08/02/05 *************************************
      *** BEG ADD ***** 04/06/06 *************************************
      *====================================================
      * SRUPDB - UPDATE BSCD'S IN ALL SUMMARIZED DIVISIONS
      *====================================================
     C     SRUPDB        BEGSR
     C     ELKYL1        SETLL     PELL1                              60
     C     ELKYL1        READE     PELL1                                  60
     C     *IN60         DOWEQ     *OFF
     C     ELARSM        IFEQ      'Y'
     C                   MOVE      SCBSCD        ELBSCD
     C                   UPDATE    PELL1
     C                   ENDIF
     C     ELKYL1        READE     PELL1                                  60
     C                   ENDDO
     C                   ENDSR
      *** END ADD ***** 04/06/06 *************************************
     C***** BEG ADD ******* 07/27/07 **********************************
      *====================================================
      * CHKFA - VERIFY FIELDS NOT BE AVAILABLE TO COPY IN
      *         DIVISION
      *====================================================
     C     CHKFA         BEGSR
     C                   OPEN      WDELP
     C                   OPEN      WDELL
     C                   OPEN      WMEPL
      *****
      * SAVE USER,DATE AND TIME
      *****
     C                   MOVEL     Q1USER        S2USER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         S2DATE
     C                   MOVEL     DSTIME        S2TIME
      *****
      *WRITE DUMMY RECORDS IN WXUAP(For Divisions)
      *****
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     ELCO#         @#CO#
     C                   MOVEL     ELDIV         @#DIV
     C                   MOVEL     ELEMP#        @#EMP#
     C                   MOVEL     ELFND         @#FND
     C                   MOVEL     ELFYR         @#FYR
     C***** END CHG ******* 10/08/07 **********************************
     C     FNPEOF        IFNE      'M'
     C     DKKY15        SETLL     PELL2
     C     DKKY15        READE     PELL2                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     SCILCD        IFNE      S@ILCD
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     INCR          UADATA
     C******               MOVEL'ELILCD'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  INCR LIMIT CODE
      *
      *****
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@ILEF        IFNE      S@ILEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INEF          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  INCR LIM EFF DATE
      *
     C     X@ILEX        IFNE      S@ILEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INEX          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  INCR LIM EFF DATE
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     SCDCDE        IFNE      S@DCDE
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEDC          UADATA
     C******               MOVEL'ELDCDE'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  DEDUCTIBLE DISCOUNT
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@DCEF        IFNE      S@DCEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEEF          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  DEDU DIS EFF DATE
      *
     C     X@DCEX        IFNE      S@DCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEEX          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  DEDU DIS EXF DATE
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     SCASP2        IFNE      S@ASP2
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEVN          UADATA
     C*********            MOVEL'ELASP2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@DVEF        IFNE      S@DVEF
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 06/21/10 **********************************
     C************       MOVEL     DEEF          UADATA
     C                   MOVEL     DVEF          UADATA
     C***** END CHG ******* 06/21/10 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C***** BEG CHG ******* 06/21/10 **********************************
     C**********     X@DVEX        IFNE      X@DVEX
     C     X@DVEX        IFNE      S@DVEX
     C***** END CHG ******* 06/21/10 **********************************
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 06/21/10 **********************************
     C***********        MOVEL     DEEX          UADATA
     C                   MOVEL     DVEX          UADATA
     C***** END CHG ******* 06/21/10 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** END CHG ******* 10/08/07 **********************************
      *
     C***** BEG ADD ******* 06/21/10 **********************************
     C     SCD1S2        IFNE      S@D1S2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN1SP2      UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     X@D1D2        IFNE      S@D1D2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DVEF1SP2      UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@D1X2        IFNE      X@D1X2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DVEX1SP2      UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** END ADD ******* 06/21/10 **********************************
     C     SCDEV2        IFNE      S@DEV2
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEVN2         UADATA
     C*********            MOVEL'ELDEV2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     SCSTFC        IFNE      S@STFC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     STFN2         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@D2EF        IFNE      S@D2EF
     C                   MOVEL     *BLANKS       UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     DE2EF         UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     STFEF         UADATA
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@D2EX        IFNE      S@D2EX
     C                   MOVEL     *BLANKS       UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     DE2EX         UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     STFEX         UADATA
     C                   ENDIF                                                  CLTID<> '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     SCD2S2        IFNE      S@D2S2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2SP2      UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     X@D2D2        IFNE      S@D2D2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DE2EFSP2      UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     X@D2X2        IFNE      S@D2X2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DE2EXSP2      UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*** END   ADD ****** 06/21/10 ************************************
      *
     C     SCEXM1        IFNE      S@EXM1
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXMO1         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@EMD1        IFNE      S@EMD1
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXEF1         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     SCEXM2        IFNE      S@EXM2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXMO2         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@EMD2        IFNE      S@EMD2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXEF2         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     SCEXM3        IFNE      S@EXM3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXMO3         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@EMD3        IFNE      S@EMD3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXEF3         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@PFDT        IFNE      S@PFDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  POLICY FROM DATE
      *
     C     X@PTDT        IFNE      S@PTDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLT          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  POLICY TO DATE
      *
     C     SCARSM        IFNE      S@ARSM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUMR          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  A/R SUMMARY
      *
     C     SCIFFL        IFNE      S@IFFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ORDC          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  EXEMPT FEE
      *
     C***** BEG CHG ******* 10/08/07 **********************************
     C     SCQTYP        IFNE      S@QTYP
     C*********  SCQTYP    IFNE S@MBCD
     C***** END CHG ******* 10/08/07 **********************************
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLY          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  TYPE OF POLICY
      *
     C     SCADVR        IFNE      S@ADVR
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ADVF          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  ADVERSE ACC FLAG
      *
     C     SVRLFL        IFNE      S@RLFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DNRN          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  DO NOT RENEW
      *
     C     X@BSDT        IFNE      S@BSDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     BILS          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  BILL START DATE
      *
     C     SCAFLG        IFNE      S@STAT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ARFG          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  A-RATING
      *
     C     SCCXBL        IFNE      S@CXBL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEBI          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  DEDUCT BILL
      *
     C     SCMCCD        IFNE      S@MCCD
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MCOT          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  MCO TYPE
      *
     C     SCMCOD        IFNE      S@MCGP
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MCOT          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  MCO GROUP
      *
     C     SCMCOD        IFNE      S@MCGP
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MGOT          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  MCO GROUP
      *
     C     SCSRPR        IFNE      S@SRPR
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SHPF          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  SHORT RATE
      *
     C     SCASP3        IFNE      S@ASP3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DICO          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  STOCK DISC%
      *
     C     SCPROM        IFNE      S@PROM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INPR          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  INTERNALLY PROM
      *
     C     SCCXDY        IFNE      S@CXDY
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CXND          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  CANCELLATION NOT
      *
     C     SCPP3C        IFNE      S@PP3C
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     PPIC          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  POLICY PAGE ITEM
      *
     C     SCPPC3        IFNE      S@PPC3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     PP3C          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  POLICY PAGE ITEM
      *
     C     SCSFPC        IFNE      S@SFPC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SAFP          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  SAFETY FEE %
      *
     C     SCSFMN        IFNE      S@SFMN
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SAMA          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  SAFETY MIN
      *
     C     SCADDM        IFNE      S@ADDM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     AMAL          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  ADDITIONAL MAILING
      *
     C     X@ANDT        IFNE      S@ANDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ANRD          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  ANNIVERSARY RATING
      *
     C     SCEXMF        IFNE      S@EXMF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ECMD          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF                                                  EXCLUDE MEDICAL
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     SCASP1        IFNE      S@ASP1
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     CONTR         UADATA
     C*********            MOVEL'ELASP1'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@CREF        IFNE      S@CREF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONEF         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@CREX        IFNE      S@CREX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONEX         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     SCASPM        IFNE      S@ASPM
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     SUBJA         UADATA
     C*********            MOVEL'ELASPM'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@MCEF        IFNE      S@MCEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBEF         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@MCEX        IFNE      S@MCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBEX         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     SCASPS        IFNE      S@ASPS
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     SUBJB         UADATA
     C*******              MOVEL'ELASPS'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@SCEF        IFNE      S@SCEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBBE         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@SCEX        IFNE      S@SCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBBX         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     SCDBCR        IFNE      S@DBCR
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     SCHFL         UADATA
     C*********            MOVEL'ELDBCR'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** BEG CHG ******* 10/08/07 **********************************
     C     X@SDEF        IFNE      S@SDEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SCHEF         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@SCEX        IFNE      S@SCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SCHEX         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** END CHG ******* 10/08/07 **********************************
      *
     C     SCDISC        IFNE      S@DISC
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DISCT         UADATA
     C*********            MOVEL'ELDISC'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     E@WVTP        IFNE      S@WVTP
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     WAIVE         UADATA
     C*******              MOVEL'ELWVTP'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     E@WVFL        IFNE      S@WVFL
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     LVLPR         UADATA
     C*******              MOVEL'ELWVFL'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     E@WVPC        IFNE      S@WVPC
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     WSPEC         UADATA
     C*******              MOVEL'ELWVPC'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     E@WMIN        IFNE      S@WMIN
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     MINWAV        UADATA
     C*******              MOVEL'ELWMIN'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     E@FCHG        IFNE      S@FCHG
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     FLATCH        UADATA
     C*******              MOVEL'ELFCHG'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     E@WMAX        IFNE      S@WMAX
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     MAXWAV        UADATA
     C******               MOVEL'ELWMAX'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C***** BEG ADD ******* 10/08/07 **********************************
     C     SCDCD2        IFNE      S@DCD2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEDC2         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@2DEF        IFNE      S@2DEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DED2E         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@2DEX        IFNE      S@2DEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DED2X         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     SCINL2        IFNE      S@INL2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCR2         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@I2EF        IFNE      S@I2EF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCEF         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     X@I2EX        IFNE      S@I2EX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCEX         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     SCASCD        IFNE      S@ASTS
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ACTST         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     SCCOFL        IFNE      S@COFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SHCOF         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     SCTYPB        IFNE      S@TYPB
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     TYPBC         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     SCFCOD        IFNE      S@PFIN
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     PRFIN         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     SCCTST        IFNE      S@CTST
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONAU         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** BEG CHG ******* 10/08/07 **********************************
     C     SCMFEE        IFNE      S@MFEE
     C*********  SCMFEE    IFNE S@MFAM
     C***** END CHG ******* 10/08/07 **********************************
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MBRA          UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C***** END ADD ******* 10/08/07 **********************************
     C*
     C***  Begin Add  ***  08/25/10  **************************************
     C                   if        EFEXCF <> saveEFEXCF
     C                   eval      UADATA = 'Exclude employer from claim fee'
     C                   exsr      WrtFA1
     C                   endif
     C*
     C                   if        EFEXCO <> saveEFEXCO
     C                   eval      UADATA = 'Exclude employer from collateral'
     C                   exsr      WrtFA1
     C                   endif
     C*
     C                   if        EFSCBD <> saveEFSCBD
     C                   eval      UADATA = 'Stop collateral billing date'
     C                   exsr      WrtFA1
     C                   endif
     C***  End   Add  ***  08/25/10  **************************************
      *
     C     DKKY15        READE     PELL2                                  60
     C                   ENDDO
     C                   ENDIF
     C*
      *****
      * POPULATE XXST WITH RSJRST
      *****FOR DIVISIONS
      *****
     C     FNPEOF        IFNE      'M'
     C     DKKY15        SETLL     PELL2
     C     DKKY15        READE     PELL2                                  60
     C     *IN60         DOWEQ     *OFF
     C     SCDIV         IFNE      E2DIV
     C                   MOVEL     *BLANKS       RSJRST
     C     DKKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  RSJRST        XXST
     C                   ENDIF
      *****
      * CHECK ELILCD
      *****
     C     SCILCD        IFNE      S@ILCD
     C                   MOVEL(P)  ELILCD        XXKEY
     C     DKKY1         SETGT     PLM01
     C     DKKY2         READPE    PLM01                                  60
      *
     C*** WRITE RECORD TO WXUAL
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     INCR          UADATA
     C******               MOVEL'ELILCD'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C*********            MOVEL*BLANKS   @@ST
     C*********  DKKY12    CHAINPUA01                60
     C*********  *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C*********            ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCILCD NE ELILC
     C*
      *****
      * CHECK ELDCDE
      *****
     C     SCDCDE        IFNE      S@DCDE
     C                   MOVEL(P)  ELDCDE        XXDDCD
     C     DKKY3         SETGT     PDD01
     C     DKKY4         READPE    PDD01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEDC          UADATA
     C******               MOVEL'ELDCDE'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDCDE NE ELDSD
      *****
      * CHECK ELINL2
      *****
     C     SCINL2        IFNE      S@INL2
     C                   MOVEL(P)  ELINL2        XXKEY
     C     DKKY1         SETGT     PLM01
     C     DKKY2         READPE    PLM01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     INCR2         UADATA
     C*********            MOVEL'ELINL2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCINL2 NE S@INL
      *****
      * CHECK ELDCD2
      *****
     C     SCDCD2        IFNE      S@DCD2
     C                   MOVEL(P)  ELDCD2        XXDDCD
     C     DKKY3         SETGT     PDD01
     C     DKKY4         READPE    PDD01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEDC2         UADATA
     C******               MOVEL'ELDCD2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDCD2 NE S@DCD
      *****
      * CHECK ELASP2
      *****
     C     SCASP2        IFNE      S@ASP2
     C     DKKY5         SETGT     PRR01
     C     DKKY6         READPE    PRR01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEVN          UADATA
     C*********            MOVEL'ELASP2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C***** BEG ADD ******* 03/11/11 **********************************
     C     @@CFLG        ANDNE     '1'
     C     *IN60         OREQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDNE     'NM'
     C     *IN60         OREQ      *OFF
     C     RRDV1L        ANDEQ     *BLANKS
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDEQ     'NM'
     C***** END ADD ******* 03/11/11 **********************************
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEVN          UADATA
     C*********            MOVEL'ELASP2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCASP2 NE S@ASP2
      *
     C***** BEG ADD ******* 06/21/10 **********************************
      *****
      * CHECK ELD1S2
      *****
     C     SCD1S2        IFNE      S@D1S2
     C     DKKY5         SETGT     PRR01
     C     DKKY6         READPE    PRR01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN1SP2      UADATA
     C                   EXSR      WRTFA
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C***** BEG ADD ******* 03/11/11 **********************************
     C     @@CFLG        ANDNE     '1'
     C     *IN60         OREQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDNE     'NM'
     C     *IN60         OREQ      *OFF
     C     RRDV1L        ANDEQ     *BLANKS
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDEQ     'NM'
     C***** END ADD ******* 03/11/11 **********************************
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN1SP2      UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCD1S2 NE S@D1S2
     C***** END ADD ******* 06/21/10 **********************************
      *****
      * CHECK ELDEV2
      *****
     C     *IN60         IFEQ      *ON
     C     SCDEV2        ANDNE     S@DEV2
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEVN2         UADATA
     C************         MOVEL'ELDEV2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCDEV2        ANDNE     S@DEV2
     C     RRDV2C        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     DEVN2         UADATA
     C************         MOVEL'ELDEV2'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
     C*** BEG   ADD ****** 06/21/10 ************************************
      *****
      * CHECK ELD2S2
      *****
     C     *IN60         IFEQ      *ON
     C     SCD2S2        ANDNE     S@D2S2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2SP2      UADATA
     C                   EXSR      WRTFA
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCD2S2        ANDNE     S@D2S2
     C     RRDV2C        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2SP2      UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C*** END   ADD ****** 06/21/10 ************************************
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C* Check Stabilization factor
     C     *IN60         IFEQ      *ON
     C     SCSTFC        ANDNE     S@STFC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     STFN2         UADATA
     C                   EXSR      WRTFA
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCSTFC        ANDNE     S@STFC
     C     RRDV2C        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     STFN2         UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C*** END   ADD ****** 01/14/10 ************************************
      *
      *****
      * CHECK ELASP1
      *****
     C     *IN60         IFEQ      *ON
     C     SCASP1        ANDNE     S@ASP1
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     CONTR         UADATA
     C*********            MOVEL'ELASP1'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCASP1        ANDNE     S@ASP1
     C     RRCCCD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     CONTR         UADATA
     C*********            MOVEL'ELASP1'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
      *****
      * CHECK ELASPM
      *****
     C     *IN60         IFEQ      *ON
     C     SCASPM        ANDNE     S@ASPM
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     SUBJA         UADATA
     C*************        MOVEL'ELASPM'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCASPM        ANDNE     S@ASPM
     C     RR1ACD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     SUBJA         UADATA
     C*************        MOVEL'ELASPM'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
      *****
      * CHECK ELASPS
      *****
     C     *IN60         IFEQ      *ON
     C     SCASPS        ANDNE     S@ASPS
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     SUBJB         UADATA
     C*********            MOVEL'ELASPS'  UADATA
     C***** END CHG ******* 10/08/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCASPS        ANDNE     S@ASPS
     C     RR1BCD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C                   MOVEL     SUBJB         UADATA
     C*********            MOVEL'ELASPS'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
      *****
      * CHECK ELDBCR
      *****
     C     *IN60         IFEQ      *ON
     C     SCDBCR        ANDNE     S@DBCR
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     SCHFL         UADATA
     C*********            MOVEL'ELDBCR'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCDBCR        ANDNE     S@DBCR
     C     RRDBCD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     SCHFL         UADATA
     C*********            MOVEL'ELDBCR'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *****
      * CHECK WAIVER SURCHARGE
      *****
     C     *IN60         IFEQ      *ON
     C     E@WVTP        ANDNE     S@WVTP
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     WAIVE         UADATA
     C*********            MOVEL'ELWVTP'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WVTP    ANDNES@WVTP
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WVFL        ANDNE     S@WVFL
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     LVLPR         UADATA
     C*************        MOVEL'ELWVFL'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WVFL    ANDNES@WVFL
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WVPC        ANDNE     S@WVPC
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     WSPEC         UADATA
     C*************        MOVEL'ELWVPC'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WVPC    ANDNES@WVPC
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WMIN        ANDNE     S@WMIN
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     MINWAV        UADATA
     C*************        MOVEL'ELWMIN'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WMIN    ANDNES@WMIN
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@FCHG        ANDNE     S@FCHG
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     FLATCH        UADATA
     C*************        MOVEL'ELFCHG'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@FCHG    ANDNES@FCHG
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WMAX        ANDNE     S@WMAX
     C                   MOVEL     *BLANKS       UADATA
     C***** BEG CHG ******* 10/01/07 **********************************
     C                   MOVEL     MAXWAV        UADATA
     C*************        MOVEL'ELWMAX'  UADATA
     C***** END CHG ******* 10/01/07 **********************************
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WMAX    ANDNES@WMAX
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*ON
      *
     C***                  ELSE
      *
     C     *IN60         IFEQ      *OFF
     C     E@WVTP        ANDNE     S@WVTP
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WAIVE         UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WVTP    ANDNES@WVTP
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WVFL        ANDNE     S@WVFL
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     LVLPR         UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WVFL    ANDNES@WVFL
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WVPC        ANDNE     S@WVPC
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WSPEC         UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WVPC    ANDNES@WVPC
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WMIN        ANDNE     S@WMIN
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MINWAV        UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@WMIN    ANDNES@WMIN
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@FCHG        ANDNE     S@FCHG
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     FLATCH        UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** E@FCHG    ANDNES@FCHG
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WMAX        ANDNE     S@WMAX
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MAXWAV        UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C***********        MOVEL     *BLANKS       @@ST
     C*****DKKY12**      CHAIN     PUA01                              60
     C**** *IN60***      IFEQ      *OFF
     C**** E@WMAX        ANDNE     S@WMAX
     C                   EXSR      WRTFA
     C********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF                                                  *IN60=*OFF
      *
     C*******              ENDIF                           *IN60=*ON
      *****
      * CHECK ELDISC
      *****
     C     DKKY7         SETGT     PDS01
     C     DKKY8         READPE    PDS01                                  60
     C***                MOVEL     *BLANKS       @#DISC
     C***                MOVEL     E2DISC        @#DISC
      *
     C     SCDISC        IFNE      *BLANK
     C     *IN60         ANDEQ     *ON
     C**   e2asp3        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DISCT         UADATA
     C***** BEG CHG ******* 10/08/07 **********************************
     C**********           MOVEL*BLANKS   @@ST
     C********** DKKY12    CHAINPUA01                60
     C********** *IN60     IFEQ *OFF
     C********** SCDISC    ANDNES@DISC
     C                   EXSR      WRTFA
     C**********           ENDIF
     C***** END CHG ******* 10/08/07 **********************************
     C                   ENDIF
     C*
     C*  POLICY FROM DATE CANNOT BE LESS THAN INITIAL EFFECTIVE DATE(DIVISION)
      *
HS01 C     X@PFDT        IFNE      S@PFDT
     C                   Z-ADD     E2FND         @@FND
     C                   Z-ADD     E2EMP#        @@EMP#
     C                   Z-ADD     E2DIV         @@DIV
     C                   Z-ADD     E2FYR         @@FYR
     C*
     C     DMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C  N90XXPFDT        IFLT      EMEFD1
     C                   SETON                                        29  90
     C                   ENDIF                                                  XXPF<EMEFD1
     C                   ENDIF                                                  CHAIN PEM01
      *
     C     *IN29         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN29 = *ON
     C*
     C*  POLICY FROM DATE MUST BE LESS THAN OR EQUAL TO ALL EXISTING
     C*  PAYROLL RECORDS(FOR ALL DIVISIONS)
     C*
     C                   MOVE      *ZEROS        *IN90
     C  N90DKKY13        CHAIN     PEP01                              60
     C  N90*IN60         DOWEQ     *OFF
     C*
     C     EPEFDT        IFLT      XXPFDT
     C     EPSTAT        ANDNE     'I'
     C                   SETON                                        306090
     C                   ENDIF                                                  EPEFDT<@@PF/EPSTAT<>
      *
     C     *IN30         IFEQ      *ON
     C     *IN60         ANDEQ     *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN3060=*ON
     C*
     C  N90DKKY13        READE     PEP01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C                   ENDIF
      *
     C*  EDIT POLICY TO DATE
     C*  DATES MUST BE ASCENDING ORDER(FOR DIVISIONS)
     C*
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        31  90
     C                   ENDIF                                                  XXDAT2<XXDAT1
     C     *IN31         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLT          UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN31 = *ON
     C*
      *===============================================================
      * CKPOLD - CHECK IF CLAIMS OUTSIDE NEW POLICY EFF DT ENTERED(FOR DIVISIONS)
      *===============================================================
      *
     C     XXOFLG        IFEQ      'N'
     C                   MOVE      'N'           XXEFLG            1            ERROR FOUND
      *
     C**   @@RRN         CHAIN(N)  WDELP                              60
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
      *
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
      *
      *  Check for claims outside policy eff dt range
      *
     C     XXPFDT        IFGT      E2PFDT
     C                   Z-ADD     E2PFDT        XXDATE
      *
     C     DKKY16        SETLL     PCM01
     C     DMKY          READE     PCM01                                  60
      *
     C                   MOVE      *ZEROS        *IN90
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     XXPFDT
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     XXPMTS        IFNE      *ZEROS                                       CLSD CLM W/PMTS
     C     CMSTAT        ORNE      'C'                                          OPN CLM
     C                   SETON                                        90
     C                   MOVE      'Y'           XXEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN90=*ON
      *
     C     DMKY          READE     PCM01                                  60
     C                   ENDDO                                                  PFDT
      *
     C                   ENDIF                                                  PFDT
      *
      *
      *
     C  N90XXPTDT        IFLT      E2PTDT
     C     XXEFLG        ANDEQ     'N'
     C                   Z-ADD     XXPTDT        XXDATE
     C     DKKY16        SETLL     PCM01
     C     DMKY          READE     PCM01                                  60
      *
     C                   MOVE      *ZEROS        *IN90
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     E2PTDT
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     XXPMTS        IFNE      *ZEROS                                       CLSD CLM W/PMTS
     C     CMSTAT        ORNE      'C'                                          OPN CLM
     C                   SETON                                        90
     C                   MOVE      'Y'           XXEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLT          UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN90=*ON
      *
     C     DMKY          READE     PCM01                                  60
     C                   ENDDO                                                  PTDT
      *
     C                   ENDIF                                                  PTDT
      *
     C                   ENDIF                                                  XXOFLG
      *
     C*  ANNIV DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE(FOR DIVISIONS)
     C*
     C     X@ANDT        IFNE      S@ANDT
     C                   MOVE      *ZEROS        *IN38
     C     XXANDT        IFGT      E2PFDT
     C                   SETON                                        38  90
     C                   ENDIF                                                  XXANDT>XXPF
     C     *IN38         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     OPTNM         UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN38 = *ON
     C                   ENDIF                                                  Anniversary date
      *
     C* BILLING START DATE CANNOT BE GREATER THAN POLICY EFFECTIVE DATE
     C*
     C     X@BSDT        IFNE      S@BSDT
     C                   MOVE      *ZEROS        *IN68
     C     XXDT1         IFGT      E2PFDT
     C                   SETON                                        68  90
     C                   ENDIF
     C     *IN68         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     BILS          UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN68 = *ON
     C*
     C* DO NOT ALLOW BILLING START DATE TO BE PRIOR TO CURRENT DATE IF
     C* WE ARE CHANGING THE VALUE OF THE FIELD OR ADDING A RECORD.
     C* UNLESS THE CHANGE IS THE BILLING START DATE TO = POLICY FROM DATE
     C*
     c                   MOVE      *ZEROS        *IN95
     C     *IN40         IFEQ      *ON
     C     XXDT2         ANDGT     SYSDT
     C     *IN40         OREQ      *OFF
     C     XXDT1         ANDNE     E2BSDT
     C*
     C     XXDT1         IFLT      SYSDT
     C     XXDT1         ANDNE     XXDT2
     C     SCARSM        ANDNE     'Y'
     C                   SETON                                        95  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C                   ENDIF                                                  Bill start date
     C*
     C     *IN95         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     BILS          UADATA
     C                   EXSR      WRTFA
     C                   ENDIF                                                  *IN95 = *ON
     C*
     C***** END CHG ******* 10/18/07 **********************************
      *****
     C                   MOVEL     S2DATE        QQDATE
     C                   MOVEL     S2USER        QQUSER
     C                   MOVEL     S2TIME        QQTIME
     C                   MOVEL     SCCXFL        QQCXFL            1
     C*
     C                   ENDIF                                                  SCDIV<>E2DIV
     C     DKKY15        READE     PELL2                                  60
     C                   ENDDO
     C                   ENDIF                                                  <> 'M'
      *
     C***** BEG CHG ******* 10/08/07 **********************************
      *****
      *WRITE DUMMY RECORDS IN WXUAP(For Policies)
      *****
     C     FNPEOF        IFEQ      'M'
     C     XXFL6         ANDEQ     *OFF
     C     DKKY15        SETLL     PELL2
     C     DKKY15        READE     PELL2                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     SCASCD        IFNE      S@ASTS
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ACTST         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
      *
     C     SCCOFL        IFNE      S@COFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SHCOF         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     SCTYPB        IFNE      S@TYPB
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     TYPBC         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     SCFCOD        IFNE      S@PFIN
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     PRFIN         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C*
     C     SCCTST        IFNE      S@CTST
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONAU         UADATA
     C                   EXSR      WRTFA1
     C                   ENDIF
     C     DKKY15        READE     PELL2                                  60
     C                   ENDDO
     C                   ENDIF
      *
     C     FNPEOF        IFEQ      'M'
     C                   Z-Add     0             #TOTDIVM          5 0
     C     DKKY17        SETLL     PELL3
     C     DKKY17        READE     PELL3                                  60
     C     *IN60         DOWEQ     *OFF
     C                   EVal      #TOTDIVM= #TOTDIVM+ 1
      *
     C     SCILCD        IFNE      S@ILCD
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCR          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  INCR LIMIT CODE
      *
     C     X@ILEF        IFNE      S@ILEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INEF          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  INCR LIM EFF DATE
      *
     C     X@ILEX        IFNE      S@ILEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INEX          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  INCR LIM EFF DATE
      *
     C     SCDCDE        IFNE      S@DCDE
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEDC          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  DEDUCTIBLE DISCOUNT
     C     X@DCEF        IFNE      S@DCEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEEF          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  DEDU DIS EFF DATE
      *
     C     X@DCEX        IFNE      S@DCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEEX          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  DEDU DIS EXF DATE
      *
     C     SCASP2        IFNE      S@ASP2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@DVEF        IFNE      S@DVEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEEF          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@DVEX        IFNE      X@DVEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEEX          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     SCD1S2        IFNE      S@D1S2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN1SP2      UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@D1D2        IFNE      S@D1D2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DVEF1SP2      UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@D1X2        IFNE      S@D1X2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DVEX1SP2      UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
     C*** END   ADD ****** 06/21/10 ************************************
     C     SCDEV2        IFNE      S@DEV2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     SCSTFC        IFNE      S@STFC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     STFN2         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
     C*** END   ADD ****** 01/14/10 ************************************
      *
     C     X@D2EF        IFNE      S@D2EF
     C                   MOVEL     *BLANKS       UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     DE2EF         UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     STFEF         UADATA
     C                   ENDIF                                                  CLTID <>'025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@D2EX        IFNE      S@D2EX
     C                   MOVEL     *BLANKS       UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C     CLTID         IFNE      '025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   MOVEL     DE2EX         UADATA
     C*** BEGIN ADD ****** 01/14/10 ************************************
     C                   ELSE
     C                   MOVEL     STFEX         UADATA
     C                   ENDIF                                                  CLTID <>'025'
     C*** END   ADD ****** 01/14/10 ************************************
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C*** BEG   ADD ****** 06/21/10 ************************************
     C     SCD2S2        IFNE      S@D2S2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2SP2      UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@D2D2        IFNE      S@D2D2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DE2EFSP2      UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@D2X2        IFNE      S@D2X2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DE2EXSP2      UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
     C*** END   ADD ****** 06/21/10 ************************************
     C     SCEXM1        IFNE      S@EXM1
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXMO1         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@EMD1        IFNE      S@EMD1
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXEF1         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCEXM2        IFNE      S@EXM2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXMO2         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@EMD2        IFNE      S@EMD2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXEF2         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCEXM3        IFNE      S@EXM3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXMO3         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@EMD3        IFNE      S@EMD3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     EXEF3         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@PFDT        IFNE      S@PFDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  POLICY FROM DATE
      *
     C     X@PTDT        IFNE      S@PTDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLT          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  POLICY TO DATE
      *
     C     SCARSM        IFNE      S@ARSM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUMR          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  A/R SUMMARY
      *
     C     SCIFFL        IFNE      S@IFFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ORDC          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  EXEMPT FEE
      *
     C     SCQTYP        IFNE      S@QTYP
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLY          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  TYPE OF POLICY
      *
     C     SCADVR        IFNE      S@ADVR
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ADVF          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  ADVERSE ACC FLAG
      *
     C     SVRLFL        IFNE      S@RLFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DNRN          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  DO NOT RENEW
      *
     C     X@BSDT        IFNE      S@BSDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     BILS          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  BILL START DATE
      *
     C     SCAFLG        IFNE      S@STAT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ARFG          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  A-RATING
      *
     C     SCCXBL        IFNE      S@CXBL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEBI          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  DEDUCT BILL
      *
     C     SCMCCD        IFNE      S@MCCD
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MCOT          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  MCO TYPE
      *
     C     SCMCOD        IFNE      S@MCGP
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MCOT          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  MCO GROUP
      *
     C     SCMCOD        IFNE      S@MCGP
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MGOT          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  MCO GROUP
      *
     C     SCSRPR        IFNE      S@SRPR
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SHPF          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  SHORT RATE
      *
     C     SCASP3        IFNE      S@ASP3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DICO          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  STOCK DISC%
      *
     C     SCPROM        IFNE      S@PROM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INPR          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  INTERNALLY PROM
      *
     C     SCCXDY        IFNE      S@CXDY
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CXND          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  CANCELLATION NOT
      *
     C     SCPP3C        IFNE      S@PP3C
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     PPIC          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  POLICY PAGE ITEM
      *
     C     SCPPC3        IFNE      S@PPC3
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     PP3C          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  POLICY PAGE ITEM
      *
     C     SCSFPC        IFNE      S@SFPC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SAFP          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  SAFETY FEE %
      *
     C     SCSFMN        IFNE      S@SFMN
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SAMA          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  SAFETY MIN
      *
     C     SCADDM        IFNE      S@ADDM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     AMAL          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  ADDITIONAL MAILING
      *
     C     X@ANDT        IFNE      S@ANDT
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ANRD          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  ANNIVERSARY RATING
      *
     C     SCEXMF        IFNE      S@EXMF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     ECMD          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF                                                  EXCLUDE MEDICAL
      *
     C     SCASP1        IFNE      S@ASP1
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONTR         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@CREF        IFNE      S@CREF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONEF         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@CREX        IFNE      S@CREX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONEX         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCASPM        IFNE      S@ASPM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBJA         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@MCEF        IFNE      S@MCEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBEF         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@MCEX        IFNE      S@MCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBEX         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCASPS        IFNE      S@ASPS
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBJB         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@SCEF        IFNE      S@SCEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBBE         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@SCEX        IFNE      S@SCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBBX         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCDBCR        IFNE      S@DBCR
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SCHFL         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@SDEF        IFNE      S@SDEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SCHEF         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@SCEX        IFNE      S@SCEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SCHEX         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCDISC        IFNE      S@DISC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DISCT         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     E@WVTP        IFNE      S@WVTP
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WAIVE         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     E@WVFL        IFNE      S@WVFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     LVLPR         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     E@WVPC        IFNE      S@WVPC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WSPEC         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     E@WMIN        IFNE      S@WMIN
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MINWAV        UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     E@FCHG        IFNE      S@FCHG
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     FLATCH        UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     E@WMAX        IFNE      S@WMAX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MAXWAV        UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCDCD2        IFNE      S@DCD2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEDC2         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@2DEF        IFNE      S@2DEF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DED2E         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@2DEX        IFNE      S@2DEX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DED2X         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCINL2        IFNE      S@INL2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCR2         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@I2EF        IFNE      S@I2EF
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCEF         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     X@I2EX        IFNE      S@I2EX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCEX         UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     SCMFEE        IFNE      S@MFEE
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MBRA          UADATA
     C                   EXSR      WRTFA1_1
     C                   ENDIF
      *
     C     DKKY17        READE     PELL3                                  60
     C                   ENDDO
     C                   ENDIF
      *
     C*
      *****
      * POPULATE XXST WITH RSJRST
      *****For Policies
      *****
     C     FNPEOF        IFEQ      'M'
     C     DKKY17        SETLL     PELL3
     C     DKKY17        READE     PELL3                                  60
     C     *IN60         DOWEQ     *OFF
     C**** SCEMP#        IFNE      E3EMP#
     C                   MOVEL     *BLANKS       RSJRST
     C     DKKY_1        CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  RSJRST        XXST
     C                   ENDIF
      *****
      * CHECK ELILCD
      *****
     C     SCILCD        IFNE      S@ILCD
     C                   MOVEL(P)  ELILCD        XXKEY
     C     DKKY1         SETGT     PLM01
     C     DKKY2         READPE    PLM01                                  60
      *
     C*** WRITE RECORD TO WXUAL
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCR          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCILCD NE ELILC
     C*
      *****
      * CHECK ELDCDE
      *****
     C     SCDCDE        IFNE      S@DCDE
     C                   MOVEL(P)  ELDCDE        XXDDCD
     C     DKKY3         SETGT     PDD01
     C     DKKY4         READPE    PDD01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEDC          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDCDE NE ELDSD
      *****
      * CHECK ELINL2
      *****
     C     SCINL2        IFNE      S@INL2
     C                   MOVEL(P)  ELINL2        XXKEY
     C     DKKY1         SETGT     PLM01
     C     DKKY2         READPE    PLM01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     INCR2         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCINL2 NE S@INL
      *****
      * CHECK ELDCD2
      *****
     C     SCDCD2        IFNE      S@DCD2
     C                   MOVEL(P)  ELDCD2        XXDDCD
     C     DKKY3         SETGT     PDD01
     C     DKKY4         READPE    PDD01                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEDC2         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCDCD2 NE S@DCD
      *****
      * CHECK ELASP2
      *****
     C     SCASP2        IFNE      S@ASP2
     C     DKKY5         SETGT     PRR01
     C     DKKY6         READPE    PRR01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN          UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C***** BEG ADD ******* 03/11/11 **********************************
     C     @@CFLG        ANDNE     '1'
     C     *IN60         OREQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDNE     'NM'
     C     *IN60         OREQ      *OFF
     C     RRDV1L        ANDEQ     *BLANKS
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDEQ     'NM'
     C***** END ADD ******* 03/11/11 **********************************
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCASP2 NE S@ASP2
      *
     C***** BEG ADD ******* 06/21/10 **********************************
      *****
      * CHECK ELD1S2
      *****
     C     SCD1S2        IFNE      S@D1S2
     C     DKKY5         SETGT     PRR01
     C     DKKY6         READPE    PRR01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN1SP2      UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C***** BEG ADD ******* 03/11/11 **********************************
     C     @@CFLG        ANDNE     '1'
     C     *IN60         OREQ      *OFF
     C     RRDV1C        ANDEQ     *ZERO
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDNE     'NM'
     C     *IN60         OREQ      *OFF
     C     RRDV1L        ANDEQ     *BLANKS
     C     @@CFLG        ANDEQ     '1'
     C     RRST          ANDEQ     'NM'
     C***** END ADD ******* 03/11/11 **********************************
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN1SP2      UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C                   ENDIF                                                  SCD1S2 NE S@D1S2
     C***** END ADD ******* 06/21/10 **********************************
      *****
      * CHECK ELDEV2
      *****
     C     *IN60         IFEQ      *ON
     C     SCDEV2        ANDNE     S@DEV2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2         UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCDEV2        ANDNE     S@DEV2
     C     RRDV2C        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C*** BEG   ADD ****** 06/21/10 ************************************
      *****
      * CHECK ELD2S2
      *****
     C     *IN60         IFEQ      *ON
     C     SCD2S2        ANDNE     S@D2S2
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2SP2      UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCD2S2        ANDNE     S@D2S2
     C     RRDV2C        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DEVN2SP2      UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C*** END   ADD ****** 06/21/10 ************************************
      *
     C*** BEGIN ADD ****** 01/14/10 ************************************
      *****
      * CHECK ELSTFC
      *****
     C     *IN60         IFEQ      *ON
     C     SCSTFC        ANDNE     S@STFC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     STFN2         UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCSTFC        ANDNE     S@STFC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     STFN2         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
     C*** END   ADD ****** 01/14/10 ************************************
      *****
      * CHECK ELASP1
      *****
     C     *IN60         IFEQ      *ON
     C     SCASP1        ANDNE     S@ASP1
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONTR         UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCASP1        ANDNE     S@ASP1
     C     RRCCCD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     CONTR         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
      *****
      * CHECK ELASPM
      *****
     C     *IN60         IFEQ      *ON
     C     SCASPM        ANDNE     S@ASPM
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBJA         UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCASPM        ANDNE     S@ASPM
     C     RR1ACD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBJA         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
      *****
      * CHECK ELASPS
      *****
     C     *IN60         IFEQ      *ON
     C     SCASPS        ANDNE     S@ASPS
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBJB         UADATA
     C**
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCASPS        ANDNE     S@ASPS
     C     RR1BCD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SUBJB         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *
      *****
      * CHECK ELDBCR
      *****
     C     *IN60         IFEQ      *ON
     C     SCDBCR        ANDNE     S@DBCR
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SCHFL         UADATA
     C                   EXSR      WRTFA_1
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     SCDBCR        ANDNE     S@DBCR
     C     RRDBCD        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     SCHFL         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
     C                   ENDIF                                                  *IN60=*ON
      *****
      * CHECK WAIVER SURCHARGE
      *****
     C     *IN60         IFEQ      *ON
     C     E@WVTP        ANDNE     S@WVTP
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WAIVE         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WVFL        ANDNE     S@WVFL
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     LVLPR         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WVPC        ANDNE     S@WVPC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WSPEC         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WMIN        ANDNE     S@WMIN
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MINWAV        UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@FCHG        ANDNE     S@FCHG
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     FLATCH        UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
      *
     C     *IN60         IFEQ      *ON
     C     E@WMAX        ANDNE     S@WMAX
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MAXWAV        UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*ON
      *
     C***                  ELSE
      *
     C     *IN60         IFEQ      *OFF
     C     E@WVTP        ANDNE     S@WVTP
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WAIVE         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WVFL        ANDNE     S@WVFL
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     LVLPR         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WVPC        ANDNE     S@WVPC
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     WSPEC         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WMIN        ANDNE     S@WMIN
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MINWAV        UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@FCHG        ANDNE     S@FCHG
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     FLATCH        UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN60=*OFF
      *
     C     *IN60         IFEQ      *OFF
     C     E@WMAX        ANDNE     S@WMAX
     C     RRWVAC        ANDEQ     *ZERO
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     MAXWAV        UADATA
     C****               MOVEL     *BLANKS       @@ST
     C***  DKKY12        CHAIN     PUA01                              60
     C**   *IN60         IFEQ      *OFF
     C**   E@WMAX        ANDNE     S@WMAX
     C                   EXSR      WRTFA_1
     C**                 ENDIF
     C                   ENDIF                                                  *IN60=*OFF
      *
      *****
      * CHECK ELDISC
      *****
     C     DKKY7         SETGT     PDS01
     C     DKKY8         READPE    PDS01                                  60
      *
     C     SCDISC        IFNE      *BLANK
     C     *IN60         ANDEQ     *ON
     C     SCDISC        ANDNE     S@DISC
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     DISCT         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF
      *
     C*  POLICY FROM DATE CANNOT BE LESS THAN INITIAL EFFECTIVE DATE(POLICIES)
      *
HS01 C     X@PFDT        IFNE      S@PFDT
     C                   Z-ADD     E3FND         @@FND
     C                   Z-ADD     E3EMP#        @@EMP#
     C                   Z-ADD     E3DIV         @@DIV
     C                   Z-ADD     E3FYR         @@FYR
     C*
     C     DMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ZEROS        *IN29
     C  N90XXPFDT        IFLT      EMEFD1
     C                   SETON                                        29  90
     C                   ENDIF                                                  XXPF<EMEFD1
     C                   ENDIF                                                  CHAIN PEM01
      *
     C     *IN29         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN29 = *ON
     C*
     C*  POLICY FROM DATE MUST BE LESS THAN OR EQUAL TO ALL EXISTING
     C*  PAYROLL RECORDS(FOR ALL POLICIES)
     C*
     C                   MOVE      *ZEROS        *IN90
     C  N90DKKY13        CHAIN     PEP01                              60
     C  N90*IN60         DOWEQ     *OFF
     C*
     C     EPEFDT        IFLT      XXPFDT
     C     EPSTAT        ANDNE     'I'
     C                   SETON                                        306090
     C                   ENDIF                                                  EPEFDT<@@PF/EPSTAT<>
      *
     C     *IN30         IFEQ      *ON
     C     *IN60         ANDEQ     *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN3060=*ON
     C*
     C  N90DKKY13        READE     PEP01                                  60
     C                   ENDDO                                                  *IN60=*OFF
     C                   ENDIF
     C*  EDIT POLICY TO DATE
     C*  DATES MUST BE ASCENDING ORDER
     C*
     C                   MOVE      *ZEROS        *IN31
     C  N90XXDAT2        IFLT      XXDAT1
     C                   SETON                                        31  90
     C                   ENDIF                                                  XXDAT2<XXDAT1
     C     *IN31         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLT          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN31 = *ON
     C*
      *===============================================================
      * CKPOLD - CHECK IF CLAIMS OUTSIDE NEW POLICY EFF DT ENTERED
      *===============================================================
      *
     C     XXOFLG        IFEQ      'N'
     C                   MOVE      'N'           XXEFLG            1            ERROR FOUND
      *
     C***  @@RRN         CHAIN(N)  WDELP                              60
      *
     C                   Z-ADD     SCPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
      *
     C                   Z-ADD     SCPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
      *
      *  Check for claims outside policy eff dt range
      *
     C     XXPFDT        IFGT      E3PFDT
     C                   Z-ADD     E3PFDT        XXDATE
      *
     C     DKKY16        SETLL     PCM01
     C     DMKY          READE     PCM01                                  60
      *
     C                   MOVE      *ZEROS        *IN90
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     XXPFDT
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     XXPMTS        IFNE      *ZEROS                                       CLSD CLM W/PMTS
     C     CMSTAT        ORNE      'C'                                          OPN CLM
     C                   SETON                                        90
     C                   MOVE      'Y'           XXEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLF          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN90=*ON
      *
     C     DMKY          READE     PCM01                                  60
     C                   ENDDO                                                  PFDT
      *
     C                   ENDIF                                                  PFDT
      *
      *
      *
     C  N90XXPTDT        IFLT      E3PTDT
     C     XXEFLG        ANDEQ     'N'
     C                   Z-ADD     XXPTDT        XXDATE
     C     DKKY16        SETLL     PCM01
     C     DMKY          READE     PCM01                                  60
      *
     C                   MOVE      *ZEROS        *IN90
     C     *IN60         DOWEQ     *OFF
     C     CMACDT        ANDLT     E3PTDT
     C                   XFOOT     AR            XXPMTS           13 2
      *
     C     XXPMTS        IFNE      *ZEROS                                       CLSD CLM W/PMTS
     C     CMSTAT        ORNE      'C'                                          OPN CLM
     C                   SETON                                        90
     C                   MOVE      'Y'           XXEFLG            1
     C                   LEAVE
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     POLT          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN90=*ON
      *
     C     DMKY          READE     PCM01                                  60
     C                   ENDDO                                                  PTDT
      *
     C                   ENDIF                                                  PTDT
      *
     C                   ENDIF                                                  XXOFLG
      *
     C*  ANNIV DATE MUST BE LESS THAN OR EQUAL TO POL FROM DATE
     C*
     C     X@ANDT        IFNE      S@ANDT
     C                   MOVE      *ZEROS        *IN38
     C     XXANDT        IFGT      E3PFDT
     C                   SETON                                        38  90
     C                   ENDIF                                                  XXANDT>XXPF
     C     *IN38         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     OPTNM         UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN38 = *ON
     C                   ENDIF                                                  Anniversary date
      *
     C* BILLING START DATE CANNOT BE GREATER THAN POLICY EFFECTIVE DATE
     C*
     C     X@BSDT        IFNE      S@BSDT
     C                   MOVE      *ZEROS        *IN68
     C     XXDT1         IFGT      E3PFDT
     C                   SETON                                        68  90
     C                   ENDIF
     C     *IN68         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     BILS          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN68 = *ON
     C*
     C* DO NOT ALLOW BILLING START DATE TO BE PRIOR TO CURRENT DATE IF
     C* WE ARE CHANGING THE VALUE OF THE FIELD OR ADDING A RECORD.
     C* UNLESS THE CHANGE IS THE BILLING START DATE TO = POLICY FROM DATE
     C*
     c                   MOVE      *ZEROS        *IN95
     C     *IN40         IFEQ      *ON
     C     XXDT2         ANDGT     SYSDT
     C     *IN40         OREQ      *OFF
     C     XXDT1         ANDNE     E3BSDT
     C*
     C     XXDT1         IFLT      SYSDT
     C     XXDT1         ANDNE     XXDT2
     C     SCARSM        ANDNE     'Y'
     C                   SETON                                        95  90
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN40 IFEQ
     C                   ENDIF                                                  Bill start date
     C*
     C     *IN95         IFEQ      *ON
     C                   MOVEL     *BLANKS       UADATA
     C                   MOVEL     BILS          UADATA
     C                   EXSR      WRTFA_1
     C                   ENDIF                                                  *IN95 = *ON
     C*
     C***** END CHG ******* 10/18/07 **********************************
      *****
     C                   MOVEL     S2DATE        QQDATE
     C                   MOVEL     S2USER        QQUSER
     C                   MOVEL     S2TIME        QQTIME
     C                   MOVEL     SCCXFL        QQCXFL            1
     C*
     C********           ENDIF                                                  SCDIV<>E3DIV
     C     DKKY17        READE     PELL3                                  60
     C                   ENDDO
     C                   ENDIF                                                  <> 'M'
     C                   CLOSE     WDELP
     C                   CLOSE     WDELL
     C                   CLOSE     WMEPL
     C                   ENDSR                                                  CHKFA
      *====================================================
      * WRTFA - WRITE RECORDS TO WXUAP WITH APPROPIATE
      *         INFORMATION
      *====================================================
     C     WRTFA         BEGSR
      *****
     C                   MOVEL     E2CO#         UACO#
     C                   MOVEL     E2DIV         UADIV
     C                   MOVEL     E2EMP#        UAEMP#
     C                   MOVEL     E2FND         UAFND
     C                   MOVEL     E2FYR         UAFYR
     C                   MOVEL     S2TIME        UATIME
     C                   MOVEL     S2USER        UAUSER
     C                   MOVEL     S2DATE        UADT
     C                   MOVEL     XXST          UAST
     C                   MOVEL     @@MODE        UAMODE
     C                   WRITE     PUA01
     C                   ENDSR                                                  WRTFA
      *====================================================
      * WRTFA1- WRITE DUMMY RECORDS TO WXUAP WITH APPROPIATE
      *         INFORMATION
      *====================================================
     C     WRTFA1        BEGSR
      *****
     C***** BEG ADD ******* 10/08/07 **********************************
     C                   MOVEL     E2CO#         UACO#
     C                   MOVEL     E2DIV         UADIV
     C                   MOVEL     E2EMP#        UAEMP#
     C                   MOVEL     E2FND         UAFND
     C                   MOVEL     E2FYR         UAFYR
     C                   MOVEL     S2TIME        UATIME
     C                   MOVEL     S2USER        UAUSER
     C                   MOVEL     S2DATE        UADT
     C************       MOVEL     ELCO#         UACO#
     C************       MOVEL     ELDIV         UADIV
     C************       MOVEL     ELEMP#        UAEMP#
     C************       MOVEL     ELFND         UAFND
     C************       MOVEL     ELFYR         UAFYR
     C************       MOVEL     S2TIME        UATIME
     C************       MOVEL     S2USER        UAUSER
     C************       MOVEL     S2DATE        UADT
     C***** END ADD ******* 10/08/07 **********************************
     C     *IN71         IFEQ      *ON
     C                   MOVEL     'T'           UAMODE
     C                   ENDIF
     C                   WRITE     PUA01
     C                   ENDSR                                                  WRTFA1
     C***** BEG ADD ******* 10/08/07 **********************************
      *====================================================
      * WRTFA_1 - WRITE RECORDS TO WXUAP WITH APPROPIATE
      *           INFORMATION
      *====================================================
     C     WRTFA_1       BEGSR
      *****
     C                   MOVEL     E3CO#         UACO#
     C                   MOVEL     E3DIV         UADIV
     C                   MOVEL     E3EMP#        UAEMP#
     C                   MOVEL     E3FND         UAFND
     C                   MOVEL     E3FYR         UAFYR
     C                   MOVEL     S2TIME        UATIME
     C                   MOVEL     S2USER        UAUSER
     C                   MOVEL     S2DATE        UADT
     C                   MOVEL     XXST          UAST
     C                   MOVEL     @@MODE        UAMODE
     C                   WRITE     PUA01
     C                   ENDSR                                                  WRTFA_1
      *====================================================
      * WRTFA1_1- WRITE DUMMY RECORDS TO WXUAP WITH APPROPIATE
      *           INFORMATION
      *====================================================
     C     WRTFA1_1      BEGSR
      *****
     C                   MOVEL     E3CO#         UACO#
     C                   MOVEL     E3DIV         UADIV
     C                   MOVEL     E3EMP#        UAEMP#
     C                   MOVEL     E3FND         UAFND
     C                   MOVEL     E3FYR         UAFYR
     C                   MOVEL     S2TIME        UATIME
     C                   MOVEL     S2USER        UAUSER
     C                   MOVEL     S2DATE        UADT
     C     *IN71         IFEQ      *ON
     C                   MOVEL     'T'           UAMODE
     C                   ENDIF
     C                   WRITE     PUA01
     C                   ENDSR                                                  WRTFA1_1
     C***** BEG END ******* 10/08/07 **********************************
      *====================================================
      * UPKFA - CALL W0051R2 CONDITIONALLY
      *
      *====================================================
     C     UPKFA         BEGSR
     C*
     C                   Eval      xxntupd  = 0
     C     WXU_K         Setll     Wxual
     C     WXU_K         Reade     Wxual
     C                   If        uast   <>  ' '
     C                   Z-Add     uaemp#        #uaemp#
     C                   Z-Add     uadiv         #uadiv
     C                   endif
     C                   Dow       not %Eof(Wxual)
     C                   If        uast   <>  ' '
     C                   If        uaemp# <>  #uaemp# or uadiv <> #uadiv
     C                   Eval      xxntupd  = xxntupd + 1
     C                   Z-Add     uaemp#        #uaemp#
     C                   Z-Add     uadiv         #uadiv
     C                   endif
     C                   endif
     C     WXU_K         Reade     Wxual
     C                   Enddo
     C
     C                   MOVEL     SCNAME        QQNAME           50
     C                   MOVEL     SCEDN         QQEDN            50
     C                   MOVEL     FNPEOF        QQMPO
     C     UAST          IFEQ      ' '
     C                   CALL      'W0051R2'                            99
     C                   PARM                    QQUSER           10
     C                   PARM                    QQDATE            7 0
     C                   PARM                    QQTIME            6 0
     C                   PARM      'U'           QQMODE            1
     C                   PARM                    QQCXFL            1
     C                   PARM                    SCEMP#
     C                   PARM                    SCFND
     C                   PARM                    SCDIV
     C                   PARM                    QQNAME
     C                   PARM                    QQEDN
     C                   PARM                    QQMPO
     C                   PARM                    SC#DIV
     C                   PARM                    #TOTDIVM                       total multiple div
     C                   PARM                    XXNTUPD           5 0          total multiple div
     C
     C                   ELSE
     C     UAST          IFNE      ' '
     C                   CALL      'W0051R2'                            99
     C                   PARM                    QQUSER           10
     C                   PARM                    QQDATE            7 0
     C                   PARM                    QQTIME            6 0
     C                   PARM      '*BLANKS'     QQMODE            1
     C                   PARM                    QQCXFL            1
     C                   PARM                    SCEMP#
     C                   PARM                    SCFND
     C                   PARM                    SCDIV
     C                   PARM                    QQNAME
     C                   PARM                    QQEDN
     C                   PARM                    QQMPO
     C                   PARM                    SC#DIV
     C                   PARM                    #TOTDIVM                       total multiple div
     C                   PARM                    XXNTUPD                        total multiple div
     C                   ENDIF                                                  UADATA NE ' '
     C                   ENDIF                                                  UADATA EQ ' '
     C                   ENDSR                                                  UPKFA
     C***** END ADD ******* 07/27/07 **********************************
     C*** beg add ***** 04/21/09 **************************************
      *================================================================
      * SRMVAR - moves S/R billings off of state reporting division
      *================================================================
     c     srmvar        begsr
      *
      *  Get S/R billing A/R code from WDF1P - Group Policy Years
      *
     c     *like         define    F1SRBB        srCode
     c                   move      *blanks       srCode
     c                   eval      xxfyr = ELFYR
     c     f1ky1         chain     pf101
     c                   if        %found
     c                   eval      srCode = F1SRBB
     c                   endif
      *
      * Leave subroutine if no A/R code for Self-Reported bills is found.
      *
     c                   if        srCode = *blanks
     c                   leavesr
     c                   endif
      *
      *  Get Tax billing A/R code from WTTTP - A/R Transaction Table
      *
     c     *like         define    TTKEY         taxCode
     c                   eval      taxCode = *blanks
     c     *loval        setll     ptt01
     c                   read      ptt01
     c                   dow       not(%eof)
     c                   if        TTCTYP = 'T' and TTTYPE = 'F'
     c                   eval      taxCode = TTKEY
     c                   leave
     c                   endif
     c                   read      ptt01
     c                   enddo
      *
      *  Get the state reporting division.
      *
     c     *like         define    ELDIV         stRptDiv
     c                   move      *zero         stRptDiv
     c     emky1         setll     pem01
     c     emky1         reade     pem01
     c                   dow       not(%eof)
     c                   if        EMSTRP   = 'Y'
     c                   eval      stRptDiv = EMDIV
     c                   leave
     c                   endif
     c     emky1         reade     pem01
     c                   enddo
      *
      * Leave subroutine if no state reporting division is found.
      *
     c                   if        (%eof)
     c                   leavesr
     c                   endif
      *
      *  Move S/R billings to the reporting division
      *
     c     eaky24        klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    stRptDiv
     c                   kfld                    ELDIV
     c                   kfld                    ELFYR

     c                   if        ELDIV <> stRptDiv
     c     eaky24        setll     wmeal24
     c     eaky24        reade     wmeal24
     c                   dow       not(%eof)
     c                   if        E24EAARCD = srCode  or
     c                             E24EAARCD = taxCode
     c                   eval      E24EADIV  = E24EARDIV
     c                   update    pea24
     c                   endif
     c     eaky24        reade     wmeal24
     c                   enddo
     c                   endif

     c                   endsr                                                  srmvar
      *================================================================
     C*** end add ***** 04/21/09 **************************************
     C*** BEGIN ADD ****** 10/20/09 ************************************
     C*====================================================================
    C* Process and write DARTS remark
     C*====================================================================
     C     SRPrcDARTS    begsr
     C*
     C                   MOVE      *blanks       xxLin2           80
     C                   MOVE      *blanks       xxComm          256
     C                   MOVE      *blanks       xxRmcd           10
     C*
     C* Load Line 1 text
     C                   MOVEL     ASCTXT        xxLin1           35
     C* Build and load Line 2 text
     C                   IF        ELASCD = *BLANKS
     C                   MOVEL     'blank'       xxFrCd            5
     C                   ELSE
     C                   MOVEL(P)  ELASCD        xxFrCd
     C                   ENDIF                                                  ELASCD = *BLANKS
     C*
     C                   IF        SCASCD = *BLANKS
     C                   MOVEL     'blank'       xxToCd            5
     C                   ELSE
     C                   MOVEL(P)  SCASCD        xxToCd
     C                   ENDIF                                                  SCASCD = *BLANKS
     C*
     C                   EVAL      xxLin2 = ASCTX1 + ' ' + %TRIM(xxFrCd)
     C                                    + ' to ' + %TRIM(xxToCd)
     C                                    + ASCTX2 + %TRIM(q1user)
     C*
     C                   EVAL      xxComm = xxLin1 + xxLin2
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C   41              MOVE      'Q'           QCQTE
     C  N41              MOVE      *BLANKS       QCQTE
     C*
     C                   EVAL      xxRmcd = SVASRM
     C*
     C* Call procedure to write DARTS
     C                   CALLP     WriteDarts(qckey:xxComm:xxRmcd)
     C*
     c                   endsr                                                  srmvar
     C*** begin Add ****** 12/31/10 ************************************
     C*====================================================================
     C*  Load Policy page 3c information from file WDF4P
     C*====================================================================
     C     load3c        begsr
     C*** beg chg ***** 08/22/11 ******************************************
      ***********
     C****Beg*Chg ***** 01/31/11 ******************************************
     C***********        move      'PP3C'        xxf4ky
     C***********        movel(p)  'PP3C'        xxf4ky
     C****End*Chg ***** 01/31/11 ******************************************
     C***********        z-add     scpfdt        srdti
     C***********        call      'SRCVTDAT'    DTCYMD
     C***********        movel     srdto         xxf4dt
     c*****xxfkey        setgt     pf401
     c*****xxfkey1       readpe    pf401                                  60
     C*Protect*fields SCPPC3,SCPP3C
     C***********        dow       *in60 = *off
     C***********        if        f4stat = 'A'
     C****Beg*Del ***** 01/31/11 ******************************************
     C***********        if        elpp3c <> *blanks
     C***********        move      elpp3c        scpp3c
     C***********        move      elppc3        scppc3
     C***********        else
     C****End*Del ***** 01/31/11 ******************************************
     C***********        if        f4txt1 <> *blanks
     C***********        move      f4txt1        scpp3c
     C***********        move      f4txt2        scppc3
     C***********        endif                                                  f4txt1<>*blanks
     C****Beg*Del ***** 01/31/11 ******************************************
     C***********        endif                                                  elpp3c<>*blanks
     C****End*Del ***** 01/31/11 ******************************************
     C***********        leave
     C***********        endif                                                  f4stat= 'A'
     C***********
     c*****xxfkey1       readpe    pf401                                  60
     C***********        enddo
      *
      *  at16f - dspatr fld for 'Overwrite 3c field' label
      *  at17f - dspatr fld for 'Overwrite 3c field' field
      *  at18f - dspatr fld for SCPP3C field (1st text field for 3c vbg)
      *  at19f - dspatr fld for SCPPC3 field (2nd text field for 3c vbg)
      *
     c                   select
     c                   when      not(WC1918instald) and ELPP3C <> *blanks
     c                   if        @@mode <> 'D'
     c                   eval      at16f = editLowLine
     c                   eval      at17f = editLowLine
     c                   else
     c                   eval      at16f = displayHigh
     c                   eval      at17f = displayHigh
     c                   endif
     c                   eval      scpp3c = ELPP3C
     c                   eval      scppc3 = ELPPC3
     c                   when      not(WC1918instald) and F2PP3C <> *blanks
     c                   if        @@mode <> 'D'
     c                   eval      at16f = editLowLine
     c                   eval      at17f = editLowLine
     c                   else
     c                   eval      at16f = displayHigh
     c                   eval      at17f = displayHigh
     c                   endif
     c                   eval      scpp3c = F2PP3C
     c                   eval      scppc3 = F2PPC3
     c                   when      WC1918instald
     c                   eval      at18f = displayLow
     c                   if        @@mode <> 'D'
     c                   eval      at19f = editLowLine
     c                   else
     c                   eval      at19f = displayHigh
     c                   endif
     c                   select
     c                   when      scffla = *blank
     c                   if        @@mode <> 'D'
     c                   eval      at16f = displayLow
     c                   eval      at17f = displayLow
     c                   else
     c                   eval      at16f = displayHigh
     c                   eval      at17f = displayHigh
     c                   endif
     c                   eval      xxf4ky = 'PP3C'
     C                   z-add     scpfdt        srdti
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     srdto         xxf4dt
     c                   open      WDF4L
     c     xxfkey        setgt     pf401
     c     xxfkey1       readpe    pf401
     c                   dow       not(%eof)
     C                   if        f4stat = 'A'
     c                   eval      scpp3c = F4TXT1
     c                   eval      scppc3 = F4TXT2
     c                   leave
     C                   endif                                                  f4txt1<>*blanks
     c     xxfkey1       readpe    pf401
     c                   enddo
     c                   close     WDF4L
     c                   when      scffla =  'Y'
     c                   if        @@mode <> 'D'
     c                   eval      at16f = editLowLine
     c                   eval      at17f = editLowLine
     c                   else
     c                   eval      at16f = displayHigh
     c                   eval      at17f = displayHigh
     C                   endif                                                  f4stat= 'A'
     c                   eval      scpp3c = ELPP3C
     c                   eval      scppc3 = ELPPC3
     c                   endsl
     c                   endsl
     C*** end chg ***** 08/22/11 ******************************************
     C                   endsr
     C*** End   Add ****** 12/31/10 ************************************
     C***** Beg Add ****** 12/08/11 ************************************
     C*** beg del ***** 01/12/12 ***************************************
     C*====================================================================
     C***Load*Policy Year correctly
     C*====================================================================
     C*****polfyr        begsr
     C***********
     C*****@@rrn2        chain     wmemp                              60
     C***********        if        *in60 = *off
     C***********        open      wdell5
     C*****elkey*        chain     pell5                              60
     C******in60*        doweq     *off
     C***********        if        emefd1 < zzptdt and
     C***********                  emefd1 > zzpfdt
     C*****elkey4        chain     wdell                              60
     C***********        if        *in60 = *on
     C***********        eval      scfyr = zzfyr
     C***********        leave
     C***********        endif
     C***********        endif
     C*****elkey*        reade     pell5                                  60
     C***********        enddo
     C***********        close     wdell5
     C***********        endif
     C***********        endsr
     C*** end del ***** 01/12/12 ***************************************
     C***** End Add ****** 12/08/11 ************************************
     C*====================================================================
    C* Write DARTS remark
     C*====================================================================
     PwriteDarts       b

     DwriteDarts       pi
     D inDkey                        40
     D inComm                       256
     D inRmcd                        10

     D*
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
    C*
     C                   eval      q#Key  = inDKey
     C                   eval      q#Comm = inComm
     C                   eval      q#RmCd = inRmcd
     C*
     C                   TIME                    DSDTTM
     C                   movel     dsdate        q#cdat
     C*
     C                   eval      q#PNam = 'W0050R'
     C                   eval      q#User = q1User
     C                   eval      q#Pgm  = q1pgm
     C                   eval      q#IPgm = q1pgm
     C                   eval      q#Seq# = '00000'
     C                   eval      q#Sbyl = 'N'
     C                   eval      q#ImgF = ' '
     C*
     C                   eval      q#Fold = *blanks
     C                   eval      q#DcId = *blanks
     C                   eval      q#DiFl = ' '
     C*
     C                   clear                   q#com2
     C                   clear                   q#secl
     C                   clear                   q#fl01
     C                   clear                   q#fl02
     C                   clear                   q#apid
     C                   clear                   q#idoc
     C                   clear                   q#ique
     C                   clear                   q#ipth
     C                   clear                   q#pidx
     C                   clear                   q#qpty
     C                   clear                   q#dctp
     C                   clear                   q#csid
     C                   clear                   q#code
     *
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
     C*** END   ADD ****** 10/20/09 ************************************
      *
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
** STRWDW MESSAGES                                         04/07/08
      ERROR                                                04/07/08
INFORMATION MESSAGE                                        04/07/08
VENDOR RECORD NOT SET UP FOR THIS GROUP IN VENDOR          04/07/08
MASTER AT THE COMPANY LEVEL.                               04/07/08
                                                           NOT USED
                                                           NOT USED
                                                           NOT USED
                                                           NOT USED
                                                           NOT USED
                                                           NOT USED
