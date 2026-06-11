     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0051R - WORK WITH EMPLOYER POLICY YEARS
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL POLICY *
     H*                YEARS FOR A SELECTED EMPLOYER.  EACH YR CAN BE *
     H*                ACCESSED FOR UPDATE, DISPLAY OR SELECTION      *
     H*                DEPENDING ON THE CALLING PROGRAM.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R - WORK WITH EMPLOYERS                               *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    W0051R02 - EMPLOYER DETAIL INFORMATION (OPT 2/5)           *
     H*    W0060R   - EMPLOYER PAYROLL INFORMATION (OPT 8)            *
     H*    W0553R   - AGENT HISTORY (OPT 9)                           *
     H*    W0051R10 - EMPLOYER PREMIUM INFORMATION (OPT 10)           *
     H*    W0065R   - PREMIUM HISTORY (OPT 11)                        *
     H*    W0810R   - CERTIFICATE HOLDERS (OPT 12)                    *
     H*    SC010R   - DARTS (OPT 14)                                  *
     H*    W7412R   - POLICY PAGE (OPT 17)                            *
     H*    W4800R   - MODIFY POLICY EFFECTIVE DATES * QUOTES (OPT 18) *
     H*    W4115R   - DEPARTMENT LETTERS (OPT 19)                     *
     H*    W0200R   - CONTRACTOR CLASS (OPT 23)                       *
     H*    W0165R   - CANCELLATION MAINTENANCE (OPT 25)               *
     H*    W0165R   - REINSTATEMENT MAINTENANCE (OPT 26)              *
     H*    W8001R   - ESTIMATED BILLING REQUEST (OPT 30)              *
     H*    W0090R   - REVISED BILLING REQUEST (OPT 31)                *
     H*             - ENDORSEMENT (OPT 32)                            *
     H*    W7230C   - AUDIT REQUEST (OPT 33)                          *
     H*    W7710R   - AUDIT BILL REQUEST (OPT 34)                     *
     H*    W8100R1  - CARRIER INFORMATION (OPT 40)                    *
     H*    W4700R   - AUDIT TRACKING (OPT 41)                         *
     H*    W1054R   - EMPLOYER ENDORSEMENT FORMS (OPT 54)             *
     H*    W0051R58 - WAIVER INFORMATION (OPT 58)                     *
     H*    W0612R   - A/R INQUIRY (OPT 60)                            *
     H*             - A/R DETAIL SUMMARY (OPT 61)                     *
     H*    W0168R   - CANCELLATION HISTORY (OPT 62)                   *
     H*    W0070R   - CLAIMS INQUIRY (OPT 80)                         *
     H*                                                               *
     H*  CREATED: 03/06/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/15/11 C:189 SUPPORT: WA133 77324                           *
     H*                3RD PART BILLING EDIT BEING DISPLAYED ON       *
     H*                RUNNIG OTHER OPTIONS                           *
     H*                                                               *
     H* 06/16/11 C:235 Project: C133WLI                               *
     H*                EMPLOYER ACT STS: CHG TO ALLOW SECURITY TO BE  *
     H*                SET ON FIELD                                   *
     H*                                                               *
     H* 05/11/11 C:257 SUPPORT: WA099 76740                           *
     H*                PREVENT ADDING POLICY YEAR TO A RENEWAL QUOTE  *
     H*                                                               *
      * 03/28/11 C:136 SUPPORT: WA146 74563                           *
      *                PREVENT 3RD PARTY BILLING IF BILL HASN'T       *
      *                FINISHED RUNNING                               *
      *                                                               *
     H* 02/11/11 C:33  Project: WC1851                                *
     H*               -Change fee edits to use WTFIL3.                *
     H*               -Remove edits on fee flags.                     *
     H*                                                               *
     H* 01/28/11 C:190 Project: WC1954                                *
     H*                Added parameter for W0051R38.                  *
     H*                                                               *
     H* 12/17/10 C:255 Project: 71WLD                                 *
     H*                Add option 96 to call Premium information ent- *
     H*                ry screen. This is client 071 specific.        *
     H*                                                               *
     H* 11/30/10 C:180 SUPPORT 74221
     H*                Check for record in use to prevent a system
     H*                error when 2 or more users are trying to
     H*                do a revised bill endorsement to the same
     H*                policy number.
     H*
     H* 11/23/10 C:255 Project :114WVZ                                *
     H*                Change table OF/OFD entry for option 48 to re- *
     H*                ad "Retro/Reten Rating".                       *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Add opt 91 for HCN Program (CLT139 only)       *
     H*                * * Clean-up comment lines for OFD             *
     H*                                                               *
     H* 10/22/10 C:255 Support: SEQ73572                              *
     H*                Check object WA073R in library list instead    *
     H*                Call Stack.                                    *
     H*                                                               *
     H* 09/23/10 C:136 PROJECT: SY190 PRISS 184                       *
     H*                DISABLE PREMIUM NOTICE IN INQUIRY MODE         *
     H*                CHANGE OFD ARRAY TO YNN FOR OPTION 32          *
     H*                                                               *
     H* 09/16/10 C:246 PROJECT: SY190   PRISS 185 AND 183             *
     H*                DO NOT OPEN THE PROGRAM IN UPDATE MODE DURING  *
     H*                INQUIRY MODE FOR OPT48 AND OPT58               *
     H*                                                               *
     H* 07/27/10 C:257 SUPPORT: SEQ73572                              *
     H*                IF API WA073R EXIST IN CALL STACK THEN USE     *
     H*                FILE WMEFL6 INSTEAD OF WMDBL3                  *
     H*                                                               *
     H* 05/26/10 C:61  PROJECT: WC1851                                *
     H*              - Correct call option 63.                        *
     H*              - Change edit on option 63.                      *
     H*                                                               *
     H* 03/14/10 C:062 PROJECT: 25WRD                                 *
     H*                Fix option 38 to check if WMLDP recortd exists.*
     H*                                                               *
     H* 03/12/10 C:062 PROJECT: 25WQK                                 *
     H*                Change to CCS if clt025                        *
     H*                                                               *
     H* 01/25/10 C:232 PROJECT: WC1851                                *
     H*                Add new option "63-Fee information". This      *
     H*                option will call W0051R63.                     *
     H*                                                               *
     H* 12/30/09 C:232 PROJECT: WC1838                                *
     H*                Add new option "38=Large Deductible Entry".    *
     H*                Check authority for the Audit Bill in case of  *
     H*                Large Deductible.                              *
     H*                                                               *
     H* 02/27/09 C:190 SUPPORT:66893                                  *
     H*                Disable option 80 when starting                *
     H*                point is W/w claims.                           *
     H*                                                               *
      * 01/05/09 C:146 PROJECT: 146WBW                                *
      *                PREMIUM ENTRY ADJ/CARRIER PREMIUM              *
      *                                                               *
      * 12/15/08 C:220 SUPPORT: 66400                                 *
      *                SCHEDULED RATING INFORMATION OPTION FOR CLIENT *
      *                ID 146.                                        *
      *                                                               *
      * 08/21/08 C:61  Project: 114WRC
      *                Add edits so schedule debit/credit factor
      *                entered does not exceed maximum values entered
      *                in Reporting Rating WDRRP record.
     H*                                                               *
     H* 04/04/08 C:65  Project: SUPP#62336                            *
     H*                PREVENT POLICY PAGE OPTION IF CANCELLATION     *
     H*                DATE IS EQUAL TO OR LESS THAN POLICY EFF DATE. *
     H*                                                               *
     H* 02/26/08 C:33  Project: 114WMS   WA099/62440                  *
     H*                Initialize opt 49 to NNN.                      *
     H*                                                               *
     H* 07/13/07 C:132 PROJECT: 114WMS                                *
     H*                Make this only for Client 114                  *
     H*                                                               *
     H* 07/05/07 C:184 SUPPORT: WA099 58865                           *
     H*                Disable Opt 54 for QUOTE                       *
     H*                                                               *
     H* 04/30/07  C:33  Seq#: WA129/45522                             *
     H*                 Use W0051R04 to determine if Emp Master       *
     H*                  already has a cx'd date. (Indicator used in  *
     H*                  DDS).                                        *
     H*                                                               *
     H* 04/13/07 C:132 PROJECT 114WMS                                 *
     H*              - Add new field edits for WDRRP/RRDBCR for chgs  *
     H*                asked for by client.                           *
     H*              - Change Option 91 to 49 implemented 12/01/06    *
     H*                to reconcile chgs with project 114WMF          *
     H*                                                               *
     H* 04/04/07    C:180      WA114/53245                            *
     H*                        Ensure sched rating info remarks are   *
     H*                        written to DARTS when in quote mode.   *
     H*
     H* 03/13/07 C:33  Seq#: WA045/54881                              *
     H*                Remove Opt 11 when running in Quote.           *
     H*                                                               *
     H* 12/01/06 C:180 PROJECT: 114WMF                                *
     H*                         Add option for Schedule rating info.
     H*                                                               *
     H* 10/27/06 C:61  PROJECT 114WMS                                 *
     H*                Add edit to prevent Policy Page submission if  *
     H*                DB/CR entered and DB/CR form not filled out    *
     H*                (No WMRFP record for policy).                  *
     H*
     H* 08/30/06 C:180 PROJECT: 114WMF                                *
     H*                                                               *
     H* 08/21/06 C:33  Seq#: WA135/52534                              *
     H*                Moved editing for Opt 4=Delete to W0051R04.    *
     H*
     H* 03/23/06 C:132 PROJECT: SUPP# 58021                           *
     H*                Do not allow option 4, DELETE policy year if   *
     H*                premium history record(s) exist in WDEHP file  *
     H*                in a prior month, where effective and posting  *
     H*                dates in WDEHP are prior to start of current   *
     H*                month.                                         *
     H*                                                               *
     H* 03/17/06 C:132 PROJECT: SUPP# 50680                           *
     H*                Do not allow option 8, payroll unless there    *
     H*                is a Rate set Code in the employer detail file *
     H*                                                               *
     H* 08/05/05 C:132 PROJECT: SUPP# 47865                           *
     H*                Correct the READE at the bottom of DO loop.    *
     H*                DO NOT ALLOW A SUMMARIZED DIVISION TO BE       *
     H*                DELETED WHEN THE STATE REPORTING DIVISION HAS  *
     H*                ANY BILLED A/R.                                *
     H*                                                               *
     H* 12/01/04 C:65  PROJECT: SUPP#43540                            *
     H*                DO NOT ALLOW A SUMMARIZED DIVISION TO BE       *
     H*                DELETED WHEN THE STATE REPORTING DIVISION HAS  *
     H*                ANY BILLED A/R.                                *
     H*                                                               *
     H* 06/24/04 C:65  PROJECT:  114WHO                               *
     H*                ADD A NEW OPTION FOR RETRO INFO - OPT 48.      *
     H*                                                               *
     H* 06/22/04 C:61  PROJECT 114WHL                                 *
     H*                Add new option "42=Renewal Tracking".          *
     H*                                                               *
     H* 04/02/04 C:65  SUPPORT WA072 41873                            *
     H*                CHANGE THE RESTRICTION ON SUBMITTING POLICY    *
     H*                PAGES TO REQUIRE AN EMPLOYER HISTORY RECORD.   *
     H*                SIMPLY CHECKING ELFAF IS NOT SUFFICIENT TO     *
     H*                GURANTEE THEY HAVE RUN A CALC,UPD,& PRT BILL.  *
     H*                                                               *
     H* 02/11/04 C:136 SUPPORT: WA129 41248                           *
     H*                Modify edit added by seq # 39218 to invoke     *
     H*                if an original bill is run from any division   *
     H*                of the policy.                                 *
     H*                                                               *
     H* 01/13/04 C:136 PROJECT: WC1116                                *
     H*                Modify edit that restricts an original bill    *
     H*                from being rerun on an ACH account to use the  *
     H*                expiration date instead of the status.         *
     H*                                                               *
     H* 01/09/04 C:136 PROJECT: WC1115                                *
     H*                Add option 43 to call Vendor History           *
     H*                                                               *
     H* 12/05/03 C:136 SUPPORT: WA071 41085                           *
     H*                Remove edit that restricts revised billing     *
     H*                from being run before billing start date.      *
     H*                See 32379 and 35449.                           *
     H*                                                               *
     H* 10/13/03 C:61  PROJECT WC1017                                 *
     H*                If called from QUOTE and employer is a renewal *
     H*                quote, do not allow opt "18=Modify effective   *
     H*                dates".                                        *
     H*                                                               *
     H* 09/26/03 C:136 SUPPORT: WA129 39218                           *
     H*                Prevent A/R from being changed by the rerunning*
     H*                of an original bill for ACH employers.         *
     H*                                                               *
     H* 01/10/03 C:136 Support WA065 35449                            *
     H*                Conditioned edit in support seq# 32379 to      *
     H*                allow revised billing before billing start date*
     H*                if adding divisions.                           *
     H*                                                               *
     H* 08/07/02 C:136 Support WA107 32379                            *
     H*                Add edit to ensure that revised billing can not*
     H*                be run before the billing start date.          *
     H*                                                               *
     H* 06/28/02 C:132 SUPPORT WA114 32849                            *
     H*                Check for Carrier ID for each Division of the  *
     H*                Employer against the WHRPP Carrier/State       *
     H*                Reporting file                                 *
     H*                                                               *
     H* 05/10/02 C:61  SUPPORT WA107 32441                            *
     H*                Disable opt "32=Premium notice" if employer is *
     H*                on Account Current plan.                       *
     H*                                                               *
     H* 04/24/02 C:136 PROJECT: WC577                                 *
     H*                ADD F-14 DARTS                                 *
     H*                                                               *
     H* 02/28/02 C:65  PROJECT SUPP#31442                             *
     H*                REMOVE SUPPORT CHANGE ON 11/04/98.             *
     H*                                                               *
     H* 10/12/01 C:90  PROJECT: WC1054                                *
     H*                If the flag is set in group carrier info., do  *
     H*                not let bill to be run if a carrier policy #   *
     H*                has not been entered.                          *
     H*                                                               *
     H* 04/30/01 C:65  PROJECT 114WAK                                 *
     H*                Add option 90=Excluded states.                 *
     H*                                                               *
     H* 05/11/01 C:90  SUPPORT: WA101/27267                           *
     H*                Allow Opt54 in QUOTE.                          *
     H*                                                               *
     H* 03/08/01 C:90  SUPPORT: WA100/26501                           *
     H*                DISALLOW OPT54  IF NOT STATE REPORTING DIV.    *
     H*                                                               *
     H* 02/13/01 C:61  SUPPORT WA107 26066                            *
     H*                Disallow billing options if employer is CX.    *
     H*                                                               *
     H* 12/05/00 C:61  PROJECT WC577                                  *
     H*                Disallow F6=Add if employer cancelled.         *
     H*                                                               *
     H* 06/30/00 C:65  PROJECT WC995                                  *
     H*                DO NOT ALLOW AN AUDIT BILL TO BE RUN BEFORE    *
     H*                EITHER THE CANCELLATION DATE(EXCEPT FOR FLAT   *
     H*                CANCELS) OR THE EXPIRATION DATE.               *
     H*                                                               *
     H* 12/21/99  C:35   PROJECT: SUPPORT WA101 #21089                *
     H*                  WHEN DELETING, DO NOT CHECK FOR NCCI RECORDS *
     H*                  OR NON-TRANSFERRED A/R BATCH RECORDS IF IN   *
     H*                  QUOTE.                                       *
     H*                                                               *
     H* 11/18/99 C:61  PROJECT WC577                                  *
     H*                Add premium notice option (opt 32).            *
     H*                                                               *
     H* 11/17/99 C:61  PROJECT WC577                                  *
     H*                From QUOTE system, do not allow options:       *
     H*                41, 60, 80                                     *
     H*                                                               *
     H*  08/13/99 C:87  PROJECT: SUPP                                 *
     H*                 Set on *inlr in pgmend to prevent error found *
     H*                 when processing option 17=Policy page.        *
     H*                                                               *
     H* 08/06/99 C:113 PROJECT: CM045                                 *
     H*                If a Waiver stat code was not defined for a    *
     H*                Group/State, give error on Opt. 58.            *
     H*                                                               *
     H*  06/24/99 C:61 PROJECT 48WQJ                                  *
     H*                Add previous fund number to header info.       *
     H*                                                               *
     H*  06/22/99 C:33  SEQ#: WA102/18550                             *
     H*                 DO NOT DISPLAY OPT 60 IF W0612R IS IN CALL    *
     H*                 STACK.  THIS CHANGE WAS IMPLEMENTED BEFORE    *
     H*                 CM045 WAS INSTALLED, THEREFORE WE MUST CHECK  *
     H*                 FOR THE PROGRAM NEEDED FOR OPT 58 AND IF IT IS*
     H*                 NOT FOUND, INACTIVATE IT.                     *
     H*                                                               *
     H*  05/23/99 C:87  PROJECT: SUPP WC096 #16192                    *
     H*                 Suppress opt 17=Policy Page if emp is not     *
     H*                 state rptg div.  Modified code to use UPDOFA  *
     H*                 from copybook to disable/enable options in    *
     H*                 place of the CLRFKY.                          *
     H*                                                               *
     H*  05/12/99 C:113 PROJECT: CM045                                *
     H*                 Waiver of Subro rewrite.                      *
     H*                 Added Option 58=Waiver Information.           *
     H*                                                               *
     H*  04/30/99 C:113 PROJECT: SUPPORT #17431                       *
     H*                 Read all records in the A/R Batch file for    *
     H*                 policy year to be deleted.                    *
     H*                                                               *
     H* 03/03/99  C:118  PROJECT: WC577                               *
     H*                  RE-COMPILED DUE TO CHANGES MADE IN DISPLAY   *
     H*                  FILE.  ADJUSTED INDICATORS FOR ADDITIONAL    *
     H*                  ERROR MESSAGE.                               *
     H*                                                               *
     H* 03/01/99  C:35   PROJECT: SUPPORT # 16886                     *
     H*                  DO NOT ALLOW OPTION 17 (POLICY PAGE) IF THE  *
     H*                  BILL HAS NOT BEEN RUN.                       *
     H*                                                               *
     H*  02/09/99 C:87  PROJECT: SUPPORT #16471                       *
     H*                 Do not call policy page program to generate a *
     H*                 policy page record (opt17) if the policy from *
     H*                 date is equal to the policy to date.          *
     H*                                                               *
     H*  01/15/99 C:09 PROJECT SUPP/WA095/SEQ #15883                  *
     H*                ADD TWO ADDITIONAL PARMS TO ACDPL3 TO MATCH    *
     H*                CURRENT PARMS TO W0810R SINCE PGM W0810R IS    *
     H*                USED BY BOTH WORK WITH CERT HOLDERS AND WORK   *
     H*                WITH WAIVERS. NEEDED TO ENSURE PGM WORKS O.K.  *
     H*                                                               *
     H*  01/12/99 C:61 PROJECT WC577                                  *
     H*                Add '# of employer divisions' to header info.  *
     H*                                                               *
     H*  11/04/98  C:65  PROJECT: SUPPORT                             *
     H*                  DO NOT ALLOW A REVISED BILL IF THERE IS ANY  *
     H*                  CANCELLATION DATE.                           *
     H*                                                               *
     H*  08/10/98  C:61 Project: 48WLC                                *
     H*                  Display old Emp#/Div from Employer Cross     *
     H*                  Reference (WZEXP).                           *
     H*                                                               *
     H*  08/07/98 C:78 SEQ#: DR095/12876                              *
     H*                ADD PARAMETER ##QTE WHEN CALLING W0051R10      *
     H*                FOR QUOTE MODE.                                *
     H*                                                               *
     H*  07/30/98  C:35  PROJECT: SUPPORT                             *
     H*                  CANCELLATIONS AND REINSTATEMENTS SHOULD BE   *
     H*                  AVAILABLE EVEN IF WCPOLS IS NOT INSTALLED.   *
     H*                                                               *
     H*  06/26/98  C:77  PROJECT: WC705                               *
     H*                  REMOVE EDIT PREVENTING REVISED BILLING       *
     H*                  PRIOR TO POLICY EFFECTIVE DATE.              *
     H*                                                               *
     H*  06/25/98  C:87  PROJECT: WC630                               *
     H*                  REMOVE SYSTEM CONTROL FLAG PROCESSING FOR    *
     H*                  OPTION 41=AUDIT TRACKING. IT IS A PART OF    *
     H*                  BASE PRODUCT, NO LONGER A SEPARATE MODULE.   *
     H*                                                               *
     H*  05/07/98 C:65 PROJECT: WC577                                 *
     H*                CHANGE CALL PARM FOR DEPT LETTER CALL.         *
     H*                                                               *
     H*  05/04/98 C:87 PROJECT: WC630                                 *
     H*                ADD OPTION 41=AUDIT TRACKING                   *
     H*                                                               *
     H*  04/28/98 C:65 PROJECT: WC705                                 *
     H*                ADD BILLING STATUS TO THE SUBFILE LISTING.     *
     H*                                                               *
     H*  03/31/98 C:65 PROJECT: WC705                                 *
     H*                ADD BILL OPTION WINDOW FOR AUDIT BILLS.        *
     H*                                                               *
     H*  03/23/98 C:85 PROJECT: WC773                                 *
     H*                DETERMINE IF CLIENT IS ADMIN-ONLY,             *
     H*                OR CLAIMS-ONLY.                                *
     H*                                                               *
     H*  02/18/98  C:33  PROJECT: WC577                               *
     H*                  ALLOW DELETION IF A/R TRANS DATE IS IN       *
     H*                  CURRENT MONTH                                *
     H*                                                               *
     H* 02/12/98  C:35  PROJECT: SUPPORT                              *
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
     H*  02/12/98  C:33  PROJECT: WC577                               *
     H*                 -CHECK NCCI SUBMISSION FOR STATUS OF '4'.     *
     H*                 -CHECK FOR NON-TRANSFERRED A/R BATCH RECORDS. *
     H*                                                               *
     H*  02/10/98  C:33  PROJECT: WC577                               *
     H*                  DO NOT USE USER PARM FYR FOR INITIAL DISPLAY.*
     H*                                                               *
     H*  02/08/98  C:65  PROJECT: WC577                               *
     H*                  FILL XXDATE WITH SYSTEM DATE FOR OPT 4.      *
     H*                                                               *
     H*  02/04/98  C:65  PROJECT: WC577                               *
     H*                  ADDED MORE CONDITIONS FOR RUNNING REVISED    *
     H*                  BILLS.                                       *
     H*                                                               *
     H*  01/15/98  C:33  PROJECT: WC577                               *
     H*                  REMOVE CODE THAT HAD BEEN CHANGED INCORRECTLY*
     H*                                                               *
     H*  12/31/97  C:33  PROJECT: CM027                               *
     H*                  REMOVE OPT 45-SUBRO WAIVERS                  *
     H*                                                               *
     H*  11/20/97  C:57  PROJECT: CM027                               *
     H*                  ADD OPT 45-SUBRO WAIVERS                     *
     H*                  ADD OPT 54-EMPLOYER ENDORSEMENT FORMS        *
     H*                                                               *
     H*  11/07/97  C:33  PROJECT: WC577                               *
     H*                  ADD OPT 18-MODIFY POLICY EFFECTIVE DATES     *
     H*                                                               *
     H*  11/05/97  C:33  PROJECT: WC577                               *
     H*                  ADD WINDOW FOR BILLING OPTIONS               *
     H*                                                               *
     H*  10/30/97  C:33  PROJECT: WC577                               *
     H*                  REMOVE OPTIONS FOR QUOTE PROCESSING          *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  31 - Called as Quote                                         *
     H*  32 - DO NOT RUN A REVISED BILL IF ACCOUNT IS AUDITED.        *
     H*  33 - DO NOT RUN A REVISED BILL IF PRIOR TO POLICY EFF DATE.  *
     H*  34 - DO NOT RUN A REVISED BILL IF ACCOUNT IS CANCELLED.      *
     H*  35 - ORIGINAL BILL/AUDIT BILL WINDOW                         *
     H*  40 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0051F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PFRCDF)
     FWDELL5    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEL01:DBRCDF)
     F***  BEGIN ADD  ***  02/09/99  **********************************
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL0X)
     F***  END   ADD  ***  02/09/99  **********************************
      **** BEGIN ADD **** 10/12/01 ************************************
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      **** END   ADD **** 10/12/01 ************************************
     F*
      **** BEG ADD ***** 09/26/03 *************************************
     F** Employer Detail History file
     FWDEHL6    IF   E           K DISK
      **** BEG ADD ***** 02/11/04 *************************************
     FWDEHL13   IF   E           K DISK
     F                                     RENAME(PEH01:PEH02)
      **** END ADD ***** 02/11/04 *************************************
     F*** BEG CHG  ******  01/13/04  **************************************
      ** Ach Direct Debit File
     F**********  WMDBL   IF  E           K        DISK
     FWMDBL3    IF   E           K DISK
     F*** END CHG  ******  01/13/04  **************************************
      **** END ADD ***** 09/26/03 *************************************
     F***** BEG ADD ******* 07/27/10 **********************************
     FWMEFL6    IF   E           K DISK
     F***** END ADD ******* 07/27/10 **********************************
     F*
     FWMEMP     IF   E             DISK
     F***** BEG ADD ******* 01/12/99 **********************************
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F***** END ADD ******* 01/12/99 **********************************
     F***** BEG ADD ******* 01/12/99 **********************************
     FWMLDL1    IF   E           K DISK
     F***** END ADD ******* 01/12/99 **********************************
     F***** BEG ADD ******* 08/10/98 **********************************
     F*
     F*  Employer Cross Reference
     FWZEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
     F***** END ADD ******* 08/10/98 **********************************
     F*
     FWMFNL     IF   E           K DISK
     F                                     INFDS(FNFDB)
     F*
     F***  Begin Del  ***  08/21/06  **************************************
     F**********WMCML   IF  E           K        DISK
     F**********
     F**********WMEAL   IF  E           K        DISK
     F***  End   Del  ***  08/21/06  **************************************
     F*
     FWMEXL     IF   E           K DISK
     F*
     FWMEPL1    IF   E           K DISK
     F***  BEGIN DEL  ***  02/10/98  ********************************
     F**********WMUPL   IF  E           K        DISK
     F***  END   DEL  ***  02/10/98  ********************************
     F***  BEGIN ADD  ***  02/12/98  ********************************
     F*
     F***  Begin Del  ***  08/21/06  **************************************
     F**********WMSHL13 IF  E           K        DISK
     F**********
     F**********WDEAL5  IF  E           K        DISK
     F**********  PEA01                             KRENAMEPEA02
     F***  End   Del  ***  08/21/06  **************************************
     F***  END   ADD  ***  02/12/98  ********************************
     F*
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PRS01)
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
     F*** BEGIN ADD *** 08/06/99 ***********************************************
     F** Statistical Reporting/Rating File
     FWDRRL     IF   E           K DISK
     F*
     F** Rate Set Code Table File
     FWMRSL     IF   E           K DISK
     F*** END   ADD *** 08/06/99 ***********************************************
      ***** BEG ADD ******* 05/26/10 **********************************
      *
      *  Fee Information
     F***  Begin Chg  ***  02/11/11  **************************************
     F***WTFIL***IF  E           K        DISK
     FWTFIL3    IF   E           K DISK
     F***  End   Chg  ***  02/11/11  **************************************
      ***** END ADD ******* 05/26/10 **********************************
      **** BEGIN ADD **** 10/12/01 ************************************
     F** Carrier/State Reporting Master File
     FWMRPL4    IF   E           K DISK
     F*
     F** Group Carrier Information File
     FWMGCL     IF   E           K DISK
      **** END   ADD **** 10/12/01 ************************************
     F***  Begin Add  ***  12/01/06  **************************************
     FWDF2L     IF   E           K DISK
     F***  End   Add  ***  12/01/06  **************************************
      ***** BEG ADD ******* 10/27/06 **********************************
      *
      * SCHEDULED DEBIT/CREDIT FORM
     FWMRFL     IF   E           K DISK
      ***** END ADD ******* 10/27/06 **********************************
      ***** BEG ADD ******* 12/30/09 **********************************
      * USER PARAMETERS FILE
     FWMUPL     IF   E           K DISK
      ***** END ADD ******* 12/30/09 **********************************
     F*** BEGIN ADD *** 03/12/10 **************************************
     FWTGTL     IF   E           K DISK
     F*** END   ADD *** 03/12/10 **************************************
      /COPY SCOPYWW01D
     D*
     D*** BEG CHG ***** 11/20/97 ***********************************************
     D***                 OF1        35  1
     D***  BEGIN CHG  ***  01/15/98  *********************************
     D**********          OF1        37  1
     D OF1             S              1    DIM(35)
     D***  END   CHG  ***  01/15/98  *********************************
     D*** END CHG ***** 11/20/97 ***********************************************
     D HD              S             74    DIM(8) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(50) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(50) ALT(OF)
     D**** BEGIN ADD **** 02/12/98 *********************************************
     D**** END   ADD **** 02/12/98 *********************************************
     D*** BEGIN ADD *** 04/28/98 ***********************************************
     D BL              S             10    DIM(4) CTDATA PERRCD(1)              BILLING STATUS
     D*** END ADD ***** 04/28/98 *************************************
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D FNFDB           DS
     D  FNRRN                397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  EATRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D                 DS
     D  EAPODT                 1      7  0
     D  XXPOYM                 1      5  0
     D                 DS
     D  XXDATE                 1      7  0
     D  XXCUYM                 1      5  0
     D*
      ***** BEG ADD ***** 03/23/06 *********************************************
     D                 DS
     D  EHEFDT                 1      7  0
     D  XHEFYM                 1      5  0
     D                 DS
     D  EHTRDT                 1      7  0
     D  XHTRYM                 1      5  0
      ***** END ADD ***** 03/23/06 *********************************************
     D*
     D OPTNM           C                   CONST('Employer Policy Year-
     D                                     s')
      ***** BEG ADD ******* 11/18/99 **********************************
     D PRENOT          C                   CONST('            PREMIUM -
     D                                      NOTICES           ')
      ***** END ADD ******* 11/18/99 **********************************
     D***  BEGIN ADD  ***  11/05/97  *********************************
     D OPTB30          C                   CONST('Original Bill Option-
     D                                     s')
     D***  END   ADD  ***  11/05/97  *********************************
     D***  BEGIN ADD  ***  03/31/98  *********************************
     D OPTA30          C                   CONST('Audit Bill Options')
     D***  END   ADD  ***  03/31/98  *********************************
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0051R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(2)
     D INFOMX          C                   CONST(1)
     D***  BEGIN DEL  ***  05/23/99  **********************************
     D***  BEGIN ADD  ***  10/30/97  *********************************
     D************* '                    -C         CLRFKY
     D************* '       NNN'
     D***  END   ADD  ***  10/30/97  *********************************
     D***  END   DEL  ***  05/23/99  **********************************
     D*** BEGIN ADD *** 03/12/10 **************************************
     D OFD17           C                   CONST('1717=CCS Packets    -
     D                                            YNN')
     D*** END   ADD *** 03/12/10 **************************************
     D*** BEGIN ADD *** 11/23/10 **************************************
     D OFD48           C                   CONST('4848=Retro/Reten Rat-
     D                                     ing    YYN')
     D*** END   ADD *** 11/23/10 **************************************
     D***  Begin Add  ***  12/17/10  **************************************
     D OFD96           C                   CONST('9696=Premium Entry  -
     D                                            YYY')
     D***  End   Add  ***  12/17/10  **************************************
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D**** BEGIN ADD **** 02/12/98 *********************************************
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D**** END   ADD **** 02/12/98 *********************************************
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0

     C*
     C     *LIKE         DEFINE    ELFYR         KKFYR
     C     *LIKE         DEFINE    EMNAME        U1PDSC
     C     *LIKE         DEFINE    EMNAME        U2PDSC
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    FNEXDT        SYSDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C***  BEGIN  EL  ***  02/10/98  ********************************
     C********** *LIKE     DEFN UPLVL     SVLVL
     C***  END   DEL  ***  02/10/98  ********************************
     C*** BEGIN ADD *** 03/12/10 **************************************
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*** END   ADD *** 03/12/10 **************************************
     C***** BEG ADD ******* 01/12/99 **********************************
     C*
     C     *LIKE         DEFINE    EMCO#         KKCO#
     C     *LIKE         DEFINE    EMFND         KKFND
     C     *LIKE         DEFINE    EMEMP#        KKEMP#
     C*
     C***** END ADD ******* 01/12/99 **********************************
     C**** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     0             I1                3 0
     C                   Z-ADD     0             SCCX              7 0
     C**** END   ADD **** 02/12/98 *********************************************
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C                   PARM      @@QTE         ##QTE             1
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C     ACDPL2        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE
     C                   PARM                    ##CO#
     C                   PARM                    SCFND
     C                   PARM                    SCFYR
     C                   PARM                    ##KEY            10
     C*
     C     ACDPL3        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C     ACDPL4        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@CALL        ##CALL
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      @@QTE         ##QTE
     C                   PARM      'Y'           ##MODE
     C*
     C     ACDPL5        PLIST
     C                   PARM                    ##MODE            1
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      SCDBRR        ##RRN             9 0
     C***** BEGIN ADD ****** 11/30/10 *********************************
     C                   PARM      @@RRN2        ##RRN2            9 0
     C                   PARM                    ##RTCD            1
     C***** END   ADD ****** 11/30/10 *********************************
     C*
     C     ACDPL6        PLIST
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*** BEG ADD ***** 10/20/97 ***********************************************
     C*
     C     ACDPL7        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@CALL        ##CALL
     C                   PARM      SCDBRR        ##RRN2
     C*
     C     ACDPL8        PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM      1             ##VIEW            1 0
     C                   PARM      'S'           ##TYPE            1
     C*** END ADD ***** 10/20/97 ***********************************************
     C*
     C*** BEGIN ADD *** 08/07/98 ***********************************************
     C     ACDPL9        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@QTE         ##QTE             1
     C*** END   ADD *** 08/07/98 ***********************************************
     C*
     C*** BEGIN ADD *** 05/12/99 ***********************************************
     C     ACDP10        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      'C'           ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*** END   ADD *** 05/12/99 ***********************************************
     C*
     C*** BEGIN ADD *** 05/04/98 ***********************************************
     C     ADTRPL        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      FNRRN         ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*** END   ADD *** 05/04/98 ***********************************************
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C***  BEGIN ADD  ***  02/10/98  ********************************
     C                   PARM                    ##LVL             3 0
     C***  END   ADD  ***  02/10/98  ********************************
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
      ***** BEG ADD ******* 05/10/02 **********************************
     C                   PARM      *ZERO         ##AREA            3 0
     C                   PARM      *ZERO         ##PHON            7 0
     C                   PARM      '*'           ##PLAN            1
     C                   PARM      *ZERO         ##DATE            8 0
      ***** END ADD ******* 05/10/02 **********************************
      ******* beg add ****** 08/21/08 *********************************
      *
      *
      *==============================================================
      * KLIST
      *==============================================================
      ******* end add ****** 08/21/08 *********************************
     C*
     C*  GROUP MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C*  GENERIC TABLE VALUES
     C*
     C     GTKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*** END   ADD *** 03/12/10 **************************************
     C***** BEG ADD ******* 08/10/98 **********************************
     C*
     C*  Employer Cross Reference
     C*
     C     ZEXKY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C***** END ADD ******* 08/10/98 **********************************
     C*
     C*  EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C***** BEGIN ADD ***** 12/01/04 **********************************
     C*
     C     EAKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    XXSDIV
     C                   KFLD                    SCFYR
     C***** END ADD ******* 12/01/04 **********************************
     C*
     C*  KEY LIST FOR ADMIN/DARTS FILE
     C*
     C     EXKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    SCFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C***** BEG ADD ******* 03/14/10 **********************************
     C*
     C*  EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     LDKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C***** END ADD ******* 03/14/10 **********************************
     C***  BEGIN ADD  ***  02/12/98  ********************************
     C*
     C*  NCCI SUBMISSINON HISTORY FILE
     C*
     C     SHKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    SCFYR
     C***  END   ADD  ***  02/12/98  ********************************
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
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    KKFYR
     C*
     C     KEY1C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C***** BEG ADD ******* 01/12/99 **********************************
     C*
     C*  Employer Master
     C*
     C     EMKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C*
     C***** END ADD ******* 01/12/99 **********************************
      *
      **** BEG ADD ***** 09/26/03 *************************************
      *
      * ACH Master file
      *
     C     DBKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C***  BEG ADD ******  01/13/04  **************************************
     C     DBKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXDATE
     C***  END ADD ******  01/13/04  **************************************
      *
      ****** BEG ADD ***** 02/11/04 *************************************
     C     DBKY2         KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
      *
     C     DBKY3         KLIST
     C                   KFLD                    EHCO#
     C                   KFLD                    EHFND
     C                   KFLD                    EHEMP#
     C                   KFLD                    EHDIV
     C                   KFLD                    XXDATE
      ****** END ADD ***** 02/11/04 *************************************
     C*
     C* Employer Premium History file
     C*
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    XXDATE
     C*
     C     EHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      **** END ADD ***** 09/26/03 *************************************
      *
      **** BEG ADD ***** 02/11/04 *************************************
     C     EHKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
      *
     C     EHKY3         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    XXDATE
      **** END ADD ***** 02/11/04 *************************************
     C*** BEGIN ADD *** 08/06/99 ***********************************************
     C*
     C*  STATISTICAL REPORTING/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST              2
     C                   KFLD                    ELPFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    ELRTCD
     C*** END   ADD *** 08/06/99 ***********************************************
      ******* BEG add ****** 08/21/08 *********************************
      *
      *  RATE SET
      *
     C     RSKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      ******* end add ****** 08/21/08 *********************************
      ***** BEG ADD ******* 05/26/10 **********************************
      *
      *  Fee Information
      *
     C     CIKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
      *
     C     CIKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
      *
      ***** END ADD ******* 05/26/10 **********************************
      **** BEGIN ADD **** 10/12/01 ************************************
     C*
     C*  CARRIER/STATE REPORTING MASTER FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C*
     C     RPKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  GROUP CARRIER INFORMATION FILE
     C*
     C     GCKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      **** END   ADD **** 10/12/01 ************************************
     C***  Begin Add  ***  12/01/06  **************************************
     C     F2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C***  End   Add  ***  12/01/06  **************************************
      *
      ***** BEG ADD ******* 10/27/06 **********************************
      *
      *  Scheduled Debit/Credit Form
      *
     C     RFKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    SCFYR
      ***** END ADD ******* 10/27/06 **********************************
      ***** BEG ADD ******* 12/30/09 **********************************
     C* KLIST OF WTBSP
     C     BSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    XXBSCD
     C*
      ***** END ADD ******* 12/30/09 **********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C*
     C*  NOTE:  @@RRN WILL RETURN THE RRN OF WDELP.
     C*         @@RRN2 NEEDS TO BE PASSED IN AS THE RRN OF WMEMP.
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* Do not allow CCS Packet submission if the policy type
     C* is not setup.  This will also include the case where
     C* renewal report is setup but the claim isn't a renewal.
     C*
B1   C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '17'
     C     XXCLT#        ANDEQ     '025'
     C*
     C                   MOVEL(P)  'PTYP'        KKTBID
     C                   MOVEL(P)  ELQTYP        KKKEY
     C     GTKY          CHAIN     PGT01                              99
B2   C     *IN99         IFEQ      *ON
     C                   SETON                                        8890
S2   C                   ELSE
B3   C     GTFG03        IFEQ      *BLANKS
     C     GTFG04        ANDEQ     *BLANKS
     C     GTFG05        ANDEQ     *BLANKS
B4   C     GTFG02        IFEQ      *BLANKS
     C                   SETON                                        8890
S4   C                   ELSE
B5   C     ELNWRW        IFEQ      'N'
     C                   SETON                                        8890
E5   C                   ENDIF
E4   C                   ENDIF
E3   C                   ENDIF
E2   C                   ENDIF
     C*
E1   C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
      ******* beg add ****** 08/21/08 *********************************
      *
      * Schedule Debit/Credit factor must not exceed entered maximum values
      *
     C     *IN90         IFEQ      *OFF
     C     SCSEL         ANDEQ     '30'
      *
     C     RSKY1         CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     ELDBCR        ANDNE     *ZERO
      *
     C     ELDBCR        IFGT      1.0
      *
     C     ELDBCR        IFGT      RRDMAX
     C     RRDMAX        ANDNE     *ZERO
     C                   SETON                                        20  90
     C                   ENDIF                                                  RRDMAX
      *
     C                   ENDIF                                                  ELDBCR > 1
      *
     C     ELDBCR        IFLT      1.0
      *
     C     ELDBCR        IFLT      RRCMAX
     C     RRCMAX        ANDNE     *ZERO
     C                   SETON                                        42  90
     C                   ENDIF                                                  RRCMAX
      *
     C                   ENDIF                                                  ELDBCR < 1
      *
     C                   ENDIF                                                  RRKY
      *
     C                   ENDIF                                                  RSKY
      *
     C                   ENDIF                                                  SCSEL 30
      *
      ******* end add ****** 08/21/08 *********************************
     C*
     C                   SELECT
     C*
     C     SCSEL         WHENEQ    '4'
     C*
     C***  Begin Chg  ***  08/21/06  **************************************
     C********** SVLVL     IFLT EXDLTL
     C********** EXDLTL    OREQ *ZERO
     C**********
     C**********           SETON                     77  90
     C**********
     C**********           ELSE
     C**********
     C***CHECK*FOR A CLAIM (NOT IN QUOTE)
     C**********
     C*****BEGIN ADD  ***  02/12/98  *********************************
     C********** @@QTE     IFNE 'Q'
     C*****END** ADD  ***  02/12/98  *********************************
     C**********
     C********** CMKY      CHAINPCM01                60
     C**********
     C**N90***** *IN60     IFEQ *OFF
     C**********
     C**********           SETON                     71  90
     C**********
     C**********           ELSE
     C**********
     C***CHECK*FOR PAYMENTS IN ACCOUNTS RECEIVABLE FILE
     C**********
     C********** EAKY      CHAINPEA01                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C**********           SELEC
     C**********
     C********** EAARTY    WHEQ 'P'
     C********** EAARTY    OREQ 'I'
     C********** EAARTY    OREQ 'Q'
     C**********           SETON                     726090
     C**********
     C********** XXTRYM    WHGT XXPOYM
     C*****BEGIN CHG  ***  02/18/98  *********************************
     C********** XXTRYM    ANDLEXXCUYM
     C********** XXTRYM    ANDLTXXCUYM
     C*****END** CHG  ***  02/18/98  *********************************
     C**********           SETON                     746090
     C**********
     C********** XXTRYM    WHLT XXCUYM
     C********** XXPOYM    ANDLTXXCUYM
     C**********           SETON                     746090
     C**********
     C**********           OTHER
     C********** EAKY      READEPEA01                    60
     C**********
     C**********           ENDSL
     C**********
     C**********           ENDDO                           *IN60=*OFF
     C**********
     C*****BEGIN ADD ****** 12/01/04 **********************************
     C********** SCDBRR    CHAINPFRCDF               60
     C**********
     C**N90***** ELARSM    IFEQ 'Y'
     C********** ELDIV     ANDNEXXSDIV
     C**********
     C***CHECK*FOR BILLINGS IN STATE REPORTING DIVISION.
     C**********
     C********** EAKY1     CHAINPEA01                60
     C**********
     C********** *IN60     DOWEQ*OFF
     C**********
     C**********           SELEC
     C**********
     C********** EAARTY    WHEQ 'H'
     C********** EAARTY    OREQ 'F'
     C********** EAARTY    OREQ 'B'
     C********** EAARTY    OREQ 'A'
     C********** EAARTY    OREQ 'D'
     C**********           SETON                     836090
     C**********
     C**********           OTHER
     C******BEG*CHG ***** 08/05/05 ******************************
     C**Need*to*read with Key having State Reporting Division
     C********** EAKY      READEPEA01                    60
     C********** EAKY1     READEPEA01                    60
     C******END*CHG ***** 08/05/05 ******************************
     C**********
     C**********           ENDSL
     C**********
     C**********           ENDDO                           *IN60=*OFF
     C**********
     C**********           ENDIF                           ELARSM IFEQ
     C**********
     C*****END*ADD ******** 12/01/04 **********************************
     C**********
     C**********           ENDIF                           *IN60=*OFF
     C**********
     C*****BEGIN ADD  ***  02/12/98  ********************************
     C**********           ENDIF                           @@QTE<>Q
     C*****BEGIN ADD **** 12/21/99 *********************************************
B04  C********** @@QTE     IFNE 'Q'
     C*****END** ADD **** 12/21/99 *********************************************
     C**********
     C***CHECK*TO BE SURE NO NCCI SUBMISSION HAS A STATUS OF '4'
     C**********
     C****BEGIN*ADD *** 04/30/99 **************************************
     C********** SHKY      SETLLPSH01
     C****END***ADD *** 04/30/99 **************************************
     C**N90***** *IN60     DOUEQ*ON
     C**********
     C********** SHKY      READEPSH01                    60
     C**********
     C********** SHSTAT    IFEQ '4'
     C********** *IN60     ANDEQ*OFF
     C**********           SETON                     80  90
     C**********           LEAVE
     C**********           ENDIF                           SHSTAT=A
     C**********
     C**********           ENDDO                           *IN60=*ON
     C**********
     C***CHECK*FOR NON-TRANSFERRED A/R BATCH RECORDS
     C**********
     C****BEGIN*ADD *** 04/30/99 **************************************
     C********** EAKY      SETLLPEA02
     C****END***ADD *** 04/30/99 **************************************
     C**N90***** *IN60     DOUEQ*ON
     C**********
     C********** EAKY      READEPEA02                    60
     C**********
     C********** EASTAT    IFNE 'T'
     C********** *IN60     ANDEQ*OFF
     C**********           SETON                     81  90
     C**********           LEAVE
     C**********           ENDIF                           EASTAT=A
     C**********
     C**********           ENDDO                           *IN60=*ON
     C*****END** ADD  ***  02/12/98  ********************************
     C*****BEGIN ADD **** 12/21/99 *********************************************
B04  C**********           ENDIF                           @@QTE IFNE 'Q'
     C*****END** ADD **** 12/21/99 *********************************************
     C**********
     C******BEG*ADD ***** 03/23/06 *********************************************
     C**********
     C********** EHKY      SETLLPEH01
     C********** EHKY1     REDPEPEH01                    60
     C********** *IN60     DOWEQ*OFF
     C**********
     C********** XHTRYM    IFLT XXCUYM
     C********** XHEFYM    ANDLTXXCUYM
     C**********           SETON                     846090
     C**********           ENDIF                           XDTRYM < XXCUYM
     C**********
     C**N90***** EHKY1     REDPEPEH01                    60
     C**********           ENDDO
     C**********
     C******END*ADD ***** 03/23/06 *********************************************
     C**********
     C**********           ENDIF                           SVLVL<EXDLTL
     C*
     C*IN72 - Payments Exist
     C*IN71 - Claims Exist
     C*IN74 - A/R Exists in prior month
     C*IN77 - Not Authorized
     C*IN80 - NCCI Submission Status '4' Exist
     C*IN81 - Non x-fer'd A/R Batch Rcds Exist
     C*IN83 - State Reporting Div Billings Exist
     C*IN84 - Detail History Exist
     C*IN86 - Payroll Exist
     C*IN87 - Employer Master Cx'd
     C                   CALL      'W0051R04'
     C                   PARM      SCDBRR        ##RRN
     C                   PARM      @@QTE         ##QTE
     C                   PARM      *ZEROS        ##IND             2 0
     C*
     C     *LIKE         DEFINE    ##IND         IN
     C*
     C     ##IND         IFNE      *ZEROS
     C                   Z-ADD     ##IND         IN
     C                   MOVE      *ON           *IN(IN)
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C***  End   Chg  ***  08/21/06  **************************************
     C*
      ***** BEG ADD ******* 04/13/07 **********************************
      * Opt 17 (Policy Page) is not allowed when either the Scheduled
      * DB/CR form is incomplete or it has not printed yet, and if is
      * client 114.
     C     SCSEL         WHENEQ    '17'
      ***** BEG ADD ***** 07/13/07 ************************************
     C     XXCLT#        ANDEQ     '114'
      ***** END ADD ***** 07/13/07 ************************************
     C     RRKY          SETGT     WDRRL
     C     RRKY1         READPE    WDRRL                                  60
     C     *IN60         IFEQ      *OFF
     C     RRDBCR        ANDEQ     'Y'
     C     SCDBCR        ORNE      *ZEROS
     C     SCDBCR        ANDNE     1
      *
     C     RFKY          CHAIN     RFP01                              60
     C     *IN60         IFEQ      *ON
      * Scheduled DB/CR form info in Employer detail is incomplete *IN17
     C                   SETON                                        17  90
     C                   ENDIF                                                  RFKY
      *
     C     *IN60         IFEQ      *OFF
     C     RFPRTF        ANDNE     'Y'
      * Scheduled DB/CR form MUST BE PRINTED
     C                   SETON                                        18  90
     C                   ENDIF                                                  RFPRTF <> Y
     C     *IN60         IFEQ      *OFF
     C     *IN90         ANDEQ     *OFF
      * Check for Missing Reasons-DB/CR form info incomplete - *IN17
     C                   SELECT
     C     RFPRCA        WHENGT    *ZEROS
     C     RFPRCA        ANDNE     1
     C     RFPRRS        ANDEQ     *BLANKS
     C                   SETON                                        17  90
     C     RFCPCA        WHENGT    *ZEROS
     C     RFCPCA        ANDNE     1
     C     RFCPRS        ANDEQ     *BLANKS
     C                   SETON                                        17  90
     C     RFMFCA        WHENGT    *ZEROS
     C     RFMFCA        ANDNE     1
     C     RFMFRS        ANDEQ     *BLANKS
     C                   SETON                                        17  90
     C     RFSDCA        WHENGT    *ZEROS
     C     RFSDCA        ANDNE     1
     C     RFSDRS        ANDEQ     *BLANKS
     C                   SETON                                        17  90
     C     RFEMCA        WHENGT    *ZEROS
     C     RFEMCA        ANDNE     1
     C     RFEMRS        ANDEQ     *BLANKS
     C                   SETON                                        17  90
     C     RFMCCA        WHENGT    *ZEROS
     C     RFMCCA        ANDNE     1
     C     RFMCRS        ANDEQ     *BLANKS
     C                   SETON                                        17  90
     C     RFMSCA        WHENGT    *ZEROS
     C     RFMSCA        ANDNE     1
     C     RFMSRS        ANDEQ     *BLANKS
     C                   SETON                                        17  90
     C                   ENDSL
     C                   ENDIF                                                  60 & 90 = OFF
      *
     C     *IN60         IFEQ      *OFF
     C     *IN90         ANDEQ     *OFF
      *
      ***** BEG DEL ***** 07/13/07 ************************************
      *  Check sum of WMRFP BD/CR fields = SCBDCR
     C********** RFPRCA    ADD  RFPRDA    ZZDBCR  52
     C**********           ADD  RFCPCA    ZZDBCR
     C**********           ADD  RFCPDA    ZZDBCR
     C**********           ADD  RFMFCA    ZZDBCR
     C**********           ADD  RFMFDA    ZZDBCR
     C**********           ADD  RFSDCA    ZZDBCR
     C**********           ADD  RFSDDA    ZZDBCR
     C**********           ADD  RFEMCA    ZZDBCR
     C**********           ADD  RFEMDA    ZZDBCR
     C**********           ADD  RFMCCA    ZZDBCR
     C**********           ADD  RFMCDA    ZZDBCR
     C**********           ADD  RFMSCA    ZZDBCR
     C**********           ADD  RFMSDA    ZZDBCR
      ***** END DEL ***** 07/13/07 ************************************
      *
      ***** BEG ADD ***** 07/13/07 ************************************
     C                   Z-ADD     *ZEROS        ZZDBCD            5 2
     C                   Z-ADD     *ZEROS        ZZDBCC            5 2
     C                   Z-ADD     *ZEROS        ZZTOTC            5 4
     C                   Z-ADD     *ZEROS        ZZTOTD            5 4
     C                   Z-ADD     *ZEROS        ZZDBCR            5 4
      *
     C                   ADD       RFPRCA        ZZDBCC
     C                   ADD       RFPRDA        ZZDBCD
     C                   ADD       RFCPCA        ZZDBCC
     C                   ADD       RFCPDA        ZZDBCD
     C                   ADD       RFMFCA        ZZDBCC
     C                   ADD       RFMFDA        ZZDBCD
     C                   ADD       RFSDCA        ZZDBCC
     C                   ADD       RFSDDA        ZZDBCD
     C                   ADD       RFEMCA        ZZDBCC
     C                   ADD       RFEMDA        ZZDBCD
     C                   ADD       RFMCCA        ZZDBCC
     C                   ADD       RFMCDA        ZZDBCD
     C                   ADD       RFMSCA        ZZDBCC
     C                   ADD       RFMSDA        ZZDBCD
      *
     C     ZZDBCC        IFNE      *ZEROS
     C     ZZDBCC        MULT      -1            ZZDBCC
     C     ZZDBCC        DIV       100           ZZTOTC                         Total Credits
     C                   ENDIF
      *
     C     ZZDBCD        IFNE      *ZEROS
     C     ZZDBCD        DIV       100           ZZTOTD                         Total Debits
     C                   ENDIF
      *
     C     ZZTOTC        IFNE      *ZEROS
     C     ZZTOTD        ORNE      *ZEROS
     C     ZZTOTC        ADD       ZZTOTD        ZZDBCR
     C     ZZDBCR        ADD       1.000         ZZDBCR
     C                   ENDIF
      ***** END ADD ***** 07/13/07 ************************************
      *
     C     SCDBCR        IFNE      ZZDBCR
     C                   SETON                                        19  90
     C                   ENDIF                                                  SCDBCR<>ZZDBCR
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDIF                                                  RRDBCR <> "Y"
      *
      ***** END ADD ******* 04/13/07 **********************************
      *
     C**** BEGIN DEL **** 07/30/98 *********************************************
     C********** SCSEL     WHEQ '25'
     C********** SCSEL     OREQ '26'
     C**********
     C** CANNOT EXECUTE THESE OPT'S IF CLIENT NOT ENROLLED IN "WCPOLS"
     C**********
     C******************** MOVEL'WC'      XXSYK
     C********** SYKY      CHAINSWC01                60
     C********** *IN60     IFEQ *ON
     C********** *IN60     OREQ *OFF
     C********** WCFL03    ANDNE'Y'                        NOT ENROLLED,
     C******************** SETON                     78  90
     C******************** ENDIF                           WCFL03<>Y
     C**** END   DEL **** 07/30/98 *********************************************
     C*
     C**
     C     SCSEL         WHENEQ    '34'
     C***  BEGIN ADD  ***  06/30/00  *********************************
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C                   Z-ADD     0             SCCX
     C*
     C                   Z-ADD     1             I1
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
     C                   Z-ADD     CX(I1)        SCCX
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C     SCCX          IFGT      ELPTDT
     C                   Z-ADD     0             SCCX
     C                   ENDIF
     C*
     C     SCCX          IFNE      *ZERO
     C     XXDT          ANDLT     SCCX
     C     ELPFDT        ANDNE     ELPTDT
     C                   MOVE      *ON           *IN36
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C     SCCX          IFEQ      *ZERO
     C     XXDT          ANDLT     ELPTDT
     C     ELPFDT        ANDNE     ELPTDT
     C                   MOVE      *ON           *IN37
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C***  END ADD    ***  06/30/00  *********************************
     C*
     C*  MAKE SURE ALL AUDITED PAYROLLS HAVE BEEN ENTERED
     C*
     C     EPKY          CHAIN     PEP01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFEQ      'A'
     C     EPSAFL        ANDNE     'A'
     C                   SETON                                        79  90
     C                   ENDIF                                                  EPSTAT=A/EPSAFL<>A
     C*
     C     EPKY          READE     PEP01                                  60
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C***  BEGIN ADD  ***  06/30/00  *********************************
     C                   ENDIF
     C***  END ADD    ***  06/30/00  *********************************
      *
     C*** BEG ADD ***** 12/30/09 *************************************
     C* CHECK THE USER AUTHORITY FOR AUDIT BILL IN CASE OF LARGE DEDUTIBLE.
     C*
     C                   MOVE      *OFF          *IN43
     C*
     C  N90ELBSCD        IFNE      *ZERO
      *
     C                   MOVEL     ELBSCD        XXBSCD           10
      *
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFEQ      'L'
      *
     C                   MOVE      EXLDAL        ##CLVL
     C                   MOVE      EXLDAT        ##CTYP
      *
     C     Q1USER        CHAIN     PUP01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPLVL         ##ULVL
     C                   MOVE      UPTYPE        ##UTYP
     C                   ENDIF
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
      *
     C     ##RES         IFEQ      '1'
     C                   SETON                                        43  90
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD ***** 12/30/09 *************************************
      **
      ***** BEG ADD ******* 02/13/01 **********************************
      *
      *  Do not allow billing if employer cancelled
      *
     C     SCSEL         WHENEQ    '30'
     C     SCCXDT        ANDNE     *ZERO
     C     SCSEL         OREQ      '31'
     C     SCCXDT        ANDNE     *ZERO
     C                   SETON                                        38  90
      *
      ***** END ADD ******* 02/13/01 **********************************
     C*
      **** BEGIN ADD **** 06/28/02 ************************************
     C     SCSEL         WHENEQ    '30'
     C     SCSEL         OREQ      '31'
      **** BEG ADD ****** 08/07/02 ************************************
      **** BEG DEL ****** 12/05/03 ************************************
      **Do*not*allow*revised billing if system date is less than billing start
      **date.************
     C***********SCSEL***  IFEQ '31'
     C***********SCDBRR**  CHAINPFRCDF               60
     C***********XXDATE**  IFLT ELBSDT
      *****BEG*ADD******* 01/10/03 ************************************
     C***********SC#DIV**  ANDEQ1
      *****END*ADD******* 01/10/03 ************************************
     C*******************  SETON                     70  90
     C*******************  ENDIF
     C*******************  ENDIF
      **** END DEL ****** 12/05/03 ************************************
      **** END ADD ****** 08/07/02 ************************************
      *
      ***** BEG ADD *****  09/26/03  ************************************
      *
      * Turn flag on here if opt 30 would rerun an original bill with a date
      * subsequent to the first run. The flag will be looked at in the opt 30
      * subroutine to prevent A/R from being changed.
      *
     C     SCSEL         IFEQ      '30'
     C     @@QTE         ANDNE     'Q'
     C     ACHFLG        ANDEQ     *ON
      *
      *** BEG CHG ***** 02/11/04 ************************************
     C********** DBKY      CHAINPDB01                60
     C********** *IN60     IFEQ *OFF
     C********** DBSTAT    ANDEQ'A'
     C********** EHKY      SETLLPEH01
     C********** EHKY1     REDPEPEH01                    60
     C********** *IN60     IFEQ *OFF
     C**********           MOVE *ON       PRVBIL  1        PREVIOUS BILL FLAG
     C**********           ENDIF
     C**********           ENDIF                           *IN60 = *OFF
      *
     C     EHKY3         SETLL     PEH02
     C     EHKY2         READPE    PEH02                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEG CHG ***** 07/27/10 ************************************
     C*
     C* CHECK IF NEW ACH MODULE IS INSTALLED ('WC1806')
     C*
      **** BEG   CHG **** 10/22/10 *********************************************
     C******     XXNACH    IFEQ *ON
     C     XXNACH        IFEQ      *OFF
      **** END   CHG **** 10/22/10 *********************************************
      **** END   CHG **** 07/27/10 *********************************************
     C     DBKY2         CHAIN     PEF01                              62
     C     *IN62         DOWEQ     *OFF
     C     XXDATE        IFLT      EFEXDT
     C     XXDATE        ANDGE     EFEFDT
     C     EFEXDT        OREQ      *ZERO
     C     XXDATE        ANDGE     EFEFDT
     C                   MOVE      *ON           PRVBIL            1            PREVIOUS BILL FLAG
     C                   LEAVE
     C                   ENDIF
     C     DBKY2         READE     PEF01                                  62
     C                   ENDDO
      **** BEG   ADD **** 07/15/11 *********************************************
     C                   SETOFF                                       62
      **** END   ADD **** 07/15/11 *********************************************
     C     PRVBIL        IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
     C                   ELSE
     C*** END CHG ***** 07/27/10 ************************************
      *
     C     DBKY3         SETGT     PDB01
     C     DBKY2         READPE    PDB01                                  60
     C     *IN60         IFEQ      *OFF
     C     DBEXDT        ANDEQ     *ZERO
     C     *IN60         OREQ      *OFF
     C     DBEXDT        ANDNE     *ZERO
     C     XXDATE        ANDLT     DBEXDT
     C                   MOVE      *ON           PRVBIL            1            PREVIOUS BILL FLAG
     C                   LEAVE
     C                   ENDIF
     C*** BEG ADD ***** 07/27/10 ************************************
     C                   ENDIF
     C*** END ADD ***** 07/27/10 ************************************
      *
     C     EHKY2         READPE    PEH02                                  60
     C                   ENDDO
      *** END CHG ***** 02/11/04 ***********************************************
      *
     C                   ENDIF                                                  SCSEL = '30'
      ***** END ADD *****  09/26/03  ************************************
     C*
     C* ONLY ALLOW BILL TO BE RUN IF A CARRIER POLICY # IS ENTERED
     C* FOR ALL DIVISIONS
     C*
     C     GCKY          CHAIN     PGC01                              60
     C  N90*IN60         IFEQ      *OFF
     C     GCCRFL        ANDEQ     'Y'
      *
     C  N90@@QTE         IFNE      'Q'
     C     SCDBRR        CHAIN     PFRCDF                             60
      *
     C                   MOVE      *BLANKS       RPCAP#
     C     RPKY          SETLL     PEL02
     C     RPKY          READE     PEL02                                  60
     C     *IN60         DOUEQ     *ON
     C  N60RPKY2         CHAIN     PRP01                              61
     C     RPCAP#        IFEQ      *BLANKS
     C     *IN61         OREQ      *ON
     C                   SETON                                        736090
     C                   ENDIF                                                  RPCAP# = BLANKS
     C  N60RPKY          READE     PEL02                                  60
     C                   ENDDO                                                  IN60
     C                   ENDIF                                                  @@QTE <> Q
      *
     C                   ENDIF                                                  *IN60, GCCRFL
     C*
      **** END  ADD ***** 06/28/02 ************************************
     C*
      *********
      ******BEG DEL ******* 04/13/07 **********************************
     C*****Begin Add  ***  12/01/06  **************************************
     C*********  SCSEL     WHEQ '91'
     C*********  SCDBCR    ANDLE*ZERO
     C*********            SETON                     39  90
     C*****End*  Add  ***  12/01/06  **************************************
      ***** END DEL ******* 04/13/07 **********************************
      *** BEG ADD ***** 03/28/11 **************************************
     C     SCSEL         WHENEQ    '35'
     C     ELFAF         ANDEQ     *BLANK
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN62
      *** END ADD ***** 03/28/11 **************************************
      ***** BEG ADD ******* 04/13/07 **********************************
      * Scheduled Rating Info-Allowed when WDRRP/RRDBCD = "Y" based on
      * Premium rating state and policy effective date.
     C     SCSEL         WHENEQ    '49'
      ***** BEG ADD ***** 07/13/07 ************************************
     C     XXCLT#        ANDEQ     '114'
      ***** END ADD ***** 07/13/07 ************************************
     C     RSKY          CHAIN     WMRSL                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     WDRRL
     C     RRKY1         READPE    WDRRL                                  61
     C     *IN61         IFEQ      *OFF
     C     RRDBCR        ANDNE     'Y'
     C                   SETON                                        45  90
     C                   ENDIF                                                  RRDBCR <> "Y"
     C     *IN60         IFEQ      *ON
     C     *IN61         OREQ      *ON
     C                   SETON                                        45  90
     C                   ENDIF                                                  60 OR 61 ON
     C                   ENDIF                                                  60 = *OFF
      ***** END ADD ******* 04/13/07 **********************************
      ***** BEG ADD ******* 12/30/09 **********************************
     C     SCSEL         WHENEQ    '38'
      *
     C     ELBSCD        IFNE      *ZERO
      *
     C                   MOVEL     ELBSCD        XXBSCD           10
     C*
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFNE      'L'
     C                   SETON                                        44  90
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ******* 12/30/09 **********************************
      *
      ***** BEG ADD ******* 01/25/10 **********************************
      ***** BEG DEL ******* 05/26/10 **********************************
     C*********  SCSEL     WHEQ '63'
      *********
     C*********  ELCGCF    IFNE 'Y'
     C*********
     C*********            SETON                     46  90
     C*********
     C*********            ENDIF
      ***** END DEL ******* 05/26/10 **********************************
      ***** END ADD ******* 01/25/10 **********************************
      ***** BEG ADD ******* 05/26/10 **********************************
      *
     C     SCSEL         WHENEQ    '63'
      *
     C     RSKY1         CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXST
     C     CIKY          SETGT     PFI01
     C     CIKY1         READPE    PFI01                                  60                   60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     FIEXDT        IFEQ      0
     C     FIEXDT        ORGT      ELPFDT
     C                   LEAVE
     C                   ENDIF
      *
     C     CIKY1         READPE    PFI01                                  60                   60
     C                   ENDDO
      *
     C                   SELECT
      *
     C     *IN60         WHENEQ    *ON
     C                   SETON                                        46  90
      *
     C***  Begin Del  ***  02/11/11  **************************************
     C********** FICFEE    WHNE 'Y'
     C********** FICOLT    ANDNE'Y'
     C**********           SETON                     46  90
     C***  End   Del  ***  02/11/11  **************************************
      *
     C                   ENDSL
      *
     C                   ENDIF                                                  RSKY1
      *
      ***** END ADD ******* 05/26/10 **********************************
      *
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C                   MOVE      *HIVAL        KKFYR
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C   21              MOVE      SCPST1        KKFYR
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C                   MOVE      SCFYR         KKFYR
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C                   MOVE      ELBSCD        SCBSCD
     C*
     C                   MOVEL     *BLANKS       SCBSDS
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEL     BSDESC        SCBSDS
     C*
     C***  Begin Add  ***  12/01/06  **************************************
     C                   Z-ADD     ELDBCR        SCDBCR
     C***  End   Add  ***  12/01/06  **************************************
     C*** BEGIN ADD ***  04/28/98  ************************************
     C                   SELECT
     C     ELFAF         WHENEQ    'E'
     C                   MOVEL     BL(1)         SCDESC
     C     ELFAF         WHENEQ    'R'
     C                   MOVEL     BL(2)         SCDESC
     C     ELFAF         WHENEQ    'A'
     C                   MOVEL     BL(3)         SCDESC
     C                   OTHER
     C                   MOVEL     BL(4)         SCDESC
     C                   ENDSL
     C*** END ADD *****  04/28/98  ************************************
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT8 - SUBROUTINE FOR OPTION 8
     C*===============================================================
     C*
     C* OPTION 8=PAYROLL INFORMATION
     C*
     C     OPT8          BEGSR
     C*
      ***** BEG ADD ***** 03/17/06 ***********************************
     C                   SETOFF                                       39
     C     ELRTCD        IFEQ      *BLANKS
     C                   SETON                                        39  90
     C                   ELSE
      ***** END ADD ***** 03/17/06 ***********************************
     C*
     C***  BEGIN CHG  ***  10/30/97  *********************************
     C**********           CALL 'W0060R'  ACDPL3       99
     C                   CALL      'W0060R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      @@CALL        ##CALL
     C                   PARM                    ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C                   PARM      @@QTE         ##QTE
     C***  END   CHG  ***  10/30/97  *********************************
      *
      ***** BEG ADD ***** 03/17/06 ***********************************
     C                   ENDIF                                                  ELRTCD = BLANKS
      ***** END ADD ***** 03/17/06 ***********************************
     C*
     C                   ENDSR                                                  OPT8
     C*
     C*===============================================================
     C* OPT9 - SUBROUTINE FOR OPTION 9
     C*===============================================================
     C*
     C* OPTION 9=AGENT HISTORY
     C*
     C     OPT9          BEGSR
     C*
     C                   CALL      'W0553R'      ACDPL4                 99
     C*
     C                   ENDSR                                                  OPT9
     C*
     C*===============================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10
     C*===============================================================
     C*
     C* OPTION 10=PREMIUM INFORMATION
     C*
     C     OPT10         BEGSR
     C*
     C******** BEGIN CHG ******** 08/07/98 ****************************
     C***************      CALL 'W0051R10'ACDPL1       99
     C                   CALL      'W0051R10'    ACDPL9                 99
     C******** END   CHG ******** 08/07/98 ****************************
     C*
     C                   ENDSR                                                  OPT10
     C*
     C*===============================================================
     C* OPT11 - SUBROUTINE FOR OPTION 11
     C*===============================================================
     C*
     C* OPTION 11=PREMIUM HISTORY
     C*
     C     OPT11         BEGSR
     C*
     C                   CALL      'W0065R'      ACDPL1                 99
     C*
     C                   ENDSR                                                  OPT11
     C*
     C*===============================================================
     C* OPT12 - SUBROUTINE FOR OPTION 12
     C*===============================================================
     C*
     C* OPTION 12=CERTFICATE HOLDERS
     C*
     C     OPT12         BEGSR
     C*
     C                   CALL      'W0810R'      ACDPL3                 99
     C*
     C                   ENDSR                                                  OPT12
     C*
     C*===============================================================
     C* OPT14 - SUBROUTINE FOR OPTION 14
     C*===============================================================
     C*
     C* OPTION 14=DARTS
     C*
     C     OPT14         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      ##CO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C   31              MOVE      'Q'           QCQTE
     C  N31              MOVE      *BLANKS       QCQTE
     C*
     C*  CALL DARTS ENTRY PROGRAM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM      'W0050R'      XXPGM
     C*
     C                   ENDSR                                                  OPT14
     C*
     C***  BEGIN ADD  ***  11/07/97  *********************************
     C*===============================================================
     C* OPT18 - SUBROUTINE FOR OPTION 18
     C*===============================================================
     C*
     C* OPTION 18=MODIFY POLICY EFFECTIVE DATES
     C*
     C     OPT18         BEGSR
     C*
     C                   CALL      'W4800R'                             99
     C                   PARM      SCPRDS        ##PRDS           40
     C                   PARM      ' '           ##CALL            1
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      ##CO#         ##CO#
     C                   PARM      SCFND         ##FND
     C                   PARM      SCEMP#        ##EMP#
     C                   PARM      SCDIV         ##DIV
     C                   PARM      SCFYR         ZQFYR
     C*
     C                   ENDSR                                                  OPT18
     C*
     C***  END   ADD  ***  11/07/97  *********************************
     C*===============================================================
     C* OPT17 - SUBROUTINE FOR OPTION 17
     C*===============================================================
     C*
     C* OPTION 17=POLICY PAGE
     C*
     C     OPT17         BEGSR
     C*
     C                   MOVE      SCEMP#        ZQEMP#
     C*
     C***  BEGIN ADD  ***  02/09/99  **********************************
     C*  Only generate policy page if policy from date <> to date
     C                   Z-ADD     SCFYR         KKFYR
     C     KEY1B         CHAIN     PEL0X                              60
     C*** BEGIN ADD ***  04/02/04  ************************************
     C                   MOVE      *BLANK        XXBILL            1
     C     EHKY1         CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C     EHTRDT        ANDNE     *ZERO
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF
     C*** BEGIN ADD *** 03/12/10 **************************************
     C*
     C* IF CLIENT 25 AND FINAL AUDIT FLAG IS NOT BLANK THEN CONSIDERED
     C* BILL.
     C*
     C     XXCLT#        IFEQ      '025'
     C     ELFAF         ANDNE     *BLANKS
     C                   MOVE      'Y'           XXBILL
     C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
     C*** END ADD *****  04/02/04  ************************************
     C*** BEGIN ADD ***  04/04/08  ************************************
     C                   MOVE      *BLANK        XXFLAT            1
     C     XXCXDT        IFNE      *ZERO
     C     XXCXDT        ANDLE     ELPFDT
     C                   MOVE      'Y'           XXFLAT
     C                   ENDIF
     C*** END ADD *****  04/04/08  ************************************
     C     ELPFDT        IFNE      ELPTDT
     C***  END   ADD  ***  02/09/99  **********************************
     C**** BEGIN ADD **** 03/01/99 *********************************************
     C*** BEGIN CHG ***  04/02/04  ************************************
     C*********  ELFAF     ANDNE*BLANK
     C     XXBILL        ANDNE     *BLANK
     C*** END CHG *****  04/02/04  ************************************
     C**** END   ADD **** 03/01/99 *********************************************
     C*** BEGIN ADD ***  04/04/08  ************************************
     C     XXFLAT        ANDEQ     *BLANK
     C*** END ADD *****  04/04/08  ************************************
     C*
     C                   CALL      'W7412R'                             99
     C                   PARM      SCPRDS        QQPRDS           40
     C                   PARM      ' '           ZQCALL            1
     C                   PARM                    ZQEMP#            9
     C                   PARM                    QQRTC             1
     C                   PARM      ##CO#         ZQCO#             3 0
     C                   PARM      SCFND         ZQFND             3 0
     C                   PARM      SCFYR         ZQFYR             3 0
     C*
     C     QQRTC         IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  QQRTC=A
     C*
     C***  BEGIN ADD  ***  02/09/99  **********************************
     C                   ELSE
     C*
     C**** BEGIN ADD **** 03/01/99 *********************************************
     C*** BEGIN CHG ***  04/02/04  ************************************
     C*********  ELFAF     IFNE *BLANK
     C     XXBILL        IFNE      *BLANK
     C*** END CHG *****  04/02/04  ************************************
     C**** END   ADD **** 03/01/99 *********************************************
     C*          Display error window
     C                   EXFMT     WDW01
     C**** BEGIN ADD **** 03/01/99 *********************************************
     C                   ELSE
     C                   EXFMT     WDW02
     C                   ENDIF                                                  ELFAF IFNE *BLANK
     C**** END   ADD **** 03/01/99 *********************************************
     C*
     C                   ENDIF                                                  ELPFDT<>ELPTDT
     C***  END   ADD  ***  02/09/99  **********************************
     C*
     C                   ENDSR                                                  OPT17
     C*
     C*===============================================================
     C* OPT19 - SUBROUTINE FOR OPTION 19
     C*===============================================================
     C*
     C* OPTION 19=DEPARTMENT LETTERS
     C*
     C     OPT19         BEGSR
     C*
     C                   MOVE      SCEMP#        ZQEMP#
     C                   MOVE      SCDIV         ZQDIV
     C  N31              MOVEL     *BLANKS       ZQCALL
     C   31              MOVEL     'Q'           ZQCALL
     C*
     C                   CALL      'W4115R'                             99
     C                   PARM      SCPRDS        QQPRDS
     C                   PARM                    ZQCALL
     C                   PARM                    ZQEMP#
     C                   PARM                    ZQDIV             5
     C                   PARM      'PS'          QQDEPT            2
     C                   PARM      ##CO#         ZQCO#             3 0
     C                   PARM      SCFND         ZQFND             3 0
     C                   PARM      SCFYR         ZQFYR             3 0
     C*
     C     ZQCALL        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF                                                  ZQCALL=A
     C*
     C                   ENDSR                                                  OPT19
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=DEPARTMENT LETTERS
     C*
     C     OPT20         BEGSR
     C*
     C*** BEGIN ADD ***  05/07/98  ************************************
     C  N31              MOVEL     *BLANKS       ZQCALL
     C   31              MOVEL     'Q'           ZQCALL
     C*** END ADD *****  05/07/98  ************************************
     C                   CALL      'W4115R'
     C                   PARM      *BLANKS       ##PRDS           40
     C*** BEGIN CHG ***  05/07/98  ************************************
     C*****                PARM           ##CALL  1        Q=QUOTE SYSTEM
     C                   PARM                    ZQCALL                                     EM
     C*** END CHG *****  05/07/98  ************************************
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM      'PS'          ##DEPT            2
     C                   PARM                    ##CO#
     C                   PARM      SCFND         ##FND
     C                   PARM      SCFYR         ##FYR
     C*
     C                   ENDSR                                                  OPT20
     C*
     C*===============================================================
     C* OPT23 - SUBROUTINE FOR OPTION 23
     C*===============================================================
     C*
     C* OPTION 23=CONTRACTOR CLASS
     C*
     C     OPT23         BEGSR
     C*
     C                   CALL      'W0200R'                             99
     C                   PARM                    ##PRDS           40
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN
     C*
     C                   ENDSR                                                  OPT23
     C*
     C*===============================================================
     C* OPT25 - SUBROUTINE FOR OPTION 25
     C*===============================================================
     C*
     C* OPTION 25=CANCELLATION MAINTENANCE
     C*
     C     OPT25         BEGSR
     C*
     C                   CALL      'W0165R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM      'C'           ##TYPE            1
     C*
     C                   ENDSR                                                  OPT25
     C*
     C*===============================================================
     C* OPT26 - SUBROUTINE FOR OPTION 26
     C*===============================================================
     C*
     C* OPTION 26=REINSTATEMENT MAINTENANCE
     C*
     C     OPT26         BEGSR
     C*
     C*
     C                   CALL      'W0165R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCDBRR        ##RRN2            9 0
     C                   PARM      'R'           ##TYPE            1
     C*
     C*
     C                   ENDSR                                                  OPT26
     C*
     C*===============================================================
     C* OPT30 - SUBROUTINE FOR OPTION 30
     C*===============================================================
     C*
     C* OPTION 30=ESTIMATED BILL REQUEST
     C*
     C     OPT30         BEGSR
     C*
     C*** BEGIN ADD ***  03/31/98  ************************************
     C                   MOVE      *OFF          *IN35
     C*** END ADD *****  03/31/98  ************************************
     C***  BEGIN ADD  ***  11/05/97  *********************************
     C                   MOVEL(P)  OPTB30        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDB
     C                   CLEAR                   SCBLOP
     C*
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C                   EXFMT     BILLWDW
     C*
     C                   MOVE      *OFF          *IN71
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C  N90SCBLOP        IFNE      '1'
     C     SCBLOP        ANDNE     '2'
     C     SCBLOP        ANDNE     '3'
     C     SCBLOP        ANDNE     '4'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCBLOP<>1/2/3/4
     C*
     C  N90@@QTE         IFEQ      'Q'
     C     SCBLOP        ANDNE     '1'
     C     SCBLOP        ANDNE     '2'
     C                   SETON                                        71  90
     C                   ENDIF                                                  @@QTE=Q
     C*
      **** BEG ADD ****** 09/26/03 ************************************
     C  N90PRVBIL        IFEQ      *ON
     C     SCBLOP        ANDEQ     '3'
     C     PRVBIL        OREQ      *ON
     C     SCBLOP        ANDEQ     '4'
     C                   SETON                                        72  90
     C                   ENDIF                                                  PRVBIL
      **** END ADD ****** 09/26/03 ************************************
      **** BEGIN ADD **** 10/12/01 ************************************
     C*
      ***** BEG DEL ******* 06/28/02 **********************************
     C* ONLY ALLOW BILL TO BE RUN IF A CARRIER POLICY # IS ENTERED
     C* FOR ALL DIVISIONS
     C*********  GCKY      CHAINPGC01                60
     C**N90****  *IN60     IFEQ *OFF
     C*********  GCCRFL    ANDEQ'Y'
     C**N90****  @@QTE     IFNE 'Q'
     C*********  SCDBRR    CHAINPFRCDF               60
     C*********  RPKY      SETLLPRP01
     C*********  RPKY      READEPRP01                    60
     C*********  *IN60     DOUEQ*ON
     C*********  RPKY2     CHAINPRP01                61
     C*********  RPCAP#    IFEQ *BLANKS
     C**********           SETON                     736090
     C**********           ENDIF                           RPCAP# = BLANKS
     C**********           MOVE *BLANKS   RPCAP#
     C**N60****  RPKY      READEPEL02                    60
     C*********            ENDDO                           IN60
     C*********            ENDIF                           @@QTE <> Q
     C********             ENDIF                           *IN60, GCCRFL
      ***** END DEL ******* 06/28/02 **********************************
      **** END   ADD **** 10/12/01 ************************************
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDDO
     C*
     C     SCBLOP        IFEQ      '1'
     C                   MOVEL     *BLANK        SCBLOP
     C                   ENDIF                                                  SCBLOP=1
     C***  END   ADD  ***  11/05/97  *********************************
     C*
     C***  BEGIN CHG  ***  11/05/97  *********************************
     C**********           CALL 'W8001R'               99
     C**********           PARM '4'       ##MODE
     C  NKL              CALL      'W8001R'                             99
     C                   PARM      SCBLOP        ##MODE
     C***  END   CHG  ***  11/05/97  *********************************
     C                   PARM      @@QTE         ##QTE
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      SCDBRR        ##RRN
     C*
     C                   ENDSR                                                  OPT30
     C*
     C*===============================================================
     C* OPT31 - SUBROUTINE FOR OPTION 31
     C*===============================================================
     C*
     C* OPTION 31=REVISED BILL REQUEST
     C*
     C     OPT31         BEGSR
     C*
     C*** BEGIN ADD ***  02/04/98  ************************************
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*** END ADD *****  02/04/98  ************************************
     C*
     C* NO BILL IF AUDITED.
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVE      *ON           *IN32
     C                   ENDIF
     C*** BEGIN ADD ***  02/04/98  ************************************
     C*** BEGIN DEL *** 06/26/98 **************************************
     C**********
     C* NO BILL PRIOR TO POLICY START DATE
     C**********
     C********** ELPFDT    IFGT XXDT
     C**********           MOVE *ON       *IN33
     C**********           ENDIF
     C*** END DEL ***** 06/26/98 **************************************
     C*
     C* NO BILL IF CANCELLED.
     C*
     C                   Z-ADD     0             SCCX
     C*
     C**** BEGIN DEL **** 02/12/98 *********************************************
     C******************** DO   5         I1      30
     C********************
     C********** CX,I1     IFNE *ZERO
     C******************** Z-ADDCX,I1     SCCX    70
     C******************** ELSE
     C******************** LEAVE
     C******************** ENDIF                           CX,I1<>*ZERO
     C********************
     C******************** ENDDO                           I1->5
     C**** END   DEL **** 02/12/98 *********************************************
     C**** BEGIN ADD **** 02/12/98 *********************************************
     C                   Z-ADD     1             I1
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
     C                   Z-ADD     CX(I1)        SCCX
     C                   ENDIF                                                  I1 IFNE 0
     C**** END   ADD **** 02/12/98 *********************************************
     C*
     C**** BEGIN CHG ***  11/04/98  ***********************************
     C*****      SCCX      IFLE XXDT
     C*****      SCCX      ANDNE*ZERO
     C     SCCX          IFNE      *ZERO
     C**** END CHG *****  11/04/98  ***********************************
     C*** BEGIN ADD ***  02/28/02  ************************************
     C     SCCX          ANDLE     XXDT
     C*** END ADD *****  02/28/02  ************************************
     C                   MOVE      *ON           *IN34
     C                   ENDIF
     C*** END ADD *****  02/04/98  ************************************
     C*
     C* IF NO ERRORS, RUN THE REVISED BILL OPTION.
     C*
     C     *IN32         IFEQ      *OFF
     C*** BEGIN ADD ***  02/04/98  ************************************
     C     *IN33         ANDEQ     *OFF
     C     *IN34         ANDEQ     *OFF
     C*** END ADD *****  02/04/98  ************************************
     C*
     C                   CALL      'W0090R'      ACDPL5                 99
     C*
     C***** BEGIN ADD ****** 11/30/10 *********************************
     C     ##RTCD        IFEQ      *OFF
     C                   MOVE      @@RRN2        ##RRN3            9
     C                   CALL      'W0105R'
     C                   PARM      'B'           @@MODE            1
     C                   PARM                    ##RRN3
     C                   PARM      'WMEMP'       @@FNM            10
     C                   ENDIF                                                  ##KC IFEQ
     C***** END   ADD ****** 11/30/10 *********************************
     C                   ENDIF                                                  *IN32 IFEQ
     C*
     C                   ENDSR                                                  OPT31
     C*
     C*
     C*
     C*===============================================================
     C* OPT32 - SUBROUTINE FOR OPTION 32
     C*===============================================================
     C*
     C* OPT32 = Print premium notice
     C*
     C     OPT32         BEGSR
     C*
     C******************** CALL 'W0155R'  ACDPL2       99
      ***** BEG ADD ******* 11/18/99 **********************************
      *
     C                   CALL      'W7320R'                             99
     C                   PARM      PRENOT        ##OPNM           40
     C                   PARM      *BLANK        ##CALL            1
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
      ***** END ADD ******* 11/18/99 **********************************
     C*
     C                   ENDSR                                                  OPT32
     C*
     C*
     C*
     C*===============================================================
     C* OPT33 - SUBROUTINE FOR OPTION 33
     C*===============================================================
     C*
     C* OPTION 33=AUDIT REQUEST
     C*
     C     OPT33         BEGSR
     C*
     C                   CALL      'W7230R'      ACDPL1                 99
     C*
     C                   ENDSR                                                  OPT33
     C*
     C*===============================================================
     C* OPT34 - SUBROUTINE FOR OPTION 34
     C*===============================================================
     C*
     C* OPTION 34=AUDIT BILL
     C*
     C     OPT34         BEGSR
     C*
     C***  BEGIN ADD  ***  06/30/00  *********************************
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C                   Z-ADD     0             SCCX
     C*
     C                   Z-ADD     1             I1
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
     C                   Z-ADD     CX(I1)        SCCX
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C     SCCX          IFGT      ELPTDT
     C                   Z-ADD     0             SCCX
     C                   ENDIF
     C*
     C     SCCX          IFNE      *ZERO
     C     XXDT          ANDLT     SCCX
     C     ELPFDT        ANDNE     ELPTDT
     C                   MOVE      *ON           *IN36
     C                   ENDIF
     C*
     C     SCCX          IFEQ      *ZERO
     C     XXDT          ANDLT     ELPTDT
     C     ELPFDT        ANDNE     ELPTDT
     C                   MOVE      *ON           *IN37
     C                   ENDIF
     C*
     C* IF NO ERRORS, RUN THE AUDIT BILL OPTION.
     C*
     C     *IN36         IFEQ      *OFF
     C     *IN37         ANDEQ     *OFF
     C***  END ADD    ***  06/30/00  *********************************
     C***  BEGIN ADD  ***  03/31/98  *********************************
     C                   MOVE      *ON           *IN35
     C                   MOVEL(P)  OPTA30        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDB
     C                   CLEAR                   SCBLOP
     C*
     C     *IN90         DOUEQ     *OFF
     C     *INKL         OREQ      *ON
     C*
     C                   MOVE      *OFF          *IN90
     C*
     C                   EXFMT     BILLWDW
     C*
     C                   MOVE      *OFF          *IN71
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C  N90SCBLOP        IFNE      '1'
     C     SCBLOP        ANDNE     '2'
     C     SCBLOP        ANDNE     '3'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCBLOP<>1/2/3/4
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C                   ENDDO
     C*
     C                   MOVE      *OFF          *IN35
     C                   MOVE      SCBLOP        ##MODE
     C***  END   ADD  ***  03/31/98  *********************************
     C*
     C***  BEGIN CHG  ***  03/31/98  *********************************
     C*****                CALL 'W7710R'  ACDPL1       99
     C  NKL              CALL      'W7710R'      ACDPL1                 99
     C***  END   CHG  ***  03/31/98  *********************************
     C***  BEGIN ADD  ***  06/30/00  *********************************
     C                   ENDIF
     C***  END ADD    ***  06/30/00  *********************************
     C*
     C                   ENDSR                                                  OPT34
     C*
     C*** BEG ADD ***** 01/05/09 *************************************
     C*===============================================================
     C* OPT35 - SUBROUTINE FOR OPTION 35 - 3RD PARTY BILLING
     C*===============================================================
     C     OPT35         BEGSR
     C                   CALL      'W0093C01'    ACDPL9
     C                   ENDSR                                                  OPT35
     C*** END ADD ***** 01/05/09 *************************************
     C*** BEG ADD ***** 12/30/09 *************************************
      *===============================================================
      * OPT38 - Large Deductible Entry
      *===============================================================
     C     OPT38         BEGSR
      *
     C***** BEG CHG ******* 03/14/10 **********************************
     C**N09*****           MOVE 'C'       #1MODE
     C     *IN09         IFEQ      *OFF
     C     LDKY          CHAIN     PLD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      'C'           #1MODE
     C                   ELSE
     C                   MOVE      'A'           #1MODE
     C                   ENDIF
     C                   ENDIF
     C***** END CHG ******* 03/14/10 **********************************
     C   09              MOVE      'D'           #1MODE
     C                   SETOFF                                       40
      *
     C     ELBSCD        IFNE      *ZERO
      *
     C                   MOVEL     ELBSCD        XXBSCD           10
     C*
     C     BSKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        IFEQ      'L'
      *
     C                   CALL      'W0051R38'                           99
     C     *INKC         PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM                    #1MODE            1
     C                   PARM                    #1RRN             9 0
     C                   PARM      SCDBRR        #1RRN1            9 0          WDELP
     C***** BEG ADD ******* 01/28/11 **********************************
     C                   PARM      ' '           #@MODE            1            Call in emp mode.
     C                   PARM      @@QTE         #@QTE             1
     C***** END ADD ******* 01/28/11 **********************************
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  OPT38
      *
     C*** END ADD ***** 12/30/09 *************************************
     C*===============================================================
     C* OPT40 - SUBROUTINE FOR OPTION 40
     C*===============================================================
     C*
     C* OPTION 40=CARRIER INFORMATION
     C*
     C     OPT40         BEGSR
     C*
     C                   CALL      'W8100R1'     ACDPL6                 99
     C*
     C                   ENDSR                                                  OPT40
     C*** BEGIN ADD *** 05/04/98 ***********************************************
     C*===============================================================
     C* OPT41 - SUBROUTINE FOR OPTION 41
     C*===============================================================
     C*
     C* OPTION 41=AUDIT TRACKING
     C*
     C     OPT41         BEGSR
     C*
     C                   CALL      'W4700R'      ADTRPL                 99
     C*
     C                   ENDSR                                                  OPT41
     C*** END   ADD *** 05/04/98 ***********************************************
      ***** BEG ADD ******* 06/22/04 **********************************
      *
      *
      *
      *===============================================================
      * OPT42 - Renewal Tracking
      *===============================================================
     C     OPT42         BEGSR
      *
     C  N09              MOVE      'C'           #1MODE
     C   09              MOVE      'D'           #1MODE
      *
     C                   CALL      'W0051R42'                           99
     C     *INKC         PARM      *BLANK        #1KC              1
     C                   PARM      *BLANK        #1KL              1
     C                   PARM                    #1MODE            1
     C                   PARM                    #1RRN             9 0          WMNBP
     C                   PARM      SCDBRR        #1RRN1            9 0          WMEMP
      *
     C                   ENDSR                                                  OPT42
      *
      ***** END ADD ******* 06/22/04 **********************************
     C*
     C*** BEGIN ADD *** 01/09/04 *************************************
     C*===============================================================
     C* OPT43 - SUBROUTINE FOR OPTION 43
     C*===============================================================
     C*
     C* OPTION 43=VENDOR HISTORY
     C*
     C     OPT43         BEGSR
     C*
     C                   CALL      'W0053R'      ACDPL7                 99
     C*
     C                   ENDSR                                                  OPT43
     C*** END   ADD *** 01/09/04 *************************************
     C*
      ***** BEGIN ADD ***** 06/24/04 **********************************
     C*===============================================================
     C* OPT48 - SUBROUTINE FOR OPTION 48
     C*===============================================================
     C*
     C* OPTION 48=RETRO INFORMATION
     C*
     C     OPT48         BEGSR
     C*
     C***  BEGIN CHG  ***  09/16/10  *********************************
     C******               CALL 'W0051R48'ACDP10       99
     C** @@CALL = '1' DISPLAY ELSE UPDATE MODE
     C                   MOVEL     *BLANKS       ##CALL            1
     C     @@CALL        IFEQ      '1'
     C                   MOVEL     'D'           ##CALL
     C                   ELSE
     C                   MOVEL     'C'           ##CALL
     C                   ENDIF
     C**
     C                   CALL      'W0051R48'                           99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##CALL
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C***  END   CHG  ***  09/16/10  *********************************
     C*
     C                   ENDSR                                                  OPT48
      ***** END ADD ******* 06/24/04 **********************************
     C*
     C*** BEG ADD ***** 11/20/97 ***********************************************
     C***  BEGIN DEL  ***  12/31/97  *********************************
     C*===============================================================
     C**OPT45*-*SUBROUTINE FOR OPTION 45
     C*===============================================================
     C**********
     C**OPTION*45=SUBRO WAIVERS
     C**********
     C********** OPT45     BEGSR
     C**********
     C**********           CALL 'W0810R'  ACDPL8       99
     C**********
     C**********           ENDSR                           OPT45
     C***  END   DEL  ***  12/31/97  *********************************
     C*
      ***** BEG ADD ******* 04/13/07 **********************************
     C*===============================================================
     C* OPT49 - Scheduled Rating Information
     C*===============================================================
     C     OPT49         BEGSR
     C*
     C                   CLEAR                   F2CAR#
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     F2KY          CHAIN     PF201                              60
     C*
     C                   MOVE      F2CAR#        ENCAR
      ***** BEG ADD   ******   04/04/07 *******************************
     C                   MOVE      @@QTE         ##QTE
      ***** END ADD   ******   04/04/07 *******************************
     C*
     C                   SELECT
     C     *IN09         WHENEQ    *ON
     C                   MOVE      'D'           #1MODE
     C     *IN09         WHENEQ    *OFF
     C                   MOVE      'C'           #1MODE
     C                   ENDSL
     C*
     C                   CALL      'W1551R02'
     C                   PARM                    ELCO#
     C                   PARM                    ELFND
     C                   PARM                    ELEMP#
     C                   PARM                    ELDIV
     C                   PARM                    ELFYR
     C                   PARM                    ENCAR             5 0
     C                   PARM                    ELDBCR
     C                   PARM                    ELSDEF
     C                   PARM                    ELSDEX
     C                   PARM                    #1MODE
     C                   PARM                    SCCONM
     C                   PARM                    SCFND
     C                   PARM                    SCNAME
     C** **                PARM           SCEDN
     C                   PARM                    ##EDN            44
     C     *IN90         PARM      '0'           ERROR             1
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
      ***** BEG ADD   ******   04/04/07 *******************************
     C                   PARM                    ##QTE
      ***** END ADD   ******   04/04/07 *******************************
     C*
     C                   ENDSR                                                  OPT49
      ***** END ADD ******* 04/13/07 **********************************
     C*
     C*===============================================================
     C* OPT54 - SUBROUTINE FOR OPTION 54
     C*===============================================================
     C*
     C* OPTION 54=EMPLOYER ENDORSEMENT FORMS
     C*
     C     OPT54         BEGSR
     C*
     C                   CALL      'W1054R'      ACDPL7                 99
     C*
     C                   ENDSR                                                  OPT54
     C*
     C*** END ADD ***** 11/20/97 ***********************************************
     C*** BEGIN ADD *** 05/12/99 **************************************
     C*===============================================================
     C* OPT58 - SUBROUTINE FOR OPTION 58
     C*===============================================================
     C*
     C* OPTION 58=WAIVER INFORMATION
     C*
     C     OPT58         BEGSR
     C*
     C*** BEGIN ADD *** 08/06/99 ***********************************************
     C*
     C* Only call this program if a stat code has been set up for waivers
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RSJRST        XXST
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C     *IN60         IFEQ      *OFF
     C     RRWVAC        ANDNE     *ZEROS
     C*** END   ADD *** 08/06/99 ***********************************************
     C***  BEGIN CHG  ***  09/16/10  *********************************
     C** @@CALL = '1' DISPLAY ELSE UPDATE MODE
     C                   MOVEL     *BLANKS       ##CALL            1
     C     @@CALL        IFEQ      '1'
     C                   MOVEL     'D'           ##CALL
     C                   ELSE
     C                   MOVEL     'C'           ##CALL
     C                   ENDIF
     C**                   CALL 'W0051R58'ACDP10       99
     C                   CALL      'W0051R58'                           99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##CALL
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C***  END   CHG  ***  09/16/10  *********************************
     C*** BEGIN ADD *** 08/06/99 ***********************************************
     C                   ELSE
     C                   SETON                                        90  82
     C                   ENDIF                                                  *IN60 = *OFF
     C*** END   ADD *** 08/06/99 ***********************************************
     C*
     C                   ENDSR                                                  OPT58
     C*** END   ADD *** 05/12/99 **************************************
     C*===============================================================
     C* OPT60 - SUBROUTINE FOR OPTION 60
     C*===============================================================
     C*
     C* OPTION 60=A/R INQUIRY
     C*
     C     OPT60         BEGSR
     C*
     C                   CALL      'W0612R'                             99
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##MODE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM      *ZEROS        QQDATE            8
     C                   PARM      *ZEROS        QQAM              9
     C                   PARM      *ZEROS        QQCHK#            7
     C                   PARM                    QQRTC             1
     C*
     C                   ENDSR                                                  OPT60
     C*
     C*===============================================================
     C* OPT61 - SUBROUTINE FOR OPTION 61
     C*===============================================================
     C*
     C* OPTION 61=A/R DETAIL SUMMARY
     C*
     C     OPT61         BEGSR
     C*
     C                   CALL      'W0610R'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM      '2'           ##MODE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQRTC
     C*
     C                   ENDSR                                                  OPT61
     C*
     C*===============================================================
     C* OPT62 - SUBROUTINE FOR OPTION 62
     C*===============================================================
     C*
     C* OPTION 62=CANCELLATION HISTORY
     C*
     C     OPT62         BEGSR
     C*
     C                   CALL      'W0168R'                             99
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      '1'           ##MODE
     C                   PARM      *ZERO         ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C*
     C                   ENDSR                                                  OPT62
     C*
     C*** BEGIN ADD *** 01/25/10 *************************************
     C*===============================================================
     C* OPT63 - SUBROUTINE FOR OPTION 63
     C*===============================================================
     C*
     C* OPTION 63=Fee information
     C*
     C     OPT63         BEGSR
     C*
     C  N09              MOVE      'C'           #2MODE            1
     C   09              MOVE      'D'           #2MODE
     C*
     C                   CALL      'W0051R63'                           99
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    #2MODE
     C                   PARM      *ZERO         ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C*
     C                   ENDSR                                                  OPT63
     C*** END   ADD *** 01/25/10 *************************************
      *
     C*** BEG   ADD *** 06/16/11 *************************************
     C*===============================================================
     C* OPT64 - SUBROUTINE FOR OPTION 64
     C*===============================================================
     C*
     C* OPTION 64=CHANGE ACTIVITY STATUS
     C*
     C     OPT64         BEGSR
     C*
     C  N09              MOVE      'C'           #2MODE            1
     C   09              MOVE      'D'           #2MODE
     C*
     C                   CALL      'W0051R64'                           99
     C     *INKC         PARM      '0'           ##KC
     C                   PARM                    #2MODE
     C                   PARM      *ZERO         ##RRN
     C                   PARM      SCDBRR        ##RRN2
     C*
     C                   ENDSR                                                  OPT64
     C*** END   ADD *** 06/16/11 *************************************
     C*
     C*===============================================================
     C* OPT80 - SUBROUTINE FOR OPTION 80
     C*===============================================================
     C*
     C* OPTION 80=CLAIMS INQUIRY
     C*
     C     OPT80         BEGSR
     C*
     C                   CALL      'W0070R'                             99
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      'Z'           ##MODE            1
     C                   PARM      @@RRN2        ##RRN             9 0
     C                   PARM      '  '          ##SORT            2
     C                   PARM                    ##RRN2            9 0
     C                   PARM      SCFYR         ##FYR             3 0
     C*
     C                   ENDSR                                                  OPT80
     C*
     C*
      ***** BEG ADD ******* 04/30/01 **********************************
      *
      *
      *
      *===============================================================
      * OPT90 - Excluded states
      *===============================================================
      *
     C     OPT90         BEGSR
      *
     C                   CALL      'W7419R'                             99
     C                   PARM      *BLANK        ##CALL            1
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      *ZERO         ##RRN             9 0          WDXSP (Return)
     C                   PARM      SCDBRR        ##RRN1            9 0          WDELP
      *
     C                   ENDSR                                                  OPT90
      *
      ***** END ADD ******* 04/30/01 **********************************
     C***  Begin Add  ***  11/10/10  **************************************
     C*===============================================================
     C* OPT91 - Subroutine for option 91=HCN Program
     C*===============================================================
     C     OPT91         BEGSR
     C*
     C                   CALL      'W1191R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C                   ENDSR                                                  OPT91
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C***  Begin Add  ***  12/17/10  **************************************
     C*===============================================================
     C* OPT96 - Subroutine for option 96=Premium information entry.
     C*===============================================================
     C     OPT96         BEGSR
     C*
     C                   CALL      'W0096R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
      *
     C                   ENDSR                                                  OPT96
     C***  End   Add  ***  12/17/10  **************************************
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
      *** BEG ADD **** 04/24/02 **********************************************
     C     *INKN         CASEQ     *ON           OPT14
      *** END ADD **** 04/24/02 **********************************************
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C                   Z-ADD     EMCO#         ##CO#
     C                   MOVE      EMCO#         QQCO#             3
     C                   MOVE      EMFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      EMEMP#        QQEMP#            9
     C                   MOVE      EMDIV         QQDIV             5
     C*
     C  N09              MOVE      'C'           ##MODE
     C   09              MOVE      'D'           ##MODE
     C  N09              MOVE      ' '           ##CALL
     C   09              MOVE      '1'           ##CALL
     C*
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '9'           OPT9
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '12'          OPT12
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '17'          OPT17
     C***  BEGIN ADD  ***  11/07/97  *********************************
     C     SCSEL         CASEQ     '18'          OPT18
     C***  END   ADD  ***  11/07/97  *********************************
     C     SCSEL         CASEQ     '19'          OPT19
     C     SCSEL         CASEQ     '20'          OPT20
     C     SCSEL         CASEQ     '23'          OPT23
     C     SCSEL         CASEQ     '25'          OPT25
     C     SCSEL         CASEQ     '26'          OPT26
     C     SCSEL         CASEQ     '30'          OPT30
     C     SCSEL         CASEQ     '31'          OPT31
     C     SCSEL         CASEQ     '32'          OPT32
     C     SCSEL         CASEQ     '33'          OPT33
     C     SCSEL         CASEQ     '34'          OPT34
     C*** BEG ADD ***** 01/05/09 *************************************
     C     SCSEL         CASEQ     '35'          OPT35
     C*** END ADD ***** 01/05/09 *************************************
     C*** BEG ADD ***** 12/30/09 *************************************
     C     SCSEL         CASEQ     '38'          OPT38
     C*** END ADD ***** 12/30/09 *************************************
     C     SCSEL         CASEQ     '40'          OPT40
     C*** BEGIN ADD *** 05/04/98 *************************************
     C     SCSEL         CASEQ     '41'          OPT41
     C*** END   ADD *** 05/04/98 *************************************
      ***** BEG ADD ******* 06/22/04 **********************************
     C     SCSEL         CASEQ     '42'          OPT42
      ***** END ADD ******* 06/22/04 **********************************
     C*** BEGIN ADD *** 01/09/04 *************************************
     C     SCSEL         CASEQ     '43'          OPT43
     C*** END   ADD *** 01/09/04 *************************************
     C*** BEG ADD ***** 11/20/97 ***********************************************
     C***  BEGIN DEL  ***  12/31/97  *********************************
     C********** SCSEL     CASEQ'45'      OPT45
     C***  END   DEL  ***  12/31/97  *********************************
      ***** BEGIN ADD ***** 06/24/04 **********************************
     C     SCSEL         CASEQ     '48'          OPT48
      ***** END ADD ******* 06/24/04 **********************************
      ***** BEG ADD ******* 04/13/07 **********************************
     C     SCSEL         CASEQ     '49'          OPT49
      ***** END ADD ******* 04/13/07 **********************************
     C     SCSEL         CASEQ     '54'          OPT54
     C*** END ADD ***** 11/20/97 ***********************************************
     C*** BEGIN ADD *** 05/12/99 **************************************
     C     SCSEL         CASEQ     '58'          OPT58
     C*** END   ADD *** 05/12/99 **************************************
     C     SCSEL         CASEQ     '60'          OPT60
     C     SCSEL         CASEQ     '61'          OPT61
     C     SCSEL         CASEQ     '62'          OPT62
     C*** BEG ADD ***** 01/25/10 *************************************
     C     SCSEL         CASEQ     '63'          OPT63
     C*** END ADD ***** 01/25/10 *************************************
     C*** BEG ADD ***** 06/16/11 *************************************
     C     SCSEL         CASEQ     '64'          OPT64
     C*** END ADD ***** 06/16/11 *************************************
     C     SCSEL         CASEQ     '80'          OPT80
      ***** BEG ADD ******* 04/30/01 **********************************
     C     SCSEL         CASEQ     '90'          OPT90
      ***** END ADD ******* 04/30/01 **********************************
      ***** BEG DEL ******* 04/13/07 **********************************
     C***  Begin Add  ***  12/01/06  **************************************
     C*********  SCSEL     CASEQ'91'      OPT91
     C***  End   Add  ***  12/01/06  **************************************
      ***** END DEL ******* 04/13/07 **********************************
     C***  Begin Add  ***  11/10/10  **************************************
     C     SCSEL         CASEQ     '91'          OPT91
     C***  End   Add  ***  11/10/10  **************************************
     C***  Begin Add  ***  12/17/10  **************************************
     C     SCSEL         CASEQ     '96'          OPT96
     C***  End   Add  ***  12/17/10  **************************************
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C***  BEGIN ADD  ***  08/13/99  **********************************
     C                   MOVE      *ON           *INLR
     C***  END   ADD  ***  08/13/99  **********************************
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     9             SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C     @@QTE         COMP      'Q'                                    31    QUOTE
     C*
     C*** BEGIN ADD ***  02/08/98  ************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** END ADD *****  02/08/98  ************************************
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*** END   ADD *** 03/12/10 **************************************
     C*
     C*** BEGIN ADD *** 03/23/98 **************************************
     C*
     C*  DETERMINE IF CLIENT IS ADMIN-ONLY OR CLAIMS-ONLY
     C*
     C*  Access System Control file 'WC' record
     C*
     C                   MOVEL     'WC'          XSKEY             2
     C*
     C     XSKEY         CHAIN     SWC01                              60
     C*
     C     *IN60         IFEQ      *OFF
      ***** BEG ADD ******* 06/22/04 **********************************
      *
      * If Policy Tracking module installed
      *
     C     WCFL26        IFEQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '42'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
      ***** END ADD ******* 06/22/04 **********************************
      ***** BEG ADD ******* 12/30/09 **********************************
      *
      * IF LARGE DEDUCTIBLE MODULE IS INSTALLED
      *
     C     WCFL43        IFEQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '38'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
      ***** END ADD ******* 12/30/09 **********************************
      ***** BEG ADD ******* 01/25/10 **********************************
      *
      * If Administrative Fees for Collateral and Claims Installed.
      *
     C     WCFL44        IFEQ      'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '63'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
      ***** END ADD ******* 01/25/10 **********************************
      ***** BEGIN ADD ***** 06/24/04 **********************************
     C     WCFL11        IFNE      'Y'
     C*   Disable option 48
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '48'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  WCFL11 IFNE
      ***** END ADD ******* 06/24/04 **********************************
     C*** BEGIN ADD *****  04/30/01  **********************************
     C     WCFL18        IFNE      'Y'
     C*   Disable option 90
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '90'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  WCFL18 IFNE
     C*** END ADD *******  04/30/01  **********************************
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C     CLTID         IFEQ      '139'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '91'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C***  Begin Add  ***  12/17/10  **************************************
     C     CLTID         IFEQ      '071'
     C     FN3PPE        ANDEQ     'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '96'          XXNMBR
     C                   MOVE      'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C***  End   Add  ***  12/17/10  **************************************
     C***  BEG   ADD  ***  02/27/09  **********************************
     C*   Disable option 80 when starting point is W/w claims.
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0070R'      ##PGM            10
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '80'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C***  END   ADD  ***  02/27/09  **********************************
     C*
     C     WCFL08        IFNE      *BLANK
     C*
     C     WCFL08        IFEQ      'A'
     C***  BEGIN CHG  ***  05/23/99  **********************************
     C*******************  MOVELCLRFKY    OFD,28           OPT 80
     C*   Disable option 80
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '80'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  05/23/99  **********************************
     C                   ENDIF                                                  WCFL08=A
     C*
     C     WCFL08        IFEQ      'C'
     C***  BEGIN CHG  ***  05/23/99  **********************************
     C*******************  MOVELCLRFKY    OFD,9            OPT 12
     C*******************  MOVELCLRFKY    OFD,11           OPT 17
     C*******************  MOVELCLRFKY    OFD,14           OPT 23
     C*******************  MOVELCLRFKY    OFD,15           OPT 25
     C*******************  MOVELCLRFKY    OFD,16           OPT 26
     C*******************  MOVELCLRFKY    OFD,17           OPT 30
     C*******************  MOVELCLRFKY    OFD,18           OPT 31
     C*******************  MOVELCLRFKY    OFD,20           OPT 33
     C*******************  MOVELCLRFKY    OFD,21           OPT 34
     C*******************  MOVELCLRFKY    OFD,24           OPT 54
     C*******************  MOVELCLRFKY    OFD,27           OPT 62
     C*
     C*   Disable the following options
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C*
     C***  Begin Add  ***  03/13/07  **************************************
     C                   MOVE      '11'          XXNMBR                         OPT 11
     C                   EXSR      UPDOFA
     C***  End   Add  ***  03/13/07  **************************************
     C*
     C                   MOVE      '12'          XXNMBR                         OPT 12
     C                   EXSR      UPDOFA
     C*
     C*** BEGIN ADD *** 03/12/10 **************************************
     C     XXCLT#        IFNE      '025'
     C*** END   ADD *** 03/12/10 **************************************
     C                   MOVE      '17'          XXNMBR                         OPT 17
     C                   EXSR      UPDOFA
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
     C*
     C                   MOVE      '23'          XXNMBR                         OPT 23
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '25'          XXNMBR                         OPT 25
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '26'          XXNMBR                         OPT 26
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '30'          XXNMBR                         OPT 30
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '31'          XXNMBR                         OPT 31
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '33'          XXNMBR                         OPT 33
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '34'          XXNMBR                         OPT 34
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '54'          XXNMBR                         OPT 54
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '62'          XXNMBR                         OPT 62
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  05/23/99  **********************************
     C                   ENDIF                                                  WCFL08=C
     C*
     C                   ENDIF                                                  WCFL08<>*BLANK
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C*** END   ADD *** 03/23/98 **************************************
     C*
     C***  BEGIN CHG  ***  05/23/99  **********************************
     C***************  **  MOVEAOFD,13    OF1
     C*** BEG CHG ***** 11/20/97 ***********************************************
     C***                  MOVEA'YYY'     OF1,28
     C***  BEGIN CHG  ***  01/15/98  *********************************
     C**********           MOVEA'YYY'     OF1,30
     C***************  **  MOVEA'YYY'     OF1,28
     C***  END   CHG  ***  01/15/98  *********************************
     C*** END CHG ***** 11/20/97 ***********************************************
     C***************  **  MOVEAOF1       OFD,13
     C***************  **
     C***************  **  MOVEAOFD,14    OF1
     C*** BEG CHG ***** 11/20/97 ***********************************************
     C***                  MOVEA'YYY'     OF1,28
     C***  BEGIN CHG  ***  01/15/98  *********************************
     C**********           MOVEA'YYY'     OF1,30
     C***************  **  MOVEA'YYY'     OF1,28
     C***  END   CHG  ***  01/15/98  *********************************
     C*** END CHG ***** 11/20/97 ***********************************************
     C***************  **  MOVEAOF1       OFD,14
     C*
     C*   Enable the following options
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'YYY'         XXMIS
     C*
     C                   MOVE      '20'          XXNMBR                         OPT 20
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '23'          XXNMBR                         OPT 23
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  05/23/99  **********************************
     C*
     C***  BEGIN ADD  ***  10/30/97  *********************************
B01  C     @@QTE         IFEQ      'Q'
     C***  BEGIN CHG  ***  05/23/99  **********************************
     C***************  **  MOVELCLRFKY    OFD,9            OPT 12
     C***************  **  MOVELCLRFKY    OFD,11           OPT 17
     C*
     C*   Disable the following options
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C*
     C***  Begin Add  ***  03/13/07  **************************************
     C                   MOVE      '11'          XXNMBR                         OPT 11
     C                   EXSR      UPDOFA
     C***  End   Add  ***  03/13/07  **************************************
     C*
     C                   MOVE      '12'          XXNMBR                         OPT 12
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '17'          XXNMBR                         OPT 17
     C                   EXSR      UPDOFA
     C*
     C***  BEGIN CHG  ***  11/07/97  *********************************
     C**********           MOVELCLRFKY    OFD,12           OPT 20
     C**********           MOVELCLRFKY    OFD,13           OPT 23
     C**********           MOVELCLRFKY    OFD,14           OPT 25
     C**********           MOVELCLRFKY    OFD,15           OPT 26
     C**********           MOVELCLRFKY    OFD,17           OPT 31
     C**********           MOVELCLRFKY    OFD,18           OPT 32
     C**********           MOVELCLRFKY    OFD,19           OPT 33
     C**********           MOVELCLRFKY    OFD,20           OPT 34
     C**********           MOVELCLRFKY    OFD,21           OPT 40
     C**********           MOVELCLRFKY    OFD,22           OPT 60
     C**********           MOVELCLRFKY    OFD,23           OPT 61
     C**********           MOVELCLRFKY    OFD,24           OPT 62
     C**********           MOVELCLRFKY    OFD,25           OPT 80
     C***************  **  MOVELCLRFKY    OFD,14           OPT 23
     C***************  **  MOVELCLRFKY    OFD,15           OPT 25
     C***************  **  MOVELCLRFKY    OFD,16           OPT 26
     C***************  **  MOVELCLRFKY
     C***************  **  MOVELCLRFKY    OFD,19           OPT 32
     C***************  **  MOVELCLRFKY    OFD,20           OPT 33
     C***************  **  MOVELCLRFKY    OFD,21           OPT 34
     C***************  **  MOVELCLRFKY    OFD,22           OPT 40
     C***************  **  MOVELCLRFKY    OFD,23           OPT 45
     C***************  **  MOVELCLRFKY    OFD,24           OPT 54
     C***************  **  MOVELCLRFKY    OFD,25           OPT 60
     C***************  **  MOVELCLRFKY    OFD,26           OPT 61
     C*** BEG ADD ***** 11/20/97 ***********************************************
     C***************  **  MOVELCLRFKY    OFD,27           OPT 62
     C***************  **  MOVELCLRFKY    OFD,28           OPT 80
     C*** END ADD ***** 11/20/97 ***********************************************
     C*
     C*   Disable the following options
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C*
     C                   MOVE      '23'          XXNMBR                         OPT 23
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '25'          XXNMBR                         OPT 25
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '26'          XXNMBR                         OPT 26
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '31'          XXNMBR                         OPT 31
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '32'          XXNMBR                         OPT 32
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '33'          XXNMBR                         OPT 33
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '34'          XXNMBR                         OPT 34
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '40'          XXNMBR                         OPT 40
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '45'          XXNMBR                         OPT 45
     C                   EXSR      UPDOFA
     C*
     C**** BEGIN DEL **** 05/11/01 ************************************
     C******************** MOVE '54'      XXNMBR           OPT 54
     C******************** EXSR UPDOFA
     C**** END   DEL **** 05/11/01 ************************************
     C*
     C                   MOVEL     '60'          XXNMBR
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '61'          XXNMBR                         OPT 61
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '62'          XXNMBR                         OPT 62
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  05/23/99  **********************************
     C                   ELSE
     C***  BEGIN CHG  ***  05/23/99  **********************************
     C*******************  MOVELCLRFKY    OFD,12           OPT 18
     C*
     C*  Disable the following options
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C*
     C                   MOVE      '18'          XXNMBR                         OPT 18
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  05/23/99  **********************************
     C***  END   CHG  ***  11/07/97  *********************************
E01  C                   ENDIF                                                  @@QTE=Q
     C***  END   ADD  ***  10/30/97  *********************************
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
      *****  BEG ADD ******  06/25/03  **********************************
      * If Group allows Payroll Vendor or ACH, set flags on.
     C                   MOVE      *OFF          VNDRFL            1
     C                   MOVE      *OFF          ACHFLG            1
     C     WCFL23        IFEQ      'Y'
     C                   MOVE      *ON           VNDRFL
     C                   ENDIF
     C     WCFL24        IFEQ      'Y'
     C                   MOVE      *ON           ACHFLG
     C                   ENDIF
     C*****  END ADD ******  06/25/03  **********************************
     C     WCFL03        IFNE      'Y'                                          NOT ENROLLED,
     C*
     C***  BEGIN CHG  ***  05/23/99  **********************************
     C***************  **  MOVEAOFD,13    OF1
     C*** BEG CHG ***** 11/20/97 ***********************************************
     C***                  MOVEA'NNN'     OF1,28
     C***  BEGIN CHG  ***  01/15/98  *********************************
     C**********           MOVEA'NNN'     OF1,30
     C***************  **  MOVEA'NNN'     OF1,28
     C***  END   CHG  ***  01/15/98  *********************************
     C*** END CHG ***** 11/20/97 ***********************************************
     C***************  **  MOVEAOF1       OFD,13
     C***************  **
     C***************  **  MOVEAOFD,14    OF1
     C*** BEG CHG ***** 11/20/97 ***********************************************
     C***                  MOVEA'NNN'     OF1,28
     C***  BEGIN CHG  ***  01/15/98  *********************************
     C**********           MOVEA'NNN'     OF1,30
     C***************  **  MOVEA'NNN'     OF1,28
     C***  END   CHG  ***  01/15/98  *********************************
     C*** END CHG ***** 11/20/97 ***********************************************
     C***************  **  MOVEAOF1       OFD,14
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C*
     C                   MOVE      '20'          XXNMBR                         OPT 20
     C                   EXSR      UPDOFA
     C*
     C                   MOVE      '23'          XXNMBR                         OPT 23
     C                   EXSR      UPDOFA
     C***  END   CHG  ***  05/23/99  **********************************
     C*
     C                   ENDIF                                                  WCFL03<>Y
     C*
     C*** BEGIN DEL *** 06/25/98 *************************************
     C*** BEGIN ADD *** 05/04/98 *************************************
     C***CANNOT*EXECUTE OPT41 IF CLIENT NOT ENROLLED IN "AUDIT TRACKING"
     C**********
     C********** *IN60     IFEQ *ON
     C********** *IN60     OREQ *OFF
     C********** WCFL12    ANDNE'Y'                        NOT ENROLLED,
     C**********
     C***REMOVE*OPTION 41=AUDIT TRACKING
     C**********
     C**********           MOVEACLRFKY    OFD,23
     C**********
     C**********           ENDIF                           WCFL03<>Y
     C****END***ADD *** 05/04/98 *************************************
     C*** END   ADD *** 06/25/98 *************************************
     C*
     C                   Z-ADD     *ZEROS        SVLVL
     C*
     C***  BEGIN CHG  ***  02/10/98  ********************************
     C********** Q1USER    CHAINPUP01                60
     C**********
     C**N60*****           Z-ADDUPLVL     SVLVL
     C**********
     C**N60***** UPFYR     IFNE *ZERO
     C**********           MOVE UPFYR     KKFYR
     C**********           ENDIF                           UPFYR<>*ZERO
     C                   Z-ADD     ##LVL         SVLVL             3 0
     C***  END   CHG  ***  02/10/98  ********************************
     C*
     C*  LOAD SCREEN HEADINGS
     C*
     C***** BEG ADD ******* 01/12/99 **********************************
     C*
     C*   Get Number of Employer Divisions to display in Screen Header
     C*
     C     @@RRN2        CHAIN     PEM01                              60
     C                   Z-ADD     EMCO#         KKCO#
     C                   Z-ADD     EMFND         KKFND
     C                   Z-ADD     EMEMP#        KKEMP#
     C**** BEGIN ADD ****** 12/01/04 **********************************
     C                   Z-ADD     0             XXSDIV            5 0
     C**** END ADD ******** 12/01/04 **********************************
     C*
     C     EMKY2         CHAIN     PEM02                              60
     C                   Z-ADD     0             SC#DIV
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             SC#DIV
     C**** BEGIN ADD ****** 12/01/04 **********************************
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XXSDIV
     C                   ENDIF                                                  EMSTRP IFEQ
     C**** END ADD ******** 12/01/04 **********************************
     C     EMKY2         READE     PEM02                                  60
     C                   ENDDO
     C*
     C***** END ADD ******* 01/12/99 **********************************
     C*
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     @@RRN2        CHAIN     PEM01                              60
     C*
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
      ***** BEG ADD ******* 10/13/03 **********************************
      *
      *  Do not allow opt "18=Modify effective dates" if called from
      *  renewal quote employer.
      *
     C     @@QTE         IFEQ      'Q'
     C     EMRQFL        ANDEQ     'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '18'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
      ***** END ADD ******* 10/13/03 **********************************
     C***  BEGIN ADD  ***  05/23/99  **********************************
     C*  If not state rptg div then suppress opt 17=Policy Page
     C     EMSTRP        IFNE      'Y'
     C*
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '17'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C*
     C**** BEGIN ADD **** 03/08/01 ************************************
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '54'          XXNMBR
     C                   MOVE      'NNN'         XXMIS
     C                   EXSR      UPDOFA
     C**** END   ADD **** 03/08/01 ************************************
     C*
     C                   ENDIF                                                  EMSTRP<>'Y'
     C***  END   ADD  ***  05/23/99  **********************************
      *
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
     C***  Begin Add  ***  12/01/06  **************************************
     C                   MOVEL     XXEDN         ##EDN
     C***  End   Add  ***  12/01/06  **************************************
     C*
     C**** BEGIN DEL **** 02/12/98 *********************************************
     C******************** DO   5         I1      30
     C********************
     C********** CX,I1     IFNE *ZERO
     C******************** Z-ADDCX,I1     SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     SCCXDT
     C******************** ELSE
     C******************** LEAVE
     C******************** ENDIF                           CX,I1<>*ZERO
     C********************
     C******************** ENDDO                           I1->5
     C**** END   DEL **** 02/12/98 *********************************************
     C**** BEGIN ADD **** 02/12/98 *********************************************
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C*** BEGIN ADD ***  04/04/08  ************************************
     C                   Z-ADD     0             XXCXDT            7 0
     C*** END ADD *****  04/04/08  ************************************
     C                   Z-ADD     1             I1
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
     C                   MOVE      *ON           *IN41
     C*** BEGIN ADD ***  04/04/08  ************************************
     C                   Z-ADD     CX(I1)        XXCXDT
     C*** END ADD *****  04/04/08  ************************************
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C**** END   ADD **** 02/12/98 *********************************************
      ***** BEG ADD ******* 12/05/00 **********************************
      *
     C     *IN41         IFEQ      *ON
      *
      * Disallow F6 if emp cancelled
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '06'          XXNMBR                         F6
     C                   EXSR      UPDOFA
      *
     C                   ELSE
      *
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'YNN'         XXMIS
     C                   MOVE      '06'          XXNMBR                         F6
     C                   EXSR      UPDOFA
     C                   ENDIF
      *
      ***** END ADD ******* 12/05/00 **********************************
     C***** BEG ADD ******* 05/11/11 **********************************
      *
      * Disallow F6 if Quote Environment and Renewal Quote Flag is 'Y'
      *
     C     @@QTE         IFEQ      'Q'
     C     EMRQFL        ANDEQ     'Y'
     C     EMRDFL        ANDEQ     ' '
     C                   MOVE      '2'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '06'          XXNMBR                         F6
     C                   EXSR      UPDOFA
     C                   ENDIF
     C***** END ADD ******* 05/11/11 **********************************
     C*
     C*  GET FUND MASTER INFORMATION
     C*
     C                   MOVEL     *BLANKS       SCNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C*
     C  N09FNEXDT        IFNE      *ZEROS
     C     SYSDT         COMP      FNEXDT                             09  09
     C                   ENDIF                                                  FNEXDT<>*ZEROS
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   Z-ADD     *ZEROS        ##FYR
     C                   CALL      AGTAPI        AGTPL
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
      ***** BEG ADD ******* 05/10/02 **********************************
      *
      *  Disable opt "32=Premium Notice" if employer is Account Current.
      *
     C     ##PLAN        IFEQ      'A'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      'NNN'         XXMIS
     C                   MOVE      '32'          XXNMBR                         F6
     C                   EXSR      UPDOFA
     C                   ENDIF                                                  ##PLAN = 'A'
      ***** END ADD ******* 05/10/02 **********************************
     C*
     C*  GET ADMIN/DARTS INFO
     C*
     C                   CLEAR                   PEX01
     C     EXKY          CHAIN     PEX01                              60
     C*
     C***** BEG ADD ******* 08/10/98 **********************************
     C*
     C*  Get Employer Cross Reference info for header
     C*
     C     ZEXKY         CHAIN     PEX02                              63
      ***** BEG ADD ******* 06/24/99 **********************************
     C  N63              Z-ADD     EXOFND        SCOFND
      ***** END ADD ******* 06/24/99 **********************************
     C  N63              Z-ADD     EXOEMP        SCOEMP
     C  N63              Z-ADD     EXODIV        SCODIV
     C*
     C***** END ADD ******* 08/10/98 **********************************
     C***  BEGIN ADD  ***  06/22/99  *********************************
     C*  ONCE PROJECT CM045 IS INSTALLED, OPT 58 WILL BE ACTIVATED.
     C*
     C                   CALL      'SRCHKOBJ'                           99
     C                   PARM      'W0051R58'    XXPGM
     C                   PARM      '*PGM'        XXOBJ             7
     C                   PARM      *BLANKS       SRERR
     C*
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '58'          XXNMBR
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVEL     'NNN'         XXMIS
     C                   ELSE
     C                   MOVEL     'YYN'         XXMIS
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   EXSR      UPDOFA
     C*
     C*  IF W0612R IS IN THE CALL STACK, OPT 60 WILL BE INACTIVATED.
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W0612R'      XXPGM
     C                   PARM      *BLANKS       SRERR
     C*
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '60'          XXNMBR
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVEL     'NNN'         XXMIS
     C                   ELSE
     C                   MOVEL     'YYY'         XXMIS
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   EXSR      UPDOFA
     C***  END   ADD  ***  06/22/99  *********************************
      ***** BEG ADD ******* 11/17/99 **********************************
      *
      * Disable options 41, 60, 80 in quote environment.
      *
     C     @@QTE         IFEQ      'Q'
      *
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     'NNN'         XXMIS
      *
     C                   MOVEL     '41'          XXNMBR
     C                   EXSR      UPDOFA
      *
     C                   MOVEL     '60'          XXNMBR
     C                   EXSR      UPDOFA
      *
     C                   MOVEL     '80'          XXNMBR
     C                   EXSR      UPDOFA
      ***** BEG ADD ******* 07/05/07 **********************************
      *
     C                   MOVEL     '54'          XXNMBR
     C                   EXSR      UPDOFA
      ***** BEG ADD ******* 07/05/07 **********************************
      *
     C                   ENDIF                                                  @@QTE = 'Q'
      *
      ***** END ADD ******* 11/17/99 **********************************
     C*
      *+*+  BEG ADD ******* 01/05/09 **********************************
     C     EX3RDB        IFEQ      'Y'
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     'YNN'         XXMIS
     C                   MOVEL     '35'          XXNMBR
     C                   EXSR      UPDOFA
     C     @@QTE         IFEQ      'Q'
     C                   MOVEL     '1'           XXOPFN
     C                   MOVEL     '60'          XXNMBR
     C                   MOVEL     'YYY'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C                   ENDIF
      *+*+  END ADD ******* 01/05/09 **********************************
     C*
      *+*+  BEG ADD ******* 08/18/06 **********************************
     C*** BEGIN DEL *** 03/12/10 **************************************
     C**********           CALL 'RTVCLTID'
     C**********           PARM           XXCLT#  3
     C*** END   DEL *** 03/12/10 **************************************
      *
     C*** BEGIN ADD *** 03/12/10 **************************************
     C                   SETOFF                                       16
     C     XXCLT#        IFEQ      '025'
     C                   MOVEL     OFD17         OFD(11)
     C                   SETON                                        16
     C                   ENDIF
     C*** END   ADD *** 03/12/10 **************************************
     C***  Begin Add  ***  12/17/10  **************************************
     C     XXCLT#        IFEQ      '071'
     C     FN3PPE        ANDEQ     'Y'
     C                   MOVEL     OFD96         OFD(40)
     C                   ENDIF
     C***  End   Add  ***  12/17/10  **************************************
     C*** BEGIN ADD *** 11/23/10 **************************************
     C     XXCLT#        IFEQ      '114'
     C                   MOVEL     OFD48         OFD(29)
     C                   ENDIF
     C*** END   ADD *** 11/23/10 **************************************
     C     XXCLT#        IFEQ      '114'
      ****  BEG ADD ******* 12/15/08 **********************************
     C     XXCLT#        OREQ      '146'
      ***** END ADD ******* 12/15/08 **********************************
     C                   MOVE      '1'           XXOPFN
      ***** BEG CHG ***** 07/13/07 ************************************
     C***************      MOVE '91'      XXNMBR
     C                   MOVE      '49'          XXNMBR
      ***** END CHG ***** 07/13/07 ************************************
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
      *+*+  END ADD ******* 08/18/06 **********************************
      *
     C*** BEG ADD ***** 07/27/10 ************************************
     C                   MOVEL(P)  'WA073R'      XXPGM
     C*** BEG CHG ***** 10/22/10 ************************************
     C*******              CALL 'SRCHKPGM'             99
     C*******              PARM           XXPGM
     C*******              PARM           XXNACH  1
    C*
     C                   MOVEL     '*PGM'        XXTYP             7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM
     C                   PARM                    XXTYP
     C                   PARM                    XXNACH            1
     C*** END CHG ***** 10/22/10 ************************************
     C*** END ADD ***** 07/27/10 ************************************
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1C         READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1C         READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCBSDS
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF YEARS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE     FNNAME        U1PDSC
     C     LO:UP         XLATE     SSPDSC        U2PDSC
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF                                                  SCNRES=0
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Headers - HD
Opt   Yr  Start Date   End Date   Billing Type              Status
Opt   Yr  Start Date   End Date   Billing Type              Status
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
1088=Payroll Info           YYY
1099=Agent History          YYY
11010=Premium Info          YYY
11111=Premium History       YYY
11212=Cert Holders          YYN
11414=DARTS                 YYN
11717=Policy Page           YNN
11818=Modify Eff Dates      YNN
12020=Department Letters    YNN
12323=Contractor Class      YYN
12525=CX Maint              YYY
12626=Rein Maint            YYY
13030=Est Bill Request      YNN
13131=Revised Bill          YNN
13232=Premium notice        YNN                            09/23/10
13333=Audit Request         YNN
13434=Audit Bill            YNN
13535=Billing Update        NNN
13838=Large Deductible EntryNNN
14040=Carrier Info          YYN
14141=Audit Tracking        YYN
14242=Renewal Tracking      NNN
14343=Vendor History        YYN
14545                       NNN
14848=Retro Rating Info     YYN
14949=Scheduled Rating Info NNN                            02/26/08
15454=Endorsement Forms     YYN
15858=Waiver Information    YYN
16060=A/R Inquiry           YYY
16161=A/R Detail Summ       YYN
16262=CX History            YYN
16363=Fee information       NNN                            01/25/10
16464=Change Activity StatusYYN                            06/16/11
18080=Claims Inquiry        YYN
19090=Excluded states       YYN
19191=HCN Program           NNN                            11/10/10
1                           NNN
1                           NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          NNN
212F12=Cancel               YYY
214F14=DARTS                YYN
3                           NNN
** Billing Status - BL
Estimated
Revised
Audited
Not Billed
