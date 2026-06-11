     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8011R - GENERATE BILLING CHANGES
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/29/25 C:65  Project: 146WKQ (Phw#118404)                   *
     H*                Remove erroneous chain to WDF1L - file is      *
     H*                already accessed earlier in program and the    *
     H*                chain statement is missing policy year key.    *
     H*                                                               *
     H* 10/18/24 C:65  Support: 117783 (Vital)                        *
     H*                Always delete A/R based on current system      *
     H*                date and forward as the front end reconciles   *
     H*                A/R based on this A/R being removed.  In       *
     H*                SRFUTI check current flag value of ELBLFL.     *
     H*                Delete future A/R if bill on eff day flag      *
     H*                changes like it does on bill plan change.      *
     H*                                                               *
     H* 08/14/24 C:291 Project: WC2974                                *
     H*                Populate the Written Actual Premium fields for *
     H*                Increased Limits.                              *
     H*                                                               *
     H* 06/24/24 C:333 Project: 146WKR                                *
     H*                Recompile                                      *
     H*                                                               *
     H* 06/12/24 C:062 Project: 146WKQ                                *
     H*                Don't create assessments billing if not S/R or *
     H*                marked as SR assessments.                      *
     H*                                                               *
     H* 03/13/24 C:062 Project: 146WKQ                                *
     H*                Recompile                                      *
     H*                                                               *
     H* 03/04/24 C:079 Project: WC2951                                *
     H*                Recompile over WAELP File changes              *
     H*                                                               *
     H* 02/27/24 C:333 PROJECT: WC2953                                *
     H*                Premium History Balance to Minimum             *
     H*                                                               *
     H* 02/13/24 C:300 Support: 115024 (LUBA)                         *
     H*                IVC: Negative Transactions                     *
     H*                1) Restore DT values into DT array after the   *
     H*                PEA01 loop is done                             *
     H*                2) In each subsequent adjustments section, set *
     H*                EAPODT to Current System Date If EAPODT = 0 and*
     H*                amount being posted is a credit                *
     H*                                                               *
     H* 02/01/24 C:62  Ticket: 114824(129)                            *
     H*                Remove orphan call to GETINV to prevent        *
     H*                erroneous information from calling into IVC.   *
     H*                                                               *
     H* 11/09/23 C:65  PROJECT: WC2810                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDMPP.  Also populate new pre-minimum *
     H*                figures in WDEHP.                              *
     H*                                                               *
     H* 09/12/23 C:62  Ticket: 113870(140)                            *
     H*                -Expand division to 5.0                        *
     H*                -Remove invoice from array if inv# found in AR *
     H*                                                               *
     H* 07/30/23 C:62  Project: WC2918                                *
     H*                Populate EHnoexm2 and EHnoexm3.                *
     H*                                                               *
     H* 09/26/22 C:65  Project: Phw#103757 (Amwins)                   *
     H*                Change div# in invoice cloud array processing  *
     H*                to be 5 digits instead of 3 digits.            *
     H*                                                               *
     H* 01/05/22 C:333 Support: 97658 (PNIC-SCS)                      *
     H*                WMLDP-Large Detuctible table issue             *
     H*                When a policy/quote is set up with a large     *
     H*                deductible billing plan and it is then switched*
     H*                to a non-deductible plan, the large deductible *
     H*                record should be deleted.                      *
      *                                                               *
      * 08/29/21 C:65  Project: WC2663                                *
      *                Get rid of HOLDAR calls when we are removing   *
      *                the temporary A/R - those do not need to be    *
      *                held off in backup file.                       *
     H*                                                               *
     H* 05/16/21 C:062 Project WC2663                                 *
     H*                Apply invoice #.                               *
     H*                                                               *
     H* 11/18/20 C:180 Project WC2757                                 *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 07/30/20 C:65  PROJECT: Phw#90082 (Sunz)                      *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDMPP.                                *
     H*                                                               *
     H* 05/12/20 C:136 Project: 150WEU                                *
     H*                Do not bill A/R on policies flagged to be      *
     H*                excluded from A/R processing.                  *
     H*                                                               *
     H* 03/16/20 C:180 Project wc2700                                 *
     H*                Allow non-renewal cancellation override        *
     H*                                                               *
     H* 12/11/19  C:295 Support: 81720                                *
     H*                 The mass update to mods is corrected to get   *
     H*                 populated correctly in the combined group.    *
     H*                                                               *
     H* 04/22/19  C:65  PROJECT: WC2610                               *
     H*                 RECOMPILED FOR NEW FIELD in WDILP.            *
     H*                                                               *
     H* 02/12/19 C:291 Project: WC2206                                *
     H*                Populate the new flag EHNOEXM1 in WDEHP file.  *
     H*                                                               *
     H* 06/11/18 C:65  Project: 150WAQ                                *
     H*                Update the writing/adjusting of A/R prevention *
     H*                to check the actual setup file now.            *
     H*                                                               *
     H* 02/01/18 C:136 Project: 148WAH                                *
     H*                Populate Policy Fee amount in WDEHP            *
     H*                                                               *
     H* 07/26/17 C:62  Project: 150WAL                                *
     H*                Add Territorial Differential Factor Process.   *
     H*                                                               *
     H* 06/22/17 C:65  PROJECT: Phw# 46465 (CoveRisk)                 *
     H*                Populate ARAP factors into employer history.   *
     H*                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Add Territory Rating for Care West.            *
     H*                                                               *
      * 04/27/16 C:306 Project: WC2424                                *
      *                Populating the new fields in WDEHP file.       *
      *                Exeperience Modifier - not final status.       *
      *                                                               *
      * 04/19/16 C:242 Project: WC2069                                *
      *                Add Billing Fees to Premium History            *
      *                                                               *
     H* 09/21/15 C:65  Support: PHW 37350 (CSI)  Related: 140WAU      *
     H*                Remove coding added to ADJAR which duplicated  *
     H*                the premium reversal adjustment.               *
     H*                                                               *
     H* 08/25/15 C:302 Project: WC2385                                *
     H*                Perform a check at the end of the revised bill *
     H*                +when a user attempts to submit the revised    *
     H*                bill to check if the live files have been      *
     H*                updated since the revised bill started.        *
     H*                (Set the timestamp to indicate the beggining   *
     H*                Of the user creating the revised bill.)        *
     H*                                                               *
     H* 12/18/14 C:136 Support: WA140 87318                           *
     H*                Correct logic for writing installment fees     *
     H*                The deposit logic for 140WAU was overwriting   *
     H*                the SAM array in error causing the installment *
     H*                fees to not write                              *
     H*                                                               *
     H* 09/23/14 C:136 Support: WA114 86797                           *
     H*                Correct WC2193 so that previous premium data   *
     H*                doesn't overwrite current premium data in      *
     H*                WDEHP                                          *
     H*                                                               *
     H* 04/01/14 C:180 Suppport: WA093 85327                          *
     H*                Write to WMLDP only when Large Ded module      *
     H*                is installed.                                  *
     H*                                                               *
     H* 02/15/14 C:180 RED3921 - Prevent rec lck.                     *
     H*                                                               *
     H* 01/06/14 C:136 Support: WA133 84130                           *
     H*                Do not let EHEFDT < ELPFDT. Causes CEU to      *
     H*                report premium incorrectly.                    *
     H*                                                               *
     H* 11/18/13 C:136 Seq#84090                                      *
     H*                Corrected Deposit on installment.              *
     H*                                                               *
     H* 07/15/13 C:282 Project: WC2211                                *
     H*                Populate the terrorism rates from the new      *
     H*                employer detail fields (ELTERR and ELDTRR)     *
     H*                into the employer history file.                *
     H*                                                               *
     H* 07/19/13 C:136 Project: WC2193                                *
     H*                Add unbilled premium                           *
     H*                 Write/Update WXELP if Calc & Print            *
     H*                 Delete WXELP if A/R is updated                *
     H*                 UserOpen on File for backwards compatibility  *
     H*                                                               *
     H* 07/02/13 C:136 Project: 147WAI                                *
     H*                Add processing for Dividend Plan               *
     H*                                                               *
     H* 02/08/13 C:190 Project: 140WAU                                *
     H*                Modify program to adjust deposit off or onto   *
     H*                policy on basis of whether it exists on policy.*
     H*                                                               *
     H* 08/30/12 C:136 SUPPORT: WA025 80122                           *
     H*                WRITE FROM WALDP TO WMLDP FOR LARGE            *
     H*                DEDUCTIBLE CHANGES                             *
     H*                                                               *
     H* 04/20/11 C:136 SUPPORT: WA146 76392                           *
     H*                FIX INSTALLMENT FEE LOGIC                      *
     H*                                                               *
     H* 02/16/11 C:180 SUPPORT 75805                                  *
     H*                Remove canceled project 139WJU.                *
     H*                                                               *
     H* 01/28/11 C:190 PROJECT: WC1954                                *
     H*                POPULATE 'DISCOUNT % #1 & #2' FIELDS IN WDEHP. *
     H*                                                               *
     H* 01/27/11 C:183 Support: 75670                                 *
     H*                The time field D2TIME needs to be validated    *
     H*                before being passed to EPTIME.                 *
     H*                                                               *
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP FOR POLICY PAGE, ITEM 3C.       *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Include HCN Program information.  MOVE THE HCN *
     H*                RECORDS FROM THE REVISED MEMBER TO THE LIVE    *
     H*                FILE.  POPULATE THE NEW FIELDS INTO WDEHP.     *
     H*                                                               *
     H* 08/09/10 C:189 Project: 71WKU                                 *
     H*                All changes assosiated with current processing *
     H*                of RDP will be replaced by the new WA080R      *
     H*                RDP API                                        *
     H*                                                               *
     H* 07/28/10 C:255 PROJECT: 132WOJ                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FLEX FIELDS IN WDELP.                          *
     H*                                                               *
     H* 06/21/10 C:255 Project: 114WVY                                *
     H*                Populate 2nd period deviations information into*
     H*                employer history file when deviations are split*
     H*                                                               *
     H* 01/14/10 C:190 Project: 25WRC                                 *
     H*                When writing the employer history record for   *
     H*                TASB, copy the field for stabilization factor  *
     H*                for deviation #2 instead of the normal perce-  *
     H*                ntage field ELDEV2.                            *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Prior to deleting A/R, write the information   *
     H*                 to the A/R Holding file                       *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records                               *
     H*                                                               *
     H* 10/19/09 C:232 SUPPORT: 67934                                 *
     H*                FIXED THE ISSUE OF POSTING DATE.               *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 05/03/09 C:136 PROJECT: WC1663                                *
     H*                ADD FUNCTIONALITY FOR REV BILL TO RUN CREDIT   *
     H*                BALANCES FROM ALL CATEGORIES THROUGH CASH      *
     H*                DISSOLVE                                       *
     H*                NOTE: WA059R MUST BE CALLED AFTER THE LAST     *
     H*                UPDATE TO A/R IS WRITTEN                       *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use the new logical(s) for editing class       *
     H*                code information.                              *
     H*                                                               *
     H* 11/12/08 C:65  PROJECT: 139WJU                                *
     H*                DO NOT ADJUST ASSESSMENTS FOR CLIENT 139 FOR   *
     H*                S/R ACCOUNTS.                                  *
     H*                                                               *
     H* 09/16/08 C:136 PROJECT: WC1568                                *
     H*                RE-IMPLEMENT ASSESSMENT CODING FOR S/R         *
     H*                POLICIES ONLY.                                 *
     H*                                                               *
     H* 09/04/08 C:62  PROJECT WC1611                                 *
     H*                USE ELNWRW                                     *
     H*                                                               *
     H* 02/25/08  C:65  PROJECT: 136WBG/SUPP#62385                    *
     H*                 RESET THE WDF1P INFORMATION BEFORE WRITING    *
     H*                 RECORDS IN THE DEPOSIT TRANSFER SUBROUTINES.  *
     H*                                                               *
     H* 01/22/08  C:65  PROJECT: WC1627                               *
     H*                 RECOMPILED FOR NEW FIELDS IN WDMPP.           *
     H*                                                               *
     H* 01/11/08 C:65  PROJECT: WC1568                                *
     H*                REMOVE ALL ADJUSTMENT CODING FOR ASSESSMENTS   *
     H*                AS ALL OF THE CALCULATIONS HAVE BEEN DONE IN   *
     H*                W0090R NOW AND THE ADJUSTMENTS ARE PASSED IN   *
     H*                USING THE WAEAP FILE.                          *
     H*                                                               *
     H* 10/11/07 C:183 PROJECT: 146WBC                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 09/17/07 C:136 PROJECT: WC1568                                *
     H*                CORRECTIONS FOR PROJECT                        *
     H*                                                               *
     H* 09/04/07 C:136 SUPPORT: WA139 59936                           *
     H*                MATCH UP PARMS FOR SINGLE DIVISION CALL        *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491                                *
     H*                IMPLEMENT LOGIC FOR NEW 'R' AND 'T' TYPES FOR  *
     H*                DEPOSIT                                        *
     H*                                                               *
     H* 06/13/07 C:136 WA132 57014                                    *
     H*                IF POLICY IS BEING CHANGED FROM SUMMARIZED     *
     H*                TO NON-SUMMARIZED, CLEAR ELARSM IF ELARSM      *
     H*                = '@'                                          *
     H*                                                               *
     H* 05/15/07 C:184 PROJECT WC1568                                 *
     H*                Charge assessments on the same basis as premium*
     H*                installments                                   *
     H*                                                               *
     H* 01/11/07 C:183 PROJECT: 139WGA                                *
     H*                IGNORE CATEGORY 'C' RECORDS WHERE A/R TYPE     *
     H*                IS 'B' OR 'A'. THIS APPLIES FOR REGULAR BI     *
     H*                LLING OR EXPENSE CONSTANT BILLINGS.            *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY POLICY YEAR MODE                *
     H*                                                               *
     H* 07/07/06 C:168 PROJECT: 132WEW                                *
     H*                ADDED CALCULATION FOR AIRCRAFT REINSURANCE     *
     H*                                                               *
     H* 06/30/06 C:136 PROJECT: 133WCV                                *
     H*                ADD HARBOR EXPOSURE PREMIUM                    *
     H*                                                               *
     H* 04/21/06  C:65  PROJECT: 71WGH(REIMPLEMENT 71FDN)             *
     H*                 ADD RETRO CREDIT/DEBIT CODE TO HANDLE ADJS.   *
     H*                                                               *
     H* 03/01/06 C:65  PROJECT: WC1327                                *
     H*                ALLOW PROGRAM TO PROCESS ONE DIVISION ONLY IF  *
     H*                INDICATED TO BE A SINGLE DIVISION RUN.         *
     H*                                                               *
     H* 02/28/06 C:65  PROJECT: SUPPORT                               *
     H*                REMOVE THE CALL TO SRINPR WITHIN THE MAIN      *
     H*                EMPLOYER LOOP AND CALL THE PROGRAM AFTER ALL   *
     H*                DIVISIONS HAVE BEEN PROCESSED.  THIS WILL      *
     H*                PREVENT SRINPR FROM RUNNING ACROSS ALL DIVS    *
     H*                FOR EACH DIVISION.  CALL SRINPR WITH 'E'       *
     H*                MODE INSTEAD OF 'R' SO IT DOES NOT ATTEMPT     *
     H*                CHAINING TO A SPECIFIC DIVISION WITH SRINPR.   *
     H*                                                               *
     H* 02/27/06 C:136 PROJECT: WC1298                                *
     H*                COPY PREMIUM PERIODS FILE FROM WAPPP TO WDPPP. *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC CHANGES TO CREDIT/DEBIT FACTOR *
     H*                IN WBILLCSPEC                                  *
     H*                                                               *
     H*  06/24/05 C:33 Project: 109WDZ                                *
     H*                {109BD}                                        *
     H*                Pull the membership fees from employer detail. *
     H*                                                               *
     H* 05/04/05 C:136 PROJECT: 126WDK                                *
     H*                MOVE 2ND DEDUCTIBLES INTO BASE.                *
     H*                                                               *
     H* 04/08/05 C:136 PROJECT: WC1288 (SUMMARIZE A/R ON S/R ACCOUNTS)*
     H*               -IF DIV IS GOING FROM SUMMARIZED TO             *
     H*                NON-SMMARIZED, THEN ESTIMATE THE AMT OF DEPOSIT*
     H*                PAYMENT THERE SHOULD BE FROM DEPOSIT BILLED    *
     H*                AND TRANSFER TO THAT DIVISION.                 *
     H*                                                               *
     H* 03/03/05 C:132 PROJECT: WC1260                                *
     H*              - Do not print the installment fees if:          *
     H*              1-The Exempt Flag is set on the employer detail  *
     H*                level, or if the policy is effective before    *
     H*              2-if the policy is effective after expiraton     *
     H*                date and the expiration date exists in WDIFP   *
     H*          C:136 Add code to delete installment fees if the     *
     H*                exempt flag is flipped to 'Y' in Revised       *
     H*                Billing, or if a premium change occurs.        *
     H*                                                               *
     H* 01/07/05 C:136 PROJECT: WC1250                                *
     H*                BILL DOMESTIC TERRORISM CHARGE                 *
     H*                                                               *
     H*  11/30/04 C:65 PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                BILL VOLUNTARY LABOR CHARGE                    *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                    *
     H*                Bill 4th and 5th assessments.                  *
     H*                                                               *
     H* 11/11/03 C:65  PROJECT: WC936                                 *
     H*                IF WE ARE REVERSING DEPOSIT ON A A/R           *
     H*                SUMMARIZED DIVISION, TRANSFER MONEY TO         *
     H*                THE STATE REPORTING DIVISION.                  *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP.  *
     H*                                                               *
     H* 10/21/03 C:65  PROJECT: WC1017                                *
     H*                GENERATE A DARTS REMARK TO THE UNDERWRITER IF  *
     H*                A REVISED BILL IS RUN ON A RENEWED EMPLR -     *
     H*                RENEWED IN QUOTE OR LIVE.                      *
     H*                                                               *
     H* 08/08/03 C:136 SUPPORT: WA129 38156                           *
     H*                CHANGE NAME OF DSTIME TO D2TIME TO PREVENT     *
     H*                DSTIME FROM BEING SET TO VALUE IN WDDSP        *
     H*                IN MULTI-DIVISIONAL RUN.                       *
     H*                                                               *
     H* 04/25/03 C:65  PROJECT: WC1116                                *
     H*                ADD IN CODE TO RECOGNIZE AND BILL TO A/R A     *
     H*                TWICE A MONTH BILLING SCHEDULE CODE.           *
     H*                                                               *
     H* 04/21/03 C:65  PROJECT: WC1115                                *
     H*                ADD IN CODE TO RECOGNIZE AND BILL TO A/R A     *
     H*                WEEKLY ESTIMATED BILLING SCHEDULE CODE.        *
     H*                                                               *
     H* 02/04/03 C:65  PROJECT: SUPP#35716/WC1055                     *
     H*                CORRECT EXPENSE CONSTANT CODE TO BE COMPLIENT  *
     H*                WITH THE A/R SUMMARY FLAG - RESULT OF BEING    *
     H*                ABLE TO BILL EXPENSE CONSTANT TO OTHER DIVS.   *
     H*                                                               *
     H* 01/29/03 C:65  PROJECT: WC744                                 *
     H*                LOAD EMPLR HISTORY FIELDS FOR ARAP CHARGE.     *
     H*                                                               *
     H* 01/17/03 C:65  PROJECT: WC1120                                *
     H*                LOAD EMPLR HISTORY FIELDS FOR TERRORISM ACT.   *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT WC1099                                  *
     H*               IF A PAYROLL CLASS CODE HAS AN EXTENSION WITH   *
     H*               A STAT CODE AND AN ADDITIONAL LOAD AMOUNT THEN  *
     H*               DO NOT ADD INTO THE ADDITIONAL LOAD TOTAL.      *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT SUPP#33855                              *
     H*               LOAD ANNUAL EXPENSE CONSTANT INTO EMPLR HISTORY *
     H*                                                               *
     H* 08/12/02 C:65 PROJECT: SUPP#31223                             *
     H*               ONLY SET THE NEW/RENEWAL FLAG TO NEW IF THE     *
     H*               POLICY YEAR IS THE EARLIEST ON THE POLICY.      *
     H*                                                               *
     H*  03/04/02  C:65  PROJECT: WC1064                              *
     H*                  GENERATE AN INSTALLMENT FEE IF DESIGNATED    *
     H*                  ON THE GROUP LEVEL.                          *
     H*                                                               *
     H* 02/28/02 C:65  PROJECT: SUPP#31442                            *
     H*                REMOVE CHANGE DONE 1/28/02 FOR CX DATE.        *
     H*                COPIED IN BY C:136.                            *
     H*                                                               *
     H* 02/06/02 C:65  PROJECT: SUPP#31089                            *
     H*                CORRECT THE A/R CODE USED FOR 3RD ASSESSMENT.  *
     H*                                                               *
     H* 01/28/02 C:65  PROJECT: SUPP#30330                            *
     H*                DO NOT RUN A REVISED BILL ON A DIVISION IF     *
     H*                IT HAS ANY CANCELLATION DATE.                  *
     H*                                                               *
     H* 12/06/01  C:65  PROJECT SUPP#29431                            *
     H*                 THE CODE WHICH ALLOWED UPDATING OF ASSESSMENT *
     H*                 FEES WAS MOVED INTO W8010R IN ORDER FOR THIS  *
     H*                 PROGRAM TO PROPERLY UPDATE A/R AMOUNTS ON     *
     H*                 MULTI-STATE POLICIES WHEN SUMMARIZED.         *
     H*                                                               *
     H* 11/15/01 C:65 PROJECT: SUPP#29767                             *
     H*               USE INTERIM DAYS IN THE ADMIN DARTS FILE FOR    *
     H*               DETERMINING POSTING DATES INSTEAD OF 15 DAYS.   *
     H*                                                               *
     H* 10/05/01  C:65  PROJECT WC1030                                *
     H*                 CHECK FOR PREVIOUS EMPLR# FOR NEW/RENEWAL.    *
     H*                                                               *
     H* 08/21/01 C:65 PROJECT: SUPP#28643                             *
     H*               PULL ASSESSMENTS PER THE ANNIV DATE IF APPLIC.  *
     H*                                                               *
     H*  07/13/01  C:65  PROJECT: SUPPORT                             *
     H*                  CHECK A/R SUMMARY FLAG FROM NEW EMPLR DETAIL *
     H*                  FOR DETERMINING IF TO REMOVE A/R.            *
     H*                                                               *
     H* 02/06/01 C:65 PROJECT: SUPPORT#26048                          *
     H*               ZERO OUT EAPODT BEFORE THE CHECKING OF DT ARRAY *
     H*                                                               *
     H* 01/02/01 C:65 PROJECT: WC1024                                 *
     H*               ADJUST 3RD ASSESSMENTS WHEN APPLICABLE.         *
     H*                                                               *
     H* 11/09/00  C:65  PROJECT SUPPORT                               *
     H*                 DO NOT WRITE A/R RECORDS FOR STATE TAX ON     *
     H*                 DEPOSIT WHEN THE AMOUNT IS ZERO.              *
     H*                                                               *
     H* 07/27/00  C:65  PROJECT WC1001                                *
     H*                 DO NOT ADJUST ASSESSMENTS IF THE STATE IS     *
     H*                 'CA' AND SOMETHING IS ALREADY BILLED.         *
     H*                                                               *
     H* 07/20/00  C:65  PROJECT SUPPORT                               *
     H*                 CHANGE ROUNDING OF MEMBERSHIP FEES TO MATCH   *
     H*                 OTHER PROGRAMS.                               *
     H*                                                               *
     H* 06/23/00  C:65  PROJECT SUPPORT                               *
     H*                 DO NOT CONDITION REMOVING A/R FOR SUMMARIZED  *
     H*                 DIVISIONS ON A BILLING SCHEDULE CODE CHANGE.  *
     H*                                                               *
     H* 06/08/00  C:65  PROJECT SUPPORT                               *
     H*                 DO NOT PROCESS A CANCELLED EMPLOYER.          *
     H*                 PASS CORRECT DIVISION TO SRINPR.              *
     H*                                                               *
     H* 11/15/99  C:65  PROJECT WC936                                 *
     H*                 PROCESS ALL EMPLOYERS ON A MULTI-DIVISION     *
     H*                 POLICY. ALSO UPDATE THE WDILP AND WDMPP FILES *
     H*                                                               *
     H* 10/07/99  C:65  PROJECT WC937                                 *
     H*                 CALCULATE ASSESSMENT FOR BOTH NON-F CLASS     *
     H*                 AND F-CLASS EXPOSURE USING PROPER PERCENTAGES *
     H*                                                               *
     H* 06/01/99  C:65  SUPPORT: CM045                                *
     H*                ADD WAIVER SURCHARGE TO EMPLOYER HISTORY.      *
     H*                                                               *
     H* 04/12/99  C:65  PROJECT WC875                                 *
     H*                 BILL STATE TAX ON DEPOSIT WITH ITS OWN A/R    *
     H*                 CODE.  REMOVE STATE TAX ON DEPOSIT A/R        *
     H*                 WHEN TRANSFERING DEPOSIT RECORDS.             *
     H*                                                               *
     H*  02/12/99  C:65  PROJECT: SUPPORT #16607                      *
     H*                  BLANK THE LATE FLAG WHEN WRITING A/R.        *
     H*                                                               *
     H*  01/06/99  C:65  PROJECT: WC705                               *
     H*                  ALLOW SAFTEY FEE MINIMUM WITH NO PERCENTAGE. *
     H*                                                               *
     H* 08/06/98  C:35   PROJECT: CM036                               *
     H*                  CHANGES TO GET CODE IN SYNCH WITH HUMANA     *
     H*                  VERSION.                                     *
     H*                                                               *
     H*  05/01/98  C:65  PROJECT: WC705                               *
     H*                  USE NEW PREM FIELDS TO CALCULATE ASSESSMENT. *
     H*                                                               *
     H*  04/17/98  C:57  PROJECT: SUPPORT                             *
     H*                  LOAD "E" TO ELFAF PRIOR TO CHAIN OF PEH01    *
     H*                  WHEN SETTING UP FOR AN OVERLAY OF AN "E" REC *
     H*                                                               *
     H*  03/18/98  C:65  PROJECT: WC705                               *
     H*                  SET XNPFDT BEFORE USING IT.                  *
     H*                                                               *
     H*  02/19/98  C:65  PROJECT: WC705                               *
     H*                  WHEN DELETING FUTURE BILLINGS, START FROM    *
     H*                  POSTING DATE(CURRENT + INTERIM).             *
     H*                                                               *
     H*  02/13/98  C:65  PROJECT: WC705                               *
     H*                  LOAD ELPFDT IF WE DON'T GET A HISTORY RECORD.*
     H*                                                               *
     H*  02/05/98  C:65  PROJECT: WC705                               *
     H*                  ADD CHAIN TO PREVENT UPDATE ERROR ON EMP HIS.*
     H*                                                               *
     H*  01/19/98  C:65  PROJECT: WC633                               *
     H*                  ADD 2ND INCR LIMITS TO EMP HISTOY FILE.      *
     H*                                                               *
     H*  12/23/97  C:65  PROJECT: WC776                               *
     H*                  1ST AND 2ND ASSESSMENT.                      *
     H*                                                               *
     H*  12/16/97  C:57  PROJECT: CM036                               *
     H*                  CALENDAR EARNED/UNEARNED RPT CHGS.  SYNCH UP *
     H*                  WDEHP FIELDS TO BE PROPERLY LOADED/PICKED    *
     H*                  UP/CREATED.                                  *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMEPP*
     H*****************************************************************
     F*
     FWAEAL     UF   E           K DISK
     FWMEAL4    UF A E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     FWAELL     UF   E           K DISK
     FWDELL     UF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F                                     INFDS(DBPEL0)
      *** beg add ***** 07/19/13 ***********************************
     FWXELL     UF A E           K DISK    RENAME(PEL01:PXEL00) USROPN
      *** end add ***** 07/19/13 ***********************************
     FWAELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWDELLQ    IF   E           K DISK
     F                                     RENAME(PEL01:PEL0Q)
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWAEPLE    UF   E           K DISK
     FWMEPLE    UF A E           K DISK
     F                                     RENAME(PEP01:PEP02)
     FWADSL     IF   E           K DISK
     FWDDSL     UF A E           K DISK
     F                                     RENAME(PDS01:PDS02)
     FWDHCLR    UF   E           K DISK    USROPN
     F                                     RENAME(PHC01:PHC0R)
     FWDHCL     UF A E           K DISK
     F                                     RENAME(PHC01:PHC00)
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWDEHL6    IF A E           K DISK
     F                                     RENAME(PEH01:PEH06)
     FWMEXL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWAILL     IF   E           K DISK
     FWDILL     UF A E           K DISK
     F                                     RENAME(PIL01:PIL02)
     FWAMPL     IF   E           K DISK
     FWDMPL     UF A E           K DISK
     F                                     RENAME(PMP01:PMP02)
     FWAPPL     IF   E           K DISK
     FWDPPL     UF A E           K DISK
     F                                     RENAME(PPP01:PPP02)
     FWMIFL     IF   E           K DISK
     FWMPXL1    IF   E           K DISK
     FWMLDL1    UF A E           K DISK
     FWALDL1    UF   E           K DISK
     F                                     RENAME(PLD01:PLD01X)
     F*** begin add ***  06/11/18  *******************************
     FWMCOL     IF   E           K DISK    RENAME(PCO01:PCO00) USROPN
     F*** end add *****  06/11/18  *******************************

     F*** BEG ADD ***** 04/01/14 **************************************
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
     F*** END ADD ***** 04/01/14 **************************************
      *** beg add ***** 07/02/13 **************************************************
     d/copy wprDivPlan
     d xxDivPrem       s                   like(ELdvpr)
     d indArSumry      s               n   inz(*off)
      *** end add ***** 07/02/13 **************************************************
      *** beg add ***** 07/19/13 ***********************************************
     d unBilPrmInst    s               n
     d  inObj          s             10
     d  inType         s             10
     d  rtnResult      s              1                                         *off = obj exists
     d ChkObjInLibl    pr                  extpgm('SRCHKOBJ')
     d  inObj                        10
     d  inType                       10
     d  rtnResult                     1                                         *off = obj exists
      *** end add ***** 07/19/13 ***********************************************

     D FYR             S              3  0 DIM(99)
     D SDT             S              7  0 DIM(12)
     D SAM             S              9  2 DIM(12)
     D TY              S              2    DIM(99)
     D AMT             S             15  4 DIM(99)
     D RMK             S             80    DIM(3) CTDATA PERRCD(1)              DARTS DIARY REMARK
      *** Beg Add ***** 02/08/13 **********************************
     d xoESDP          s                   like(BSesdp)
     d xnESDP          s                   like(BSesdp)
     d xoESD2          s                   like(BSesd2)
     d xnESD2          s                   like(BSesd2)
     d xoAB01          s                   like(BSab01)
     d xnAB01          s                   like(BSab01)
     d xoAB02          s                   like(BSab02)
     d xnAB02          s                   like(BSab02)
     d xxDepAdjCde     s                   like(qqDepAdjCde)
      *** End Add ***** 02/08/13 **********************************
     D*** BEG ADD *****  04/01/14  ************************************
     D xsKey           s              2A   INZ('WC')
     D LDModule        s               N   INZ(*OFF)
     D*** END ADD *****  04/01/14  ************************************
     d*** Begin Add *** 05/16/21 *******************************************
     d invoice#        s             10  0
     d inKey           s             20a
     d hldFnd          s                   Like(EAfnd)
     d hldFyr          s                   Like(EAfyr)
     d hldEmp#         s                   Like(EAemp#)
     d hldDiv          s                   Like(EAdiv)
     d hldPoDt         s                   Like(EApoDt)
     d hldInv#         s                   Like(EAnInv)
     d hldArSm         s                   Like(ELarSm) Inz
     D inJson          s          32000a   Inz varying
     d chrInvType      s             25a   Inz(' ')
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
     d sysDate         s                   Like(EApoDt)
     d*** End   Add *** 05/16/21 *******************************************
     D OVRHCR          C                   CONST('OVRDBF FILE(WDHCLR) -
     D                                     MBR(REVISED) -
     D                                     OVRSCOPE(*JOB)')
       //*** Beg Add ***** 03/16/20 ************************
      /copy wpremplr
     D*** Begin Add *** 05/16/21 *******************************************
     d/copy wprPmtSvc
     d/copy wprAR
     D*** End   Add *** 05/16/21 *******************************************
     d xxMode          s              3a   Inz('BIL')
     d intCxDate       s              7  0
       //*** End Add ***** 03/16/20 ************************
     d*** Begin Add *** 06/12/24 *******************************************
     d isSRAs          s               n   Inz(*Off)
     d isSkipAR        s               n   Inz(*Off)
     d*** End   Add *** 06/12/24 *******************************************
      *** beg add ***** 05/12/20 **********************************
     d isExcBillingAR  s               n
      /Free
       Dcl-S IsSelfReportExclusion Ind;
       Dcl-S isAllowedToWrtAr Ind;
      /End-Free
      *** end add ***** 05/12/20 **********************************

     D*** DATA STRUCTURES FOR DARTS REMARK
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  D2TIME                 1      6  0
     D  D2TMHH                 1      2  0
     D  D2TMMM                 3      4  0
     D  D2TMSS                 5      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ELPFDT                 1      7  0
     D  XXPFYM                 1      5  0
     D                 DS
     D  XXDATE                 1      7  0
     D  XXYYMM                 1      5  0
     D                 DS
     D  EHTRDT                 1      7  0
     D  XXTRYM                 1      5  0
     D                 DS
     D  XOPFDT                 1      7  0
     D  XOPFYM                 1      5  0
     D                 DS
     D  XNPFDT                 1      7  0
     D  XNPFYM                 1      5  0
     D                 DS
     D  DT                     1    084  0
     D                                     DIM(12)
     D  EAPO01               001    007  0
     D  EAPO02               008    014  0
     D  EAPO03               015    021  0
     D  EAPO04               022    028  0
     D  EAPO05               029    035  0
     D  EAPO06               036    042  0
     D  EAPO07               043    049  0
     D  EAPO08               050    056  0
     D  EAPO09               057    063  0
     D  EAPO10               064    070  0
     D  EAPO11               071    077  0
     D  EAPO12               078    084  0
     D                 DS
     D  AM                     1    108  2
     D                                     DIM(12)
     D  EAAM01                 1      9  2
     D  EAAM02                10     18  2
     D  EAAM03                19     27  2
     D  EAAM04                28     36  2
     D  EAAM05                37     45  2
     D  EAAM06                46     54  2
     D  EAAM07                55     63  2
     D  EAAM08                64     72  2
     D  EAAM09                73     81  2
     D  EAAM10                82     90  2
     D  EAAM11                91     99  2
     D  EAAM12               100    108  2
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
     D  XXPFDT                 1      7  0
     D  YY                     1      3  0
     D  MM                     4      5  0
     D  DD                     6      7  0
     D                 DS
     D  X1DT                   1      7  0
     D  MM1                    4      5  0
     D  DD1                    6      7  0
     D DBPEL0          DS
     D  DBRRN                397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
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
     C                   KFLD                    XXST
     C                   KFLD                    ELPFDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C     RRKYA         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    ELANDT
     C*
     C*  EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  BILLING CHANGE TRANSACTION FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV
     C                   KFLD                    XXFYR
     C*
     C*  BILLING CHANGE TRANSACTION FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C                   KFLD                    XXEXT
     C*
     C*  EMPLOYER A/R FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY
     C                   KFLD                    XXPODT
     C*
     C     ARKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY
     C*
     C     ARKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ARKEY4        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  DISCOUNT FILE
     C*
     C     DSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C     ELKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C     ELKYQ         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XQFYR             3 0
     C*
     C*  EMPLOYER DETAIL HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    ELFAF
     C                   KFLD                    XXENDT
     C*
     C     EHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     EHKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    ELFAF
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XPST              2
     C     LDKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    EPCO#         XXCO#
     C     *LIKE         DEFINE    EPFND         XXFND
     C     *LIKE         DEFINE    EPFYR         XXFYR
     C     *LIKE         DEFINE    EPEMP#        XXEMP#
     C     *LIKE         DEFINE    EPDIV         XXDIV
     C     *LIKE         DEFINE    EPEFDT        XXEFDT
     C     *LIKE         DEFINE    EPEFDT        XXENDT
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C     *LIKE         DEFINE    EPSPEC        XXSPEC
     C     *LIKE         DEFINE    EPEXT         XXEXT
     C     *LIKE         DEFINE    EAARTY        XXARTY
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    ELBSCD        XXBSCD
     C     *LIKE         DEFINE    ELTXDU        XXTXDU
     C     *LIKE         DEFINE    TTTYPE        XXTYPE
     C     *LIKE         DEFINE    BSSEFL        XXFROM
     C     *LIKE         DEFINE    BSSEFL        XXTO
     C     *LIKE         DEFINE    EAARTY        SVARTY
     C     *LIKE         DEFINE    EAAMT         SVAMT
     C     *LIKE         DEFINE    EAFYR         SVFYR
     C     *LIKE         DEFINE    BSSEFL        XOSEFL
     C     *LIKE         DEFINE    BSSEFL        XNSEFL
     C     *LIKE         DEFINE    ELBSCD        XOBSCD
     C     *LIKE         DEFINE    ELBSCD        XNBSCD
     C     *LIKE         DEFINE    EHIDT         XXIDT
     C     *LIKE         DEFINE    EHIUSR        XXIUSR
     C     *LIKE         DEFINE    EAAMT         XXAMT
     C     *LIKE         DEFINE    EAAMT         XXMFBT
     C     *LIKE         DEFINE    ELIFFL        XOIFFL
     C     *LIKE         DEFINE    ELIFFL        XNIFFL
     C     *LIKE         DEFINE    ELARSM        XOARSM
     C     *LIKE         DEFINE    ELARSM        XNARSM
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQAR              2
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQCALL            1
     C*** Beg   Add ***  02/08/13  ************************************
     C                   PARM                    qqDepAdjCde       2
     C*** End   Add ***  02/08/13  ************************************
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY6        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** Beg   Add ***  02/08/13  ************************************
      /free
       If %parms >=9;
          xxDepAdjCde = qqDepAdjCde;
       Endif;
      /end-free
     C*** End   Add ***  02/08/13  ************************************
      *** beg add ***** 07/19/13 *************************************
      /free
       inObj = 'WXELP';
       inType = '*FILE';
       Callp ChkObjInLibl (inObj:inType:rtnResult);
       If rtnResult = *off;
          unBilPrmInst = *on;
       Endif;
      /end-free
      *** end add ***** 07/19/13 *************************************
     C*** BEG ADD ***** 04/01/14 ******************************************
      /Free
       LDModule = *OFF;
       Chain xsKey SWC01;
       If WCFL43 =  'Y';
          LDModule = *ON;
       Endif;
      /End-Free
     C*** End ADD ***** 04/01/14 ******************************************
     c*** Begin Add *** 05/16/21 ******************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         sysDate
     c*** End   Add *** 05/16/21 ******************************************
     C                   CALL      'QCMDEXC'
     C                   PARM      OVRHCR        CMD              55
     C                   PARM      55            LENGTH           15 5
     C*
     C                   OPEN      WDHCLR
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQEFDT        X1ENDT            8 0
     C                   Z-ADD     X1ENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*
     C*** begin add ***  06/11/18  *******************************
     C                   Clear                   xxColt            1
      /free
       Open wmcol;
       Chain(n) (XXCO#:XXFND:XXEMP#) PCO00;
       If %found;
          xxColt = 'Y';
       Endif;
       Close wmcol;
       //*** beg add ***** 05/12/20 ***********************
       Chain (xxCo#:xxFnd) pex01;
       // Check if client has flagged this group to exclude A/R
       //  processing on Self-Reporting policies. Note: A/R
       //  processing is allowed when changing from S/R to Est or
       //  changing from non-summarized to summarized.
       If %Found;
          isSelfReportExclusion = EXexcArBl = 'Y';
       EndIf;
       //*** end add ***** 05/12/20 ***********************
      /end-free
     C*** end add *****  06/11/18  *******************************
     C*
     C* SINGLE DIVISION MODE.
     C*
     C     QQCALL        IFEQ      'S'
     C                   MOVE      *ON           *IN67
     C                   MOVE      '#'           QQRNB             1
     C                   ELSE
     C                   MOVE      *OFF          *IN67
     C                   MOVE      *BLANKS       QQRNB             1
     C                   ENDIF                                                  @@CALL IFEQ
     C*
     C*  GET THE USER PARMS
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C                   Z-ADD     0             XTSFAM           11 2
     C                   Z-ADD     0             XTMFAM           11 2
     C                   Z-ADD     0             XT1AAM           11 2
     C                   Z-ADD     0             XT2AAM           11 2
     C                   Z-ADD     0             XT3AAM           11 2
     C                   Z-ADD     0             XT4AAM           11 2
     C                   Z-ADD     0             XT5AAM           11 2
     C                   Z-ADD     0             XT6AAM           11 2
     C                   Z-ADD     0             XT7AAM           11 2
     C                   Z-ADD     0             XTEXCN           11 2
     C     ELKY          CHAIN     PEL03                              60
     C     *IN60         DOWEQ     *OFF
     C*
       //*** Begin Add *** 06/12/24 ****************************************
       // Check to see if SR and if SR Assessments is off
       isSRAs = *Off;
       xxKey = %EditC(ELbsCd:'X');
       Chain (xxCo#:xxKey) pbs01;
       If %Found(wtabl) And BSseFl = 'S';
          If ELsrAs = ' ';
          //*** begin chg ***  01/29/25  ****************************
          //*****   Chain (xxCo#:xxFnd) pf101;
          //*****   If %Found(wdf1l) And F1SRAs = 'Y';
             If F1SRAs = 'Y';
          //*** begin chg ***  01/29/25  ****************************
                isSRAs = *On;
             EndIf;
          Else;
             isSRAs = *On;
          EndIf;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
     C     ELARSM        IFEQ      'Y'
     C                   ADD       ELSFAM        XTSFAM
     C                   ADD       ELMFAM        XTMFAM
     C                   ADD       EL1AAM        XT1AAM
     C                   ADD       EL2AAM        XT2AAM
     C                   ADD       EL3AAM        XT3AAM
     C                   ADD       EL4AAM        XT4AAM
     C                   ADD       EL5AAM        XT5AAM
     C                   ADD       EL6AAM        XT6AAM
     C                   ADD       EL7AAM        XT7AAM
     C                   ADD       ELPRTD        XTEXCN
     C                   ENDIF
     C*
     C     ELKY          READE     PEL03                                  60
     C                   ENDDO
     C                   Z-ADD     0             XSDIV             5 0
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C                   LEAVE
     C                   ENDIF                                                  EMSTRP IFEQ
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* PROCESS EACH DIVISION
     C*
     C  N67EMKY          CHAIN     PEM01                              60
     C   67EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     EMDIV         XXDIV
     C     EAKEY         CHAIN     PEL02                              60
     C                   Z-ADD     DBRRN         ELRRN
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANK        XXFIRS            1
     C*** begin add ***  10/18/24  *********************************************
     C                   MOVE      *BLANK        xxBLFL            1
     C                   Move      ELBLFL        XOBLFL            1
     C*** end add *****  10/18/24  *********************************************
     C                   Z-ADD     EL1AAM        XX1AAM           11 2
     C                   Z-ADD     EL2AAM        XX2AAM           11 2
     C                   Z-ADD     EL1APR        XX1APR           11 2
     C                   Z-ADD     EL2APR        XX2APR           11 2
     C                   Z-ADD     EL3AAM        XX3AAM           11 2
     C                   Z-ADD     EL3APR        XX3APR           11 2
     C                   Z-ADD     EL4AAM        XX4AAM           11 2
     C                   Z-ADD     EL4APR        XX4APR           11 2
     C                   Z-ADD     EL5AAM        XX5AAM           11 2
     C                   Z-ADD     EL5APR        XX5APR           11 2
     C                   Z-ADD     EL6AAM        XX6AAM           11 2
     C                   Z-ADD     EL6APR        XX6APR           11 2
     C                   Z-ADD     EL7AAM        XX7AAM           11 2
     C                   Z-ADD     EL7APR        XX7APR           11 2
     C*
     C                   Z-ADD     ELSFAM        XXSAMT           11 2
     C                   Z-ADD     ELMFAM        XXMFAM           11 2
     C                   Z-ADD     ELPRTD        XXPRTD           11 2
     C*
     C     ELFAF         IFEQ      *BLANK
     C                   MOVE      'Y'           XXFIRS
     C                   ENDIF
     C                   MOVE      ELARSM        XXARSM            1
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE            7 0
     C                   Z-ADD     1             Y
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
     C                   MOVE      ' '           XXCNCL            1
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     X1DATE
     C     Y             ORNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     ELPFDT
     C                   MOVE      'Y'           XXCNCL            1
     C                   END
     C*
       //*** Begin Add *** 03/16/20 ***************************
       // Check for override if cx'd. Allow to process if
       //  override is valid.
       intCxDate = cd(y);
       If xxCncl = 'Y'
        And OvrRenewalCx(xxMode:xxCo#:xxFnd:xxEmp#:
         xxDiv:xxFyr:intCxDate) = *On;
          xxCncl = ' ';
       Endif;
       //*** End   Add *** 03/16/20 ***************************
      /End-Free
     C*  ONLY PROCESS DIVISION IF NOT CANCELLED.
     C*
     C     XXCNCL        IFNE      'Y'
     C                   Z-ADD     0             XXACDT            8 0
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
     C     @@PLAN        IFEQ      'A'
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     EXAOID        XXDAYS            5 0
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
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C*
       //*** Beg Chg ***** 05/12/20 ************************
     C*
     C***     F1UPAR        IFEQ      'Y'
     C*
     C*  IF THERE WAS A BILLING SCHEDULE CODE CHANGE, THEN DELETE
     C*  ALL FUTURE BILLINGS AND ADJUSTMENTS FROM A/R
     C*
     C***     EAKEY         CHAIN     PEL01                              60
     C***     *IN60         IFEQ      *OFF
     C***                   MOVEL     ELBSCD        XXBSCD
     C***                   MOVEL     ELBSCD        XNBSCD
     C***                   MOVE      ELPFDT        XNPFDT
     C***                   MOVEL     ELIFFL        XNIFFL
     C***                   MOVEL     ELARSM        XNARSM
     C***                   MOVEL     ELTXDU        XXTXDU
     C***     EAKEY         CHAIN(N)  PEL02                              60
     C***     *IN60         IFEQ      *OFF
     C***                   MOVEL     ELBSCD        XOBSCD
     C***                   MOVEL     ELIFFL        XOIFFL
     C***                   MOVEL     ELARSM        XOARSM
     C****
     C****  SAVE OLD BILLING SCHEDULE CODE INFO
     C****
     C***                   MOVEL(P)  XOBSCD        XXKEY
     C***     TBKEY         CHAIN     PBS01                              60
     C***     *IN60         IFEQ      *OFF
     C***                   MOVEL     BSSEFL        XOSEFL
      ****** Beg   Add ***  02/08/13  ************************************
      ***/free
       //xoesdp = BSesdp;
       //xoesd2 = BSesd2;
       //xoab01 = BSab01;
       //xoab02 = BSab02;
      ***/end-free
      ****** End   Add ***  02/08/13  ************************************
     C***                   ELSE
     C***                   CLEAR                   XOSEFL
      ****** Beg   Add ***  02/08/13  ************************************
      ***/free
       //Clear xoesdp;
       //Clear xoesd2;
       //Clear xoab01;
       //Clear xoab02;
      ***/end-free
      ****** End   Add ***  02/08/13  ************************************
      ***
     C***                   ENDIF                                                  *IN60 IFEQ
     C****
     C****  SAVE NEW BILLING SCHEDULE CODE INFO
     C****
     C***                   MOVEL(P)  XNBSCD        XXKEY
     C***     TBKEY         CHAIN     PBS01                              60
     C***     *IN60         IFEQ      *OFF
     C***                   MOVEL     BSSEFL        XNSEFL
      ****** Beg   Add ***  02/08/13  ************************************
      ***/free
       //xnesdp = BSesdp;
       //xnesd2 = BSesd2;
       //xnab01 = BSab01;
       //xnab02 = BSab02;
      ***/end-free
      ****** End   Add ***  02/08/13  ************************************
     C***                   ELSE
     C***                   CLEAR                   XNSEFL
      ****** Beg   Add ***  02/08/13  ************************************
      ***/free
       //Clear xnesdp;
       //Clear xnesd2;
       //Clear xnab01;
       //Clear xnab02;
      ***/end-free
      ****** End   Add ***  02/08/13  ************************************
     C***                   ENDIF                                                  *IN60 IFEQ
      /Free
       // Get revised policy info
       isAllowedToWrtAr = *On;
       Chain eakey pel01;
       If %Found(WAELL);
          xxBsCd = ELBsCd;
       //*** begin add ***  10/18/24  ************************************
          xxBLFL = ELBLFL;
       //*** end add *****  10/18/24  ************************************
          xnBsCd = ELBsCd;
          xnPfDt = ELPfDt;
          xnIfFl = ELIfFl;
          xnArSm = ELArSm;
          xxTxDu = ELTxDu;
          xxKey = %EditC(xnBsCd:'X');
         // Get billing plan
          Chain tbKey pbs01;
          If %Found;
             xnSeFl = BSSeFl;
             xnesdp = BSesdp;
             xnesd2 = BSesd2;
             xnab01 = BSab01;
             xnab02 = BSab02;
          Else;
             Clear xnSeFl;
             Clear xnEsdp;
             Clear xnEsd2;
             Clear xnab01;
             Clear xnab02;
          Endif;
       Else;
          isAllowedToWrtAr = *Off;
       Endif;
       // Get Live policy info
       Chain(n) eakey pel02;
       If %Found(WDELL);
          xoBsCd = ELBsCd;
          xoIfFl = ELIfFl;
          xoArSm = ELArSm;
          xxKey = %EditC(xoBsCd:'X');
         // Get billing plan
          Chain tbKey pbs01;
          If %Found;
             xoSeFl = BSSeFl;
             xoesdp = BSesdp;
             xoesd2 = BSesd2;
             xoab01 = BSab01;
             xoab02 = BSab02;
          Else;
             Clear xoSeFl;
             Clear xoEsdp;
             Clear xoEsd2;
             Clear xoab01;
             Clear xoab02;
          Endif;
       Else;
          isAllowedToWrtAr = *Off;
       Endif;
       // Don't write A/R if:
       //  1) the group policy year isn't flagged for it
       //  2) the policy is self reporting and the group is
       //      flagged to exclude A/R from updating such
       Select;
       When F1UpAr <> 'Y';
          isAllowedToWrtAr = *Off;
       When isSelfReportExclusion = *On
        And (xoSeFl = 'S' Or xnSeFl = 'S');
          isAllowedToWrtAr = *Off;
       EndSl;

       If isAllowedToWrtAr = *On;
      /End-Free
      //*** End Chg ***** 05/12/20 ************************
      *
     C     XXTXDU        SUB       ELTXDU        XXTXDU
     C     XOBSCD        IFNE      XNBSCD
     C*** begin add ***  10/18/24  ****************************************
     C     XOBLFL        ORNE      XXBLFL
     C*** end add *****  10/18/24  ****************************************
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   Z-ADD     0             IFIFEE
     C                   ENDIF
      //*** Beg Del ***** 05/12/20 ************************
     C****
     C****  SAVE OLD BILLING SCHEDULE CODE INFO
     C****
     C***                   MOVEL(P)  XOBSCD        XXKEY
     C***     TBKEY         CHAIN     PBS01                              60
     C***     *IN60         IFEQ      *OFF
     C***                   MOVEL     BSSEFL        XOSEFL
     C***                   ELSE
     C***                   CLEAR                   XOSEFL
     C***                   ENDIF                                                  *IN60 IFEQ
     C****
     C****  SAVE NEW BILLING SCHEDULE CODE INFO
     C****
     C***                   MOVEL(P)  XNBSCD        XXKEY
     C***     TBKEY         CHAIN     PBS01                              60
     C***     *IN60         IFEQ      *OFF
     C***                   MOVEL     BSSEFL        XNSEFL
     C***                   ELSE
     C***                   CLEAR                   XNSEFL
     C***                   ENDIF                                                  *IN60 IFEQ
      //*** End Del ***** 05/12/20 ************************
     C*
     C                   MOVE      'B'           XXARTY
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   Z-ADD     XXPODT        X1DT
     C*** begin del ***  10/18/24  ****************************************
     C*****              EXSR      SRFUTI
     C*** end del *****  10/18/24  ****************************************
     C                   Z-ADD     X1DT          XXPODT
     C*
     C     ARKEY         SETLL     PEA02
     C     ARKEY2        READE     PEA02                                  50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
      *** beg add ***** 07/02/13 **************************************
     C     EAARCD        ANDNE     F1DVBL
      *** end add ***** 07/02/13 **************************************
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ARKEY2        READE     PEA02                                  50
     C                   ENDDO
     C*
     C                   MOVE      'A'           XXARTY
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   Z-ADD     XXPODT        X1DT
     C*** begin del ***  10/18/24  ****************************************
     C*****              EXSR      SRFUTI
     C*** end del *****  10/18/24  ****************************************
     C                   Z-ADD     X1DT          XXPODT
     C*
     C     ARKEY         SETLL     PEA02
     C     ARKEY2        READE     PEA02                                  50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF
     C                   ENDIF
     C*
     C     ARKEY2        READE     PEA02                                  50
     C                   ENDDO
     C*
     C*  DELETE ANY FUTURE STATE TAX BILLING RECORDS
     C*
     C                   MOVE      'F'           XXARTY
     C     ARKEY         SETLL     PEA02
     C     ARKEY2        READE     PEA02                                  50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C     EAARCD        OREQ      IFIFBL
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF
     C                   ENDIF
     C*
     C     ARKEY2        READE     PEA02                                  50
     C                   ENDDO
     C*
     C                   ENDIF                                                  ELBSCD <> XXBSCD
      //*** Beg Chg ***** 05/12/20 ************************
     C***                   ENDIF                                                  *IN60
     C***                   ENDIF                                                  *IN60
      //*** End Chg ***** 05/12/20 ************************
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* REMOVE A/R FROM EMPLOYER IF SUMMARIZED.
     C*
     C     ELARSM        IFEQ      'Y'
     C     EMSTRP        ANDNE     'Y'
     C     F1UPAR        ANDEQ     'Y'
     C                   EXSR      ADJAR
     C                   ENDIF
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  GET THE BILLING UPDATE TRANSACTION
     C*
     C                   Z-ADD     *ZERO         AM
     C                   Z-ADD     *ZERO         DT
     C                   Z-ADD     *ZERO         SAM
     C                   Z-ADD     *ZERO         SDT
     C                   MOVE      *BLANK        SW                1
     C                   Z-ADD     *ZERO         SX                3 0
     C                   Z-ADD     *ZERO         SDT2              7 0
     C                   MOVE      *OFF          *IN68
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   MOVE      *ON           *IN68
     C                   ENDIF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C  N68EAKEY         CHAIN     PEA01                              60
     C   68EAKEY2        CHAIN     PEA01                              60
     C*
     C*  THERE WILL BE 2 BILLING UPDATE RECORDS, ONE FOR THE PREMIUM AN
     C*  FOR THE STATE TAX.  USE THE A/R CODE TO PROCESS THE CORRECT RE
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAARCD        IFEQ      QQAR
     C     EATRDT        IFGT      55
     C*
     C* BILL TO A/R NON-WEEKLY INSTALLMENTS.
     C*
     C                   MOVE      DT            SDT
     C                   MOVE      AM            SAM
     C*
     C*** WRITE EMPLOYER A/R RECORD
     C*
     C     1             DO        12            X                 3 0
     C     AM(X)         IFNE      *ZERO
     C                   Z-ADD     DT(X)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  QQAR          XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     QQAR          EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(X)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF
     C                   ENDDO
     C                   ELSE
     C     EAEDIT        IFEQ      'T'
     C*
     C* BILL TO A/R TWICE MONTHLY INSTALLMENT ADJUSTMENTS.
     C*
     C                   MOVE      'T'           SW
     C                   Z-ADD     EATRDT        X
     C                   Z-ADD     EATRDT        SX
     C                   Z-ADD     DT(2)         SDT2
     C*
     C     AM(1)         IFNE      *ZERO
     C                   Z-ADD     DT(1)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  QQAR          XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     QQAR          EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(1)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** end   Add *** 06/12/24 **********************************
     C*
     C                   ENDIF                                                  AM,1 IFNE
     C*
     C* REMAINING INSTALLMENTS.
     C*
     C     AM(2)         IFNE      *ZERO
     C*
     C                   Z-ADD     DT(2)         XXPFDT
     C     1             DO        X
     C*
     C                   Z-ADD     XXPFDT        EAPODT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  QQAR          XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     QQAR          EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(2)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C     DD            IFEQ      1
     C                   Z-ADD     15            DD
     C                   ELSE
     C*
     C                   Z-ADD     1             DD
     C                   ADD       1             MM
     C     MM            IFEQ      13
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*
     C                   ENDIF                                                  WDD IFEQ
     C*
     C                   ENDDO                                                  1 DO TRDT
     C*
     C                   ENDIF                                                  AM,2 IFNE
     C*
     C                   ELSE                                                   EAEDIT IFEQ
     C*
     C* BILL TO A/R WEEKLY INSTALLMENT ADJUSTMENTS.
     C*
     C                   MOVE      'Y'           SW
     C                   Z-ADD     EATRDT        X
     C                   Z-ADD     EATRDT        SX
     C                   Z-ADD     DT(2)         SDT2
     C*
     C     AM(1)         IFNE      *ZERO
     C                   Z-ADD     DT(1)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  QQAR          XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     QQAR          EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(1)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ENDIF                                                  AM,1 IFNE
     C*
     C* REMAINING INSTALLMENTS.
     C*
     C     AM(2)         IFNE      *ZERO
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C*
     C     1             DO        X
     C*
     C                   Z-ADD     DT(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
     C                   CALL      'SRFUTR8'
     C                   PARM                    QQDT              8 0
     C                   PARM      XXCNT         QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  QQAR          XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     QQAR          EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(2)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ADD       7             XXCNT
     C                   ENDDO                                                  1 DO TRDT
     C*
     C                   ENDIF                                                  AM,2 IFNE
     C*
     C                   ENDIF                                                  EAEDIT IFEQ
     C                   ENDIF                                                  EATRDT IFGT
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA01
     C                   ENDIF                                                  QQAR IFEQ
      *** Beg Add ***** 02/08/13 **********************************
     C                   IF        %Parms >= 9
     C     EAARCD        IFEQ      xxDepAdjCde
     C     EATRDT        IFGT      55
     C*
     C* BILL TO A/R NON-WEEKLY INSTALLMENTS.
     C*
      *** beg del ***** 12/18/14 ******************************
     C***********        MOVE      DT            SDT
     C***********        MOVE      AM            SAM
      *** end del ***** 12/18/14 ******************************
     C*
     C*** WRITE EMPLOYER A/R RECORD
     C*
     C     1             DO        12            X                 3 0
     C     AM(X)         IFNE      *ZERO
     C                   Z-ADD     DT(X)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  xxDepAdjCde   XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     xxDepAdjCde   EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(X)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF
     C                   ENDDO
     C                   ELSE
     C     EAEDIT        IFEQ      'T'
     C*
     C* BILL TO A/R TWICE MONTHLY INSTALLMENT ADJUSTMENTS.
     C*
     C                   MOVE      'T'           SW
     C                   Z-ADD     EATRDT        X
     C                   Z-ADD     EATRDT        SX
     C                   Z-ADD     DT(2)         SDT2
     C*
     C     AM(1)         IFNE      *ZERO
     C                   Z-ADD     DT(1)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  xxDepAdjCde   XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     xxDepAdjCde   EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(1)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ENDIF                                                  AM,1 IFNE
     C*
     C* REMAINING INSTALLMENTS.
     C*
     C     AM(2)         IFNE      *ZERO
     C*
     C                   Z-ADD     DT(2)         XXPFDT
     C     1             DO        X
     C*
     C                   Z-ADD     XXPFDT        EAPODT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  xxDepAdjCde   XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     xxDepAdjCde   EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(2)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C     DD            IFEQ      1
     C                   Z-ADD     15            DD
     C                   ELSE
     C*
     C                   Z-ADD     1             DD
     C                   ADD       1             MM
     C     MM            IFEQ      13
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*
     C                   ENDIF                                                  WDD IFEQ
     C*
     C                   ENDDO                                                  1 DO TRDT
     C*
     C                   ENDIF                                                  AM,2 IFNE
     C*
     C                   ELSE                                                   EAEDIT IFEQ
     C*
     C* BILL TO A/R WEEKLY INSTALLMENT ADJUSTMENTS.
     C*
     C                   MOVE      'Y'           SW
     C                   Z-ADD     EATRDT        X
     C                   Z-ADD     EATRDT        SX
     C                   Z-ADD     DT(2)         SDT2
     C*
     C     AM(1)         IFNE      *ZERO
     C                   Z-ADD     DT(1)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  xxDepAdjCde   XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     xxDepAdjCde   EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(1)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ENDIF                                                  AM,1 IFNE
     C*
     C* REMAINING INSTALLMENTS.
     C*
     C     AM(2)         IFNE      *ZERO
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C*
     C     1             DO        X
     C*
     C                   Z-ADD     DT(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
     C                   CALL      'SRFUTR8'
     C                   PARM                    QQDT              8 0
     C                   PARM      XXCNT         QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  xxDepAdjCde   XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     xxDepAdjCde   EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(2)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ADD       7             XXCNT
     C                   ENDDO                                                  1 DO TRDT
     C*
     C                   ENDIF                                                  AM,2 IFNE
     C*
     C                   ENDIF                                                  EAEDIT IFEQ
     C                   ENDIF                                                  EATRDT IFGT
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA01
     C                   ENDIF                                                  xxDepAdjCde IfEq
     C                   Endif
      *** End Add ***** 02/08/13 **********************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     TTTYPE        ANDEQ     'F'
     C     TTCTYP        OREQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C     TTCTYP        OREQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C     TTCTYP        OREQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C     TTCTYP        OREQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C     TTCTYP        OREQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C     TTCTYP        OREQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C     TTCTYP        OREQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C*
     C*** WRITE EMPLOYER A/R RECORD
     C*
     C     EATRDT        IFGT      55
     C*
     C* BILL TO A/R NON-WEEKLY TAX INSTALLMENTS.
     C*
     C     1             DO        12            X                 3 0
     C     AM(X)         IFNE      *ZERO
     C                   Z-ADD     DT(X)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     TTTYPE        EAARTY                                   **
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(X)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  AM,X IFNE
     C                   ENDDO                                                  DO 12
     C                   ELSE
     C     EAEDIT        IFEQ      'T'
     C*
     C* BILL TO A/R TWICE MONTHLY INSTALLMENT ADJUSTMENTS.
     C*
     C                   MOVE      'T'           SW
     C                   Z-ADD     EATRDT        X
     C                   Z-ADD     EATRDT        SX
     C                   Z-ADD     DT(2)         SDT2
     C*
     C     AM(1)         IFNE      *ZERO
     C                   Z-ADD     DT(1)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(1)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ENDIF                                                  AM,1 IFNE
     C*
     C* REMAINING INSTALLMENTS.
     C*
     C     AM(2)         IFNE      *ZERO
     C*
     C                   Z-ADD     DT(2)         XXPFDT
     C     1             DO        X
     C*
     C                   Z-ADD     XXPFDT        EAPODT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(2)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C     DD            IFEQ      1
     C                   Z-ADD     15            DD
     C                   ELSE
     C*
     C                   Z-ADD     1             DD
     C                   ADD       1             MM
     C     MM            IFEQ      13
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*
     C                   ENDIF                                                  WDD IFEQ
     C*
     C                   ENDDO                                                  1 DO TRDT
     C*
     C                   ENDIF                                                  AM,2 IFNE
     C*
     C                   ELSE                                                   EAEDIT IFEQ
     C*
     C* BILL TO A/R WEEKLY TAX INSTALLMENT ADJUSTMENTS.
     C*
     C                   Z-ADD     EATRDT        X
     C*
     C     AM(1)         IFNE      *ZERO
     C                   Z-ADD     DT(1)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     TTTYPE        EAARTY                                   **
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(1)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ENDIF                                                  AM,1 IFNE
     C*
     C* REMAINING INSTALLMENTS.
     C*
     C     AM(2)         IFNE      *ZERO
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C*
     C     1             DO        X
     C*
     C                   Z-ADD     DT(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
     C                   CALL      'SRFUTR8'
     C                   PARM                    QQDT              8 0
     C                   PARM      XXCNT         QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     TTTYPE        EAARTY                                   **
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(2)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ADD       7             XXCNT
     C                   ENDDO                                                  1 DO TRDT
     C*
     C                   ENDIF                                                  AM,2 IFNE
     C*
     C                   ENDIF                                                  EAEDIT IFEQ
     C                   ENDIF                                                  EATRDT IFGT
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA01
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C     TTCTYP        IFEQ      'D'
     C     TTTYPE        ANDEQ     'F'
     C*
     C*** WRITE TAX ON DEPOSIT RECORD.
     C*
     C*
     C     AM(1)         IFNE      *ZERO
     C                   Z-ADD     DT(1)         EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     AM(1)         EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  AM,1 IFNE
     c*** begin del ***  08/29/21  *****************************************
     C*********          EXSR      HOLDAR
     c*** end del *****  08/29/21  *****************************************
     C                   DELETE    PEA01
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C  N68EAKEY         READE     PEA01                                  60
     C   68EAKEY2        READE     PEA01                                  60
     C                   ENDDO
     C*** Begin Add *** 02/13/24 *******************************************
     C* Restore the DT values to the DT array
     C                   MOVE      SDT           DT
     C*** End   Add *** 02/13/24 *******************************************
     C     *IN67         IFEQ      *ON
     C     XXARSM        ANDNE     'Y'
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     0             XXSAMT
     C                   Z-ADD     0             XXMFAM
     C                   Z-ADD     0             XXPRTD
     C                   Z-ADD     0             XX1AAM
     C                   Z-ADD     0             XX2AAM
     C                   Z-ADD     0             XX3AAM
     C                   Z-ADD     0             XX4AAM
     C                   Z-ADD     0             XX5AAM
     C                   Z-ADD     0             XX6AAM
     C                   Z-ADD     0             XX7AAM
     C                   ENDIF
     C*
     C*  CHECK EXPENSE CONSTANT BILLED RECORD FOR ANY NEEDED ADJUSTMENT
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      'B'           XXARTY
     C                   Z-ADD     *ZEROS        XXAMT
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'E'
     C     TTCTYP        ANDNE     'C'
     C                   ADD       EAAMT         XXAMT
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XXPRTD        XXAMT
     C                   ENDIF
     C                   Z-ADD     0             XXEXCN
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XTEXCN        XXEXCN           11 2
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPRTD        XXEXCN
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C*  GET EMPLOYER DETAIL RECORD TO USE EXPENSE CONSTANT FIELD
     C*
     C*
     C     XXEXCN        SUB       XXAMT         EAAMT
     C     EAAMT         IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'E'
     C     TTTYPE        ANDEQ     'B'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C                   Z-ADD     EAPODT        X1DT
     C                   EXSR      SRFUTI
     C                   Z-ADD     X1DT          EAPODT
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  CHECK MEMBER FEE BILLED FOR ANY NEEDED ADJUSTMENTS.
     C*
     C                   Z-ADD     *ZERO         XXMFBT
     C                   Z-ADD     ELMFEE        F1MFEE
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XXMFAM        XXMFBT
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         XXMFEE           11 2
     C     F1PMFL        IFEQ      'Y'                                          PRORATE MEMBER FEE
     C                   Z-ADD     *ZEROS        XXDPRO
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELPROR        XXDPRO            5 3
     C                   ENDIF
     C     F1MFEE        MULT(H)   XXDPRO        XXMFEE
     C                   ELSE
     C                   Z-ADD(H)  F1MFEE        XXMFEE                         BILL FULL MEMBER FEE
     C                   ENDIF
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XTMFAM        XXMFEE
     C                   ELSE
     C                   Z-ADD     0             XXMFEE
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELMFAM        XXMFEE
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C     XXMFEE        SUB       XXMFBT        XXDIFM           11 2
     C*
     C     XXDIFM        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR MEMBERSHIP FEE
     C*
     C                   Z-ADD(H)  XXDIFM        EAAMT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'M'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C                   Z-ADD     EAPODT        X1DT
     C                   EXSR      SRFUTI
     C                   Z-ADD     X1DT          EAPODT
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIFM IFNE
     C*
     C*  CHECK INSTALLMENT FEES FOR ANY NEEDED ADJUSTMENTS.
     C*
     C*
     C     *IN67         IFEQ      *OFF
     C                   Z-ADD     *ZERO         XXINSB           11 2
     C                   MOVE      *BLANKS       IFIFBL
     C                   Z-ADD     0             IFIFEE
     C                   Z-ADD     *ZERO         XXINST           11 2
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
      *
      * If ELIFFL flag is set to 'Y' or the IFEXPD date exists and  policy
      * effective date is after IFEXPD, do not bill the installment fees
      *
     C     *IN60         IFEQ      *OFF
     C     XNIFFL        IFEQ      'Y'                                          C:136
     C     IFEXPD        ORNE      *ZEROS
     C     ELPFDT        ANDGE     IFEXPD                                       C:136
     C                   EXSR      SRDLIF                                       C:136
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  EFIFFL = 'Y'
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   Z-ADD     0             IFIFEE
     C                   ENDIF
     C                   ENDIF
     C*
     C     IFIFBL        IFNE      *BLANKS
     C     XOBSCD        ANDNE     XNBSCD
     C*** begin add ***  10/18/24  **************************************
     C     IFIFBL        ORNE      *BLANKS
     C     XOBLFL        ANDNE     XXBLFL
     C*** end add *****  10/18/24  **************************************
     C     IFIFBL        ORNE      *BLANKS
     C     XXFIRS        ANDEQ     'Y'
     C     IFIFBL        ORNE      *BLANKS
     C     XXARSM        ANDNE     ELARSM
     C     EMSTRP        ANDNE     'Y'
     C     IFIFBL        ORNE      *BLANKS
     C     XOIFFL        ANDEQ     'Y'
     C     XNIFFL        ANDEQ     *BLANK
     C*
     C     ELARSM        IFEQ      'Y'
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     IFIFEE        XXINSB
     C                   ELSE
     C                   Z-ADD     0             XXINSB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C                   Z-ADD     IFIFEE        XXINSB
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C     XXINSB        IFNE      *ZERO
     C*
     C*  DELETE FUTURE INSTALLMENT FEES
     C*
     C                   XFOOT     SAM           XXSAM            15 2
     C     XXSAM         IFNE      *ZERO
     C                   EXSR      SRDLIF
     C                   ENDIF
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR INSTALLMENT FEE
     C*
     C     SW            IFEQ      *BLANK
     C     2             DO        12            X                 3 0
     C     SAM(X)        IFNE      *ZERO
     C                   Z-ADD     SDT(X)        EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  IFIFBL        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     IFIFBL        EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     XXINSB        EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF
     C                   ENDDO
     C                   ELSE
     C     SW            IFEQ      'T'
     C*
     C                   Z-ADD     SDT2          XXPFDT
     C     1             DO        SX
     C*
     C                   Z-ADD     XXPFDT        EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  IFIFBL        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     IFIFBL        EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     XXINSB        EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C     DD            IFEQ      1
     C                   Z-ADD     15            DD
     C                   ELSE
     C*
     C                   Z-ADD     1             DD
     C                   ADD       1             MM
     C     MM            IFEQ      13
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*
     C                   ENDIF                                                  WDD IFEQ
     C*
     C                   ENDDO                                                  1 DO SX
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXCNT             5 0
     C*
     C     1             DO        SX
     C*
     C                   Z-ADD     SDT2          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT
     C                   CALL      'SRFUTR8'
     C                   PARM                    QQDT              8 0
     C                   PARM      XXCNT         QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  IFIFBL        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     IFIFBL        EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   Z-ADD     XXINSB        EAAMT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ADD       7             XXCNT
     C                   ENDDO                                                  1 DO SX
     C*
     C                   ENDIF                                                  SW IFEQ
     C                   ENDIF                                                  SW IFEQ
     C                   ENDIF                                                  XXINSB IFNE
     C                   ENDIF                                                  IFIFBL IFNE
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C* CHECK IF SAFETY FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XXSFBT           11 2
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XXSAMT        XXSFBT
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         XXDIFS           11 2
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'S'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXSFBL            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXSFBL        IFNE      *BLANK
     C     ELSFPC        ANDNE     *ZEROS
     C     XXSFBL        ORNE      *BLANK
     C     ELSFMN        ANDNE     *ZEROS
     C*
     C*   CALCULATE SAFETY FEE BILLED.
     C*
     C                   Z-ADD     *ZERO         XXPREM           15 4
     C                   SELECT
     C     RRSFAP        WHENEQ    'J'
     C                   Z-ADD     ELESBJ        XXPREM
     C     RRSFAP        WHENEQ    'E'
     C                   Z-ADD     ELEFRS        XXPREM
     C     RRSFAP        WHENEQ    'S'
     C                   Z-ADD     ELSTPR        XXPREM
     C     RRSFAP        WHENEQ    'D'
     C                   Z-ADD     ELENMP        XXPREM
     C     RRDV1L        IFEQ      'D'
     C                   ADD       ELEDC2        XXPREM
     C                   ENDIF
     C     RRDV2L        IFEQ      'D'
     C                   ADD       ELEDV2        XXPREM
     C                   ENDIF
     C                   ENDSL                                                  RRSFAP SELC
     C*
     C     ELSFPC        MULT      XXPREM        XXSFAM           15 4
     C     XXSFAM        DIV(H)    100           XXSFEE           11 0
     C*
     C     RRSPFL        IFEQ      'Y'
     C     XXSFEE        MULT(H)   ELPROR        XXSFEE
     C                   ENDIF                                                  RRSPFL IFEQ
     C*
     C     XXSFEE        IFLT      ELSFMN
     C                   Z-ADD     ELSFMN        XXSFEE
     C                   ENDIF                                                  XXSFEE IFLT
     C                   ENDIF                                                  XXSFBL IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XTSFAM        XXSFEE
     C                   ELSE
     C                   Z-ADD     0             XXSFEE
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELSFAM        XXSFEE
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XXSFEE        SUB       XXSFBT        XXDIFS           11 2
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIFS        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR MEMBERSHIP FEE
     C*
     C                   Z-ADD(H)  XXDIFS        EAAMT
     C                   MOVEL     XXSFBL        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XXSFBL        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C                   Z-ADD     EAPODT        X1DT
     C                   EXSR      SRFUTI
     C                   Z-ADD     X1DT          EAPODT
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
     C                   ENDIF                                                  XXDIFS IFNE
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C     XNSEFL        IFEQ      'S'
     C*
     C* CHECK IF 1ST ASSESSMENT FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XX1BAL           15 4
     C                   Z-ADD     *ZERO         XXDIF1           15 4
     C                   Z-ADD     *ZERO         XX1AMT
     C                   Z-ADD     *ZERO         XX1STB
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XX1AAM        XX1BAL
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX1CDE            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     ELANDT        ADD       10000         XXANDT            7 0
     C*
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C     RRKYA         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C     XX1CDE        IFNE      *BLANKS
     C     RR1STP        ANDNE     *ZEROS
     C*
     C*   CALCULATE 1ST ASSESSMENT BILLED.
     C*
     C                   Z-ADD     EL1APR        XXPREM           15 4
     C*
     C     RR1STF        MULT      EL1FPR        XX1AMT           15 4
     C     RR1STP        MULT      EL1APR        XXPREM
     C                   ADD       XXPREM        XX1AMT
     C     XX1AMT        DIV(H)    100           XX1STB           11 0
     C*
     C                   ENDIF                                                  XXSFBL IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XT1AAM        XX1STB
     C                   ELSE
     C                   Z-ADD     0             XX1STB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EL1AAM        XX1STB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XX1STB        SUB       XX1BAL        XXDIF1
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIF1        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR 1ST ASSESSMENT
     C*
     C                   Z-ADD(H)  XXDIF1        EAAMT
     C                   MOVEL(P)  XX1CDE        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XX1CDE        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIF1 IFNE
     C*
     C* CHECK IF 2ND ASSESSMENT FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XX2BAL           15 4
     C                   Z-ADD     *ZERO         XXDIF2           15 4
     C                   Z-ADD     *ZERO         XX2AMT
     C                   Z-ADD     *ZERO         XX2NDB
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XX2AAM        XX2BAL
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX2CDE            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     ELANDT        ADD       10000         XXANDT            7 0
     C*
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C     RRKYA         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C     XX2CDE        IFNE      *BLANKS
     C     RR2NDP        ANDNE     *ZEROS
     C*
     C*   CALCULATE 2ND ASSESSMENT BILLED.
     C*
     C                   Z-ADD     EL2APR        XXPREM
     C*
     C     RR2NDF        MULT      EL2FPR        XX2AMT           15 4
     C     RR2NDP        MULT      EL2APR        XXPREM
     C                   ADD       XXPREM        XX2AMT
     C     XX2AMT        DIV(H)    100           XX2NDB           11 0
     C*
     C                   ENDIF                                                  XX2CDE IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XT2AAM        XX2NDB
     C                   ELSE
     C                   Z-ADD     0             XX2NDB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EL2AAM        XX2NDB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XX2NDB        SUB       XX2BAL        XXDIF2
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIF2        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR 2ND ASSESSMENT
     C*
     C                   Z-ADD(H)  XXDIF2        EAAMT
     C                   MOVEL(P)  XX2CDE        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XX2CDE        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIF2 IFNE
     C*
     C* CHECK IF 3RD ASSESSMENT FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XX3BAL           15 4
     C                   Z-ADD     *ZERO         XXDIF3           15 4
     C                   Z-ADD     *ZERO         XX3AMT
     C                   Z-ADD     *ZERO         XX3RDB
     C*
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XX3AAM        XX3BAL
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX3CDE            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     ELANDT        ADD       10000         XXANDT            7 0
     C*
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C     RRKYA         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C     XX3CDE        IFNE      *BLANKS
     C     RR3RDP        ANDNE     *ZEROS
     C*
     C*   CALCULATE 3RD ASSESSMENT BILLED.
     C*
     C                   Z-ADD     EL3APR        XXPREM
     C*
     C     RR3RDF        MULT      EL3FPR        XX3AMT           15 4
     C     RR3RDP        MULT      EL3APR        XXPREM
     C                   ADD       XXPREM        XX3AMT
     C     XX3AMT        DIV(H)    100           XX3RDB           11 0
     C*
     C                   ENDIF                                                  XX3CDE IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XT3AAM        XX3RDB
     C                   ELSE
     C                   Z-ADD     0             XX3RDB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EL3AAM        XX3RDB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XX3RDB        SUB       XX3BAL        XXDIF3
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIF3        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR 3RD ASSESSMENT
     C*
     C                   Z-ADD(H)  XXDIF3        EAAMT
     C                   MOVEL(P)  XX3CDE        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XX3CDE        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIF3 IFNE
     C*
     C* CHECK IF 4TH ASSESSMENT FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XX4BAL           15 4
     C                   Z-ADD     *ZERO         XXDIF4           15 4
     C                   Z-ADD     *ZERO         XX4AMT
     C                   Z-ADD     *ZERO         XX4THB
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XX4AAM        XX4BAL
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX4CDE            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELANDT        ADD       10000         XXANDT            7 0
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C     RRKYA         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C     XX4CDE        IFNE      *BLANKS
     C     RR4THP        ANDNE     *ZEROS
     C*
     C*   CALCULATE 4TH ASSESSMENT BILLED.
     C*
     C                   Z-ADD     EL4APR        XXPREM
     C*
     C     RR4THF        MULT      EL4FPR        XX4AMT           15 4
     C     RR4THP        MULT      EL4APR        XXPREM
     C                   ADD       XXPREM        XX4AMT
     C     XX4AMT        DIV(H)    100           XX4THB           11 0
     C*
     C                   ENDIF                                                  XX4CDE IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XT4AAM        XX4THB
     C                   ELSE
     C                   Z-ADD     0             XX4THB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EL4AAM        XX4THB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XX4THB        SUB       XX4BAL        XXDIF4
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIF4        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR 4TH ASSESSMENT
     C*
     C                   Z-ADD(H)  XXDIF4        EAAMT
     C                   MOVEL(P)  XX4CDE        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XX4CDE        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIF4 IFNE
     C*
     C* CHECK IF 5TH ASSESSMENT FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XX5BAL           15 4
     C                   Z-ADD     *ZERO         XXDIF5           15 4
     C                   Z-ADD     *ZERO         XX5AMT
     C                   Z-ADD     *ZERO         XX5THB
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XX5AAM        XX5BAL
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX5CDE            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELANDT        ADD       10000         XXANDT            7 0
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C     RRKYA         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C     XX5CDE        IFNE      *BLANKS
     C     RR5THP        ANDNE     *ZEROS
     C*
     C*   CALCULATE 5TH ASSESSMENT BILLED.
     C*
     C                   Z-ADD     EL5APR        XXPREM
     C*
     C     RR5THF        MULT      EL5FPR        XX5AMT           15 4
     C     RR5THP        MULT      EL5APR        XXPREM
     C                   ADD       XXPREM        XX5AMT
     C     XX5AMT        DIV(H)    100           XX5THB           11 0
     C*
     C                   ENDIF                                                  XX5CDE IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XT5AAM        XX5THB
     C                   ELSE
     C                   Z-ADD     0             XX5THB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EL5AAM        XX5THB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XX5THB        SUB       XX5BAL        XXDIF5
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIF5        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR 5TH ASSESSMENT
     C*
     C                   Z-ADD(H)  XXDIF5        EAAMT
     C                   MOVEL(P)  XX5CDE        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XX5CDE        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIF4 IFNE
     C*
     C* CHECK IF 6TH ASSESSMENT FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XX6BAL           15 4
     C                   Z-ADD     *ZERO         XXDIF6           15 4
     C                   Z-ADD     *ZERO         XX6AMT
     C                   Z-ADD     *ZERO         XX6THB
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XX6AAM        XX6BAL
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX6CDE            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELANDT        ADD       10000         XXANDT            7 0
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C     RRKYA         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C     XX6CDE        IFNE      *BLANKS
     C     RR6THP        ANDNE     *ZEROS
     C*
     C*   CALCULATE 6TH ASSESSMENT BILLED.
     C*
     C                   Z-ADD     EL6APR        XXPREM
     C*
     C     RR6THF        MULT      EL6FPR        XX6AMT           15 4
     C     RR6THP        MULT      EL6APR        XXPREM
     C                   ADD       XXPREM        XX6AMT
     C     XX6AMT        DIV(H)    100           XX6THB           11 0
     C*
     C                   ENDIF                                                  XX6CDE IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XT6AAM        XX6THB
     C                   ELSE
     C                   Z-ADD     0             XX6THB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EL6AAM        XX6THB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XX6THB        SUB       XX6BAL        XXDIF6
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIF6        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR 6TH ASSESSMENT
     C*
     C                   Z-ADD(H)  XXDIF6        EAAMT
     C                   MOVEL(P)  XX6CDE        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XX6CDE        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIF6 IFNE
     C*
     C* CHECK IF 7TH ASSESSMENT FEES NEED AN ADJUSTMENT.
     C*
     C                   Z-ADD     *ZERO         XX7BAL           15 4
     C                   Z-ADD     *ZERO         XXDIF7           15 4
     C                   Z-ADD     *ZERO         XX7AMT
     C                   Z-ADD     *ZERO         XX7THB
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
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
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XX7AAM        XX7BAL
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX7CDE            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELANDT        ADD       10000         XXANDT            7 0
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C     RRKYA         SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C     XX7CDE        IFNE      *BLANKS
     C     RR7THP        ANDNE     *ZEROS
     C*
     C*   CALCULATE 7TH ASSESSMENT BILLED.
     C*
     C                   Z-ADD     EL7APR        XXPREM
     C*
     C     RR7THF        MULT      EL7FPR        XX7AMT           15 4
     C     RR7THP        MULT      EL7APR        XXPREM
     C                   ADD       XXPREM        XX7AMT
     C     XX7AMT        DIV(H)    100           XX7THB           11 0
     C*
     C                   ENDIF                                                  XX7CDE IFNE
     C*
     C     ELARSM        IFEQ      'Y'
     C     *IN67         ANDEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     XT7AAM        XX7THB
     C                   ELSE
     C                   Z-ADD     0             XX7THB
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ELSE
     C*
     C     EAKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EL7AAM        XX7THB
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C     XX7THB        SUB       XX7BAL        XXDIF7
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXDIF7        IFNE      *ZERO
     C*
     C*  WRITE ADJUSTMENT A/R RECORD FOR 7TH ASSESSMENT
     C*
     C                   Z-ADD(H)  XXDIF7        EAAMT
     C                   MOVEL(P)  XX7CDE        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      XX7CDE        EAARCD
     C                   SORTA     DT
     C*
     C                   Z-ADD     1             XX                3 0
     C                   Z-ADD     0             EAPODT
     C     XX            DOWLT     13
     C     DT(XX)        IFNE      *ZERO
     C                   Z-ADD     DT(XX)        EAPODT
     C                   Z-ADD     12            XX
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C     EAPODT        IFEQ      *ZERO
     C*
     C*   DEFAULT PAYMENT DUE DATE TO CURRENT SYSTEM DATE + 15 DAYS.
     C*
     C*
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C*** Begin Add *** 02/13/24 *******************************************
     C* Use current system date if amount being posted is a credit
     C                   IF        EAAMT > 0
     C*** End   Add *** 02/13/24 *******************************************
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** Begin Add *** 02/13/24 *******************************************
     C                   ENDIF
     C*** End   Add *** 02/13/24 *******************************************
     C*
     C     EAPODT        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        EAPODT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   Z-ADD     0             EARDIV
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   Z-ADD     XXDIV         EARDIV
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANKS       EAEDIT
     C                   Z-ADD     *ZERO         EABUL1
     C                   Z-ADD     *ZERO         EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  XXDIF7 IFNE
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XNSEFL = 'S'
     C*** Begin Del *** 02/01/24 *******************************************
     C**********         EXSR      GETINV
     C*** End   Del *** 02/01/24 *******************************************
     C*
     c*
     C     F1ORCR        IFEQ      'Y'
     C     CLTID         ANDEQ     '071'
     C                   CALL      'WA080R'
     C                   PARM      'R'           #1MOD             1
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      XXFND         #1FND             3 0
     C                   PARM      XXEMP#        #1EMP#            9 0
     C                   PARM      XXDIV         #1DIV             5 0
     C                   PARM      XXFYR         #1FYR             3 0
     C                   PARM      *ZEROS        #1BAT#            9 0
     C                   ENDIF                                                  F1ORCR
     C*
     C*
     C                   ENDIF                                                  F1UPAR = 'Y'
     C*
     C*  UPDATE LIVE FILES WITH NEW INFORMATION AND DELETE RECORDS
     C*  FROM EMPLOYER DETAIL AND EMPLOYER PAYROLL CHANGED RECORD TRANSACTION
     C*  FILES
     C*
     C     EAKEY         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C     EAKEY         CHAIN     PEL01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        ELTIME
     C                   DELETE    PEL01
     C                   ENDIF
     C     ELARSM        IFEQ      '@'
     C                   MOVE      *BLANK        ELARSM
     C                   ENDIF
      ******* Beg add * 08/25/15  ************************************
     C                   Z-Add     *Zeros        ELRTM
     C                   Z-Add     *Zeros        ELRDT
      ******* End add * 08/25/15  ************************************
     C                   UPDATE    PEL02
     C                   ENDIF
      *** beg add ***** 07/19/13 **********************************************
      /free
       // Delete Unbilled Premium Record if it exists
       If unBilPrmInst = *on;
          Open WXELL;
       //*** beg chg ***** 09/23/14 **********************************************
       //***Chain (ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr) pxel00;
       //***If %found;
       //***   Delete pxel00;
       //***Endif;
          Delete (ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr) pxel00;
       //*** end chg ***** 09/23/14 **********************************************
          Close WXELL;
       Endif;
      /end-free
      *** end add ***** 07/19/13 **********************************************
      *** beg add ***** 07/02/13 **********************************************
      /free
       // Write out dividend A/R for non-summarized divisions.
       If ValidDivClient = *on and ELarsm <> 'Y';
          xxDivPrem = UpdateDividend(ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr);
       Else;
          indArSumry = *on;
       Endif;
      /end-free
      *** end add ***** 07/02/13 **********************************************
     C                   Z-ADD     0             XXADLD           11 2
     C*
     C     EAKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   Z-ADD     EPCLAS        XXCLAS
     C                   MOVEL     EPSPEC        XXSPEC
     C                   MOVEL     EPEXT         XXEXT
     C     EPKEY         CHAIN     PEP02                              50
     C     EPKEY         CHAIN     PEP01                              60
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C     PXCODE        IFEQ      *ZERO
     C                   ADD       EPADLD        XXADLD
     C                   ENDIF                                                  PXCODE IFEQ
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   ADD       1             D2TIME
     C     D2TMSS        IFGE      60
     C                   Z-ADD     0             D2TMSS
     C                   ADD       100           D2TIME
     C                   ENDIF
     C     D2TMMM        IFGE      60
     C                   Z-ADD     0             D2TMMM
     C                   ADD       10000         D2TIME
     C                   ENDIF
     C*
     C     D2TMHH        IFGE      24
     C*
     C                   Z-ADD     EPDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY6
     C                   MOVEL     SRDTO         XXDT              6 0
     C*
     C* ADD ONE DAY IF HRS GETS GREATER THAN 24
     C*
     C                   CALL      'SRFUTR'
     C                   PARM                    XXDT
     C                   PARM      1             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C*
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   Z-ADD     0             D2TMHH
     C                   ENDIF
     C                   Z-ADD     D2TIME        EPTIME
     C     *IN50         IFEQ      *OFF
     C                   UPDATE    PEP02
     C                   ELSE
     C                   WRITE     PEP02
     C                   ENDIF                                                  *IN50
     C     EAKEY         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C     EAKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PEP01
     C     EAKEY         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXIN
     C*
     C     *LIKE         DEFINE    HCIN          XXIN
     C*
     C     EAKEY         CHAIN     PHC0R                              60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     HCIN          XXIN
     C     HCKY          CHAIN     PHC00                              50
     C     HCKY          CHAIN     PHC0R                              60
     C                   MOVEL     Q1USER        HCUSER
     C                   MOVEL     Q1PGM         HCPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        HCTIME
     C     *IN50         IFEQ      *OFF
     C                   UPDATE    PHC00
     C                   ELSE
     C                   WRITE     PHC00
     C                   ENDIF                                                  *IN50
     C     EAKEY         READE     PHC0R                                  60
     C                   ENDDO                                                  *IN60
     C*
     C     *IN60         DOUEQ     *ON
     C     EAKEY         DELETE    PHC0R                              60
     C                   ENDDO                                                  *IN60
     C*
     C*  UPDATE LIVE FILES WITH NEW INFORMATION AND DELETE RECORDS
     C*  FROM DISCOUNT FILE.  DO NOT DELETE THE RECORD BECAUSE OTHER   ACTION
     C*  DIVISIONS WILL BE CALLING W8010R WITH THE OVERRIDE TO WADSP
     C*  THEREFORE THIS RECORD WILL REMAIN UNTIL CLEARED IN THE DAILY
     C*  BATCH PROGRAM
     C*
     C     DSKEY         CHAIN     PDS02                              60
     C     DSKEY         CHAIN     PDS01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     Q1USER        DSUSER
     C                   MOVEL     Q1PGM         DSPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        DSTIME
     C  N60              UPDATE    PDS02
     C   60              WRITE     PDS02
     C                   ENDIF
     C     DSKEY         CHAIN     PIL02                              60
     C     DSKEY         CHAIN     PIL01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ILDT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        ILTIME
     C  N60              UPDATE    PIL02
     C   60              WRITE     PIL02
     C                   ENDIF
     C*
     C     DSKEY         CHAIN     PMP02                              60
     C     DSKEY         CHAIN     PMP01                              50
     C     *IN50         IFEQ      *OFF
     C                   MOVEL     Q1USER        MPUSER
     C                   MOVEL     Q1PGM         MPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MPDT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        MPTIME
     C  N60              UPDATE    PMP02
     C   60              WRITE     PMP02
     C                   ENDIF
     C*
     C*
     C     DSKEY         CHAIN     PPP02                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PPP02
     C     DSKEY         READE     PPP02                                  60
     C                   ENDDO
     C*
     C     DSKEY         CHAIN     PPP01                              50
     C     *IN50         DOWEQ     *OFF
     C                   MOVEL     Q1USER        PPUSER
     C                   MOVEL     Q1PGM         PPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPDT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        PPTIME
     C                   WRITE     PPP02
     C     DSKEY         READE     PPP01                                  50
     C                   ENDDO                                                  *IN50 DOWEQ
     C*
      *
     C     EAKEY         CHAIN     PLD01                              60
     C     EAKEY         CHAIN     PLD01X                             61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        LDTIME
     C*** BEGIN DELETE *** 04/01/14 ****************************************
     C**N60              UPDATE    PLD01
     C***60              WRITE     PLD01
     C*** END   DELETE *** 04/01/14 ****************************************
      /Free
       //** Begin Add    ******  04/01/14 **********************************
       If LdModule;
          If *IN60;
             Write PLD01;
          Else;
             Update PLD01;
          Endif;
       Endif;
       //** End   Add    ******  04/01/14 **********************************
      /End-Free
     C                   DELETE    PLD01X
     C                   ENDIF                                                  *IN61=*OFF
      *
     C                   MOVE      ELPFDT        XNPFDT
     C*
     C*  GET THE POLICY EFFDT FROM THE MOST RECENT WDEHP RECORD.
     C*
     C     EHKY1         SETGT     PEH06
     C     EHKY1         READPE    PEH06                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EHPFDT        XOPFDT
     C                   ELSE
     C                   MOVE      ELPFDT        XOPFDT
     C                   ENDIF
     C*  CHECK FOR EXISTING REVISIONS.  IF THERE ARE REVISIONS WE CANNOT
     C*  UPDATE THE EXISTING "E" RECORD.
      *
      *  CHECK TO SEE IF THE POLICY EFFECTIVE DATE HAS CHANGED TO A DIFFERENT
      *  DATE.
      *
B1   C     *IN60         IFEQ      *OFF                                         EHP REC FOUND
     C     XOPFDT        ANDNE     XNPFDT                                       PFDT CHGD
      *                                                    MONTH
      *  IF THE POLICY EFF DT HAS CHANGED, REVERSE OUT THE WDEHP RECS
      *  WITH OLD PFDT
      *
     C                   MOVE      ELCO#         Q2CO#             3
     C                   MOVE      ELFND         Q2FND             3
     C                   MOVE      ELEMP#        Q2EMP#            9
     C                   MOVE      ELDIV         Q2DIV             5
     C                   MOVE      ELFYR         Q2FYR             3
     C                   MOVE      XOPFDT        Q2PFDT            7
     C                   CALL      'SRRVEH'
     C                   PARM                    Q2CO#
     C                   PARM                    Q2FND
     C                   PARM                    Q2EMP#
     C                   PARM                    Q2DIV
     C                   PARM                    Q2FYR
     C                   PARM                    Q2PFDT
     C                   ENDIF
     C*
     C                   CLEAR                   PEH06
     C*
     C                   MOVE      ELCO#         EHCO#
     C                   MOVE      ELFND         EHFND
     C                   MOVE      ELFYR         EHFYR
     C                   MOVE      ELEMP#        EHEMP#
     C                   MOVE      ELDIV         EHDIV
     C                   MOVE      XXENDT        EHEFDT
      *** beg add ***** 01/06/14 ***********************************************
      /free
       If EHefdt < ELpfdt;
          EHefdt = ELpfdt;
       Endif;
      /end-free
      *** end add ***** 01/06/14 ***********************************************
     C                   MOVE      ELFAF         EHFAF
     C                   MOVE      ELBSCD        EHBSCD
     C                   MOVE      ELPFDT        EHPFDT
     C                   MOVE      ELPTDT        EHPTDT
     C                   MOVE      ELANDT        EHANDT
     C                   MOVE      ELPROR        EHPROR
     C                   MOVE      ELMAPR        EHWMPR
     C                   MOVE      ELSTPR        EHWSPR
     C                   MOVE      ELENMP        EHWNMP
     C                   MOVE      ELANPR        EHWAPR
     C*** begin add ***  11/07/23  **************************************
     C* Actual figures from WDMPP
     C                   MOVE      MPMAPR        EHWAMAN
     C                   MOVE      MPSTPR        EHWASTD
     C                   MOVE      MPENMP        EHWANORM
     C                   MOVE      MPANPR        EHWAANN
     C* Balance to minimum
     C                   If        ELEmnp = 'Y'
     C                   Eval      EHWBal = MPEBAL
     C                   Else
     C                   Eval      EHWBal = 0
     C                   Endif
     C*** end add *****  11/07/23  **************************************
     C*** Beg Add *****  02/27/24  **************************************
       EHWASBJ = MPESBJ;
       EHWAMOD = MPEMPM;
       //*** Beg Chg ***** 08/14/24 *************************************
       //***EHWAIL1 = ELEIN1;
       //***EHWAIL2 = ELEIN2;
       EHWAIL1 = MPEIN1;
       EHWAIL2 = MPEIN2;
       EHWBIL  = MPEBIL;
       EHWBIL2 = MPEBIL2;
       //*** End Chg ***** 08/14/24 *************************************
       EHWADC1 = MPEDC1;
       EHWADC2 = MPEDC2;
       EHWADC3 = MPEDC3;
       EHWADV2 = MPEDV2;
       EHWADCT = MPEDCT;
       EHWADD2 = MPEDD2;
       EHWADCM = MPEDCM;
       EHWADCS = MPEDCS;
       EHWADC = MPEDC;
       EHWAWAV = MPEWAV;
       EHWATCA = MPETCA;
       EHWAPRD = MPPRTD;
       EHWAPRF = 0;
       EHWAXPA = MPEXPA;
       EHWAVLC = MPEVLC;
       EHWAARC = MPEARC;
       EHWAD12 = MPD1E2;
       EHWAD22 = MPD2E2;
       EHWAHCN = MPEHCN;
       EHWAUSA = MPAUSA;
       EHWAXP1 = MPEXP1;
       EHWAXP2 = MPEXP2;
       EHWAXP3 = MPEXP3;
       EHWARP1 = MPRPE1;
       EHWARP2 = MPRPE2;
       EHWARP3 = MPRPE3;
       EHWALOS = MPELOS;
     C*** End Add *****  02/27/24  **************************************
     C                   MOVE      ELEMNP        EHWMNP
     C                   MOVE      ELPRTD        EHPRTD
     C                   MOVE      ELPRTF        EHPRTF
     C                   MOVE      ELWVFL        EHWVFL
     C                   MOVE      ELASP1        EHASP1
     C                   MOVE      ELCREF        EHCREF
     C                   MOVE      ELCREX        EHCREX
     C                   MOVE      ELEDC1        EHWDC1
     C                   MOVE      ELASP2        EHASP2
     C                   MOVE      *BLANKS       XXLRGD            1
     C                   MOVE      *BLANKS       XXKEY
     C*
     C*   VERIFY IF POLICY IS LARGE DEDUCTIBLE BASED ON BILLING SCHEDULE CODE
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        ANDEQ     'L'
     C                   MOVE      'Y'           XXLRGD
     C                   ENDIF
     C*** BEG ADD ******* 01/05/22 ***************************
      /Free
       If BSLPGM <> 'L';                       // Not Large Deductible Plan
          Chain LDKY1 PLD01;
          If %found(WMLDL1);
             Delete PLD01;
          Endif;
       Endif;
      /End-Free
     C*** END ADD ******* 01/05/22 ***************************
     C*
     C     XXLRGD        IFEQ      'Y'
     C     LDKY1         SETGT     PLD01
      *** BEG CHANGE **** 02/15/14 *******
     C***  LDKY1         READPE    PLD01                                  60
     C     LDKY1         READPE(N) PLD01                                  60
      *** END'CHANGE **** 02/15/14 *******
     C     *IN60         IFEQ      *OFF
     C                   MOVE      LDPCT1        EHPCT1
     C                   MOVE      LDPCT2        EHPCT2
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      ELD1S2        EHD1S2
     C                   MOVE      ELD1D2        EHD1D2
     C                   MOVE      ELD1X2        EHD1X2
     C                   MOVE      ELD1E2        EHD1W2
     C                   MOVE      ELD2S2        EHD2S2
     C                   MOVE      ELD2D2        EHD2D2
     C                   MOVE      ELD2X2        EHD2X2
     C                   MOVE      ELD2E2        EHD2W2
     C                   MOVE      ELDVEF        EHDVEF
     C                   MOVE      ELDVEX        EHDVEX
     C                   MOVE      ELEDC2        EHWDC2
     C                   MOVE      ELASP3        EHASP3
     C                   MOVE      ELDISC        EHDISC
     C                   MOVE      ELEDC3        EHWDC3
     C                   MOVE      ELEHCN        EHWHCN
     C                   MOVE      ELHCN         EHHCN
     C                   MOVE      ELEXM1        EHEXM1
     C                   MOVE      ELEMD1        EHEMD1
     C                   MOVE      ELEXP1        EHWXP1
     C                   MOVE      ELEXM2        EHEXM2
     C                   MOVE      ELEMD2        EHEMD2
     C                   MOVE      ELEXP2        EHWXP2
     C                   MOVE      ELEXM3        EHEXM3
     C                   MOVE      ELEMD3        EHEMD3
     C                   MOVE      ELEXP3        EHWXP3
     C*** begin add ***  06/22/17  ***************************************************
     C                   Move      ELARP1        EHARP1
     C                   Move      ELARP2        EHARP2
     C                   Move      ELARP3        EHARP3
     C*** end add *****  06/22/17  ***************************************************
     C                   MOVE      ELILCD        EHILCD
     C                   MOVE      ELILEF        EHILEF
     C                   MOVE      ELILEX        EHILEX
     C                   MOVE      ELEIN1        EHWIN1
     C                   MOVE      ELINL2        EHI2CD
     C                   MOVE      ELI2EF        EHI2EF
     C                   MOVE      ELI2EX        EHI2EX
     C                   MOVE      ELEIN2        EHWIN2
     C                   MOVE      ELDCDE        EHDCDE
     C                   MOVE      ELDDCT        EHDDCT
     C                   MOVE      ELCXBL        EHCXBL
     C                   MOVE      ELDCEF        EHDCEF
     C                   MOVE      ELDCEX        EHDCEX
     C                   MOVE      ELEDCT        EHWDCT
     C                   MOVE      ELDCD2        EHDCD2
     C                   MOVE      ELDD2P        EHDD2P
     C                   MOVE      EL2DEF        EH2DEF
     C                   MOVE      EL2DEX        EH2DEX
     C                   MOVE      ELEDD2        EHWDD2
     C                   MOVE      ELASPM        EHASPM
     C                   MOVE      ELMCEF        EHMCEF
     C                   MOVE      ELMCEX        EHMCEX
     C                   MOVE      ELEDCM        EHWDCM
     C                   MOVE      ELASPS        EHASPS
     C                   MOVE      ELSCEF        EHSCEF
     C                   MOVE      ELSCEX        EHSCEX
     C                   MOVE      ELEDCS        EHWDCS
     C                   MOVE      ELRTCD        EHRTCD
     C                   MOVE      ELSTAT        EHSTAT
     C                   MOVE      ELUSER        EHUSER
     C                   MOVE      ELPGM         EHPGM
     C                   MOVE      ELTIME        EHTIME
     C                   MOVE      ELDT          EHDT
     C*
     C*** Begin Chg *** 04/07/17 *******************************
     C*****CLTID         IFEQ      '025'
     C**********         MOVE      ELSTFC        EHSTFC
     C**********         ELSE
     C**********         MOVE      ELDEV2        EHDEV2
     C**********         ENDIF                                                  CLTID = '025'
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stabilization Factor
     C                   when      cltID = '025'
     C                   Eval      EHstFc  = ELstFc
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   when      cltID = '136'
     C                   Eval      EHdev2F   = ELdev2F
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C                   Eval      EHdev2  = ELdev2
     C                   EndSl
     C                   Eval      EHdev2WBP = ELdev2BP
     C                   Eval      EHdev2WAP = ELdev2AP
     C*** End   Chg *** 04/07/17 *******************************
     C*** Begin Add *** 07/26/17 ******************************
     C                   Eval      EHtdf     = ELtdf
     C*** End   Add *** 07/26/17 ******************************
     C                   MOVE      ELEDV2        EHWDV2
     C                   MOVE      ELD2EF        EHD2EF
     C                   MOVE      ELD2EX        EHD2EX
     C                   MOVE      ELDBCR        EHDBCR
     C                   MOVE      ELCMUL        EHCMUL
     C                   MOVE      ELEDCR        EHWDCR
     C                   MOVE      ELEDC         EHWDC
     C                   MOVE      ELESBJ        EHWSBJ
     C                   MOVE      ELEMPM        EHWMPM
     C                   MOVE      ELEFRS        EHWFRS
     C                   MOVE      ELTXPR        EHTXPR
     C*
     C                   MOVE      ELAVCC        EHAVCC
     C                   MOVE      ELEIN2        EHWIN2
     C                   MOVE      ELDBCR        EHDBCR
     C                   MOVE      ELCMUL        EHCMUL
     C                   MOVE      ELSDEF        EHSDEF
     C                   MOVE      ELSDEX        EHSDEX
     C                   MOVE      ELEDCR        EHWDCR
     C                   MOVE      ELLFEF        EHLFEF
     C                   Z-ADD     ELEWAV        EHWWAV
     C                   Z-ADD     XXADLD        EHWADL
     C                   MOVE      ELNWRW        EHNWRW
     C                   Z-ADD     ELETER        EHWTER
     C*** BEG ADD *****  07/15/13  ************************************
     C                   Z-ADD     ELTERR        EHWTRR
     C*** END ADD *****  07/15/13  ************************************
     C                   Z-ADD     ELVLCA        EHWVLC
     C                   Z-ADD     ELEDTR        EHWDTR
     C*** BEG ADD *****  07/15/13  ************************************
     C                   Z-ADD     ELDTRR        EHWDRR
     C*** END ADD *****  07/15/13  ************************************
     C                   Z-ADD     ELHXMN        EHWHXM
     C                   Z-ADD     ELEARC        EHWARC
     C                   Z-ADD     ELARCP        EHARCP
     C                   Z-ADD     ELARCA        EHARCA
     C                   Z-ADD     ELAREF        EHAREF
     C                   Z-ADD     ELAREX        EHAREX
     C                   Z-ADD     ELRPE1        EHWRP1
     C                   Z-ADD     ELRPE2        EHWRP2
     C                   Z-ADD     ELRPE3        EHWRP3
      *** Beg Add ***** 04/19/16 ***********************************
     C                   Z-ADD     EL1AAM        EHW1AS
     C                   Z-ADD     EL2AAM        EHW2AS
     C                   Z-ADD     EL3AAM        EHW3AS
     C                   Z-ADD     EL4AAM        EHW4AS
     C                   Z-ADD     EL5AAM        EHW5AS
     C                   Z-ADD     EL6AAM        EHW6AS
     C                   Z-ADD     EL7AAM        EHW7AS
     C                   Z-ADD     ELSFAM        EHWSF
     C                   Z-ADD     ELMFAM        EHWMF
      *** End Add ***** 04/19/16 ***********************************
      *** Beg Add ***** 02/01/18 ***********************************
     c                   Eval      EHwpf = ELepf
      *** End Add ***** 02/01/18 ***********************************
      *** Beg Add ***** 04/27/16 ***********************************
      /Free
       EHexMd1Nf =  ELexMd1Nf;
       EHexMd2Nf =  ELexMd2Nf;
       EHexMd3Nf =  ELexMd3Nf;
      /End-free
      *** End Add ***** 04/27/16 ***********************************
      *** Beg Add ***** 02/12/19 ***********************************
      /Free
        EHnoexm1 =  ELnoexm1;
        //*** Begin Add *** 07/30/23 ***************************************
        EHnoexm2 =  ELnoexm2;
        EHnoexm3 =  ELnoexm3;
        //*** End   Add *** 07/30/23 ***************************************
      /End-free
      *** End Add ***** 02/12/19 ***********************************
     C*
     C                   MOVE      EHUSER        EHIUSR
     C                   MOVE      EHDT          EHIDT
     C                   WRITE     PEH06
     C*
     C*  CALL THE INFORCED PREMIUM SUBROUTINE TO UPDATE THE HISTORY RECORD
     C*
     C                   MOVE      ELDIV         Q1DIV             5
      *** Beg Chg *****  02/08/13  ************************************
     C*
     C*  IF AR UPDATABLE, AND A BILLING SCHEDULE CHANGE OCCURRED, CHECK
     C*  FOR THE FOLLOWING CASES THAT REQUIRE, A/R ADJUSTING ENTRIES.
     C*
     C**** CASE 1: S/R BILLING TO ESTIMATING BILLING.
     C****
     C**** F1UPAR        IFEQ      'Y'
     C**** XOBSCD        ANDNE     XNBSCD
     C****
     C**** CASE 1: S/R BILLING TO ESTIMATING BILLING.
     C**** XOSEFL        IFEQ      'S'
     C**** XNSEFL        ANDEQ     'E'
     C****               EXSR      ACASE1
     C****               ENDIF

      /free
       //*** beg chg ***** 11/18/13 ***********************************
       //***If F1upar = 'Y';
       //***If (xobscd <> xnbscd and xosefl = 'S' and xnsefl = 'E') Or
       //***   (xnsefl = 'E' and (xoesdp <> xnesdp or xoesd2 <> xnesd2) Or
       //***    xnsefl = 'S' and (xoab01 <> xnab01 or xoab02 <> xnab02));
       //***   Exsr ACASE1;
       //***Endif;
       //***Endif;
       Select;
       When F1upar <> 'Y';
       When xoSeFl = 'S' and xnSeFl = 'E' and xnEsDp = 0 and xnEsD2 = 0;
          Exsr aCase1;
       Endsl;
       //*** end chg ***** 11/18/13 ***********************************
      /end-free

      *** End Chg *****  02/08/13  ************************************
     C*** Beg DEL *****  02/08/13  ************************************
     C**********         ENDIF                                                  F1UPAR IFEQ
     C*** End DEL *****  02/08/13  ************************************
     C*
      *
      *  TRANSFER DEPOSIT IF SELF-REPORTING DIVISION IS CHANGING FROM
      *  SUMMARIZED TO NON-SUMMARIZED
      *
      *** Beg   Chg ***  02/08/13  ************************************
     C**** F1UPAR        IFEQ      'Y'
     C**** F1DPTR        ANDNE     *BLANKS
     C**** XOSEFL        ANDEQ     'S'
     C**** XNSEFL        ANDEQ     'S'
     C**** XOARSM        ANDEQ     'Y'
     C**** XNARSM        ANDEQ     ' '
     C**** ELDIV         ANDNE     XSDIV
     C****               EXSR      ACASE2
     C****               ENDIF

      /free
       //*** beg chg ***** 11/18/13 *******************************************
       //***If F1upar = 'Y' and F1dptr <> ' ' and (xosefl = 'S' or xosefl = 'E') and
       //*** xnsefl = 'S'  and xoarsm = 'Y' and xnarsm = ' ' and ELdiv <> xsdiv;
       //*** Exsr ACASE2;
       //***Endif;
       Select;
       When F1upar <> 'Y';
       When Eldiv = xsDiv;
       When xoArSm= 'Y' and xnArSm = *blank;
          Exsr aCase2;
       Endsl;
       //*** end chg ***** 11/18/13 *******************************************
      /end-free

      *** End   Chg ***  02/08/13  ************************************
      *
      *  CALL API TO TRANSFER CATEGORY CREDITS TO PREMIUM AND CALL CASH DISSOLVE
      *
      *** beg chg ***** 05/12/20 ******************************
     C***     F1UPAR        IFEQ      'Y'
      /Free
       If F1upar = 'Y' And isSelfReportExclusion = *Off;
      /End-Free
      *** end chg ***** 05/12/20 ******************************
     C                   CALL      'WA059R'
     C                   PARM                    ELRRN             9 0
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* GENERATE DARTS REMARK TO UNDERWRITER IF POLICY HAS RENEWED
     C*
     C     EMSTRP        IFEQ      'Y'
     C     EMUWTR        ANDNE     *BLANKS
     C     EXRVRN        ANDNE     *BLANKS
     C*
     C     XXFYR         ADD       1             XQFYR
     C     ELKYQ         CHAIN(N)  PEL02                              60
     C   60ELKYQ         CHAIN     PEL0Q                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   EXSR      UDART
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** Beg add ***** 12/11/19 **************************************
      /free
       // Process Update All Division changes for cx'd divisions
       Else;
       Chain eaKey pel02;
       If %Found;
          Chain eaKey pel01;
          If %Found;
             Delete pel01;
          Endif;
          Update pel02;
       Endif;
      /end-free
     C*** End add ***** 12/11/19 **************************************
     C                   ENDIF                                                  XXCNCL IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C  N67EMKY          READE     PEM01                                  60
     C   67              MOVE      *ON           *IN60
     C                   ENDDO                                                  *IN60 DOWEQ
       //*** Begin Add *** 05/16/21 ****************************************
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
             Clear chrInvType;
             If hldPoDt <= sysDate And
                DeleteUnpaidInvoices(EAco#:hldFnd:hldEmp#:hldDiv:hldFyr:
                hldInv#:chrInvtype);
                ProcessAdjustedInvoicing(inJson);
             Else;
                ThirdPartyInvoicing(inJson);
             EndIf;
             FinalizeThirdPartyInvoicing(EAco#:hldFnd);
          EndIf;

          // Send deleted unused invoice#
          ReportUnusedInvoices(EAco#:hldFnd:hldEmp#:*on);

       EndIf;
       //*** End   Add *** 05/16/21 ****************************************

      *** beg add ***** 07/02/13 **************************************
      /free
       If ValidDivClient = *on and indArSumry = *on;
          xxDivPrem = UpdateDividend(ELCo#:ELFnd:ELEmp#:xsDiv:ELFyr);
       Endif;
      /end-free
      *** end add ***** 07/02/13 **************************************

     C                   MOVE      '00000'       Q1DIV
     C                   MOVE      'E'           QQFAF
     C                   CALL      'SRINPRC'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    Q1DIV
     C                   PARM                    QQFYR
     C                   PARM                    QQEFDT
     C                   PARM                    QQFAF             1
     C                   PARM                    QQRNB             1
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C*
     C*  SUBROUTINE: ACASE1 - TRANSFER A/R FOR S/R TO ESTIMATING.
     C*
     C*****************************************************************
     C*
     C     ACASE1        BEGSR
     C*
     C*** begin add ***  06/11/18  *******************************
     C                   If        xxColt = 'Y'
     C                   Leavesr
     C                   Endif
     C*** begin add ***  06/11/18  *******************************
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
     C     F1PPTR        IFNE      *BLANKS
     C     F1DBTR        ANDNE     *BLANKS
     C     F1DPTR        ANDNE     *BLANKS
     C*
     C                   Z-ADD     *ZERO         X                 3 0
     C                   MOVE      'Y'           XXFRST            1
     C                   MOVE      *ZEROS        SVAMT
     C                   MOVE      *ZEROS        SVARTY
     C                   MOVE      *ZEROS        SVFYR
     C*
     C*  ACCUMULATE "H" & "I" AMOUNTS BY FUND YEAR
     C*
     C     ARKEY3        CHAIN     PEA02                              50
     C     *IN50         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTCTYP        IFEQ      'D'
     C*
     C     EAARTY        IFEQ      'F'
     C     F1TFTR        ANDNE     *BLANKS
     C     F1TQTR        ANDNE     *BLANKS
     C     F1TPTR        ANDNE     *BLANKS
     C     EAARTY        OREQ      'Q'
     C     F1TFTR        ANDNE     *BLANKS
     C     F1TQTR        ANDNE     *BLANKS
     C     F1TPTR        ANDNE     *BLANKS
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
     C     EAARTY        IFNE      SVARTY
     C     EAFYR         ORNE      SVFYR
     C     XXFRST        ANDEQ     'N'
     C     SVAMT         IFNE      *ZEROS
     C                   ADD       1             X
     C                   Z-ADD     SVFYR         FYR(X)
     C                   MOVEL     SVARTY        TY(X)
     C                   Z-ADD     SVAMT         AMT(X)
     C                   ENDIF                                                  SVAMT <> 0
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      EAFYR         SVFYR
     C                   MOVE      *ZEROS        SVAMT
     C                   ENDIF                                                  LEVEL BRK
     C                   ADD       EAAMT         SVAMT
     C     XXFRST        IFEQ      'Y'
     C                   Z-ADD     EAFYR         SVFYR
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      'N'           XXFRST
     C                   ENDIF                                                  XXFRST = 'Y'
     C                   ENDIF                                                  EAARTY IFEQ
      *
     C                   ELSE
      *
     C     EAFYR         IFEQ      XXFYR
      *
     C     EAARTY        IFNE      SVARTY
     C     XXFRST        OREQ      'N'
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
     C                   ENDIF                                                  EAARTY =
     C*                                                    'H' OR 'I'
     C     ARKEY3        READE     PEA02                                  50
     C                   ENDDO                                                  *IN50
     C*                                                    'H' OR 'I'
     C     SVAMT         IFNE      *ZEROS
     C                   ADD       1             X
     C                   Z-ADD     SVFYR         FYR(X)
     C                   MOVEL     SVARTY        TY(X)
     C                   Z-ADD     SVAMT         AMT(X)
     C                   ENDIF                                                  XXFRST = 'N'
     C*
     C                   Z-ADD     *ZEROS        XXAMT
     C                   Z-ADD     *ZEROS        XTAMT            11 2
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXEMP#        EAEMP#
     C                   Z-ADD     XXDIV         EADIV
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     SRDTO         EAPODT
     C                   MOVE      *BLANKS       EAEDIT
     C                   MOVE      *BLANKS       EASTAT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C     F1KEY         CHAIN     PF101                              60
     C*
     C*  CREATE REVERSING ENTRIES FOR "H" & "I" AMOUNTS BY FUND YEAR
     C*
     C                   DO        X             Y                 3 0
     C                   Z-ADD     FYR(Y)        EAFYR
     C                   SELECT
     C     TY(Y)         WHENEQ    'I'
     C                   MOVE      F1DPTR        EAARCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     F1DPTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     TTDESC        EADESC
     C     TY(Y)         WHENEQ    'H'
     C                   MOVE      F1DBTR        EAARCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     F1DBTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     TTDESC        EADESC
     C     TY(Y)         WHENEQ    'F'
     C                   MOVE      F1TFTR        EAARCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     F1TFTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     TTDESC        EADESC
     C     TY(Y)         WHENEQ    'Q'
     C                   MOVE      F1TQTR        EAARCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     F1TQTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     TTDESC        EADESC
     C                   ENDSL
     C     AMT(Y)        MULT      -1            EAAMT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C     TY(Y)         IFEQ      'I'
     C                   ADD       AMT(Y)        XXAMT
     C                   ENDIF
     C     TY(Y)         IFEQ      'Q'
     C                   ADD       AMT(Y)        XTAMT
     C                   ENDIF
     C                   ENDDO                                                  DO  X  Y
     C*
     C*  TRANSFER DEPOSITS PAID TO PREMIUM PAID
     C*
     C     XXAMT         IFNE      *ZEROS
     C                   MOVE      F1PPTR        EAARCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     F1PPTR        XXKEY
     C     F1KEY         CHAIN     PF101                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   MOVE      XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   ENDIF
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     SRDTO         EAPODT
     C                   Z-ADD     XXAMT         EAAMT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *ZEROS        EABUL1
     C                   MOVE      *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  *IN60
     C                   ENDIF                                                  XXAMT <> 0
     C*
     C*  TRANSFER TAX ON DEPOSIT PAID TO PREMIUM PAID
     C*
     C     XTAMT         IFNE      *ZEROS
     C                   MOVE      F1TPTR        EAARCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     F1TPTR        XXKEY
     C     F1KEY         CHAIN     PF101                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   MOVE      XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         EADIV
     C                   ENDIF
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   MOVEL     TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     SRDTO         EAPODT
     C                   Z-ADD     XTAMT         EAAMT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      *ZEROS        EABUL1
     C                   MOVE      *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF                                                  *IN60
     C                   ENDIF                                                  XTAMT <> 0
     C*
     C                   ENDIF                                                  F1PPTR IFNE
     C*
     C                   ENDSR                                                  ACASE1
     C*================================================================
     C*  SUBROUTINE: ACASE2 - TRANSFER A/R FOR SUMMARIZED S/R TO NON-
     C*  SUMMARIZED S/R
     C*================================================================
     C     ACASE2        BEGSR
     C*** begin add ***  06/11/18  *******************************
     C                   If        xxColt = 'Y'
     C                   Leavesr
     C                   Endif
     C*** begin add ***  06/11/18  *******************************
      *
     C                   CLEAR                   XXSRDA
     C                   CLEAR                   XXSRDT
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     XSDIV         XXDIV
     C*
     C*  ACCUMULATE "D/I" AND "D/Q" AMOUNTS FOR THE STATE REPORTING DIV
     C*
     C     ARKEY3        CHAIN     PEA02                              50
     C     *IN50         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTCTYP        IFEQ      'D'
     C*
     C                   SELECT
     C     EAARTY        WHENEQ    'Q'
     C     F1TQTR        ANDNE     *BLANKS
     C                   ADD       EAAMT         XXSRDT           15 2          STATE REP DEP TAX
     C     EAARTY        WHENEQ    'I'
     C     F1DPTR        ANDNE     *BLANKS
     C     EAARTY        OREQ      'T'
     C     F1DPTR        ANDNE     *BLANKS
     C                   ADD       EAAMT         XXSRDA           17 2          STATE REP DEP AMT
     C     EAARTY        WHENEQ    'R'
     C     F1DPTR        ANDNE     *BLANKS
     C                   SUB       EAAMT         XXSRDA           17 2          STATE REP DEP AMT
     C                   ENDSL
      *
     C                   ENDIF                                                  TTCTYP = 'D'
      *
     C     ARKEY3        READE     PEA02                                  50
     C                   ENDDO                                                  *IN50
      *
      *
      *  GET DEPOSIT ADJUSTMENTS FOR CURRENT DIV/YEAR
      *
     C                   Z-ADD     SVDIV         XXDIV
     C     ARKEY4        CHAIN     PEA02                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTCTYP        IFEQ      'D'
     C*
     C                   SELECT
     C     EAARTY        WHENEQ    'Q'
     C     F1TQTR        ANDNE     *BLANKS
     C                   ADD       EAAMT         XXCDDT           15 2          CUR DIV/YR DEP TAX
     C     EAARTY        WHENEQ    'I'
     C     F1DPTR        ANDNE     *BLANKS
     C     EAARTY        OREQ      'T'
     C     F1DPTR        ANDNE     *BLANKS
     C                   ADD       EAAMT         XXCDDA           17 2          CUR DIV/YR DEP AMT
     C     EAARTY        WHENEQ    'R'
     C     F1DPTR        ANDNE     *BLANKS
     C                   SUB       EAAMT         XXCDDA           17 2          STATE REP DEP AMT
     C                   ENDSL
      *
     C                   ENDIF                                                  TTCTYP = 'D'
      *
     C     ARKEY4        READE     PEA02                                  60
     C                   ENDDO
      *
      *  WRITE A/R ADJUSTMENTS
      *
     C                   Z-ADD     0             X
     C                   DO        4
     C                   ADD       1             X
      *
     C                   SELECT
     C     X             WHENEQ    1
      *
      *  DEPOSIT XFER FROM STATE REP DIV
      *
     C                   CLEAR                   PEA02
     C     XXSRDA        SUB       XXCDDA        XXWORK           17 2
     C     XXCDDA        IFNE      0
     C     F1DPTR        ANDNE     *BLANKS
      *
     C     XXWORK        IFLT      0
     C                   Z-SUB     XXSRDA        XXAAMT
     C                   ELSE
     C                   Z-SUB     XXCDDA        XXAAMT
     C                   ENDIF
      *
     C                   MOVEL     F1DPTR        XXAACD
     C                   EXSR      ARW
     C                   ENDIF
      *
     C     X             WHENEQ    2
      *
      *  DEPOSIT XFER TO CURRENT DIV
      *
     C                   CLEAR                   PEA02
     C     XXSRDA        SUB       XXCDDA        XXWORK           17 2
     C     XXCDDA        IFNE      0
     C     F1DPTR        ANDNE     *BLANKS
      *
     C     XXWORK        IFLT      0
     C                   Z-ADD     XXSRDA        XXAAMT
     C                   ELSE
     C                   Z-ADD     XXCDDA        XXAAMT
     C                   ENDIF
      *
     C                   MOVEL     F1DPTR        XXAACD
     C                   EXSR      ARW
     C                   ENDIF
      *
     C     X             WHENEQ    3
      *
      *  DEPOSIT TAX XFER FROM STATE REP DIV
      *
     C                   CLEAR                   PEA02
     C     XXSRDT        SUB       XXCDDT        XXWORK
     C     XXCDDT        IFNE      0
     C     F1TQTR        ANDNE     *BLANKS
      *
     C     XXWORK        IFLT      0
     C                   Z-SUB     XXSRDT        XXAAMT
     C                   ELSE
     C                   Z-SUB     XXCDDT        XXAAMT
     C                   ENDIF
      *
     C                   MOVEL     F1TQTR        XXAACD
     C                   EXSR      ARW
     C                   ENDIF
      *
     C     X             WHENEQ    4
      *
      *  DEPOSIT TAX XFER TO CURRENT DIV
      *
     C                   CLEAR                   PEA02
     C     XXSRDT        SUB       XXCDDT        XXWORK
     C     XXCDDT        IFNE      0
     C     F1TQTR        ANDNE     *BLANKS
      *
     C     XXWORK        IFLT      0
     C                   Z-ADD     XXSRDT        XXAAMT
     C                   ELSE
     C                   Z-ADD     XXCDDT        XXAAMT
     C                   ENDIF
      *
     C                   MOVEL     F1TQTR        EAARCD
     C                   EXSR      ARW
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   ENDDO                                                   D0 4
      *
     C                   ENDSR                                                  ACASE2
      *================================================================
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
     C                   Z-ADD     0             XAIFBL           11 2
     C                   Z-ADD     0             XADIFH           11 2
     C                   Z-ADD     0             XADIFT           11 2
     C                   Z-ADD     0             XBDIFH           11 2
     C                   Z-ADD     0             XBDIFT           11 2
     C                   Z-ADD     0             XADITR           11 2
     C                   Z-ADD     0             XADQTR           11 2
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   Z-ADD     0             IFIFEE
     C                   ENDIF
     C*
     C     EAKEY         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     '0'

      *** beg add ***** 07/02/13 **************************************
      * Skip dividend billings. The Dividend API will take care of them.
     C     EAARCD        IFEQ      F1DVBL
     C     EAKEY         READE     PEA02                                  60
     C                   ITER
     C                   ENDIF
      *** end add ***** 07/02/13 **************************************

     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* BILLED
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C*
     C     TTCTYP        IFNE      'C'
     C     EAPODT        IFLT      XXDATE
     C*
     C     TTCTYP        IFNE      'E'
     C                   ADD       EAAMT         XADIFF
     C                   ELSE
     C                   ADD       EAAMT         XADIFE
     C                   ENDIF                                                  TTCTYP IFNE
     C*
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* TAX
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     'T'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIFX
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* SAFETY FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     'S'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIFS
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* MEMBERSHIP FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     'M'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIFM
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* INSTALLMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     EAARCD        ANDEQ     IFIFBL
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XAIFBL
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* 1ST ASSESSMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '1'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIF1
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* 2ND ASSESSMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '2'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIF2
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* 3RD ASSESSMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '3'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIF3
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* 4TH ASSESSMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '4'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIF4
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* 5TH ASSESSMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '5'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIF5
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* 6TH ASSESSMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '6'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIF6
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* 7TH ASSESSMENT FEES
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '7'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIF7
     C                   ELSE
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* REMOVE DEPOSIT
     C*
     C     EAARTY        IFEQ      'H'
     C     TTCTYP        ANDEQ     'D'
     C*** begin add ***  06/11/18  *******************************
     C     xxColt        ANDNE     'Y'
     C*** end add *****  06/11/18  *******************************
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIFH
     C                   ELSE
     C                   ADD       EAAMT         XBDIFH
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C* REMOVE DEPOSIT TAX
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     'D'
     C*
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XADIFT
     C                   ELSE
     C                   ADD       EAAMT         XBDIFT
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF                                                  EAPODT IFLT
     C*
     C                   ENDIF                                                  EAARTY IFEQ
      *
      * ACCUMULATE DEPOSIT PAYMENTS FOR DIVISION
      *
     C     EAARTY        IFEQ      'I'
     C     TTCTYP        ANDEQ     'D'
     C*** begin add ***  06/11/18  *******************************
     C     xxColt        ANDNE     'Y'
     C*** end add *****  06/11/18  *******************************
     C     EAARTY        OREQ      'T'
     C     TTCTYP        ANDEQ     'D'
     C                   ADD       EAAMT         XADITR
     C                   ENDIF                                                  EAARTY IFEQ
      *
     C     EAARTY        IFEQ      'R'
     C     TTCTYP        ANDEQ     'D'
     C                   SUB       EAAMT         XADITR
     C                   ENDIF                                                  EAARTY IFEQ
      *
      * ACCUMULATE DEPOSIT TAX PAYMENTS FOR DIVISION
      *
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     'D'
     C                   ADD       EAAMT         XADQTR
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKEY         READE     PEA02                                  60
     C                   ENDDO                                                  *IN60 DOW='0'
     C*
     C* WRITE ADJUSTING RECORDS
     C*
     C     XADIFF        IFNE      *ZERO
     C*
     C*** begin add ***  09/21/15  *********************************************
     C                   Clear                   XXAACD
     C*** end add *****  09/21/15  *********************************************
     C     QQAR          IFNE      *BLANKS
     C*** begin add ***  09/21/15  *********************************************
     C                   MOVEL(P)  QQAR          XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTTYPE        ANDEQ     'A'
     C     TTCTYP        ANDEQ     'P'
     C*** end add *****  09/21/15  *********************************************
     C                   MOVE      QQAR          XXAACD            2
     C*** begin chg ***  09/21/15  *********************************************
     C*********          ELSE
     C                   Endif
     C*
     C                   Endif
     C*
     C                   If        XXAACD = *blanks
     C*** end chg *****  09/21/15  *********************************************
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'P'
     C     TTTYPE        ANDEQ     'A'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   ENDIF
     C*
     C                   Z-SUB     XADIFF        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFF IFNE
     C*
     C*** begin del ***  09/21/15  *********************************************
      *** Beg Add ***** 02/08/13 **********************************
     C*****XADIFF        IFNE      *ZERO
     C*****xxDepAdjCde   IFNE      *BLANKS
     C*****              MOVE      xxDepAdjCde   XXAACD            2
     C*****              ELSE
     C******LOVAL        SETLL     PTT01
     C*****              READ      PTT01                                  60
     C******IN60         DOWEQ     *OFF
     C*****TTCTYP        IFEQ      'P'
     C*****TTTYPE        ANDEQ     'A'
     C*****              MOVEL     TTKEY         XXAACD            2
     C*****              MOVE      *ON           *IN60
     C*****              ENDIF                                                  TTCTYP IFEQ
     C**N60              READ      PTT01                                  60
     C*****              ENDDO                                                  *IN60 DOWEQ
     C*****              ENDIF
     C*****
     C*****              Z-SUB     XADIFF        XXAAMT           11 2
     C*****              EXSR      ARW
     C*****              ENDIF                                                  XADIFF IFNE
      *** End Add ***** 02/08/13 **********************************
     C*** end del *****  09/21/15  *********************************************
     C     XADIFE        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'E'
     C     TTTYPE        ANDEQ     'B'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIFE        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFE IFNE
     C*
     C     XADIFX        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'T'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIFX        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFX IFNE
     C*
     C     XADIFS        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'S'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIFS        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFS IFNE
     C*
     C     XADIFM        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      'M'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIFM        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFM IFNE
     C     XAIFBL        IFNE      *ZERO
     C     IFIFBL        ANDNE     *BLANKS
     C*
     C                   MOVEL     IFIFBL        XXAACD
     C                   Z-SUB     XAIFBL        XXAAMT           11 2
     C                   EXSR      ARW
     C*
     C                   ENDIF                                                  XAIFBL IFNE
     C*
     C     XADIF1        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIF1        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF1 IFNE
     C*
     C     XADIF2        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIF2        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF2 IFNE
     C     XADIF3        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIF3        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF3 IFNE
     C*
     C     XADIF4        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIF4        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF4 IFNE
     C*
     C     XADIF5        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIF5        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF5 IFNE
     C     XADIF6        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIF6        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF6 IFNE
     C*
     C     XADIF7        IFNE      *ZERO
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     *OFF
     C     TTCTYP        IFEQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXAACD            2
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-SUB     XADIF7        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF7 IFNE
      *
      *  NEGATE PAST DEPOSIT AND DEPOSIT TAX A/R  BILLINGS
      *
     C     XADIFH        IFNE      *ZERO
     C     F1DBTR        ANDNE     *BLANKS
     C*** begin add ***  06/11/18  *******************************
     C     xxColt        ANDNE     'Y'
     C*** end add *****  06/11/18  *******************************
     C                   MOVEL     F1DBTR        XXAACD            2
     C                   Z-SUB     XADIFH        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFH IFNE
      *
     C     XADIFT        IFNE      *ZERO
     C     F1TFTR        ANDNE     *BLANKS
     C                   MOVEL     F1TFTR        XXAACD            2
     C                   Z-SUB     XADIFT        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFT IFNE
      *
      *  WRITE ACCUMULATED DEPOSIT AND DEPOSIT TAX A/R TO STATE REP DIV
      *
     C                   ADD       XBDIFH        XADIFH
     C                   ADD       XBDIFT        XADIFT
     C     XADIFH        IFNE      *ZERO
     C     F1DBTR        ANDNE     *BLANKS
     C*** begin add ***  06/11/18  *******************************
     C     xxColt        ANDNE     'Y'
     C*** end add *****  06/11/18  *******************************
     C                   MOVEL     F1DBTR        XXAACD            2
     C                   Z-ADD     XADIFH        XXAAMT           11 2
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     XSDIV         XXDIV
     C                   EXSR      ARW
     C                   Z-ADD     SVDIV         XXDIV
     C                   ENDIF                                                  XADIFH IFNE
      *
     C     XADIFT        IFNE      *ZERO
     C     F1TFTR        ANDNE     *BLANKS
     C                   MOVEL     F1TFTR        XXAACD            2
     C                   Z-ADD     XADIFT        XXAAMT           11 2
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     XSDIV         XXDIV
     C                   EXSR      ARW
     C                   Z-ADD     SVDIV         XXDIV
     C                   ENDIF                                                  XADIFT IFNE
      *
      *  TRANSFER DEPOSIT AND DEPOSIT TAX PAYMENTS TO STATE REP DIVISION
      *
     C     XADITR        IFNE      0
     C     F1DPTR        ANDNE     *BLANKS
     C*** begin add ***  06/11/18  *******************************
     C     xxColt        ANDNE     'Y'
     C*** end add *****  06/11/18  *******************************
     C                   MOVEL     F1DPTR        XXAACD            2
     C                   Z-SUB     XADITR        XXAAMT           11 2
     C                   EXSR      ARW
      *
     C                   Z-ADD     XADITR        XXAAMT           11 2
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     XSDIV         XXDIV
     C                   EXSR      ARW
     C                   Z-ADD     SVDIV         XXDIV
     C                   ENDIF
      *
     C     XADQTR        IFNE      0
     C     F1TQTR        ANDNE     *BLANKS
     C                   MOVEL     F1TQTR        XXAACD            2
     C                   Z-SUB     XADQTR        XXAAMT           11 2
     C                   EXSR      ARW
      *
     C                   Z-ADD     XADQTR        XXAAMT           11 2
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     XSDIV         XXDIV
     C                   EXSR      ARW
     C                   Z-ADD     SVDIV         XXDIV
     C                   ENDIF
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
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL(P)  TTDESC        EADESC
     C                   MOVEL     XXAACD        EAARCD
     C                   Z-ADD     XXAAMT        EAAMT
     C*
     C                   MOVE      XXDATE        EAPODT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   Z-ADD     XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             D2TIME
     C                   Z-ADD     D2TIME        EATIME
     C                   MOVE      ' '           EAEDIT
     C                   Z-ADD     0             EABUL1
     C                   Z-ADD     0             EACHK#
     C                   MOVE      *BLANK        EALATE
     C*
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR = *Off;
       If isSRAs And (EActyp = '1' Or EActyp = '2' Or
                      EActyp = '3' Or EActyp = '4' Or
                      EActyp = '5' Or EActyp = '6' Or
                      EActyp = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   EXSR      GETINV
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   ENDSR                                                  ADJAR
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    EACO#         #ICO#
     C     *LIKE         DEFINE    EAFND         #IFND
     C     *LIKE         DEFINE    EAEMP#        #IEMP#
     C     *LIKE         DEFINE    EADIV         #IDIV
     C     *LIKE         DEFINE    EAFYR         #IFYR
     C     *LIKE         DEFINE    EAPODT        #IDT
     C     *LIKE         DEFINE    EANINV        #IINV#
     C*
     C     CLTID         IFEQ      '025'
     C*
     C     EAARTY        IFEQ      'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      EACO#         #ICO#
     C                   PARM      EAFND         #IFND
     C                   PARM      EAEMP#        #IEMP#
     C                   PARM      EADIV         #IDIV
     C                   PARM      EAFYR         #IFYR
     C                   PARM      EAPODT        #IDT
     C     EANINV        PARM                    #IINV#
     C*
     C                   ENDIF
     C*
     C*** Begin Add *** 05/16/21 *******************************************
       Else;
          // Assign Invoice #
          If hldFnd <> EAfnd Or hldEmp# <> EAemp# Or hldDiv <> EAdiv Or
             hldPoDt <> EApoDt;
             // Send Invoice to Invoice Cloud
             If hldInv# <> 0 And ICGroupOpt(EACo#:hldFnd);
                // Is this a state reported division or non-summarized
                If GetStRptDv(EAco#:hldFnd:hldEmp#) = hldDiv Or
                   hldArSm <> 'Y';
                   inJson = '{"type":"billing",' +
                            '"invoice":"'  + %EditC(hldInv#:'X') + '",' +
                            '"company":"'  + %EditC(EAco#:'X')  + '",' +
                            '"group":"'    + %EditC(hldFnd:'X')  + '",' +
                            '"employer":"' + %EditC(hldEmp#:'X') + '",' +
                            '"division":"' + %EditC(hldDiv:'X')  + '",' +
                            '"active":"true"}';
                   If hldPoDt <= sysDate And
                      DeleteUnpaidInvoices(EAco#:hldFnd:hldEmp#:hldDiv:
                         hldFyr:hldInv#:chrInvtype);
                      ProcessAdjustedInvoicing(inJson);
                   Else;
                      ThirdPartyInvoicing(inJson);
                   EndIf;
                EndIf;
             EndIf;

             // Get invoice#
             // See if there are resused invoice numbers
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
             hldFyr  = EAfyr;
             hldEmp# = EAemp#;
             hldDiv  = EAdiv;
             hldPoDt = EApoDt;
             hldInv# = invoice#;
             hldArSm = ELarSm;
          EndIf;

          EAnInv = invoice#;
     C*** End   Add *** 05/16/21 *******************************************
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR

       //*** Begin Add *** 05/16/21 ***************************************
       // Save off invoice #'s for reuse
       If EAninv <> 0;
          decInvoiceDiv = (EAdiv * 10000000000) + EAninv;
          index = %LookUp(decInvoiceDiv:arrInvoice);
          If index = 0;
          //*** begin chg ***  09/26/22  *********************************************
          //*****   index = %LookUp(9999999999999:arrInvoice);
             index = %LookUp(999999999999999:arrInvoice);
          //*** end chg *****  09/26/22  *********************************************
             arrInvoice(index) = decInvoiceDiv;
             SortA arrInvoice;
          EndIf;
       EndIf;
       //*** End   Add *** 05/16/21 ***************************************
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
     C*
     C*****************************************************************
     C*
     C* CREATE DARTS REMARK TO UNDERWRITER.
     C*
     C     UDART         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C     EMUWTR        CHAIN     PUP02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     XXEMP#        QCEMP#
     C                   Z-ADD     XXDIV         QCDIV
     C                   MOVE      *BLANK        QCQTE
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  EXRVRN        Q#RMCD            5
     C                   MOVE      *BLANKS       Q#COMM
     C                   MOVEL(P)  RMK(1)        Q#COMM          256
     C     Q#COMM        CAT       Q1USER:1      Q#COMM
     C     Q#COMM        CAT       '.':0         Q#COMM
     C     Q#COMM        CAT       RMK(2):2      Q#COMM
     C     Q#COMM        CAT       RMK(3):1      Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVE(P)   *BLANKS       Q#DCID           12
     C                   MOVE(P)   *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  UPUSR         Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVE(P)   'Y'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVE(P)   'N'           Q#SBYL            1
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
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDSR                                                  UDART
     C*****************************************************************
     C*
     C*****************************************************************
     C*
      *================================================================
      * S/R to Delete Future Installment Fees
      *================================================================
     C     SRDLIF        BEGSR
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF
     C*
     C* ACCOUNT CURRENT DATE
     C*
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C                   Z-ADD     XXPODT        X1DT
     C*** begin del ***  10/18/24  ************************************
     C******             EXSR      SRFUTI
     C*** end del *****  10/18/24  ************************************
     C                   Z-ADD     X1DT          XXPODT
     C*
     C                   MOVE      'F'           XXARTY
     C     ARKEY         SETLL     PEA02
     C     ARKEY2        READE     PEA02                                  50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     EAARCD        IFEQ      IFIFBL
     C                   EXSR      HOLDAR
     C                   DELETE    PEA02
     C                   ENDIF
     C                   ENDIF
      *
     C     ARKEY2        READE     PEA02                                  50
     C                   ENDDO
      *
     C                   ENDSR
     C     SRFUTI        BEGSR
     C*
     C* MOVE POSTING DATE UP TO EFF DAY OF POLICY IF BILLING ON EFF DAY.
     C*
     C*** begin chg ***  10/18/24  ***************************************************
     C*****ELBLFL        IFEQ      'Y'
     C     xxBLFL        IFEQ      'Y'
     C*** end chg *****  10/18/24  ***************************************************
     C                   MOVE      ELPFDT        XXDDP             2 0
     C*
     C     XXDDP         IFGT      DD1
     C                   MOVE      XXDDP         X1DT
     C*
     C     MM1           IFEQ      2
     C     DD1           ANDGE     29
     C                   MOVE      29            X1DT
     C                   Z-ADD     X1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      28            X1DT
     C                   ENDIF                                                  SRERR IFEQ *ON
      *
     C                   ELSE
      *
     C     DD1           IFGT      30
      *
     C     MM1           IFEQ      4
     C     MM1           OREQ      6
     C     MM1           OREQ      9
     C     MM1           OREQ      11
     C                   MOVE      30            X1DT
     C                   ENDIF                                                  MM1 IFEQ 4
      *
     C                   ENDIF                                                  DD1 IFGT 30
      *
     C                   ENDIF                                                  MM1 IFEQ 2
     C*
     C                   ENDIF                                                  XXDDP IFGT
     C*
     C                   ENDIF                                                  ELBLFL IFEQ
     C*
     C                   ENDSR                                                  SRFUTI
     P*** Begin Add *** 05/16/21 **************************************
      *=============================================================
      * ProcessImages - Process Images
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
          //*****   division = %Dec(%Subst(%EditC(arrInvoice(index):'X'):1:3):3:0);
          //*****   invoice# = %Dec(%Subst(%EditC(arrInvoice(index):'X'):4:10):10:0);
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
             ThirdPartyInvoicing(inJson);
          EndIf;
       EndFor;

       Reset arrInvoice;

      /End-Free

     p ReportUnusedInvoices...
     p                 e
     P*** End   Add *** 05/16/21 **************************************
** RMK - DARTS REMARK
Please note that a Revised Bill was generated today for this policy by user
Corresponding changes may be necessary for the new policy year in either the
quote or live environment.
