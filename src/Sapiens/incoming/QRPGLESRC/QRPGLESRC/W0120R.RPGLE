     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0120R - A/P TRANSACTION MAINTENANCE                    *
     H****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW THE USER TO ENTER A   *
     H*                MANUAL PAYMENT, ENTER A REFUND, OR VOID, STOP,*
     H*                REVERSE OR ADJUST A PAYMENT.                  *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    MENU                                                      *
     H*    W0070R - WORK WITH CLAIMS                                 *
     H*  CALLS TO:                                                   *
     H*                                                              *
     H*  CREATED: 08/15/97  PROGRAMMER: C:33/30  PROJECT: WC659      *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/29/19 C:180 Project:  WC2604
     H* 07/10/19       Indemnity Data Call
     H*
     H* 05/30/18 C:053 Project : WC2515
     H*                Additional contact name change
     H*
     H* 09/21/16 C:180 Support: 43415                                 *
     H*                Enhance Days lost edit                         *
     H*                                                               *
     H* 07/21/15 C:308 Support: 34397                                 *
     H*                On Load event missed on profound screen        *
     H*                correct  it and recompile the program.         *
     H*
     H* 06/26/15 C:180 Support: 35247                                 *
     H*                Changed so that Override weeks/days windows    *
     H*                applies to Comp reserve type only.             *
     H*
     H* 06/15/15 C:308 Support PHW34660                               *
     H*                Use ##Seq#  for reading WDRPP  for correct     *
     H*                Adjustment posting.                            *
     H*                                                               *
     H* 03/12/15 C:180 Support PHW33610                               *
     H*                Fix weeks/days                                 *
     H*                                                               *
     H* 09/18/14 C:079 Support MC025 86719                            *
     H*                Use correct seq# when reading WDRPP            *
     H*                                                               *
     H* 08/19/14 C:290 SUPPORT: WC114 86405                           *
     H*                To ensure that no record get unnecessarily     *
     H*                locked, replaced chain with chain(n) if the    *
     H*                read is not immediately followed by a write.   *
     H*                                                               *
     H* 08/04/14 C:300 Project: WC2247                                *
     H*                1) Added hidden field on SCR02 for company no. *
     H*                   scCO#, Job name, User name, Job number      *
     H*                2) Added onblur & onload property on Payment   *
     H*                   Value 1 field to call AJAX program W0080R01 *
     H*                3) Do not calculate Weeks/Days if exclude flag *
     H*                   for Payment value 1 is 'Y'                  *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Add call to apportionment window for           *
     H*                cltid 129 only.                                *
     H*                                                               *
     H* 03/12/13 C:180 SUPPORT RED2506                                *
     H*                Add extra validation for net recovert PV1.     *
     H*                                                               *
     H* 03/11/13 C:180 SUPPORT RED2506                                *
     H*                Fix voucher flag validation for CSI.           *
     H*
     H* 02/19/13 C:180 SUPPORT WC102 81718                            *
     H*                Recompiled only - CPF4121 on W0120F            *
     H*                                                               *
     H* 02/05/13 C:062 Project: Redmine 2201                          *
     H*                Change check/voucher edit to include X.        *
     H*                                                               *
     H* 10/29/12 C:062 Project: Redmine 1568                          *
     H*                Apply error message to correct situation       *
     H*                                                               *
     H* 04/17/12 C:255 Project: WC2044                                *
     H*                Add following changes in program               *
     H*                1) Commented SRUEFT & SRREFT                   *
     H*                2) Use GetBankcode instead of WA023R1          *
     H*                3) Use GetBankAcc instead of WA023R            *
     H*                                                               *
     H* 12/15/11 C:062 SUPPORT WC114 78722                            *
     H*                Write to WDRPP when claim changes.             *
     H*                                                               *
     H* 08/29/11 C:277 SUPPORT 77320                                  *
     H*                Populate WDPAP initial fields                  *
     H*                                                               *
     H* 04/29/11 C:180 SUPPORT 74449
     H*                Fix wrong payee assignment.
     H*
     H* 04/27/11 C:062 SUPPORT 76629                                  *
     H*                Change full salary edit to only apply to       *
     H*                payments.                                      *
     H*                                                               *
     H* 04/26/11 C:136 SUPPORT: WA025 76707                           *
     H*                CORRECT EFT EDIT                               *
     H*                                                               *
     H* 04/06/11 C:255 SUPPORT 76236                                  *
     H*                Manual payment - Added EFT edit in display file*
     H*                Recompiled only.                               *
     H*                                                               *
     H* 12/04/10 C:183 SUPPORT 74375                                  *
     H*                Full salary date edit override to be applicable*
     H*                only when payment type = 'CO'                  *
     H*                                                               *
     H* 07/13/10 C:235 Project: 114WWK                                *
     H*                Add Recovery Option to Subro Drop Down         *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 02/22/10 C:232 Project: PRISS#9/WC1761                        *
     H*                Fixed the issue of project WC1761.             *
     H*                                                               *
     H* 12/29/09 C:234 SUPPORT WC071/71016                            *
     H*                NOT IMPLEMENTED AS EDIT 15 IS SKIPPED          *
     H*                                                               *
     H* 12/24/09 C:232 SUPPORT WC025/71123                            *
     H*                FIX THE ISSUE OF PROJECT WC1761. REMOVED THE   *
     H*                STATE 'TX' CONDITIONS FROM THE EDITS.          *
     H*                                                               *
     H* 11/19/09 C:191 PROJECT: WC1806 - PRISS                        *
     H*                NON EFT BANK CODE SHOULD BE RETRIEVED FOR      *
     H*                MANUAL CHECKS                                  *
     H*                                                               *
     H* 11/05/09 C:220  Project: PRISS#9/WC1761                       *
     H*                 Pop up msg screen for compensation pmt only   *
     H*                 with full salary continued flag.              *
     H*                                                               *
     H* 11/02/09 C:232 PROJECT: 139WMR                                *
     H*                OFAC Reporting-Add Pending Status Indicator.   *
     H*                                                               *
     H* 09/18/09 C:180 SUPPORT 69787                                  *
     H*                FIX MISSING MANUAL CHECKS ERROR
     H*
     H* 07/27/09 C:136 PROJECT: 114WUK (CLIENT SPECIFIC)              *
     H*                CHANGE PROCESSING FOR VOUCHER FLAG SO THAT     *
     H*                CSI CAN PROCESS XML FILE TRANSACTION TYPE      *
     H*                BANK ACCOUNTS                                  *
     H*                                                               *
     H* 05/04/09 C:062  Project: WC1807                               *
     H*                 IGNORE E TYPE WORK HISTORY ENTRIES            *
     H*                                                               *
     H* 12/25/08 C:197  Project: WC1761                               *
     H*                 CONTINUED SALARY FLAG EDIT FOR TX             *
     H*                                                               *
     H* 05/01/08 C:190 WC1647                                         *
     H*                Add parm #MCKL to call WA044R passing a        *
     H*                value 'Y'.                                     *
     H*                                                               *
     H* 04/01/08 C:191  Project: 139MIV                               *
     H*                 Handle new edit for negative total paid on    *
     H*                 the reserve. Change in DSPF only              *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H* 08/09/07 C:191 PROJECT: WC1453                                *
     H*                 EFT PAYMENTS TO CLAIMANTS/PROVIDERS           *
     H*
     H* 05/30/07 C:180 SUPPORT SEQ# 57735 - VERSIONING.               *
     H*          C:53  CHANGE ERROR INDICATOR ON SCR01 FROM 27 TO 19  *
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*
     H* 05/23/07 C:191 Support: 48404                                 *
     H*              - Add search functionality on Code               *
     H*              - Suppresed search on Year and Case fields when  *
     H*                called from w/w claims                         *
     H*                                                               *
     H* 04/26/07 C:132 Project 133WEB                                 *
     H*                Add a Security Leve for EXACT Dups and a       *
     H*                Separate one for POSSIBLE dups.                *
     H*                                                               *
     H* 02/23/07 C:185 Project: 114WMF                                *
     H*                Back To Base:114WBY Dated 06/11/01 & 05/29/01. *
     H*                             114WER Dated 06/12/03.            *
     H* 01/25/07 C:183 Project: 114WLY                                *
     H*                Added a PARM ##RTCD to accept the exact dupl-  *
     H*                cate indicator. If this returns as '1' from    *
     H*                WA019R then call SRCCSC to check whether the   *
     H*                user has got authority to enter the payment.   *
     H*                                                               *
     H*  09/21/06 C:62 PROJECT SUPP#54936                             *
     H*                ADD SEARCHES TO BR2 AND BR7 FIELDS SCR02       *
     H*                                                               *
     H* 11/01/06 C:53  PROJECT: 114WMF   114WIH 04/12/05  **REMOVED**
     H*                Back to base
     H*                                                               *
     H* 09/05/06 C:15  Project: 139WES                                *
     H*                Added code to remove the "remaining reserves"  *
     H*                function from all screens.                     *
     H*                                                               *
     H* 07/17/06 C:15  Support: WC089/53625                           *
     H*                Added code to load the V/S/Z reason fields into*
     H*                the reamrk fields in WDRPP.                    *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H* 02/06/06 C:168 Project: 132WBU                                *
     H*                Changed Error message id in display file from  *
     H*                WC30041 -> WC30068                             *
     H*                                                               *
     H* 01/11/06 C:53  Support: WC129/50722                           *
     H*                Added code to clear the out of country address *
     H*                screen fields to prevent invalid data from     *
     H*                being loaded into other WARPP records.         *
     H*                                                               *
     H* 10/03/05 C:15  Support seq# 47749                             *
     H*                Added code to write a WDRPP record for payment *
     H*                adjustments.                                   *
     H*                                                               *
     H* 09/23/05 C:62  Support seq# 40219                             *
     H*                Added edit.  Week/Days can't be > lost days.   *
     H*
     H* 09/21/05 C:53  Project: WC1309
     H*                Auto calculate the comp rate if amt 0
     H*                                                               *
     H* 08/17/05 C:53  Support seq# 44583                             *
     H*                Added edit.  Cannot issue a stop or void on a  *
     H*                cleared check.                                 *
     H*                                                               *
     H* 07/11/05 C:15  Support seq# 47977                             *
     H*                Added edit.  For adjustments, the rsv code of  *
     H*                selected transaction must match the rsv code of*
     H*                transaction code entered on the screen.        *
     H*                                                               *
     H* 04/07/05 C:156 PROJECT: 126WDK                                *
     H*                PROJECT: 126WCD                                *
     H*                Multiple Deductible discounts per Year.        *
     H*                                                               *
     H* 04/14/05 C:53  Support seq# 45880                             *
     H*                Add edit on reserve/PV1 code                   *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*
     H*  2/03/05 C:53  Project: 132WBQ
     H*                Add warning for Method of Settlement.
     H*                                                               *
     H* 07/01/04 C:53  Project: WC909                                 *
     H*                Additional changes to clear country fields     *
     H*                                                               *
     H* 04/06/04 C:53 Support: WC044/41791                            *
     H*               Lookup name if no rcd found in WDRPP            *
     H*                                                               *
     H* 10/20/03 C:53  Project: WC909                                 *
     H*                Added country info.                            *
     H*                                                               *
     H* 08/12/03 C:53 Support: WC102/38307                            *
     H*               Write check detail on the adjustment rcd if the *
     H*               FEIN or MOD changes.                            *
     H*                                                               *
     H* 06/15/03 C:15 Support: WC099/36792                            *
     H*               On SCR04, when the wks/dys are auto calculated, *
     H*               the cursor will now position to the PV1 field.  *
     H*                                                               *
     H* 05/15/03 C:15 Project: WC577                                  *
     H*               Added code to load the View Check File seq#.    *
     H*                                                               *
     H* 11/08/02 C:15  Support: WC114/34821                           *
     H*                Chgd manual pmt edits to always call WA023R1   *
     H*                to get the bank code.                          *
     H*                                                               *
     H* 10/10/02 C:62  Project: WC1078                                *
     H*                - Populate PAXFYR/PAXCAS                       *
     H*                - Call Fa736R to transfer eob                  *
     H*                                                               *
     H* 08/22/02 C:15  Support: WC094/33951                           *
     H*                Added code to prevent pgm from trying to update*
     H*                WDPAL without a successful CHAIN.              *
     H*                                                               *
     H* 08/20/02 C:53  Support: WC113/33909                           *
     H*                Changed DSPF CMD 3 to be specific not global   *
     H*                                                               *
     H* 06/26/02 C:15  Support: WC065/33192                           *
     H*                Added code to carry over certain fields when   *
     H*                an adjustment is made.                         *
     H*                                                               *
     H* 04/29/02 C:15  Project: WC835                                 *
     H*                Added code to clear remark fields for R/S/V/Z  *
     H*                type transactions.                             *
     H*                                                               *
     H* 02/27/02 C:15  Project: WC835                                 *
     H*                Added window for V/S/Z description and added   *
     H*                remark codes and description for recoveries.   *
     H*                                                               *
     H* 12/19/01 C:15  Support: WC048/29558                           *
     H*                Added code to prevent the acct# from being     *
     H*                blank on recoveries.                           *
     H*                                                               *
     H* 12/07/01 C:15  Support: WC048/29558                           *
     H*                Added code to load correct acct# and CKTP when *
     H*                an adjustment is made to an EOMB pmt.          *
     H*                                                               *
     H* 07/13/01 C:62  PROJECT: SUPPORT                               *
     H*                SWAP OUT CHAIN TO WDPAP IN SRVSZ WITH RRN CHAIN*
     H*                                                               *
     H* 04/25/01 C:15  Project: WC1020                                *
     H*                Added code to retrieve the bank code for mult  *
     H*                bank account processing.                       *
     H*                                                               *
     H* 03/13/01  C:35   PROJECT: SUPPORT GU114 #26708                *
     H*                  DO NOT CLEAR MESSAGE UNTIL DISPLAYING THE    *
     H*                  SECOND, THIRD OR FOURTH SCREEN.              *
     H*                                                               *
     H* 02/26/01 C:15  Support: WC065/26496                           *
     H*                When 'final pmt' flag is 'Y', create EDI A49   *
     H*                'S1' instead of 'FN'.                          *
     H*                                                               *
     H* 02/15/01 C:15  Project: WC577                                 *
     H*                Added code to subset the PV1 search to only    *
     H*                list codes which are valid for the reserve     *
     H*                code of the pmt.                               *
     H*                                                               *
     H* 05/31/00 C:53  Project: WC461                                 *
     H*                Add Daily batch lockout process                *
     H*                                                               *
     H* 05/30/00 C:53  Project: WC991                                 *
     H*                Add parms for WA013R to facilitate edit # 12   *
     H*                                                               *
     H* 05/24/00 C:15  Support: MC048/22221                           *
     H*               -Added recon# to SCR03 and SCR04.               *
     H*               -Added EOMB search.                             *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 04/25/00 C:15  Support: WC025/22607                           *
     H*                Added code to use the provider DBA flag to     *
     H*                deteremine which field is loaded into RPPNAM.  *
     H*                                                               *
     H* 04/04/00 C:15  Support: WC048/22331                           *
     H*                Changed code to prevent neg reserves on closed *
     H*                claims when an EOMB pmt is adjusted from one   *
     H*                claim to another.                              *
     H*                                                               *
     H* 11/09/99 C:58  Project:  SUPP WC090   Seq 20551
     H*                Standardize name arrangement all programs which
     H*                create payment request record.
     H*                                                               *
     H* 07/15/99 C:15  Project: WC577                                 *
     H*                Fixed the edit on the recovery screen which    *
     H*                prevents a deduct recovery code from being     *
     H*                used if the employer is 'Gross' reporting.     *
     H*                Changed this edit to an informational msg to   *
     H*                allow the clients to back out ded recoveries.  *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H*  06/14/99 C:118 SUPPORT:  WC033/13825 & WA031/18025           *
     H*                 Changed void process to load the current bank *
     H*                 acct# if the original pmt doesn't have an     *
     H*                 acct#.                                        *
     H*                                                               *
     H*  06/03/99 C:58 SUPP: WC051  12246                             *
     H*                Use WMWHL Work history for RTW date.           *
     H*                                                               *
     H*  06/01/99 C:112 Support Seq# 17362                            *
     H*           When a change is made to the Payment Value 1 Code   *
     H*           and the value is invalid the program returns to the *
     H*           start an turns on an indicator which also causes    *
     H*           another field to display. Reference to this field   *
     H*           screen 2 is removed in the display file. Added an   *
     H*           indicator value to validation of Pay Val 1 code     *
     H*                                                               *
     H*  05/11/99 C:15  Project: WC577                                *
     H*                 Fixed the reserve editing for S/V/Z.          *
     H*                                                               *
     H*  04/22/99 C:15  Project: WC577                                *
     H*                 Fixed the reserve editing for adjustments.    *
     H*                                                               *
     H*  03/25/99 C:118 PROJECT:  WC577                               *
     H*                 CORRECTED SCR03 SO THE DESCRIPTIONS ON VALUE  *
     H*                 1 AND VALUE 2   DID NOT CARRY OVER FROM       *
     H*                 PREVIOUS ENTRY. FORMATTED RECEIVED DATE TO    *
     H*                 8 DIGIT FORMAT.                               *
     H*                                                               *
     H*  03/15/99 C:33 SEQ#: WC025/16882                              *
     H*                DEFAULT AMOUNT FROM RECOVERY SEARCH TO SCR04.  *
     H*                                                               *
     H*  03/02/99 C:33 SEQ#: WC095/13271                              *
     H*                PASS THE RESERVE TYPE TO WA019R.               *
     H*                                                               *
     H*  09/25/98 C:112 Project:  WC577                               *
     H*           Allow 'F15=Remaining Reserve' function from         *
     H*           screen 2; Changes to W0120F                         *
     H*                                                               *
     H*           Changed mode for function F15 from Display 'D'      *
     H*           to Change 'C'.  DISABLED F6 FUNCTION IN SCREEN      *
     H*           PROGRAM (W0120R).                                   *
     H*                                                               *
     H* 09/06/98 C:02 PROJECT: 48WLU                                  *
     H*               MOVED CALL TO W0070RFL TO PRIOR TO DSIPLAYING   *
     H*               SCR02.  CALL WAS AFTER PAYMENT REQUEST WAS      *
     H*               ENTERED.                                        *
     H*                                                               *
     H* 09/08/98 C:15  Support: WC021/13581                           *
     H*                Chgd pgm to prevent a record lock on WDPAP.    *
     H*                                                               *
     H* 09/01/98 C:90 PROJECT: WC659                                  *
     H*               ALLOW BYPASS OF BLANK DATE OF DISABILTY FIELD.  *
     H*               WILL DISPLAY MESSAGE, THEN PASS TO NEXT SCREEN. *
     H*                                                               *
     H* 08/03/98 C:15 PROJECT: WC577                                  *
     H*               Chgd pgm to update the emp detail 'total claims *
     H*               paid' and 'outstanding reserves' correctly.     *
     H*                                                               *
     H* 07/29/98 C:78 PROJECT: WC577                                  *
     H*               Recovery Entry - select from rqst recovery,     *
     H*               PV2 desc shows, but code does not.              *
     H*                                                               *
     H* 07/17/98 C:15 Support: WC021/12597                            *
     H*               Added code to check for valid dates before      *
     H*               automatically calculating weeks and days on     *
     H*               comp payments.                                  *
     H*                                                               *
     H* 07/16/98 C:33 PROJECT: WC833                                  *
     H*               ALLOW FOR F4=ADDITIONAL ADDRESS SELECTION ON    *
     H*               SCR03 FOR OTHER THAN COMPENSATION PAYMENTS      *
     H*                                                               *
     H* 06/24/98 C:33 SEQ#: WC999/11838                               *
     H*              -POSITION CURSOR ON ACCT TYPE, INSTEAD OF CHECK#,*
     H*               WHEN ENTERING SCR04                             *
     H*              -CLEAR THE FEIN, NAME AND ADDRESS BEFORE SCR04.  *
     H*                                                               *
     H* 06/02/98 C:15 Support: WC033/                                 *
     H*               Fixed the condition in ASGNNM which determines  *
     H*               whether or not the entered payee matches the    *
     H*               claimant's SS#.                                 *
     H*                                                               *
     H* 05/28/98 C:15 Support: WC060/11877                            *
     H*               Fixed the correcting adjustment payee edits.    *
     H*                                                               *
     H* 05/20/98 C:15 Project: WC659                                  *
     H*               Fixed late reason code msg window processing.   *
     H*                                                               *
     H* 05/16/98 C:33 Support: WC060/11154                            *
     H*              -KEEP FROM CREATING DUPLICATE SEQ# WHEN MAKING AN*
     H*               ADJUSTMENT                                      *
     H*              -IF ENTER IS PRESSED FROM SCR05, RELOAD THE PRIOR*
     H*               SCREEN TO Q2RECF                                *
     H*              -BE SURE THE PROGRAM NAME IS DISPLAYED ON SCREENS*
     H*                                                               *
     H* 05/15/98 C:15 Support: WC060/11531                            *
     H*               Added FND to the KLIST for WMCXP so the correct *
     H*               record is retrieved.                            *
     H*                                                               *
     H* 05/14/98 C:90 Project: WC577                                  *
     H*               ADDED PAYEE SEARCH ON SCREEN 2                  *
     H*                                                               *
     H* 04/28/98 C:15 Project: WC577                                  *
     H*               Added 5 addl parms to SRCCSC.  These parms are  *
     H*               addl security types.                            *
     H*                                                               *
     H* 03/26/98 C:33 SEQ#: WC060/10842                               *
     H*               CLEAR SCR01 FIELDS IN BEGINING.                 *
     H*                                                               *
     H* 03/25/98 C:33 SEQ#: WC078/10822                               *
     H*               USE A LARGER RECIEVED DATE FOR ERROR CHECKING   *
     H*               ON SCR04 SO THAT ERROR #35 WILL NOT OCCUR.      *
     H*                                                               *
     H* 03/24/98 C:33 SEQ#: WC095/10663                               *
     H*               IF NO SELECTION MADE FROM ADDITIONAL ADDRESS    *
     H*               SEARCH, RETURN TO SCREEN WITHOUT ANY UPDATES.   *
     H*                                                               *
     H* 03/20/98 C:77 Project: WC999  SEQ# 10712                      *
     H*               Changed the display of the payee name from      *
     H*               LAST, FIRST to  FIRST LAST.                     *
     H*                                                               *
     H* 03/18/98 C:15 Project: WC577                                  *
     H*               Removed an edit on the refund process which     *
     H*               will allow the entry of a FEIN on comp refunds. *
     H*                                                               *
     H* 03/13/98 C:15 Project: WC577                                  *
     H*               Fixed the refund process.  The claim master file*
     H*               was not being updated if the claim was closed.  *
     H*                                                               *
     H* 02/23/98 C:85 Project: WC577                                  *
     H*               CHANGED THE FIELD MOVED TO C3SMCD               *
     H*               FROM *BLANKS TO CTSMCD.                         *
     H*                                                               *
     H* 02/02/98 C:85 Project: WC577                                  *
     H*               CHANGED SCREEN 4, FIELD SCVFLG TO ACCEPT THE    *
     H*               VALUES 'C' & 'V' (CHECK & VOUCHER).             *
     H*               INSTEAD OF 'Y' & 'N'.                           *
     H*               ALSO, CHANGED ERROR MESSAGE IN W0120F.         *
     H*                                                              *
     H* 11/06/97 C:15 Project: WC659                                 *
     H*               Added 'Voucher flag' to the Refund screen and  *
     H*               removed 'Bank Code'.  The voucher flag will be *
     H*               handled the same as in the manual pmt process. *
     H*                                                              *
     H* 10/22/97 C:30 Project: 60ANN                                 *
     H*          Add 'Voucher Y/N' flag.  Call WA023R to obtain      *
     H*          "current" Check or Voucher Bank Information record  *
     H*          prior to print process.                             *
     H*                                                              *
     H* 10/15/97 C:15 PROJECT: WC659                                 *
     H*               Changed code to comply with WC659 specs.       *
     H*                                                              *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H****************************************************************
     H*  INDICATOR SUMMARY                                           *
     H*                                                              *
     H*   GLOBAL                                                     *
     H*                                                              *
     H*   04 *ON  - Claim # input to pgm                             *
     H*   09 *ON  - @@MODE is 'D'                                    *
     H*   13 *ON  - Reserve Type is 'C'                              *
     H*   17 REDPE WMWHL
     H*   21 *ON  - RI 'EXCESS' in Claim headings                    *
     H*   22 *ON  - RI 'CONTROVERT' in Claim headings                *
     H*   23 *ON  - RI 'DENIED' in Claim headings                    *
     H*   24 *ON  - RI 'GROUP' provider entered in Claim headings    *
     H*   25 *ON  - RI 'ADDITIONAL ADDRESS' in Claim headings        *
     H*                                                              *
     H*   60      - General File Access                              *
     H*   96 *ON  - Claim # input to pgm                             *
     H*                                                              *
     H*   SCR01                                                      *
     H*                                                              *
     H*   24 *ON  - Payment Transaction code has Invalid Payment Type*
     H*   27 *ON  - Security Type and/or Level invalid               *
     H*   62 *OFF - Claim record found                               *
     H*   71 <-> 94 - Screen Edit Indicators                         *
     H*                                                              *
     H*   SCR02                                                      *
     H*                                                              *
     H*   71 <-> 94 - Screen Edit Indicators                         *
     H*                                                              *
     H*   SCR03                                                      *
     H*                                                              *
     H*   12 *ON  - Additional Address Found for Claim               *
     H*   38 *OFF - Late Reason code Found                           *
     H*   43 *ON  - RTTYPE is 'C'                                    *
     H*   42 *ON  - Zero < CMERTW < "TO" Service DT                  *
     H*   43 *ON  - Zero < CMRTW  < "TO" Service DT                  *
     H*   57 *ON  - Left Work history date > "FROM" service date
     H*   42 *ON or 43 *ON - Display Screen SCR05                    *
     H* ------------ EDTDLW sub results ---------------------------- *
     H* 1)61 *ON    No 'C' Res record found in Payment Detail,       *
     H*   69 *OFF   Payment Value 1 is Not 'II', and CMDTLW is zero  *
     H* 2)61 *OFF   No 'C' Res record found in Payment Detail,       *
     H*   69 *ON    Payment Value 1 is 'II', and CMMMI is zero       *
     H* 3)61 *OFF   'C' Payment Detail record Found                  *
     H*   69 *OFF                                                    *
     H* ------------ EDTDLW sub results ---------------------------- *
     H*                                                              *
     H*   64 *ON  - Late Reason flag LRREAS is 'Y'                   *
     H*   67 *ON  - RTTYPE is 'C'                                    *
     H*   69                                                         *
     H*   75 *ON  - Additional Address Found for Claim               *
     H*   71 <-> 94 - Screen Edit Indicators                         *
     H*                                                              *
     H*   SCR04                                                      *
     H*                                                              *
     H*   42 *ON or 43 *ON - Display Screen SCR05                    *
     H*   71 <-> 94 - Screen Edit Indicators                         *
     H****************************************************************
     FW0120F    CF   E             WORKSTN
     F                                     INFDS(FIDS01)
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FSMCOL     IF   E           K DISK
     FWDELL     UF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
      *_________________________________________________________________
     FWTABL     IF   E           K DISK
     FWMCML1    UF   E           K DISK
     F                                     INFDS(DBFDSC)
      *_________________________________________________________________
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWDPAP     UF   E             DISK
     F                                     RENAME(PPA01:PPA05)
     FWDPAL     UF A E           K DISK    infds(DBFDS2)
     F                                     RENAME(PPA01:PPA02)
     FWMRRL     UF A E           K DISK
     FWDF1L     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWARPL     IF   E           K DISK
     F*
     F*  Request Payment Detail file      KEY = CO#/FND/FYR/CASE/CHK#/TRDT
     FWDRPL     IF A E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWDRPL3    IF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
     FWDPALA    IF   E           K DISK
     F                                     RENAME(PPA01:PPA03)
     FWDACL     IF   E           K DISK
     FWDACP     IF   E             DISK
     F                                     RENAME(PAC01:PAC02)
     F                                     INFDS(DBFDSA)
      *_________________________________________________________________
      *_________________________________________________________________
     FSDCML     IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FWMCXL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F                                     RENAME(PRS01:PRS02)
     FWTDDL3    IF   E           K DISK
      *_________________________________________________________________
      *  Group Provider
     FWMNRL     IF   E           K DISK
      *________________________________________________________________
      *  DARTS Remark Code
     FSTCTL1    IF   E           K DISK
      *_________________________________________________________________
      *  Check# Control File
     FWSCKL     UF   E           K DISK
      *_________________________________________________________________
      *  System Control File
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
      *_________________________________________________________________
      *  EDI Control File
     F*_________________________________________________________________
     F*  Work History File
     FWMWHL     IF   E           K DISK
     FWTGTL     IF   E           K DISK
     FWMLLL     IF   E           K DISK
     FWMXRL     IF   E           K DISK
     FWMEFL7    UF   E           K DISK
     D I6              S              6    DIM(63)
     D MS              S             43    DIM(5) CTDATA PERRCD(1)
     D ER              S             78    DIM(7) CTDATA PERRCD(1)
     D MSG             S             46    DIM(8) CTDATA PERRCD(1)
     D RECD            S              2    DIM(999)
     D RETP            S              1    DIM(999)
     D REM             S             11  2 DIM(999)
     D SEQ             S              5  0 DIM(999)
     D FK1             S             79    DIM(8) CTDATA PERRCD(1)
     D FK3             S             79    DIM(4) CTDATA PERRCD(1)
     D MODE            S             10    DIM(2) CTDATA PERRCD(1)
     D FK2             S             79    DIM(1) CTDATA PERRCD(1)
     D FK4             S             79    DIM(1) CTDATA PERRCD(1)
     D FK1X            S             79    DIM(4) CTDATA PERRCD(1)
     D FK2X            S             79    DIM(1) CTDATA PERRCD(1)
     D FK3X            S             79    DIM(4) CTDATA PERRCD(1)
     D FK4X            S             79    DIM(1) CTDATA PERRCD(1)
     D VCHD            S             21    DIM(2) CTDATA PERRCD(1)
     D VCHE            S             40    DIM(1) CTDATA PERRCD(1)
     D fk1#            S              3  0
     D fk2#            S              3  0
     D fk3#            S              3  0
     D fk4#            S              3  0
      *** Begin Add 07/10/19 ***************************************************
     D/Copy sprmodule
     D/Copy wpridc
     D intRRN          S              9  0 INZ
      *** End   Add 07/10/19 ***************************************************
     D NF              C                   CONST('NOT FOUND')
     D*
     D OPTNM           C                   CONST('A/P Transactions')
     D OPTNM2          C                   CONST('Adjustment Entry')
     D OPTNM3          C                   CONST('Manual Payment Entry')
     D OPTNM4          C                   CONST('Recovery Entry')
     D CHGNUM          C                   CONST('WA003R')
     D*
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
      *** Begin Add 07/10/19 ***************************************************
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
      *** End   Add 07/10/19 ***************************************************
     D DBFDSC          DS
     D  DBRRNC               397    400B 0
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
      *
      *  SCCS02 - Field Definitions for Claim Heading Line 2
      *
     D                 DS
     D  SCCS02                 1     79
     D  S2SS1                 26     28
     D  S2SS2                 30     31
     D  S2SS3                 33     36
     D  S2NAME                39     60
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCCASE                10     16  0
     D                 DS
     D  XXCOMM                 1    160
     D  SCDS01                 1     40
     D  SCDS02                41     80
     D  SCDS03                81    120
     D  SCDS04               121    160
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
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
     D                 DS
     D  RC                     1     20
     D                                     DIM(10)
     D  B2RC01                 1      2
     D  B2RC02                 3      4
     D  B2RC03                 5      6
     D  B2RC04                 7      8
     D  B2RC05                 9     10
     D  B2RC06                11     12
     D  B2RC07                13     14
     D  B2RC08                15     16
     D  B2RC09                17     18
     D  B2RC10                19     20
     D                 DS
     D  MSGALL                 1     78
     D  MSG1                   1     42
     D  MSG2                  14     20
     D  MSG3                  44     56
     D  MSG4                  10     14
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D                 DS
     D  DR                     1      8
     D                                     DIM(4)
     D  FNMETR                 1      2
     D  FNCOTR                 3      4
     D  FNEXTR                 5      6
     D  FNLETR                 7      8
     D*
     D                 DS
     D  DATA                   1     75
     D  QQTRDT                 1      7  0
     D  QQFRDT                 8     14  0
     D  QQTODT                15     21  0
     D  QQTRCD                22     23
     D  QQRECD                24     25
     D  QQPV1                 26     27
     D  QQPV2                 28     29
     D  QQSEQ#                30     34  0
     D  QQEOB#                35     43
     D  Q1FEIN                44     52  0
     D  Q1MOD                 53     55  0
     D  Q1DSQ#                56     60  0
     D*
     D FL1             S             78    DIM(14)
     D FL2             S             78    DIM(14)
     D FL3             S             78    DIM(14)
     D FL4             S             78    DIM(14)
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *******Beg Add********04/17/12**********************************
     D/copy WPRBANK
     d ##Appl          s              2
     d ##Data          s             75
     d ##Acct          s             20
     d ##hdcy          s              1
     d ##chkq          s             10
     d ##cktp          s             10
     d ##cpyq          s             10
     d ##efdt          s              7
     d ##eftc          s              1
     d ##coid          s             20
     d ##FTYP          s             10
     d #Rtcd           s              1
     d ##srdt          s              8
      *******End Add********04/17/12**********************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Beg Add ***  11/25/13  ***********************************************
     d intCO#          s              3  0
     d intFND          s              3  0
     d intFYR          s              3  0
     d intCASE         s              7  0
     d intAMT          s              9  2
     d charBR2         s              2
     d intFRDT         s              8  0
     d intAAMT         s              9  2
     d charRTCD        s              1
     d flgPSIG         s              1
      *
      *** Beg Add ***  03/12/15  ***********************************************
     d jsWKS           S                   LIKE(PAWKS)
     d jsDYS           S                   LIKE(PADAYS)
      *** End Add ***  03/12/15  ***********************************************
     d ApprActPgm      pr                  ExtPgm('WA901R')
     d intCO#                         3  0
     d intFND                         3  0
     d intFYR                         3  0
     d intCASE                        7  0
     d intAMT                         9  2
     d charBR2                        2
     d intFRDT                        8  0
     d intAAMT                        9  2
     d charRTCD                       1
      *** End Add ***  11/25/13  ***********************************************
     I*
     IPPA03
     I              PAFYR                       P3FYR
     I              PACASE                      P3CASE
     I              PASEQ#                      P3SEQ#
     I              PACHK#                      P3CHK#
     I              PABASQ                      P3BASQ
     I              PATRDT                      P3TRDT
     I              PAPODT                      P3PODT
     I              PAFEIN                      P3FEIN
     I              PAMOD                       P3MOD
     I              PAAMT                       P3AMT
     I              PABR2                       P3BR2
     I              PABR7                       P3BR7
     I              PAFRDT                      P3FRDT
     I              PATODT                      P3TODT
     I              PAWKS                       P3WKS
     I              PADAYS                      P3DAYS
     I              PATRCD                      P3TRCD
     I              PATYPE                      P3TYPE
     I              PARECD                      P3RECD
     I              PARETP                      P3RETP
     I              PADESC                      P3DESC
     I              PAADST                      P3ADST
     I              PASTAT                      P3STAT
     I              PABUL1                      P3BUL1
     I              PARCVD                      P3RCVD
     I              PADUE                       P3DUE
     I              PAPGTP                      P3PGTP
     I              PAEOB#                      P3EOB#
     I              PACKTP                      P3CKTP
     I              PAACCT                      P3ACCT
     I              PAOFF#                      P3OFF#
     I              PALTCD                      P3LTCD
     I              PADPCD                      P3DPCD
     I              PADPCU                      P3DPCU
     I              PARQST                      P3RQST
     I              PAFNAL                      P3FNAL
     I              PARCDT                      P3RCDT
     I              PARADJ                      P3RADJ
     I              PAEOBS                      P3EOBS
     IPCM03
     I              CMKEY                       C3KEY
     I              CMCSID                      C3CSID
     I              CMCOMM                      C3COMM
     I              CMCDAT                      C3CDAT
     I              CMCODE                      C3CODE
     I              CMDCID                      C3DCID
     I              CMPNAM                      C3PNAM
     I              CMTRDT                      C3TRDT
     I              CMTRTM                      C3TRTM
     I              CMSEQ#                      C3SEQ#
     I              CMSECL                      C3SECL
     I              CMSBYL                      C3SBYL
     I              CMSTAT                      C3STAT
     I              CMUSER                      C3USER
     I              CMPGM                       C3PGM
     I              CMTIME                      C3TIME
     I              CMDT                        C3DT
     I              CMIUSR                      C3IUSR
     I              CMFOLD                      C3FOLD
     I              CMDCDT                      C3DCDT
     I              CMDIFL                      C3DIFL
     I              CMAPID                      C3APID
     I              CMIDOC                      C3IDOC
     I              CMIQUE                      C3IQUE
     I              CMIMGF                      C3IMGF
     I              CMIPTH                      C3IPTH
     I              CMPIDX                      C3PIDX
     I              CMQPTY                      C3QPTY
     I              CMDCTP                      C3DCTP
     I              CMIPGM                      C3IPGM
     I              CMSMCD                      C3SMCD
     I              CMFL01                      C3FL01
     I              CMFL02                      C3FL02
     I              CMFL03                      C3FL03
     I              CMFL04                      C3FL04
     I              CMFL05                      C3FL05
     I              CMRMCD                      C3RMCD
     I              CMIDT                       C3IDT
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     FNKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     TBKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  A/P TRANSACTION KEY LIST
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     APKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXSEQ
     C*
     C*  PAYMENT DETAIL FILE KEY LIST
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C                   KFLD                    XXSEQ
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*  CLAIM HISTORY FILE
     C*
     C     CHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXHIVL            7 0
     C*
     C     CHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*  CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKY          KLIST
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  REFUND KEY LIST
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    SCREQ#
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    SCFYR
     C*
     C*  BANK INFORMATION FILE
     C*
     C*  REQUEST PAYMENT FILE
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     RPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  AUXILIARY COMPENSATION PAYEE FILE
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C                   KFLD                    XXSEQ#
     C*
     C     ACKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*  KEY LIST FOR THE CLAIM CONTROL FILE
     C*
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C*  Request Payment Detail File
     C*
     C     RPDKY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    PACHK#
     C                   KFLD                    XXTRDT            7 0
     C*
     C     RPDKY2        KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    SCOFYR
     C                   KFLD                    SCOFCA
     C                   KFLD                    PACHK#
     C                   KFLD                    XXTRDT            7 0
     C*
     C     R3KY          KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C                   KFLD                    XXSEQ#
     C*
     C     R3KY2         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    SCOFYR
     C                   KFLD                    SCOFCA
     C                   KFLD                    XXSEQ#
     C*
     C*  PAYMENT DETAIL FILE - PARTIAL
     C*
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C*  KEY LIST FOR THE EDI CONTROL MAINTENANCE FILE
     C*
     C     EIKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
      *
      *   KEY LIST FOR THE REVIEW ID FILE
      *
     C     NRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  PAYROLL CLASSIFICATION FILE (FOR RATE SET CODE) WMRSP
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*
     C*  DEDUCTIBLE DISCOUT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDEF
     C*
     C     DDKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    XXDDCD
     C*
     C*  Key list for the Check # Tracking file
     C*
     C     CKKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXACCT
     C*
     C*  Key list for the System Control file
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXAPPL
     C*
     C*  Key list for the EDI Control file
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTRID
     C*  Key list for the Work History file
     C*
     C     WHKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXWKDT
     C*                                                                *
     C*
     C     WHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
      *
      *   KEY LIST FOR THE GENERIC TABLE FILE
      *
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
      *
     C     XRKY          KLIST
     C                   KFLD                    COCO#
     C                   KFLD                    Q1USER
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C     EFKY          KLIST
     C                   KFLD                    XXTYPE            1
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    XXEKY            30
     C                   KFLD                    XXDATE            7 0
     C*
     C     EFKY1         KLIST
     C                   KFLD                    XXTYPE            1
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    XXEKY
     C*
     C     A011PL        PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM      *BLANK        ##RC              2
     C                   PARM                    ##03              1
     C                   PARM                    ##05              1
     C                   PARM                    ##09              1
     C*
     C     A012PL        PLIST
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCTRCD
     C                   PARM                    SCFEIN
     C                   PARM                    SCMOD
     C                   PARM                    ##RC
     C                   PARM                    ##RECD            2
     C                   PARM                    ##RETP            1
     C                   PARM                    ##TYPE            1
     C*
     C     A014PL        PLIST
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM      'N'           ##GRP             1
     C                   PARM                    SCPODT
     C                   PARM                    SCRCVD
     C                   PARM                    SCFRDT
     C                   PARM                    SCTODT
     C                   PARM                    RTTYPE
     C                   PARM                    PCRECD
     C                   PARM                    SCBR2
     C                   PARM                    SCBR7
     C                   PARM      'PY'          ##STS             2
     C                   PARM                    SCLTCD
     C                   PARM                    SCDYWK
     C                   PARM                    ##RC
     C                   PARM                    SCEOB#
     C                   PARM                    SCAMT
     C                   PARM                    SCRCN
     C                   PARM                    ##EKEY           31
     C*
     C     PRCCPL        PLIST
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    PARECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    PAAMT
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     SRCHPL        PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##KEY            10
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
     C     CUSPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJRST            2
     C                   PARM                    XXTRID            3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
      *
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C     *LIKE         DEFINE    PAPGTP        SCPGTP
     C     *LIKE         DEFINE    PASEQ#        SVSEQ
     C     *LIKE         DEFINE    PASEQ#        SVSEQ#
     C     *LIKE         DEFINE    PASEQ#        SCSEQ
     C     *LIKE         DEFINE    PABASQ        SVBASQ
     C     *LIKE         DEFINE    PAEOB#        XXEOB#
     C     *LIKE         DEFINE    PASEQ#        XXSEQ
     C     *LIKE         DEFINE    ACSEQ#        XXSEQ#
     C     *LIKE         DEFINE    CMDTLW        SVDTLW
     C     *LIKE         DEFINE    CMMMI         SVMMI
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    SCAMT         SVAMT
     C     *LIKE         DEFINE    SCFRDT        SVFRDT
     C     *LIKE         DEFINE    SCTODT        SVTODT
     C     *LIKE         DEFINE    SCFRDT        SVFDT
     C     *LIKE         DEFINE    SCTODT        SVTDT
     C     *LIKE         DEFINE    SCFRDT        SVFDT2
     C     *LIKE         DEFINE    SCTODT        SVTDT2
     C     *LIKE         DEFINE    SCCOWK        SVCOWK
     C     *LIKE         DEFINE    SCCODY        SVCODY
     C     *LIKE         DEFINE    SCWKS         SVWKS
     C     *LIKE         DEFINE    SCDYWK        SVDYWK
     C     *LIKE         DEFINE    SCTODT        ##DUE
     C     *LIKE         DEFINE    SCTODT        XXFROM
     C     *LIKE         DEFINE    SCTODT        XXTO
     C     *LIKE         DEFINE    SCFEIN        SVFEIN
     C     *LIKE         DEFINE    SCMOD         SVMOD
     C     *LIKE         DEFINE    SCWKS         XXWK
     C     *LIKE         DEFINE    SCDYWK        XXDY
     C     *LIKE         DEFINE    PATRCD        SVTRCD
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PASEQ#        KKSEQ
     C     *LIKE         DEFINE    PACKTP        SVACCD
     C     *LIKE         DEFINE    PAACCT        SVACCT
     C     *LIKE         DEFINE    PALTCD        SVLTCD
     C     *LIKE         DEFINE    PAFNAL        SVFNAL
     C     *LIKE         DEFINE    CKACCT        XXACCT
     C     *LIKE         DEFINE    CMACDT        XXTEST
     C     *LIKE         DEFINE    CMACDT        XXDAT1
     C     *LIKE         DEFINE    CMACDT        XXDAT2
     C     *LIKE         DEFINE    CMACDT        XTDATE
     C     *LIKE         DEFINE    SCCOFE        XNSS#
     C     *LIKE         DEFINE    ELOUTS        SVOUTS
     C     *LIKE         DEFINE    Q1PGM         SVPGM
     C     *LIKE         DEFINE    SCTRCD        SVTRC
     C     *LIKE         DEFINE    WHDATE        XXFRDT
     C     *LIKE         DEFINE    WHDATE        XXWKDT
     C     *LIKE         DEFINE    CMRTW         XXRTW
     C     *LIKE         DEFINE    *IN60         WHFLAG
     C                   MOVE      *OFF          WHFLAG
     C     *LIKE         DEFINE    PAEOB#        SVEOB#
     C     *LIKE         DEFINE    PAEOBS        SVEOBS
     C     *LIKE         DEFINE    PADPCD        SVDPCD
     C     *LIKE         DEFINE    PADPCU        SVDPCU
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C*
     C* Retrieve client ID.
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       ##CLT#            3
     C                   SELECT
     C     ##CLT#        WHENEQ    '114'
     C                   MOVE      VCHD(1)       SCVCHD
     C                   MOVE      VCHE(1)       SCVCHE
     C                   OTHER
     C                   MOVE      VCHD(2)       SCVCHD
     C                   ENDSL
     C*
      /Copy SINDIC01
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANKS       ##WCIF            1
     C                   PARM      *BLANKS       ##MCIF            1
     C                   PARM      *BLANKS       ##WCIN            1
     C                   PARM      *BLANKS       ##AR              1
     C                   PARM      *BLANKS       ##NTWK            1
     C                   PARM      *BLANKS       ##UR              1
     C                   PARM      *BLANKS       ##MED             1
     C                   PARM      *BLANKS       ##EDI             1
     C                   PARM      *BLANKS       ##WCDK            1
     C*  Get the system control records
     C*
     C                   MOVE      'WC'          XXAPPL
     C     SYKY          CHAIN     SWC01                              60
     C*
     C                   MOVEL(P)  Q1PGM         SVPGM
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C                   MOVEL(P)  SVPGM         Q1PGM
     C*
     C                   CLEAR                   SCFYR
     C                   CLEAR                   SCCASE
     C                   CLEAR                   SCCSNM
     C                   CLEAR                   SCTRCD
     C*
     C                   Z-ADD     *ZERO         XXCO#
     C                   Z-ADD     *ZERO         XXFND
     C                   MOVE      'WC'          XXAPPL            2
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C                   Z-ADD     UPFYR         SCYR
     C                   Z-ADD     UPFYR         SCFYR
     C                   ENDIF
     C*
     C*  IF CALLED WITH A CLAIM GET THE CLAIM INFO
     C*
     C                   MOVE      *OFF          *IN96
     C*
     C     Q1PRMS        IFGE      4
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN04
     C                   MOVE      *ON           *IN96
     C                   Z-ADD     CMCO#         XXCO#
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
      *
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C                   MOVEL     S2NAME        SCCSNM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  GET THE COMPANY AND FUND RECORDS
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              60
     C                   Z-ADD     XXFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET THE FUND AUXILIARY INFO
     C*
     C     FNKY          CHAIN     PF201                              60
     C*
     C*  GET THE CLAIM/DARTS CONTROL FILE RECORD
     C*
     C     CXKY          CHAIN     PCX01                              60
     C*
     C                   MOVE      *BLANKS       SCPGTP
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   MOVE      *OFF          *IN09
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVE      '1'           QQCALL
     C                   MOVE      *ON           *IN09
     C                   ELSE
     C                   MOVE      ' '           QQCALL
     C                   ENDIF
      *
     C                   MOVE      *BLANK        SVVFLG            1
     C*
     C                   MOVE      *BLANK        SCVFLG
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       XXDSQ#            5 0
     C                   MOVE      *BLANKS       SVDSQ#            5 0
      *
      *  Assign 2 lines of Function Key legend for SCR01
      *
     C     *IN04         IFEQ      *ON
     C                   Z-ADD     1             CK                3 0
     C                   ELSE
     C     ##CLT#        IFEQ      '139'
     C                   Z-ADD     3             CK
     C                   ELSE
     C                   Z-ADD     5             CK
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      FK1(CK)       SCFK1A
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1A
     C                   ENDIF
     C                   ADD       1             CK
     C                   MOVE      FK1(CK)       SCFK1B
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1B
     C                   ENDIF
      *
      /Free
       // Load the function keys for web display;
       Exsr LoadFKeys;
      /End-free
     C*
     C*  ONE TIME READ TO DARTS TO USE IT LIKE INPUT FILE
     C*  AND RENAME THE FORMAT NAME AND FIELDS NAME.
     C*
     C                   READ      PCM03                                  60
     C*
     C*  DISPLAY ALL FORMATS
     C                   MOVE      *BLANKS       SCDBR7
     C                   MOVE      *BLANKS       SCDBR2
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   EXSR      SCRNNM
      /Free
       //******* Beg Add *** 08/04/14 **************************************
       //* Populate company number on SCR03 screen
       scCo# = kkCo#;
       //******* End Add *** 08/04/14 **************************************
      /End-free
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl1
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR01
     C                   ENDIF
     C*
     C*  CLEAR THE MESSAGES
     C*
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   CLEAR                   SCDE78
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK2X(1)       SCFK2
     C                   ELSE
     C                   MOVE(P)   FK2(1)        SCFK2
     C                   ENDIF
     C                   MOVEL(P)  OPTNM2        SRDFLD
     C                   EXSR      SCRNNM
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl2
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR02
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   CLEAR                   SCDE78
     C*
     C                   MOVEL(P)  OPTNM3        SRDFLD
     C                   EXSR      SCRNNM
     C*
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl3
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR03
     C                   ENDIF
     C*
     C     Q2RECF        IFEQ      'SCR04'
     C                   CLEAR                   SCDE78
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK4X(1)       SCFK4
     C                   ELSE
     C                   MOVE(P)   FK4(1)        SCFK4
     C                   ENDIF
     C                   MOVEL(P)  OPTNM4        SRDFLD
     C                   EXSR      SCRNNM
      /Free
       // Function keys will be cleared for web display;
       Exsr ClearFKeys;
      /End-free
      /If Defined (WEB)
     C                   Eval      AV_KEYS_DS.ARRAY = fl4
     C                   Eval      AV_KEYS = AV_KEYS_DS.KEYS
      /Endif
     C                   EXFMT     SCR04
     C                   ENDIF
     C*
      *===============================================================
      *  PROCESS ALL COMMAND KEYS
      *===============================================================
     C*
     C     *IN11         IFEQ      *ON
      *
      * =======================================================
      *   SCR01 - If F4,F7, F8, F9, F11, F15, or F16, then
      *           use screen fields and attempt claim master access
      * =======================================================
      *
     C     Q2RECF        IFEQ      'SCR01'
     C     *INKD         IFEQ      *ON
     C     *INKG         OREQ      *ON
     C     *INKH         OREQ      *ON
     C     *INKI         OREQ      *ON
     C     *INKK         OREQ      *ON
     C     *INKP         OREQ      *ON
     C     *INKQ         OREQ      *ON
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
      *
     C     CMKY          CHAIN(N)  PCM01                              62
      *
     C     *IN62         IFEQ      *ON
     C                   MOVEA     I6(6)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
      *
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVEA     I6(6)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN19
     C     XRKY          CHAIN     PXR01                              60
     C     *IN60         IFEQ      *OFF
     C     XRSTAT        ANDEQ     'A'
     C     *INKC         ANDNE     *ON
     C                   MOVE      *ON           *IN19
     C                   GOTO      STRT
     C                   ENDIF
      *
     C* =======================================================
     C*   F3 = EXIT
     C* =======================================================
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* =======================================================
     C*   F12 = CANCEL
     C* =======================================================
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C*
     C                   OTHER
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C* =======================================================
     C*  F4=ADDITIONAL NAME/ADDRESS SEARCH
     C* =======================================================
     C*
     C     *INKD         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C*
     C*
     C     ACKY1         CHAIN     PAC01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'W0573R'
     C                   PARM      '2'           XCALL             1
     C                   PARM      'Y'           XXPMTF            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      DBRRNC        XXRRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C     XXRRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     XXRRN         IFNE      *ZEROS
     C     XXRRN         CHAIN     PAC02                              60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ACPNAM        SCPNAM
     C*** Beg Add *** 05/30/18 *******************************************
     C     ACFNAM        IFNE      *BLANK
     C     ACFNAM        CAT(P)    ACPNAM:1      SCPNAM
     C                   ENDIF
     C*** Beg Add *** 05/30/18 *******************************************
     C                   MOVEL     ACADR1        SCADR1
     C                   MOVEL     ACADR2        SCADR2
     C                   MOVEL     ACCTY         SCCTY
     C                   MOVEL     ACST          SCST
     C                   MOVEL     ACZIP         SCZP1
     C                   MOVE      ACZIP         SCZIP2
     C                   MOVE      ACSEQ#        XXDSQ#
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C     ACCTRY        IFNE      *BLANK
     C     ACCTRY        ANDNE     'USA'
     C                   MOVEL(P)  ACADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  ACCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  ACCTRY        SCCTRY
     C                   MOVE      *ON           *IN05
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C                   ENDIF
     C*
     C                   MOVE      '1'           *IN75
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN75
     C                   MOVE      *ON           *IN76
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
      *
      * =======================================================
      *  F7 - Financial summary
      * =======================================================
      *
     C     *INKG         IFEQ      '1'
      *
     C                   MOVE      Q2RECF        SVRECF           10
      *
     C                   CALL      'WA009R'
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      *BLANK        ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
      *
      * If F3=Exit is hit, Return to Menu.
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * =======================================================
      *  F8 - Accident info
      * =======================================================
      *
     C     *INKH         IFEQ      '1'
      *
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R08'                           99
     C     *INKC         PARM      *ZERO         ##KC
     C     *INKL         PARM      *ZERO         ##KL
     C                   PARM      'C'           ##MODE
     C                   PARM      DBRRNC        ##RRN
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * =======================================================
      *  F9 - Management info
      * =======================================================
      *
     C     *INKI         IFEQ      '1'
      *
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R09'
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRNC        ##RRN
      *
      * If F3=Exit is hit, Return to Menu.
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
      *
      * =======================================================
      *  F16  - Payment Transaction Search (subset USRPRF ID)
      * =======================================================
      *
     C     *INKQ         IFEQ      *ON
      *
     C                   MOVE      Q2RECF        SVRECF
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   CALL      'W0581R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM      *BLANK        ##CALL            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM      *ZERO         ##FYR             3
     C                   PARM      *ZERO         ##CASE            7
     C                   PARM      *BLANK        ##SPS            10
     C                   PARM      *ZERO         ##SFDT            8
     C                   PARM      *ZERO         ##STDT            8
     C                   PARM      *BLANK        ##SPC            10
     C                   PARM      *BLANK        ##STT             5
     C                   PARM      *BLANK        ##SPV1           10
     C                   PARM      *BLANK        ##SRC            10
     C                   PARM      *ZERO         ##SFEI            9
     C                   PARM      *ZERO         ##SCHK            7
     C                   PARM      *ZERO         ##SEOB            9
     C                   PARM      UPUSR         ##SIRQ           10
     C                   PARM      *BLANK        ##SGRP            1
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
      *
     C* =======================================================
     C*  F10 - SEARCH                          KJDO
     C* =======================================================
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*  Run dates through SRCVTDAT with DATCHK for editing on display
     C*
     C     SCFRDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVEA     '010011'      *IN(71)
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     SCTODT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVEA     '011100'      *IN(71)
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C     SCRCVD        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     SRDTO         SCRCVD
     C                   ENDIF
     C                   ENDIF
     C*
      *__________________________________________________________________
      *
     C                   SELECT
      *__________________________________________________________________
     C     HFLD          WHENEQ    'SCCASE'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCOCA'
     C     HFLD          OREQ      'SCCOYR'
      *
     C                   MOVEL(P)  Q2RECF        SVRECF           10
      *
     C     HFLD          IFEQ      'SCCASE'
     C     HFLD          OREQ      'SCFYR'
     C*
     C     *IN04         IFEQ      *ON
     C                   MOVEA     I6(63)        CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   Z-ADD     SCFYR         ##YEAR
     C*
     C                   ENDIF
     C     *IN04         IFEQ      *ON
     C     HFLD          IFEQ      'SCCOCA'
     C     HFLD          OREQ      'SCCOYR'
     C                   MOVE      *ON           *IN77
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   CALL      'W0070R'                             60
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC
     C                   PARM      *INKL         ##KL
     C                   PARM      *BLANK        ##MODE
     C                   PARM      *ZERO         ##RRN
     C                   PARM      'CN'          ##SORT            2
     C                   PARM      DBRRN1        ##RRN1            9 0
     C                   PARM                    ##YEAR            3 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##RRN         IFGT      *ZERO
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCASE'
     C     HFLD          OREQ      'SCFYR'
     C                   EXSR      ASGNCS
     C                   MOVE      *ON           *IN26
     C*
     C**
     C     HFLD          WHENEQ    'SCCOCA'
     C     HFLD          OREQ      'SCCOYR'
     C*
     C     ##RRN         CHAIN     WMCMP                              62
     C*
     C                   Z-ADD     CMFYR         SCCOYR
     C                   Z-ADD     CMCASE        SCCOCA
     C                   MOVE      *ON           *IN46
     C*
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCOCA'
     C     HFLD          OREQ      'SCCOYR'
     C                   MOVE      *ON           *IN45
     C*
     C                   ENDSL
     C*
     C                   ENDIF
      *
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
      *__________________________________________________________________
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C     HFLD          OREQ      'SCCOFE'
     C     HFLD          OREQ      'SCCOMO'
     C                   MOVEL(P)  Q2RECF        SVRECF           10
      *
     C     HFLD          IFEQ      'SCCOFE'
     C     HFLD          OREQ      'SCCOMO'
     C                   MOVE      SCCOFE        QQFEIN
     C                   MOVE      SCCOMO        QQMOD
     C                   ELSE
     C                   MOVE      SCFEIN        QQFEIN
     C                   MOVE      SCMOD         QQMOD
     C                   ENDIF
     C                   MOVE      '2'           QQCALL
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM      *BLANKS       QQRTCD            1
      *
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        IFEQ      'B'
     C     QQRTCD        OREQ      *BLANKS
     C     HFLD          CABEQ     'SCCOFE'      STRT                     46
     C     HFLD          CABEQ     'SCCOMO'      STRT                     46
     C                   END
      *
     C     QQRTCD        IFEQ      '*'
     C     HFLD          IFEQ      'SCCOFE'
     C     HFLD          OREQ      'SCCOMO'
     C                   MOVE      QQFEIN        SCCOFE
     C                   MOVE      QQMOD         SCCOMO
     C                   MOVE      *ON           *IN47
     C                   ELSE
     C                   MOVE      QQFEIN        SCFEIN
     C                   MOVE      QQMOD         SCMOD
     C                   ENDIF
     C                   EXSR      ASGNNM
     C                   MOVEA     '1111'        *IN(82)
     C                   ENDIF
      *
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
     C     HFLD          WHENEQ    'SCCODC'
     C*
     C                   CALL      'W2141R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##KEY
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCCODC
     C                   MOVE      *ON           *IN45
     C                   ELSE
     C                   MOVE      *ON           *IN44
     C                   ENDIF
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*
     C* PAYMENT VALUE 1 SEARCH
     C*
     C     HFLD          WHENEQ    'SCCOB2'
     C*
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C                   PARM      *BLANKS       ##RECD
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCCOB2
     C                   MOVE      *ON           *IN49
     C                   ELSE
     C                   MOVE      *ON           *IN48
     C                   ENDIF
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C* PAYMENT VALUE 2 SEARCH
     C*
     C     HFLD          WHENEQ    'SCCOB7'
     C*
     C                   CALL      'W2151R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM      *BLANKS       ##KEY            10
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCCOB7
     C                   MOVE      *ON           *IN50
     C                   ELSE
     C                   MOVE      *ON           *IN49
     C                   ENDIF
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
      *__________________________________________________________________
     C     HFLD          WHENEQ    'SCTRCD'
     C                   MOVEL(P)  Q2RECF        SVRECF           10
      *
     C                   MOVE      *BLANK        SCKEY            10
     C                   CALL      'W2141R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    SCKEY
      *
     C                   MOVEL     SCKEY         SCTRCD
     C     *INKC         CABEQ     *ON           TERM
      *
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCBR2'
     C                   MOVE      SCFND         ##FND
     C                   MOVEL(P)  Q2RECF        SVRECF           10
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2146R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
     C                   PARM                    ##RECD
     C                   PARM                    ##FND
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCBR2
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C                   MOVE      *ON           *IN41
     C                   MOVE      *BLANK        SCDBR2
     C     TBKY          CHAIN     PB201                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B2DESC        SCDBR2
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN40
     C                   ENDIF
      *
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCBR7'
     C                   MOVEL(P)  Q2RECF        SVRECF           10
     C                   MOVE      SCFND         ##FND
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2151R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
     C                   PARM                    ##FND
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCBR7
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR7         XXKEY
     C                   MOVE      *ON           *IN51
     C                   MOVE      *BLANK        SCDBR7
     C     TBKY          CHAIN     PB701                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     B7DESC        SCDBR7
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *ON           *IN41
     C                   ENDIF
      *
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRMK1'
     C     HFLD          OREQ      'SCRMK2'
     C     HFLD          OREQ      'SCRMK3'
     C                   MOVEL(P)  Q2RECF        SVRECF           10
     C                   MOVE      *BLANK        ##KEY
     C                   CALL      'W2171R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ##KEY         XXKEY
     C                   SETOFF                                       444547
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCRMK1'
     C                   MOVEL     ##KEY         SCRMK1
     C                   SETON                                        44
     C     HFLD          WHENEQ    'SCRMK2'
     C                   MOVEL     ##KEY         SCRMK2
     C                   SETON                                        45
     C     HFLD          WHENEQ    'SCRMK3'
     C                   MOVEL     ##KEY         SCRMK3
     C                   SETON                                        47
     C                   ENDSL
      *
     C     TBKY          CHAIN     PRC01                              61
     C     *IN61         IFEQ      *OFF
     C                   ENDIF
      *
     C                   ELSE
      *
     C                   SELECT
     C     HFLD          WHENEQ    'SCRMK1'
     C                   SETON                                        46
     C     HFLD          WHENEQ    'SCRMK2'
     C                   SETON                                        44
     C     HFLD          WHENEQ    'SCRMK3'
     C                   SETON                                        45
     C                   ENDSL
      *
     C                   ENDIF
      *
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
      *
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCLTCD'
     C                   MOVEL(P)  Q2RECF        SVRECF           10
     C                   MOVE      *BLANK        ##KEY
      *
     C                   CALL      'W2221R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C                   PARM      *INKL         ##KL              1
     C                   PARM                    ##KEY            10
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     ##KEY         IFNE      *BLANK
     C                   MOVEL     ##KEY         SCLTCD
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCLTCD        XXKEY
     C                   MOVEA     '1011'        *IN(82)
     C                   MOVE      *BLANK        SCDLTC
     C     TBKY          CHAIN     PLR01                              61
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     LRDESC        SCDLTC           30
     C                   ENDIF
     C                   ELSE
     C                   MOVEA     '1100'        *IN(82)
     C                   ENDIF
      *
     C                   MOVEL     SVRECF        Q2RECF
     C                   GOTO      STRT
      *__________________________________________________________________
     C     HFLD          WHENEQ    'SCEOB#'
     C     HFLD          OREQ      'SCRCN'
     C*
     C                   MOVE      XXCO#         #1CO#
     C                   MOVE      XXFND         #1WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1WCF#            3
     C                   PARM      *BLANKS       #1SUB#            5
     C                   PARM      *BLANKS       #1FND             3
     C*
     C                   MOVE      #1SUB#        #2SUB#
     C                   MOVE      #1FND         #2FND
     C                   MOVE      SCFEIN        #2FEIN
     C                   MOVE      SCMOD         #2MOD
     C*
     C                   CALL      'F0350R'                             99
     C                   PARM      '2'           #2CALL            1
     C                   PARM                    #2SUB#            5 0
     C                   PARM                    #2FND             3 0
     C                   PARM      SCFYR         #2FYR             3 0
     C                   PARM      SCCASE        #2CASE            7 0
     C                   PARM      *ZEROS        #2EOB#            9 0
     C                   PARM      *BLANKS       #2CERT           15
     C                   PARM      *BLANKS       #2SFDT            8
     C                   PARM      *BLANKS       #2STDT            8
     C                   PARM                    #2FEIN            9
     C                   PARM                    #2MOD             3
     C                   PARM                    #2RCN             3 0
     C*
     C     #2EOB#        IFEQ      *ZEROS
     C                   MOVEA     '101000'      *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   SETON                                        40
     C                   Z-ADD     #2EOB#        SCEOB#
     C                   Z-ADD     #2RCN         SCRCN
     C                   GOTO      STRT
     C*
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCPODT'
     C                   MOVEA     '0001'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCAMT'
     C                   MOVEA     '0010'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCFRDT'
     C                   MOVEA     '0011'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCTODT'
     C                   MOVEA     '0100'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCWKS'
     C                   MOVEA     '0101'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCDYWK'
     C                   MOVEA     '0110'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRCVD'
     C                   MOVEA     '0111'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCTRDS'
     C                   MOVEA     '1000'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCFNAL'
     C                   MOVEA     '1001'        *IN(82)
     C                   SETON                                        90
     C                   GOTO      STRT
      *________________________________________________________________
     C     HFLD          WHENEQ    'SCRMDS'
     C                   MOVEA     '1010'        *IN(82)
     C                   SETON                                        9047
     C                   GOTO      STRT
      *________________________________________________________________
     C                   OTHER
      *
      *  No Search available for field in cursor selection
     C                   SETON                                        7790
     C                   GOTO      STRT
      *
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* =======================================================
     C*  F11 - Benefit info
     C* =======================================================
     C     *INKK         IFEQ      '1'
      *
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R11'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   ENDIF
     C*                                                    JJJ 9-29-89
     C* =======================================================
     C*  F15 - Remaining Reserve
     C* =======================================================
     C*                                                    |
     C     *INKP         IFEQ      '1'
      *
     C                   MOVE      Q2RECF        SVRECF
      *
     C                   CALL      'W0070R12'                           99
     C     *INKC         PARM      *ZERO         ##KC              1
     C     *INKL         PARM      *ZERO         ##KL              1
     C                   PARM      'C'           ##MODE            1
     C                   PARM      DBRRNC        ##RRN             9 0
      *
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
      *
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVE      SVRECF        Q2RECF
     C                   GOTO      STRT
      *
     C                   END
     C*
      * =======================================================
     C*  CMD 18 - CALL TO DIARY DATE MAINTENANCE
      * =======================================================
     C*
     C     *INKS         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVE      *ZERO         QQPRDS
     C                   MOVE      *BLANK        QQCO#
     C                   MOVE      *BLANK        QQFND
     C                   MOVE      SCYR          QQFYR             3
     C                   MOVE      SCCASE        QQCSE             7
     C                   MOVE      *BLANK        QQACDT
     C                   MOVE      'C'           QQCD
     C                   CALL      'W0190R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCSE             7
     C                   PARM                    QQACDT            6
     C                   PARM                    QQCD              1
     C*                                                    |
     C     QQRTCD        CABEQ     'A'           TERM
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
      * =======================================================
     C*   PROCESS HELP REQUESTS
      * =======================================================
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
      * =======================================================
     C*   PROCESS CHANGE OF USER PARAMETERS
      * =======================================================
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C* =======================================================
     C*   F21 - Change Provider Status (Group vs. Non-Group)
     C* =======================================================
      *
     C     *INKV         IFEQ      '1'
      *
     C     *IN24         IFEQ      *ON
     C                   SETOFF                                       24
     C                   ELSE
      *  IF THE PAYEE EXISTS IN THE GROUP ID FILE, DISPLAY THE GROUP
      *  TYPE FLAG.
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
      *
     C                   EXSR      ASGNGP
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C* =======================================================
     C*   F24 - Change Function Key Display
     C* =======================================================
      *
     C     *INKY         IFEQ      *ON
      *
     C                   SELECT
      *
     C     Q2RECF        WHENEQ    'SCR01'
     C                   ADD       1             CK                3 0
     C     *IN04         IFEQ      *ON
     C     CK            IFGT      4
     C     ##CLT#        OREQ      '139'
     C     CK            ANDGT     2
     C                   Z-ADD     1             CK
     C                   ENDIF
     C                   ELSE
     C     CK            IFGT      8
     C     ##CLT#        OREQ      '139'
     C     CK            ANDGT     4
     C                   SELECT
     C     ##CLT#        WHENEQ    '139'
     C                   Z-ADD     3             CK
     C                   OTHER
     C                   Z-ADD     5             CK
     C                   ENDSL
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      FK1(CK)       SCFK1A
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1A
     C                   ENDIF
     C                   ADD       1             CK
     C                   MOVE      FK1(CK)       SCFK1B
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK1X(CK)      SCFK1B
     C                   ENDIF
      *
     C     Q2RECF        WHENEQ    'SCR03'
     C                   ADD       1             CK3
     C     CK3           IFGT      4
     C     ##CLT#        OREQ      '139'
     C     CK3           ANDGT     3
     C                   Z-ADD     1             CK3
     C                   ENDIF
      *
     C                   MOVE      FK3(CK3)      SCFK3
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK3X(CK3)     SCFK3
     C                   ENDIF
      *
     C                   ENDSL
      *
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
      *__________________________________________________________________
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*
     C*_________________________________________________________________
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      'Y'           ##MCKL            1
     C     ##RTCD        CABEQ     'F'           TERM
      *
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
      *
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
      *
     C                   MOVE      *OFF          *IN19
     C     XRKY          CHAIN     PXR01                              60
     C     *IN60         IFEQ      *OFF
     C     XRSTAT        ANDEQ     'A'
     C     *INKC         ANDNE     *ON
     C                   MOVE      *ON           *IN19
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Edit SCR01 Fields
     C*
     C*** Call claims security API to check claim authority
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      SCFND         ##FND
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVEA     I6(6)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C                   MOVE      'Y'           ##03
     C                   MOVE      'Y'           ##05
     C                   MOVE      'Y'           ##09
     C*
     C                   CALL      'WA011R'      A011PL
     C*
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA012R'      A012PL
     C                   ENDIF
      *
     C     ##RC          IFNE      *BLANK
     C     ##RC          ANDNE     '15'
     C     ##RC          ANDNE     '16'
     C     ##RC          ANDNE     '17'
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  Assign *IN13 - *ON  = NOT Compensation
     C*                 *OFF = Compensation
     C*
     C                   MOVE      *OFF          *IN13
     C     ##RETP        IFNE      'C'
     C                   MOVE      *ON           *IN13
     C                   ENDIF
     C*
     C*  Assign *IN14 - *ON  = NOT Medical/Expense
     C*                 *OFF = Medical/Expense
     C*
     C                   MOVE      *ON           *IN14
     C     ##RETP        IFEQ      'M'
     C     ##RETP        OREQ      'E'
     C                   MOVE      *OFF          *IN14
     C                   ENDIF
      *
     C                   MOVE      ##RECD        SCRECD
     C                   MOVE      ##RETP        SCRETP
     C                   MOVE      ##TYPE        SCTYPE
      *
      *  Access Claim Master
      *
     C     CMKY          CHAIN(N)  PCM01                              62
     C*
     C*
     C     SVCASE        IFNE      SCCASE
     C*
     C                   Z-ADD     SCCASE        SVCASE            7 0
     C*
     C     *IN62         IFEQ      '0'
     C     CMSTAT        ANDEQ     'C'
     C*
     C     CMKY          CHAIN     PLL01                              60
     C     *IN60         IFEQ      *OFF
     C     CMJRST        CAT(P)    'MS':0        KKTBID
     C                   MOVEL(P)  LLMTST        KKKEY
     C*
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     '  MS'        KKTBID
     C     GTKY          CHAIN     PGT01                              60
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     GTFG01        ANDEQ     'Y'
     C                   CALL      'WA720R'
     C                   PARM      '1'           KKMGID            1
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM      *BLANK        ##KL
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  Date of Disability must be in Claim Master if Comp payment
      *
     C     SVTRC         IFNE      SCTRCD
     C     ##RETP        IFEQ      'C'
     C     CMDBDT        ANDEQ     *ZERO
     C                   MOVEL(P)  ER(5)         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCDE78
     C*
     C                   MOVE      SCTRCD        SVTRC
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
     C                   CLEAR                   SCDE78
     C*
     C*  SYSTEM PAYMENTS ARE NOT ALLOWED
     C*
     C     ##TYPE        IFEQ      'P'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  IF THE TRANSACTION IS A RECOVERY, CHECK THE GROUP EXPIRATION
     C*  DATE
     C*  IF THE TRANSACTION IS A MANUAL PAYMENT, CHECK PREVIOUSLY
     C*  SKIPPED ERRORS
     C*
     C     ##TYPE        IFEQ      'M'
     C     ##TYPE        OREQ      'R'
     C*
     C                   MOVE      'N'           ##03
     C*
     C     ##TYPE        IFEQ      'M'
     C                   MOVE      'N'           ##05
     C                   MOVE      'N'           ##09
     C                   ENDIF
     C*
     C                   CALL      'WA011R'      A011PL
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6               6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* IF VOID, STOP, OR REVERSAL, CHECK SECURITY LEVEL
     C*
     C     ##TYPE        IFEQ      'V'
     C     ##TYPE        OREQ      'S'
     C     ##TYPE        OREQ      'Z'
     C*
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  Security level and Type check
     C*
     C                   MOVE      UPTYPE        SRUTYP            1
     C                   MOVE      UPLVL         SRULVL            3
     C                   MOVE      CXVSZT        SRCTYP            1
     C                   MOVE      CXVSZL        SRCLVL            3
     C                   MOVE      '1'           SREXEC            1
     C                   MOVE      *ON           SRRES             1
     C*
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXVSZ2        SRCTP2            1
     C                   PARM      CXVSZ3        SRCTP3            1
     C                   PARM      CXVSZ4        SRCTP4            1
     C                   PARM      CXVSZ5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
     C*
     C     SRRES         IFEQ      *ON
     C                   SETON                                        19
     C                   GOTO      STRT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
      *
      *  Initialize save fields.
      *
     C                   Z-ADD     *ZERO         SVFDT2
     C                   Z-ADD     *ZERO         SVTDT2
     C                   Z-ADD     *ZERO         SVFDT
     C                   Z-ADD     *ZERO         SVTDT
     C                   MOVE      *ON           FCALC             1
     C                   MOVE      ##TYPE        SVTRTP            1
     C                   MOVE      *ON           XFIRST            1
     C                   MOVE      *ZEROS        SVAMT
     C                   MOVE      *ZEROS        SVFRDT
     C                   MOVE      *ZEROS        SVTODT
     C                   MOVE      *ZEROS        SVFEIN
     C                   MOVE      *ZEROS        SVMOD
     C*
     C*  Assign Claim Heading
     C*
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
     C*
     C                   MOVEL     S2NAME        SCCSNM
      *
      *  Execute Re-Insurance logic for Claim
      *
     C                   CALL      'WA010R'
     C                   PARM      DBRRNC        ##RRN
     C                   PARM      *ZERO         XXFNDR           13 2
     C                   PARM      *BLANK        XXEXRE            1
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C                   MOVE      CMJRST        SCJRST
     C                   MOVE      CMNCCI        SCNCCI
     C                   MOVE      CMLTFL        SCLTFL
     C*
     C*  Assign Payment Transaction Description
     C*
     C                   MOVEL(P)  SCTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PCDESC        SCTRDS
     C                   ENDIF
     C*
     C*  SAVE THE TRANSACTION CODE TYPE FOR LATER USE
     C*
     C                   MOVE      PCTRTP        SVTRTP            1
     C*
     C                   Z-ADD     1             X
     C     PCRECD        LOOKUP    RT(X)                                  60
     C                   Z-ADD     X             SVCMX             3 0
      *
      *  Assign Payment Reserve Type
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
      *
     C                   Z-ADD     *ZERO         SCRCVD
     C                   MOVE      'N'           SCFNAL
     C*
     C                   MOVEL(P)  SCTRCD        XXKEY
     C*
     C*_____________________________________________________________________
     C*
     C*  IF THE TYPE IS FOR MANUAL CHECKS GOTO TO SCREEN 3.
     C*
     C     ##TYPE        IFEQ      'M'
      *
      *  Only perform the following if EDI A49s are being processed
      *  for the claim's jurisdiction state.
      *
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     'A49'         XXTRID
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      '0'
      *
      *  Call the FL EDI State Maint screen if this is the first comp
      *  pmt and the claim's jurisdiction state is FL.
      *
     C     *IN13         IFEQ      *OFF
     C     CMJRST        ANDEQ     'FL'
      *
      *  Determine if this was the first comp payment.
      *
     C                   MOVE      'Y'           XXFCPT            1
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPRETP        IFEQ      'C'
     C     RPSTAT        ANDNE     'D'
     C                   MOVE      'N'           XXFCPT
     C                   LEAVE
     C                   ENDIF
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
      *
     C     APKY          CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
     C     PARETP        IFEQ      'C'
     C                   MOVE      'N'           XXFCPT
     C                   LEAVE
     C                   ENDIF
     C     APKY          READE     PPA02                                  60
     C                   ENDDO
      *
     C     XXFCPT        IFEQ      'Y'
     C                   MOVE      'C'           QSCALL            1
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCASE        QQCASE            7
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W0070RFL'                           60
     C                   PARM                    QSCALL
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C     QQRTCD        CABEQ     'A'           TERM
     C                   END
      *
     C                   END
     C                   ENDIF
     C                   EXSR      SRPREM
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*_________________________________________________________________
     C*
     C*  IF THE TYPE IS FOR REFUNDS GOTO TO SCREEN 4.
     C*
     C     ##TYPE        IFEQ      'R'
     C                   EXSR      SRPRER
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*_________________________________________________________________
     C*
     C*  BRANCH TO THE SEARCH FOR TYPES V/S/Z/A.
     C*
     C                   Z-ADD     DBRRNC        ##RRN
     C                   CALL      'W0620R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCTYPE        ##TYPE            1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT
     C*
     C                   CLEAR                   SCSEQ
     C     ##RRN         CHAIN(N)  WDPAP                              60
     C  N60              Z-ADD     PASEQ#        SCSEQ
     C*
     C     SCSEQ         IFEQ      *ZERO
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VERIFY THE RESERVE CODE OF THE SELECTED TRANSACTION IS VALID
     C*  FOR THE CLAIM.
     C*
     C     PARECD        LOOKUP    RT                                     60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ER(2)         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCDE78
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*  Verify that the reserve code of the selected pmt transaction
     C*  matches the reserve code of the transaction code.
     C*
     C     PARECD        IFNE      ##RECD
     C     ##TYPE        ANDEQ     'A'
     C*
     C                   MOVEL(P)  ER(6)         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCDE78
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*_________________________________________________________________
     C*
     C     SCSEQ         IFNE      *ZERO
     C     SCTYPE        ANDNE     'A'
     C     SCCASE        ANDNE     *ZERO
     C                   EXSR      SRVSZ
     C                   GOTO      TOP
     C                   ENDIF
     C*
     C*_________________________________________________________________
     C*
     C     SCTYPE        IFEQ      'A'
     C     SCSEQ         ANDNE     *ZERO
     C                   EXSR      SRPREA
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*_________________________________________________________________
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*_________________________________________________________________
     C*
     C* PROCESS SCREEN 2 - ADJUSTMENT ENTRY
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      'N'           REDSP2            1
     C*
     C*  Calculate Number of Days between "From" & "To" Service Dates
     C*  for Comp Payments Only
     C*
     C     RTTYPE        IFEQ      'C'
     C*
     C                   Z-ADD     SCCOFR        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCOFR
     C     SRERR         CABEQ     *ON           STRT                     27
     C*
     C                   Z-ADD     SCCOTO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCOTO
     C     SRERR         CABEQ     *ON           STRT                     28
     C*
     C*
     C     SVFDT2        IFNE      SCCOFR
     C     SVTDT2        ORNE      SCCOTO
     C*
     C                   Z-ADD     SCCOFR        SVFDT2
     C                   Z-ADD     SCCOTO        SVTDT2
     C                   Z-ADD     SCCOFR        QQBEG
     C                   Z-ADD     SCCOTO        QQEND
     C                   EXSR      SRWKDY
     C*
     C     QQERR         IFEQ      *OFF
     C                   Z-ADD     XXWK          SCCOWK
     C                   Z-ADD     XXDY          SCCODY
     C                   Z-ADD     XXWK          SVCOWK
     C                   Z-ADD     XXDY          SVCODY
     C*
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C                   ENDIF
     C*
     C*  GET THE CLAIM MASTER RECORD TO VALIDATE THE CASE NUMBER
     C*
     C                   Z-ADD     SCOFYR        SCFYR
     C                   Z-ADD     SCOFCA        SCCASE
     C*
     C     CMKY          CHAIN(N)  PCM01                              60
      *
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  TEST OFFSETTING ADJUSTMENT CODE
     C*
     C                   MOVEL(P)  SCOFCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C     *IN60         CABEQ     *ON           STRT                     20
     C     PCSTAT        CABEQ     'I'           STRT                     20
     C*
     C*  TEST OFFSETTING ADJUSTMENT CODE TYPE AGAINST TX CD TYPE
     C*
     C     PCTRTP        CABNE     SVTRTP        STRT                 7272
     C*
     C                   MOVEL(P)  PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C                   MOVE      PCRECD        SV1REC            2
     C                   MOVE      RTTYPE        SV1RTP            1
     C*
     C*  THE OFFSETTING ADJUSTMENT DESCRIPTION MUST BE FILLED IN
     C*
     C     SCOFDS        CABEQ     *BLANK        STRT                     34
     C*
     C*  EDIT THE OFFSETTING POSTING DATE
     C*
     C                   Z-ADD     SCOFPO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOFPO
     C*
     C     SRERR         CABEQ     *ON           STRT                     35
     C*
     C*  TEST CORRECTING ADJUSTMENT CODE
     C*
     C                   MOVEL(P)  SCCODC        XXKEY
     C     TBKY          CHAIN     PPC01                              32
     C     *IN32         CABEQ     *ON           STRT
     C     PCSTAT        CABEQ     'I'           STRT                     32
     C*
     C*  TEST CORRECTING ADJUSTMENT CODE TYPE AGAINST TX CD TYPE
     C*
     C     PCTRTP        CABNE     SVTRTP        STRT                 3333
     C*
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER
     C*
     C                   MOVEL(P)  PCRECD        XXKEY
     C     TBKY          CHAIN     PRT01                              60
     C                   MOVE      PCRECD        SV2REC            2
     C                   MOVE      RTTYPE        SV2RTP            1
     C*
     C*  IF THE PMT IS BEING TRANSFERRED TO ANOTHER CASE AND HAS AN
     C*  EOMB ATTACHED, CHECK THE RESERVES FOR ALL ASSOCIATED PMTS
     C*
     C     SCOFYR        IFNE      SCCOYR
     C     SCOFCA        ORNE      SCCOCA
     C*
     C     XXEOB#        IFNE      *ZERO
     C     SVTYPE        ANDNE     'R'
     C                   EXSR      CHKRS1
     C                   GOTO      SKIP
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  IF THE OFFSETTING CLAIM IS OPEN:
     C*
     C*  VALIDATE THE RESERVE AMOUNT REMAINING VS THE OFFSETTING AMOUNT
     C*
     C                   Z-ADD     *ZERO         XXREM             9 2
     C*
     C     SV2REC        IFEQ      SV1REC
     C     SCCOYR        ANDEQ     SCOFYR
     C     SCCOCA        ANDEQ     SCOFCA
     C                   ADD       SCCOAM        XXREM
     C                   ENDIF
     C*
     C     SVTYPE        IFEQ      'R'
     C*
     C                   Z-ADD     SCOFYR        XXFYR
     C                   Z-ADD     SCOFCA        XXCASE
     C*
     C     CMSTAT        IFNE      'C'
     C*
     C     RPKY1         SETLL     PRP01
     C     *IN60         DOUEQ     *ON
     C     RPKY1         READE     PRP01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     RPSTAT        ANDEQ     ' '
     C     RPRECD        ANDEQ     SV1REC
     C                   SUB       RPAMT         XXREM
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     1             X
     C     SV1REC        LOOKUP    RT(X)                                  60
     C                   ADD       RR(X)         XXREM
     C     SCOFAM        CABGT     XXREM         STRT                 58
     C*
     C                   ELSE
     C*
     C*  If the claim is closed, check the reserve cushion.
     C*
     C                   Z-ADD     0             XXCUSH            9 2
     C                   CALL      'SRCUSH'
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    SV1REC
     C                   PARM                    CMLTFL
     C                   PARM                    XXCUSH
     C*
     C                   ADD       XXCUSH        XXREM
     C     SCOFAM        CABGT     XXREM         STRT                 58
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             X
     C     SV1REC        LOOKUP    RT(X)                                  60
     C                   ADD       AR(X)         XXREM
     C     SCOFAM        CABGT     XXREM         STRT                 58
     C*
     C                   ENDIF
     C*
     C*
     C     SKIP          TAG
     C*
     C                   Z-ADD     SCCOYR        SCFYR
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         CABEQ     *ON           STRT                     36
     C*
     C*  GET THE CLAIM MASTER RECORD TO VALIDATE THE CASE NUMBER
     C*
     C                   Z-ADD     SCCOYR        SCFYR
     C                   Z-ADD     SCCOCA        SCCASE
     C*
     C     CMKY          CHAIN(N)  PCM01                              60
     C     *IN60         CABEQ     *ON           STRT                     72
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN72
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER
     C*
     C                   Z-ADD     1             X
     C     SV2REC        LOOKUP    RT(X)                                  60
     C*
     C     *IN60         CABEQ     *OFF          STRT                     38
     C*
     C*  IF THE PMT IS BEING TRANSFERRED TO ANOTHER CASE AND HAS AN
     C*  EOMB ATTACHED, CHECK THE RESERVES FOR ALL ASSOCIATED PMTS
     C*
     C     SCOFYR        IFNE      SCCOYR
     C     SCOFCA        ORNE      SCCOCA
     C*
     C     XXEOB#        IFNE      *ZERO
     C     SVTYPE        ANDNE     'R'
     C                   EXSR      CHKRS2
     C                   GOTO      SKP1
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  IF THE CORRECTING CLAIM IS OPEN:
     C*
     C*  VALIDATE THE RESERVE AMOUNT REMAINING VS THE CORRECTING AMOUNT
     C*
     C                   Z-ADD     *ZERO         XXREM
     C*
     C     SV1REC        IFEQ      SV2REC
     C     SCCOYR        ANDEQ     SCOFYR
     C     SCCOCA        ANDEQ     SCOFCA
     C                   ADD       SCOFAM        XXREM
     C                   ENDIF
     C*
     C     SVTYPE        IFNE      'R'
     C*
     C                   Z-ADD     SCCOYR        XXFYR
     C                   Z-ADD     SCCASE        XXCASE
     C*
     C     CMSTAT        IFNE      'C'
     C*
     C     RPKY1         SETLL     PRP01
     C     *IN60         DOUEQ     *ON
     C     RPKY1         READE     PRP01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     RPSTAT        ANDEQ     ' '
     C     RPRECD        ANDEQ     SV2REC
     C                   SUB       RPAMT         XXREM
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     1             X
     C     SV2REC        LOOKUP    RT(X)                                  60
     C                   ADD       RR(X)         XXREM
     C     SCCOAM        CABGT     XXREM         STRT                 51
     C*
     C                   ELSE
     C*
     C*  If the claim is closed, check the reserve cushion.
     C*
     C                   Z-ADD     0             XXCUSH            9 2
     C                   CALL      'SRCUSH'
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    SV2REC
     C                   PARM                    CMLTFL
     C                   PARM                    XXCUSH
     C*
     C                   ADD       XXCUSH        XXREM
     C     SCCOAM        CABGT     XXREM         STRT                 51
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             X
     C     SV2REC        LOOKUP    RT(X)                                  60
     C                   ADD       AR(X)         XXREM
     C     SCCOAM        CABGT     XXREM         STRT                 51
     C*
     C                   ENDIF
     C*
     C*  IF CLAIM IS CLOSED CHECK RESERVE CUSHION
     C*
     C     SKP1          TAG
     C*
     C*  THE CORRECTING ADJUSTMENT DESCRIPTION MUST BE ENTERED
     C*
     C     SCCODS        CABEQ     *BLANK        STRT                     74
     C*
     C*  THE PAYEE & MOD MUST BE VALIDATED
     C*
     C                   MOVE      CMSS#         XNSS#
     C*
     C     RTTYPE        IFNE      'C'
     C     SCCOFE        ANDNE     XNSS#
     C     RTTYPE        OREQ      'C'
     C     SCCOFE        ANDNE     XNSS#
     C     SCCOFE        ANDNE     *ZERO
     C*
     C                   MOVE      SCCOFE        XXFEIN
     C                   MOVE      SCCOMO        XXMOD
     C*
     C*  CHECK THE VALIDITY AND ACTIVITY OF THE PROVIDER
     C*
     C     PDKY          CHAIN     PPD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     PDSTAT        ORNE      'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN56
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  TEST THE PAYMENT VALUE 1 CODE FOR VALIDITY AND ACTIVITY
     C*
     C                   MOVEL(P)  SCCOB2        XXKEY
     C*
     C     TBKY          CHAIN     PB201                              25
     C*
     C     *IN25         CABEQ     *ON           STRT                     75
     C     B2STAT        CABEQ     'I'           STRT                     75
     C*
     C*  PAYMENT VAL 1 RSRV CODE MUST EQUAL RSRV CODE OF TRANS CODE
     C*
     C     PCRECD        LOOKUP    RC                                     60
     C*
     C     *IN60         CABEQ     *OFF          STRT                     99
     C*
     C*  TEST THE PAYMENT VALUE 2 CODE FOR VALIDITY AND ACTIVITY
     C*
     C                   MOVEL(P)  SCCOB7        XXKEY
     C*
     C     TBKY          CHAIN     PB701                              60
     C*
     C     *IN60         IFEQ      *ON
     C     B7STAT        OREQ      'I'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     CMLTFL        IFEQ      'I'
     C     B7LTLF        CABNE     'Y'           STRT                 5454
     C                   ELSE
     C     B7LTLF        CABEQ     'Y'           STRT                     54
     C                   ENDIF
     C*
     C*  EDIT THE CORRECTING POSTING DATE
     C*
     C                   Z-ADD     SCCOPO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCOPO
     C*
     C     SRERR         CABEQ     *ON           STRT                     37
     C*
     C*  EDIT FROM DATE
     C*
     C                   Z-ADD     SCCOFR        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCOFR
     C*
     C     SRERR         CABEQ     *ON           STRT                     27
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT1
     C*
     C     XXDAT1        CABLT     CMACDT        STRT                   52
     C*
     C*  EDIT TO DATE
     C*
     C                   Z-ADD     SCCOTO        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCCOTO
     C*
     C     SRERR         CABEQ     *ON           STRT                     28
     C*
     C*  THE DATES MUST BE IN LOGICAL ORDER
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT2
     C*
     C     XXDAT2        CABLT     XXDAT1        STRT                   29
     C*
     C*  Compare Claimant Aux Return to Work DTs against Service To DT
     C*
     C     RTTYPE        IFEQ      'C'
     C*
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN43
     C*
     C     CMERTW        IFGT      *ZERO
     C     CMERTW        ANDLT     XXDAT2
     C                   MOVE      *ON           *IN42
     C                   ENDIF
     C*
     C*
     C*     Check RTW date in work history.
     C                   Z-ADD     CMRTW         XXRTW
     C                   Z-ADD     XXDAT1        XXFRDT
     C     WHFLAG        CASEQ     *OFF          CKRTW
     C                   ENDCS
     C*
     C*     Edit service date.
     C     XXRTW         IFGT      *ZERO
     C     XXRTW         ANDLT     XXDAT2
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C*
     C     *IN43         IFEQ      *ON
     C                   Z-ADD     XXRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C                   ENDIF
     C*
     C     *IN42         IFEQ      *ON
     C     *IN43         OREQ      *ON
     C     *IN57         OREQ      *ON
     C*
     C                   Z-ADD     CMERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTWE
     C*
     C                   Z-ADD     XXRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTW
     C*
     C                   Z-ADD     SCTODT        SCTDT1
     C                   Z-ADD     SCTODT        SCTDT2
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   EXFMT     SCR05
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C*
     C*  Check for user response F12 = Cancel
     C*
     C     *INKL         CABEQ     *ON           TOP
     C*                                                    *IN42=*ON=*IN43
     C                   ENDIF
     C*
     C*  COMPARE THE FULL SALARY PAID THROUGH DATE TO THE FROM DATE
     C*
     C     CMFSDT        IFNE      *ZERO
     C     CMFSDT        ANDGE     XXDAT2
     C                   MOVE      *ON           *IN86
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XTDATE
     C*
     C*  THE NUMBER OF WEEKS CAN NOT BE LESS THAN ZERO
     C*
     C     SCCOWK        CABLT     *ZERO         STRT                   30
     C*
     C*  THE NUMBER OF DAYS MUST 0-7
     C*
     C     SCCODY        IFLT      0
     C     SCCODY        ORGT      7
     C                   MOVE      *ON           *IN31
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C*  TEST THE PAYMENT VALUE 1 CODE FOR VALIDITY AND ACTIVITY
     C*
     C                   MOVEL(P)  SCOFB2        XXKEY
     C*
     C     TBKY          CHAIN     PB201                              25
     C*
     C*  PAYMENT VAL 1 RSRV CODE MUST EQUAL RSRV CODE OF TRANS CODE
     C*
     C     SV1REC        LOOKUP    RC                                     60
     C*
     C     *IN60         CABEQ     *OFF          STRT                     68
     C*
     C*  ALL EDITS HAVE BEEN PASSED, WRITE THE OFFSETTING ADJUSTMENT
     C*
     C*  IF THE PMT IS BEING TRANSFERRED TO ANOTHER CLAIM AND HAS AN
     C*  EOMB ATTACHED, TRANSFER ALL ASSOCIATED PAYMENTS
     C*
     C     SCOFYR        IFNE      SCCOYR
     C     SCOFCA        ORNE      SCCOCA
     C*
     C     XXEOB#        IFNE      *ZERO
     C     SVTYPE        ANDNE     'R'
     C                   EXSR      TRNSFR
     C                   GOTO      SKP2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     SCSEQ         XXSEQ
     C                   Z-ADD     SCOFCA        SCCASE
     C                   Z-ADD     SCOFYR        SCFYR
     C                   MOVE      *BLANKS       SVTYPE            1
     C*
     C                   CLEAR                   SVACCD
     C                   CLEAR                   SVACCT
     C                   CLEAR                   SVLTCD
     C                   CLEAR                   SVFNAL
     C                   CLEAR                   SVEOB#
     C                   CLEAR                   SVEOBS
     C                   CLEAR                   SVDPCD
     C                   CLEAR                   SVDPCU
     C*
     C*  Update the original transaction to indicate that it has been
     C*  adjusted, and save various fields for subsequent use.
     C*
     C     APKY1         CHAIN     PPA02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     PACKTP        SVACCD
     C                   MOVEL     PAACCT        SVACCT
     C                   MOVEL     PALTCD        SVLTCD
     C                   MOVEL     PAFNAL        SVFNAL
     C                   Z-ADD     PAEOB#        SVEOB#
     C                   Z-ADD     PAEOBS        SVEOBS
     C                   Z-ADD     PADPCD        SVDPCD
     C                   MOVEL     PADPCU        SVDPCU
     C                   MOVEL     PADSQ#        SVDSQ#
     C                   Z-ADD     SCCOYR        PAXFYR
     C                   Z-ADD     SCCOCA        PAXCAS
     C*
     C                   MOVE      SCTYPE        PAADST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C                   MOVE      PATYPE        SVTYPE
     C*
     C                   UPDATE    PPA02
     C*
     C                   ENDIF
     C*
     C                   EXSR      SROFAJ
     C                   EXSR      SRCOAJ
     C*
     C*  WRITE THE OFFSETTING ADJUSTMENT RECORDS
     C*  IF THE OFFSETTING ADJUSTMENT CLAIM RECORD IS CLOSED USE
     C*  SRPRCC TO UPDATE THE RESERVES
     C*  WRITE THE CORRECTING ADJUSTMENT RECORDS.
     C*  IF THE CORRECTING ADJUSTMENT CLAIM RECORD IS CLOSED USE
     C*  SRPRCC TO UPDATE THE RESERVES
     C*  PROCESS THE CLAIM MASTER RECORDS EFFECTED
     C*    -PROCESS THE OFFSETTING RECORD
     C*  TEST TRANSACTION CODE VALUE ENTERED
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER.
     C**N60***             Z-ADD10        X
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID
     C*  DO NOT UPDATE ELOUTS IF THE CLAIM IS CLOSED
     C*  Access the View Check record.
      *  Access Payee Demographic record
     C*    -PROCESS THE CORRECTING RECORD
     C*  TEST TRANSACTION CODE VALUE ENTERED
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER
     C**N60***             Z-ADD10        X
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID
     C*  DO NOT UPDATE ELOUTS IF THE CLAIM IS CLOSED
     C*
     C     REDSP2        IFEQ      'Y'
     C                   MOVE      'N'           REDSP2
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SKP2          TAG
     C*
     C                   CLEAR                   SCR02
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
     C*_________________________________________________________________
     C*
     C* PROCESS SCREEN 3 - MANUAL CHECK REQUEST.
     C*
     C     SCR03T        TAG
     C*
     C                   CALL      'WA044R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM      'Y'           ##MCKL            1
     C     ##RTCD        CABEQ     'F'           TERM
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
     C                   MOVE      'N'           REDSP3            1
     C*
     C*  Calculate Number of Days between "From" & "To" Service Dates
     C*  for Comp Payments Only
     C*
     C***** Begin add **** 03/12/15 ********************************************
     C                   MOVE      SCWKS         jsWKS
     C                   MOVE      SCDYWK        jsDYS
     C
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C                   IF        *IN60
     C                   MOVEA     '1111'        *IN(72)
     C                   GOTO      STRT
     C                   ENDIF
     C
     C                   IF        B2EXWDC = 'Y'
     C                   MOVE      *ZEROS        SCWKS
     C                   MOVE      *ZEROS        SCDYWK
     C                   ELSE
      *** End add 03/12/15 *****************************************************

     C     RTTYPE        IFEQ      'C'
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      *ON
     C                   MOVEA     '010011'      *IN(71)
     C                   ENDIF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C     SRERR         IFEQ      *ON
     C                   MOVEA     '011100'      *IN(71)
     C                   ENDIF
     C*
     C***** Begin delete **** 03/12/15 *****************************************
     C***  SVFDT         IFNE      SCFRDT
     C***  SVTDT         ORNE      SCTODT
     C***** End delete **** 03/12/15 *******************************************
     C*
     C                   Z-ADD     SCFRDT        SVFDT
     C                   Z-ADD     SCTODT        SVTDT
     C                   Z-ADD     SCFRDT        QQBEG
     C                   Z-ADD     SCTODT        QQEND
     C                   EXSR      SRWKDY
     C*
     C     QQERR         IFEQ      *OFF
     C                   Z-ADD     XXWK          SCWKS
     C                   Z-ADD     XXDY          SCDYWK
     C                   Z-ADD     XXWK          SVWKS
     C                   Z-ADD     XXDY          SVDYWK
     C*
     C                   MOVE      *ON           *IN30
      *** Begin delete *** 03/12/15 ********************************************
     C***                GOTO      STRT
      *** End   delete *** 03/12/15 ********************************************
     C                   ENDIF
     C*
      ****Begin delete *** 06/26/15 ********************************************
     C****                   ENDIF
     C*
     C*
     C****                   ENDIF
      ****End   delete *** 06/26/15 ********************************************
     C**** Begin Add *** 03/12/15 **********************************************
      /Free

       // Display override window

       If jsWKS <> SCWKS or jsDYS <> SCDYWK;
          Dou SCWDOV = 'Y' or SCWDOV ='N';
             Exfmt WDW03;
             Select;
                When *inkl;
                     leave;
                When SCWDOV = 'Y';
                     SCWKS  = jsWKS;
                     SCDYWK = jsDYS;
                When SCWDOV = 'N';
                     *in87= *off;
                Other;
                     *in87= *on;
                     *in90 = *on;
             Endsl;
          Enddo;
       Endif;
       Q2RECF = *blanks;
       Q2RECF = %triml(Q2RECF) + 'SCR03';
      /End-free
     C   KL              GOTO      STRT
     C**** End Add  **** 03/12/15 **********************************************

      ****Begin Add  *** 06/26/15 **********************************************
     C                   ENDIF
     C                   ENDIF
      ****End   Add  *** 06/26/15 **********************************************
      *
      *
      *  Voucher Flag must be Y/N. For CLT 114, it must be P,V, or X
      *
     C     ##CLT#        IFEQ      '114'
     C     SCVFLG        IFNE      'P'
     C     SCVFLG        ANDNE     'V'
     C     SCVFLG        ANDNE     'X'
     C                   MOVEA     I6(55)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   eval      mgVflg='Process method must be P, V, or X'
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   ELSE
     C     SCVFLG        IFNE      'Y'
     C     SCVFLG        ANDNE     'N'
     C                   MOVEA     I6(55)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   eval      mgVflg='Process method must be Y or N'
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *
      *
      *  Verify valid Bank Information record exists
      *
     C     ##CLT#        IFEQ      '114'
     C                   SELECT
     C                   WHEN      SCVFLG='V'
     C                   MOVE      'V'           ##CALL
     C                   WHEN      SCVFLG='P'
     C                   MOVE      'C'           ##CALL
     C                   WHEN      SCVFLG='X'
     C                   MOVE      'X'           ##CALL
     C                   ENDSL
     C                   ELSE
     C     SCVFLG        IFEQ      'Y'
     C                   MOVE      'V'           ##CALL            1
     C                   ELSE
     C                   MOVE      'C'           ##CALL            1
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      FNCO#         ##CO#
     C                   MOVE      FNFND         ##FND
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##SRDT
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQTRDT
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQTODT
      *
     C                   MOVEL     SCTRCD        QQTRCD
     C                   MOVEL     SCRECD        QQRECD
     C                   MOVEL     SCBR2         QQPV1
     C                   MOVEL     SCBR7         QQPV2
     C                   MOVE      *ZEROS        QQSEQ#
     C                   MOVE      SCEOB#        QQEOB#
     C                   MOVE      SCFEIN        Q1FEIN
     C                   MOVE      SCMOD         Q1MOD
     C                   MOVE      XXDSQ#        Q1DSQ#
     C* FOR MANUAL CHECKS - DO NOT USE EFT BANK CODE
     C*  SEARCH AN EFFECTIVE EFT RECORD FOR PAYEE, IF FOUND DISABLE THE
     C*  RECORD. RETRIEVE BANK CODE AND THEN ENABLE THE EFT RECORD AGAIN
     C*  THIS LOGIC IS REPLICATED FROM W0583R02 - CHANGE EFT STATUS FUNCTION
     C*
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     QQTRDT        XXDATE
     C*
      *******Beg Del********04/17/12**********************************
     C******             EXSR      SRUEFT
      *******End Del********04/17/12**********************************
      *
      *******Beg Chg********04/17/12**********************************
     C*****              CALL      'WA023R1'
     C*****              PARM      '2'           ##MODE            1
     C*****              PARM                    ##CO#
     C*****              PARM                    ##FND
     C*****              PARM                    ##FYR
     C*****              PARM                    ##CASE
     C*****              PARM                    XXAPPL
     C*****              PARM      DATA          ##DATA           75
     C*****              PARM                    ##CKTP           10
     C*****              PARM                    ##RTCD
      *
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: xxappl:
                 ##data: #rtcd: ##eftc);
      /End-Free
      *******End Chg********04/17/12**********************************
      *
      *  Call API to access "Current" Bank Account based on ##CALL value
      *
      *******Beg Chg********04/17/12**********************************
     C*****              CALL      'WA023R'
     C*****              PARM                    ##CO#             3
     C*****              PARM                    ##FND             3
     C*****              PARM                    XXAPPL
     C*****              PARM                    ##SRDT            8
     C*****              PARM                    ##ACCT           20
     C*****              PARM                    ##CALL            1
     C*****              PARM      *BLANK        ##HDCY            1
     C*****              PARM      *BLANK        ##CHKQ           10
     C*****              PARM      *BLANK        ##FTYP           10
     C*****              PARM      *BLANK        ##RTCD            1
     C*****              PARM      *ZERO         ##EFDT            7
     C*****              PARM      *BLANKS       ##CPYQ           10
     C*****              PARM                    ##CKTP
      *
      /Free
       ##coid = '  ';
       //Call the Procedure to retrieve Bank Account.
       ##acct = GetBankAcc(##co#: ##fnd: xxappl: ##srdt: ##call:
                 ##hdcy: ##chkq: ##ftyp: ##efdt: ##cpyq:
                 ##cktp: ##eftc: ##coid: ##rtcd);
      /End-Free
      *******End Chg********04/17/12**********************************
     C* Reset Expiration date in WMEFP
      *******Beg Del********04/17/12**********************************
     C*****              EXSR      SRREFT
      *******End Del********04/17/12**********************************
      *
     C                   SELECT
     C     ##CLT#        WHENEQ    '114'
     C     ##RTCD        IFEQ      '1'
     C                   SELECT
     C     ##CALL        WHENEQ    'C'
     C*** Begin Chg *** 10/29/12 ***********************************************
     C**********         MOVEA     I6(58)        CH6
     C                   MOVEA     I6(57)        CH6
     C*** End   Chg *** 10/29/12 ***********************************************
     C     ##CALL        WHENEQ    'V'
     C*** Begin Chg *** 10/29/12 ***********************************************
     C**********         MOVEA     I6(57)        CH6
     C                   MOVEA     I6(58)        CH6
     C*** End   Chg *** 10/29/12 ***********************************************
     C     ##CALL        WHENEQ    'X'
     C                   MOVEA     I6(56)        CH6
     C                   ENDSL
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   OTHER
     C     ##RTCD        IFEQ      '1'
     C     ##CALL        IFEQ      'V'
     C                   MOVEA     I6(58)        CH6
     C                   ELSE
     C                   MOVEA     I6(57)        CH6
     C                   ENDIF
      *
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDSL
      *
      *
     C*
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
      *
     C                   CALL      'WA012R'      A012PL
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  If the MICR check control flag is 'Y', the check # will be
     C*  assigned automatically.
     C*
     C     WCFL10        IFNE      'Y'
     C*
     C*  CHECK THE MANUAL CHECK NUMBER.
     C*
     C     SCCHK#        IFLT      FNCHK1
     C     SCCHK#        ORGT      FNCHK2
     C                   MOVEA     I6(60)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
      *
     C                   CALL      'WA014R'      A014PL
      *
     C     ##CLT#        IFEQ      '114'
     C     SCFND         IFEQ      440
     C     ##RC          ANDEQ     '33'
     C                   GOTO      SKIP1
     C                   ENDIF
     C                   ENDIF
     C*
     C*** IF COMP PAYMENT
     C*
     C     SCRETP        IFEQ      'C'
     C*
     C     SCWKS         IFGT      0
     C     SCDYWK        ORGT      0
     C*
     C*** IF ERROR CODE = 39 - SCAMT <= 0 CHECK FOR AUTO COMP
     C*
     C     ##RC          IFEQ      '39'
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      SCWKS         ##WKS             3
     C                   MOVE      SCDYWK        ##DYWK            1
     C*
     C                   CALL      'WA058R'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##WKS
     C                   PARM                    ##DYWK
     C                   PARM                    SCBR2
     C                   PARM      SCAMT         ##AMT             9 2
     C*
     C     ##AMT         IFGT      *ZEROS
     C                   Z-ADD     ##AMT         SCAMT
     C                   MOVE      *BLANKS       ##RC
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
      *
     C     CMJRST        IFEQ      'TX'
     C     CMFSAL        ANDEQ     'Y'
     C     ##RC          ANDEQ     '42'
     C                   MOVE      *BLANKS       ##RC
     C                   ENDIF
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SKIP1         TAG
      *
      *
      *  Payment Description is required
      *
     C     SCTRDS        IFEQ      *BLANK
     C                   MOVEA     I6(62)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Final Comp Payment flag
      *
     C     *IN13         IFEQ      *OFF
     C     SCFNAL        ANDNE     'Y'
     C     SCFNAL        ANDNE     'N'
     C                   MOVEA     I6(61)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  Execute Claim/Payment/Reserve Edits
      *
     C     ##RC          IFEQ      *BLANK
     C                   CALL      'WA013R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCEOB#
     C                   PARM      *ZERO         RPSEQ#
     C                   PARM                    SCAMT
     C                   PARM                    PCRECD
     C                   PARM                    RTTYPE
     C                   PARM                    ##RC
     C                   PARM                    SCAMT
     C                   PARM                    SCRCN
     C                   PARM                    SCTRCD
     C                   ENDIF
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
      *  If "Pay To" field is blank, call sub to assign Name/Address
      *
     C     SCPNAM        IFEQ      *BLANK
     C     SVFEIN        ORNE      SCFEIN
     C     SVMOD         ORNE      SCMOD
     C                   Z-ADD     SCFEIN        SVFEIN
     C                   Z-ADD     SCMOD         SVMOD
     C                   EXSR      ASGNNM
     C                   MOVE      'Y'           REDSP3
     C                   ENDIF
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7               7 0
     C                   Z-ADD     XX7           XXFROM
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7
     C                   Z-ADD     XX7           XXTO
     C*
     C*  Check for Duplicate or Possible Duplicate Payment
     C*
     C     XFIRST        IFEQ      *ON
     C     SVAMT         ORNE      SCAMT
     C     SVFRDT        ORNE      SCFRDT
     C     SVTODT        ORNE      SCTODT
     C     SVFEIN        ORNE      SCFEIN
     C*
     C                   MOVE      *OFF          XFIRST
     C                   MOVE      SCAMT         SVAMT
     C                   MOVE      SCFRDT        SVFRDT
     C                   MOVE      SCTODT        SVTODT
     C                   MOVE      SCFEIN        SVFEIN
      *
     C     51            SUBST     SCCS01:12     QQDFND
     C                   MOVEL     S2NAME        QQDCNM
     C                   MOVEL     SCPNAM        QQDPNM
      *
     C                   MOVE      '0'           ##RTCD
      *
     C                   CALL      'WA019R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    KKSEQ
     C                   PARM                    XXFROM
     C                   PARM                    XXTO
     C                   PARM                    SCAMT
     C                   PARM                    SCRECD
     C                   PARM                    SVFEIN
     C                   PARM                    QQDFND           51
     C                   PARM                    QQDCNM           36
     C                   PARM                    QQDPNM           36
     C     *IN90         PARM      *ZERO         I90               1
     C                   PARM                    SCRETP
     C                   PARM                    SCMOD
     C                   PARM                    ##RTCD            1
      *
     C     ##RTCD        IFEQ      '2'
     C     ##RTCD        OREQ      '3'
     C                   MOVE      *ON           XFIRST
     C                   ENDIF
     C*
     C     ##RTCD        IFEQ      '1'
     C*
     C*  Security level and Type check
      *
     C*
     C                   MOVE      UPTYPE        SRUTYP
     C                   MOVE      UPLVL         SRULVL
     C                   MOVE      CXDPT1        SRCTYP
     C                   MOVE      CXDPLV        SRCLVL
     C                   MOVE      '1'           SREXEC
     C                   MOVE      *ON           SRRES
     C*
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXDPT2        SRCTP2
     C                   PARM      CXDPT3        SRCTP3
     C                   PARM      CXDPT4        SRCTP4
     C                   PARM      CXDPT5        SRCTP5
     C                   PARM      ' '           SRCTP6
     C*
     C     SRRES         IFEQ      '1'
     C                   MOVE      *ON           XFIRST
     C                   MOVE      '1'           *IN63
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *  If *IN90 is *ON, a Duplicate record was found, re-display SCR02
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      STRT
     C*
     C                   ELSE
     C     ##AMT         IFGT      *ZEROS
     C                   Z-ADD     *ZEROS        ##AMT
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *
      *  Compare Claimant Auxiliary Return to Work DTs against Service To DT
      *
     C     RTTYPE        IFEQ      'C'
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN43
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7               7 0
     C                   Z-ADD     XX7           XXFROM
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX7               7 0
     C                   Z-ADD     XX7           XXTO
      *
     C     CMERTW        IFGT      *ZERO
     C     CMERTW        ANDLT     XXTO
     C                   MOVE      *ON           *IN42
     C                   ENDIF
     C*
     C*
     C*     Check RTW date in work history.
     C                   Z-ADD     CMRTW         XXRTW
     C                   Z-ADD     XXDAT1        XXFRDT
     C     WHFLAG        CASEQ     *OFF          CKRTW
     C                   ENDCS
     C*
     C*     Use work history to edit service date.
     C     XXRTW         IFGT      *ZERO
     C     XXRTW         ANDLT     XXTO
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C*
     C     *IN43         IFEQ      *ON
     C                   Z-ADD     WHDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDATE
     C                   ENDIF
     C*
      *
     C     *IN42         IFEQ      *ON
     C     *IN43         OREQ      *ON
     C     *IN57         OREQ      *ON
      *
     C                   Z-ADD     CMERTW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTWE
      *
     C                   Z-ADD     XXRTW         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTW
      *
     C                   Z-ADD     SCTODT        SCTDT1
     C                   Z-ADD     SCTODT        SCTDT2
     C*
     C                   MOVEL(P)  Q2RECF        SVRECF
     C*
     C                   EXFMT     SCR05
     C*
     C                   MOVEL(P)  SVRECF        Q2RECF
     C*
      *  Check for user response F12 = Cancel
     C     *INKL         IFEQ      *ON
     C                   GOTO      TOP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     REDSP3        IFEQ      'Y'
     C                   MOVE      'N'           REDSP3
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SCPOD1            7 0
     C*
     C     CMJRST        IFEQ      'TX'
     C*
     C*  EDIT TO CHECK RESRVE CODE TYPE OR PAYMENT TYPE IS COMP TYPE
     C*
     C*
     C     CMFSAL        IFEQ      'Y'
     C     SCPOD1        ANDLE     CMFSDT
     C     SCRECD        ANDEQ     'CO'
     C     SCRETP        ANDEQ     'C'
     C*
     C                   SETOFF                                       55
     C*
     C     *IN55         DOUEQ     *OFF
     C                   EXFMT     SCR07
     C*
     C     SCFSOV        IFNE      'Y'
     C     SCFSOV        ANDNE     'N'
     C                   SETON                                        5590
     C*
     C                   ELSE
     C                   SETOFF                                       55
     C     SCFSOV        IFEQ      'N'
     C                   SETON                                        6590
     C                   GOTO      STRT
     C                   ELSE
     C     SCFSOV        IFEQ      'Y'
     C* Create check request
     C                   SETON                                            59
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     *INKL         IFEQ      *ON
     C                   SETOFF                                         59KL
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     CMFSAL        IFEQ      'Y'
     C     SCPOD1        ANDLE     CMFSDT
     C                   SETON                                          6590
     C*
     C                   ELSE
     C* Create check request
     C                   SETON                                            59
     C                   ENDIF
     C                   ENDIF
     C*  ALL EDITS HAVE BEEN PASSED, WRITE THE MANUAL CHECK.
     C*
      *
      *  Determine if "Paid on Time" is not 'Y' and Detailed Reason
      *  required is 'Y'
      *
     C                   MOVEL(P)  SCLTCD        XXKEY
     C     TBKY          CHAIN     PLR01                              60
     C   60              MOVE      *BLANKS       LRREAS
      *
     C     LRREAS        IFEQ      'Y'
     C     RTTYPE        ANDEQ     'C'
     C                   MOVE      *BLANK        XXCOMM
     C                   EXSR      SCR06T
     C                   ENDIF
     C*
     C*  WRITE THE MANUAL CHECK PAYMENT RECORD,
     C*
     C*
     C                   Z-ADD     99999         XXSEQ
     C     APKY1         SETLL     PPA02
     C     APKY          READPE(N) PPA02                                  60
     C*
     C   60              Z-ADD     *ZERO         PASEQ#
     C     PASEQ#        ADD       1             XXSEQ#
     C                   CLEAR                   PPA02
     C*
     C                   Z-ADD     XXCO#         PACO#
     C                   MOVE      SCFND         PAFND
     C                   MOVE      SCFYR         PAFYR
     C                   MOVE      SCCASE        PACASE
     C                   MOVE      XXSEQ#        PASEQ#
     C                   MOVE      SCCHK#        PACHK#
     C                   Z-ADD     *ZERO         PABASQ
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
      *
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
      *
     C                   MOVE      SCFEIN        PAFEIN
     C                   MOVE      SCMOD         PAMOD
      ******* BEG ADD *** 11/25/13 **************************************
     C*****              MOVE      SCAMT         PAAMT
      /Free
       xxkey = *Blanks;
       xxKey = scbr2;
       flgPSIG = *blanks;
       Chain TBky pb201;
       If %found(wtabl);
          flgPSIG = B2psig;
       EndIf;

       If ##CLT# = '129' And flgPSIG = 'Y';

          intCO#   = kkCO#;
          intFND   = scFND;
          intFYR   = scFYR;
          intCASE  = scCASE;
          intAMT   = scAMT;
          charBR2  = scBR2;
          intFRDT  = scFRDT;
          intAAMT  = *Zeros;
          charRTCD = *Blank;

          ApprActPgm(intCo#:intFND:intFyr:intCase:
                     intAmt:charBr2:intFrdt:intAAmt:charRtcd);
          If charRtcd = 'Y';
             PAamt = intAAmt;
          Else;
             PAamt = intAmt;
         EndIf;
       Else;
          PAamt = scAmt;
       EndIf;
      /End-Free
      ******* END ADD *** 11/25/13 **************************************
     C                   MOVE      SCBR2         PABR2
     C                   MOVE      SCBR7         PABR7
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
      *
     C                   MOVE      SCWKS         PAWKS
     C                   MOVE      SCDYWK        PADAYS
     C                   MOVE      SCTRCD        PATRCD
     C                   MOVE      ##TYPE        PATYPE
     C                   MOVE      PCRECD        PARECD
     C                   MOVE      ##RETP        PARETP
     C                   MOVEL     SCTRDS        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      *BLANK        PAADST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
      *
     C                   Z-ADD     DSTIME        PATIME
     C                   Z-ADD     SCEOB#        PAEOB#
     C                   MOVE      *BLANK        PAPGTP
     C                   Z-ADD     SCRCN         PAEOBS
      *
      *
     C                   MOVEL     ##ACCT        PAACCT
     C                   MOVE      ##CKTP        PACKTP
      *
     C*
     C*  If the MICR check control flag is 'Y', automatically assign
     C*  the check #.
     C*
     C     WCFL10        IFEQ      'Y'
     C*
     C                   MOVEL     PAACCT        XXACCT
     C                   EXSR      SRCHK#
     C                   MOVE      SCCHK#        PACHK#
     C*
     C                   ENDIF
     C*
     C                   MOVE      CMOFF#        PAOFF#
     C                   Z-ADD     *ZERO         PARCVD
     C                   Z-ADD     *ZERO         PADUE
      *
     C     SCRCVD        IFGT      *ZERO
     C                   Z-ADD     SCRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PARCVD
      *
      *  Access Payment Value 1 table to obtain # days to due date
      *
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     SCBR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
      *                                                    defaults
     C                   Z-ADD     PARCVD        PADUE
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     B2NDDT        ##NDDT            5 0
     C                   Z-ADD     SCRCVD        ##DUE
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DUE
     C                   PARM                    ##NDDT
     C                   PARM                    ##RTCD            1
      *
     C     ##RTCD        IFEQ      *OFF
     C                   Z-ADD     ##DUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADUE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      SCLTCD        PALTCD
     C                   Z-ADD     *ZERO         PADPCD
     C                   MOVE      *BLANK        PADPCU
     C                   MOVE      Q1USER        PARQST
     C                   MOVE      SCFNAL        PAFNAL
     C                   Z-ADD     *ZERO         PARCDT
     C                   MOVE      *BLANK        PARADJ
     C     ##CLT#        IFEQ      '114'
     C     SCVFLG        ANDEQ     'X'
     C                   MOVE      'X'           PASTAT
     C                   ENDIF
     C                   MOVE      XXDSQ#        PADSQ#
      //* Begin Add *** 08/29/19 **********************************************
      /Free
       PAlmps = SClmps;
      /End-Free
      //* End   Add *** 08/29/19 **********************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
      //* Begin Add *** 08/29/19 **********************************************
     C                   WRITE     PPA02
      /Free
       //*** Begin Add *** 07/10/19 *******************************************

       // IDC processing for Manual Payment

        If isModEnabled('IDC') and PAretp = 'C';
           intRRN =DBrrn2;
           WriteTxLog(intRRN);
        Endif;
       //*** End   Add *** 07/10/19 *******************************************
      /End-Free
     C     TXEDT         TAG
     C                   SETOFF                                       59
     C*
     C*  IF THE CLAIM IS CLOSED, CALL SRPRCC TO ALLOCATE THE RESERVES
     C*  FOR THIS PAYMENT.
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'R'           QQTYPE
     C                   CALL      'SRPRCC'      PRCCPL
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X
     C     PCRECD        LOOKUP    RT(X)                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   SUB       SCAMT         RR(X)
     C                   ADD       SCAMT         AR(X)
     C                   ADD       1             CMPCTR
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   ENDIF
     C*
     C                   UPDATE    PCM01
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   ADD       SCAMT         ELTCLP
     C                   SUB       SCAMT         ELOUTS
     C*
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C                   END
     C                   END
     C*
     C*  CREATE REQUEST PAYMENT DETAIL RECORD
     C*
     C                   Z-ADD     XXCO#         RPCO#
     C                   MOVE      SCFND         RPFND
     C                   MOVE      SCFYR         RPFYR
     C                   MOVE      SCCASE        RPCASE
     C                   MOVE      SCCHK#        RPCHK#
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPTRDT
     C                   MOVEL     SCRMDS        RPRMDS
     C                   MOVEL     SCRMK1        RPRMK1
     C                   MOVEL     SCRMK2        RPRMK2
     C                   MOVEL     SCRMK3        RPRMK3
     C                   MOVEL     SCPNAM        RPPNAM
     C                   MOVEL     SCADR1        RPADR1
     C                   MOVEL     SCADR2        RPADR2
     C                   MOVEL     SCCTY         RPCTY
     C                   MOVEL     SCST          RPST
     C                   MOVE      SCZP1         XXZIP1
     C                   MOVE      SCZIP2        XXZIP2
     C                   MOVE      XXZIP         RPZIP
     C                   MOVEL(P)  SCCTRY        RPCTRY
     C                   MOVEL(P)  SCADR3        RPADR3
     C                   MOVE      *BLANK        RPSTAT
     C                   MOVE      Q1USER        RPUSER
     C                   MOVE      Q1PGM         RPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C                   Z-ADD     DSTIME        RPTIME
     C                   Z-ADD     *ZEROS        RPBUL1
     C                   MOVEL     SCLTCD        RPLTCD
      //*** Begin Add *** 08/29/19 ********************************************
      /Free
       RPlmps = SClmps;
      /End-Free
      //*** Begin Add *** 08/29/19 ********************************************
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   WRITE     PRP02
     C*
     C** WRITE DARTS RECORD IF "PAID ON TIME' IS NOT 'Y ' BUT THE
     C** DETAILED REASON REQUIRED IS 'N' (NO).
     C*   GET LAST SEQUENCE USED
      *
     C     ##HDCY        IFEQ      'Y'
     C*
     C*  CALL CHECK/VOUCHER PRINT PROGRAM
     C*
     C                   MOVE      PACO#         ##CO#             3
     C                   MOVE      PAFND         ##FND             3
     C                   MOVE      PAFYR         ##FYR             3
     C                   MOVE      PACASE        ##CASE
     C                   MOVE      PATRDT        ##TRDT            7
     C                   MOVE      PASEQ#        ##SEQ#            5
     C*
     C                   CALL      'W7017C'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##CASE
     C                   PARM                    ##TRDT
     C                   PARM                    ##SEQ#
     C                   PARM                    ##FTYP
     C                   PARM                    ##CHKQ
     C                   ENDIF
     C*
     C*  Only perform the following if EDI A49s are being processed
     C*  for the claim's jurisdiction state.
     C*
     C                   MOVEL     CMJRST        XXJRST
     C                   MOVEL     'A49'         XXTRID
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C     *IN60         IFEQ      '0'
     C*
     C*  Call the FL EDI State Maint screen if this is the first comp
     C*  pmt and the claim's jurisdiction state is FL.
     C*
     C     RPRETP        IFEQ      'C'
     C     CMJRST        ANDEQ     'FL'
     C*
     C*  Determine if this was the first comp payment.
     C*
     C                   MOVE      'Y'           XXFCPT            1
     C                   MOVE      PASEQ#        SVSEQ#
     C     APKY          CHAIN(N)  PPA02                              60
     C     *IN60         DOWEQ     '0'
     C     PARETP        IFEQ      'C'
     C     PASEQ#        ANDNE     SVSEQ#
     C                   MOVE      'N'           XXFCPT
     C                   LEAVE
     C                   ENDIF
     C     APKY          READE(N)  PPA02                                  60
     C                   ENDDO
     C*
     C     XXFCPT        IFEQ      'Y'
     C*
     C*
     C                   MOVE      CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET
     C                   PARM      'IP'          QQMTC
     C                   PARM                    QQJST
     C                   PARM                    QQRTCD
     C*
     C                   END
     C*
     C                   END
     C*
     C*  If the LAST PMT FLAG = 'Y', call the EDI Generic Log API to
     C*    create an A49 'FN' lgo record.
     C*
     C     SCFNAL        IFEQ      'Y'
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      CMJRST        QQJST
     C                   MOVE      *BLANKS       QQRTCD
     C                   CALL      'W8070R4'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM      'A49'         QQTSET            3
     C                   PARM      'S1'          QQMTC             2
     C                   PARM                    QQJST             2
     C                   PARM                    QQRTCD
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
      *
     C     *IN04         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCCASE
     C                   ENDIF
      *
     C                   MOVE      *BLANKS       SCTRCD
     C                   MOVE      *BLANKS       SCREQ#
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCMOD
     C                   MOVE      *BLANKS       SCPODT
     C                   MOVE      *BLANKS       SCCHK#
     C                   MOVE      *BLANKS       SCFRDT
     C                   MOVE      *BLANKS       SCTODT
     C*
     C                   Z-ADD     *ZEROS        SCWKS
     C                   Z-ADD     *ZEROS        SCDYWK
     C                   Z-ADD     *ZEROS        SVWKS
     C                   Z-ADD     *ZEROS        SVDYWK
     C*
     C                   MOVE      *BLANKS       SCBR2
     C                   MOVE      *BLANKS       SCBR7
     C                   Z-ADD     *ZERO         SCAMT
     C                   MOVE      *BLANKS       SCTRDS
     C                   MOVE      *BLANKS       SCLTCD
      /Free
      //*** Begin Add *** 08/29/19 *********************************************
       SClmps = 'N';
      //*** End   Add *** 08/29/19 *********************************************
      /End-Free
     C                   MOVE      *ZERO         SCRCVD
     C                   MOVE      *ZERO         SCEOB#
     C                   MOVE      *BLANKS       SCFNAL
     C                   MOVE      *BLANKS       SCRMK1
     C                   MOVE      *BLANKS       SCRMK2
     C                   MOVE      *BLANKS       SCRMK3
     C                   MOVE      *BLANKS       SCRMDS
     C                   Z-ADD     0             SCRCN
     C                   GOTO      TOP
     C*
     C*_________________________________________________________________
     C*
     C* PROCESS SCREEN 4 - REFUND ENTRY
     C*
     C     SCR04T        TAG
     C*
     C                   MOVEA     '00000'       *IN(71)
     C                   MOVEA     '00000'       *IN(76)
     C                   MOVEA     '00000'       *IN(81)
     C                   MOVEA     '000000'      *IN(86)
     C                   MOVEA     '0000'        *IN(91)
     C                   MOVE      'N'           REDSP4            1
     C*
     C*  Calculate Number of Days between "From" & "To" Service Dates
     C*  for Comp Payments Only.
     C*
     C     RTTYPE        IFEQ      'C'
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C     SRERR         IFEQ      *ON
     C                   MOVEA     '011010'      *IN(71)
     C                   ENDIF
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVEA     '011100'      *IN(71)
     C                   ENDIF
     C*
     C*
     C     SVFDT         IFNE      SCFRDT
     C     SVTDT         ORNE      SCTODT
     C*
     C                   Z-ADD     SCFRDT        SVFDT
     C                   Z-ADD     SCTODT        SVTDT
     C                   Z-ADD     SCFRDT        QQBEG
     C                   Z-ADD     SCTODT        QQEND
     C                   EXSR      SRWKDY
     C*
     C     QQERR         IFEQ      *OFF
     C                   Z-ADD     XXWK          SCWKS
     C                   Z-ADD     XXDY          SCDYWK
     C                   Z-ADD     XXWK          SVWKS
     C                   Z-ADD     XXDY          SVDYWK
     C*
     C                   MOVE      *ON           *IN40
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C* Days/Weeks can't exceed the number of lost days
     C*
      **** Begin Change 09/21/16 *************************************
     C***  SCWKS         MULT      7             DAYS              5 0
     C***                ADD       SCDYWK        DAYS
     C***  DAYS          IFGT      CMLOST
     C***                SETON                                        78
     C***                GOTO      STRT
     C***                ENDIF
      ***
     C     CMDAYS        IFNE      *ZERO
     C     SCWKS         MULT      CMDAYS        DAYS              5 0
     C                   ELSE
     C     SCWKS         MULT      7             DAYS
     C                   ENDIF
     C                   ADD       SCDYWK        DAYS
     C     DAYS          IFNE      *ZERO
     C     DAYS          ANDGT     CMLOST
     C                   SETON                                        78
     C                   GOTO      STRT
     C                   ENDIF
      **** End   Change 09/21/16 *************************************
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     XXCO#         KKCO#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCCASE        XXCASE
      *
      *
      *  Voucher Flag must be Y/N
      *
      **** Beg delete **** 03/11/13 ***************************************
     C**   SCVFLG        IFNE      'V'
     C**   SCVFLG        ANDNE     'C'
     C*** Begin Add *** 02/05/13 ***********************************************
     C**   SCVFLG        ANDNE     'X'
     C*** End   Add *** 02/05/13 ***********************************************
     C**
     C**                 MOVEA     I6(59)        CH6
     C**                 MOVEA     CH6           *IN(71)
     C**                 SETON                                        90
     C**                 GOTO      STRT
     C**                 ENDIF
      **** End delete **** 03/11/13 ***************************************
      *** Begin Add  ***** 03/11/13 ****************************************
      *
      *
      *  Voucher Flag must be Y/N. For CLT 114, it must be P,V, or X
      *
     C     ##CLT#        IFEQ      '114'
     C     SCVFLG        IFNE      'P'
     C     SCVFLG        ANDNE     'V'
     C     SCVFLG        ANDNE     'X'
     C                   MOVEA     I6(55)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   eval      mgVflg='Process method must be P, V, or X'
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   ELSE
     C     SCVFLG        IFNE      'Y'
     C     SCVFLG        ANDNE     'N'
     C                   MOVEA     I6(55)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   eval      mgVflg='Process method must be Y or N'
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDIF
      *** End   Add  ***** 03/11/13 ****************************************
      *
      *  Verify valid Bank Information record exists
      *
     C*
     C*** Begin Chg *** 02/05/13 ***********************************************
     C*****SCVFLG        IFEQF     'V'
     C**********         ENDIF
     C**********         MOVEF     'V'           ##CALL            1
     C**********         ELSEF
     C**********         MOVEF     'C'           ##CALL            1
     C**********         ENDIF
     C**********         MOVE      SCVFLG        ##CALL            1
      *
      *  Verify valid Bank Information record exists
      *
     C     ##CLT#        IFEQ      '114'
     C                   SELECT
     C                   WHEN      SCVFLG='V'
     C                   MOVE      'V'           ##CALL
     C                   WHEN      SCVFLG='P'
     C                   MOVE      'C'           ##CALL
     C                   WHEN      SCVFLG='X'
     C                   MOVE      'X'           ##CALL
     C                   ENDSL
     C                   ELSE
     C     SCVFLG        IFEQ      'Y'
     C                   MOVE      'V'           ##CALL            1
     C                   ELSE
     C                   MOVE      'C'           ##CALL            1
     C                   ENDIF
     C                   ENDIF
     C*** End   Chg *** 02/05/13 ***********************************************
      *
     C                   MOVE      KKCO#         ##CO#
     C                   MOVE      KKFND         ##FND
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##SRDT
      *
      *  Call API to determine the Bank "Code".
      *
     C                   MOVE      SCFYR         ##FYR
     C                   MOVE      SCCASE        ##CASE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQTRDT
      *
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQFRDT
      *
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         QQTODT
      *
     C                   MOVEL     SCTRCD        QQTRCD
     C                   MOVEL     SCRECD        QQRECD
     C                   MOVEL     SCBR2         QQPV1
     C                   MOVEL     SCBR7         QQPV2
     C                   MOVE      *ZEROS        QQSEQ#
     C                   MOVE      SCEOB#        QQEOB#
      *
     C                   MOVE      SCFEIN        Q1FEIN
     C                   MOVE      SCMOD         Q1MOD
     C                   MOVE      XXDSQ#        Q1DSQ#
     C* FOR MANUAL CHECKS - DO NOT USE EFT BANK CODE
     C*  SEARCH AN EFFECTIVE EFT RECORD FOR PAYEE, IF FOUND DISABLE THE
     C*  RECORD. RETRIEVE BANK CODE AND THEN ENABLE THE EFT RECORD AGAIN
     C*  THIS LOGIC IS REPLICATED FROM W0583R02 - CHANGE EFT STATUS FUNCTION
     C*
     C                   Z-ADD     QQTRDT        XXDATE
      *******Beg Del********04/17/12**********************************
     C******             EXSR      SRUEFT
      *******End Del********04/17/12**********************************
      *******Beg Chg********04/17/12**********************************
     C*****              CALL      'WA023R1'
     C*****              PARM      '2'           ##MODE
     C*****              PARM                    ##CO#
     C*****              PARM                    ##FND
     C*****              PARM                    ##FYR
     C*****              PARM                    ##CASE
     C*****              PARM                    XXAPPL
     C*****              PARM      DATA          ##DATA           75
     C*****              PARM                    ##CKTP
     C*****              PARM                    ##RTCD
      *
      *  Call API to access "Current" Bank Account based on ##CALL value
      *
     C*****              CALL      'WA023R'
     C*****              PARM                    ##CO#
     C*****              PARM                    ##FND
     C*****              PARM                    XXAPPL
     C*****              PARM                    ##SRDT
     C*****              PARM                    ##ACCT
     C*****              PARM                    ##CALL
     C*****              PARM      *BLANK        ##HDCY
     C*****              PARM      *BLANK        ##CHKQ
     C*****              PARM      *BLANK        ##FTYP
     C*****              PARM      *BLANK        ##RTCD
     C*****              PARM      *ZERO         ##EFDT
     C*****              PARM      *BLANKS       ##CPYQ
     C*****              PARM                    ##CKTP
     C*
      /Free
       //Call the Procedure to retrieve Bank Code.
       ##data = DATA;
       ##appl = 'WC';
       ##eftc = '  ';
       ##cktp = GetBankCode(##co#: ##fnd: ##fyr: ##case: xxappl:
                 ##data: #rtcd: ##eftc);

       //Call the Procedure to retrieve Bank Account.
       ##coid = '  ';
       ##acct = GetBankAcc(##co#: ##fnd: xxappl: ##srdt: ##call:
                 ##hdcy: ##chkq: ##ftyp: ##efdt: ##cpyq:
                 ##cktp: ##eftc: ##coid: ##rtcd);
      /End-Free
      *******End Chg********04/17/12**********************************
     C* Reset Expiration date in WMEFP
      *******Beg Del********04/17/12**********************************
     C******             EXSR      SRREFT
      *******End Del********04/17/12**********************************
      *
     C*** Begin Chg *** 03/13/13 ***********************************************
     C*****##RTCD        IFEQ      '1'
     C*****##CALL        IFEQ      'V'
     C**********         MOVEA     I6(58)        CH6
     C**********         ELSE
     C*** Begin Add *** 02/05/13 ***********************************************
     C*****##CALL        IFEQ      'C'
     C*** Begin Add *** 02/05/13 ***********************************************
     C**********         MOVEA     I6(57)        CH6
     C*** Begin Add *** 02/05/13 ***********************************************
     C**********         ELSE
     C**********         MOVEA     I6(61)        CH6
     C**********         ENDIF
     C*** End   Add *** 02/05/13 ***********************************************
     C**********         ENDIF
      *
     C**********         MOVEA     CH6           *IN(71)
     C**********         GOTO      STRT
     C**********         ENDIF
      *
     C                   SELECT
     C     ##CLT#        WHENEQ    '114'
     C     ##RTCD        IFEQ      '1'
     C                   SELECT
     C     ##CALL        WHENEQ    'C'
     C                   MOVEA     I6(57)        CH6
     C     ##CALL        WHENEQ    'V'
     C                   MOVEA     I6(58)        CH6
     C     ##CALL        WHENEQ    'X'
     C                   MOVEA     I6(61)        CH6
     C                   ENDSL
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   OTHER
     C     ##RTCD        IFEQ      '1'
     C     ##CALL        IFEQ      'V'
     C                   MOVEA     I6(58)        CH6
     C                   ELSE
     C                   MOVEA     I6(57)        CH6
     C                   ENDIF
      *
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C                   ENDSL
     C*** End   Chg *** 03/13/13 ***********************************************
      *
     C*
     C                   CALL      'WA012R'      A012PL
     C*
     C     ##RC          IFNE      *BLANK
     C                   MOVE      'Y'           ERROR4            1
     C                   ELSE
     C                   MOVE      'N'           ERROR4
     C                   ENDIF
     C*
     C     ##RC          IFEQ      '15'
     C     ##RC          OREQ      '41'
     C                   MOVE      'N'           ERROR4
     C                   ENDIF
     C*
     C     ##RC          IFEQ      '16'
     C     ##RC          OREQ      '17'
     C*
     C     SCTRCD        LOOKUP    DR                                     60
     C*
     C     *IN60         IFEQ      *OFF
     C     SCFEIN        ORNE      *ZERO
     C                   MOVE      'Y'           ERROR4
     C                   ELSE
     C                   MOVE      'N'           ERROR4
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  If "Pay To" field is blank, call sub to assign Name/Address
     C*
     C     SCPNAM        IFEQ      *BLANK
     C     SVFEIN        ORNE      SCFEIN
     C     SVMOD         ORNE      SCMOD
     C                   Z-ADD     SCFEIN        SVFEIN
     C                   Z-ADD     SCMOD         SVMOD
     C                   EXSR      ASGNNM
     C                   WRITE     SCR04
     C                   MOVE      'Y'           REDSP4
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ERROR4        IFEQ      'Y'
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  BE SURE THE CHECK NUMBER IS ENTERED
     C*
     C     SCCHK#        IFLE      *ZEROS
     C                   MOVEA     I6(60)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  GET A VALID PAID ON TIME CODE FOR THE API TO USE SO THAT A
     C*  FALSE ERROR WILL NOT OCCUR (ERROR #38)
     C*
     C     TBKY1         CHAIN     PLR01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     LRSTAT        ANDEQ     'I'
     C     TBKY1         READE     PLR01                                  60
     C                   ENDDO
     C*
     C                   MOVEL(P)  LRKEY         SCLTCD
     C*
     C*  LOAD A VALID RECIEVED DATE FOR THE API TO USE SO THAT FALSE
     C*  ERRORS WILL NOT OCCUR (ERROR #34 & #35)
     C*
     C                   Z-ADD     12312099      SCRCVD
     C*
     C*  IF SCAMT IS NEGATIVE, SAVE IT AND MAKE IT POSITIVE SO THAT
     C*  WA014R WILL ONLY GIVE AN ERROR IF SCAMT IS ZERO
     C*
     C                   Z-ADD     SCAMT         SVAMT
     C*
     C     SCAMT         IFLT      *ZERO
     C                   MULT      -1            SCAMT
     C                   ENDIF
     C*
     C                   CALL      'WA014R'      A014PL
     C     SCTYPE        IFEQ      'R'
     C     ##RC          ANDEQ     '42'
     C                   CLEAR                   ##RC
     C                   ENDIF
     C*
     C                   Z-ADD     SVAMT         SCAMT
     C*
     C     ##CLT#        IFEQ      '114'
     C     SCFND         IFEQ      440
     C     ##RC          ANDEQ     '33'
     C                   GOTO      SKIP2
     C                   ENDIF
     C                   ENDIF
      *
     C     ##RC          IFNE      *BLANK
     C                   MOVE      ##RC          Y
     C                   MOVEA     I6(Y)         CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
      *
     C     SKIP2         TAG
     C*
     C*
     C*  CLEAR FALSE ERROR FIELDS
     C*
     C                   CLEAR                   SCLTCD
     C                   CLEAR                   SCRCVD
     C*
     C*  Payment Description is required
     C*
     C     SCTRDS        IFEQ      *BLANK
     C                   MOVEA     I6(62)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  IS PAYMENT VALUE 1 A DEDUCTIBLE PAYMENT VALUE 1
     C*
     C* Begin add **** 03/12/13 *******************************************
     C     SCBR2         IFGT      *BLANKS
     C* End add   **** 03/12/13 *******************************************
     C     SCBR2         IFEQ      FNMEP1
     C     SCBR2         OREQ      FNCOP1
     C     SCBR2         OREQ      FNEXP1
     C     SCBR2         OREQ      FNLEP1
     C*
     C                   MOVE      *ON           *IN31
     C*
     C     ELKY          CHAIN(N)  PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     RSKY          CHAIN     PRS02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     *LIKE         DEFINE    ELDCDE        XXDDCD
     C     *LIKE         DEFINE    ELDCEF        XXDDEF
     C     *LIKE         DEFINE    ELDCEX        XXDCEX
     C     *LIKE         DEFINE    EL2DEX        XX2DEX
     C*
     C                   Z-ADD     ELDCEX        XXDCEX
     C                   Z-ADD     EL2DEX        XX2DEX
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        XXDCEX
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        XX2DEX
     C                   ENDIF
     C*
     C                   SELECT
     C     ELDCD2        WHENNE    *BLANK
     C     EL2DEF        ANDLE     CMACDT
     C     XX2DEX        ANDGE     CMACDT
     C                   MOVE      ELDCD2        XXDDCD
     C                   MOVE      EL2DEF        XXDDEF
     C     ELDCDE        WHENNE    *BLANK
     C     ELDCEF        ANDLE     CMACDT
     C     XXDCEX        ANDGE     CMACDT
     C                   MOVE      ELDCDE        XXDDCD
     C                   MOVE      ELDCEF        XXDDEF
     C                   ENDSL
     C                   MOVE      ELDCEF        Q2DATE
     C                   MOVE      ELCO#         Q1CO#
     C                   MOVE      ELFND         Q1FND
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q2DATE            7
     C*
     C                   MOVE      Q2DATE        XXDDEF
     C*
     C     DDKY          SETLL     PDD01
     C     DDKY1         READE     PDD01                                  60
     C     *IN60         IFEQ      *OFF
     C     DDSTAT        ANDEQ     'A'
     C*
     C     DDTYPE        IFEQ      'G'
     C                   MOVE      *ON           *IN30
     C                   MOVE      *OFF          *IN31
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Send an informational msg if a deduct rec PV1 code is used.
     C*
     C                   EXFMT     WDW001
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C* Begin add **** 03/12/13 *******************************************
     C                   ELSE
     C                   MOVEA     I6(30)        CH6
     C                   MOVEA     CH6           *IN(71)
     C                   SETON                                        90
     C                   GOTO      STRT
     C                   ENDIF
     C* End add   **** 03/12/13 *******************************************
     C*
     C*  AMOUNT RECOVERED CANNOT BE EQUAL TO 0
     C*
     C     SCAMT         CABEQ     *ZERO         STRT                     33
     C*
     C*  AMOUNT MUST NOT EXCEED TOTAL PAYMENT AMT FOR THAT RSV CODE
     C*
     C                   Z-ADD     SVCMX         X
     C     SCAMT         CABGT     AR(X)         STRT                 34
     C*
     C*  IF ENTERING A NEGATIVE REFUND, THE AMT WILL BE SUBTRACTED
     C*  FROM RESERVES, THEREFORE RESERVES MUST BE ADEQUATE.
     C*
     C     SCAMT         IFLT      *ZERO
     C     SCAMT         MULT      -1            XXRSVA            9 2
     C     XXRSVA        CABGT     RR(X)         STRT                 35
     C                   ENDIF
     C*
     C*  VALIDATE BANK CODE
     C*
     C*  Validate the remark codes.
     C*
     C     SCRMK1        IFNE      *BLANKS
     C                   MOVEL(P)  SCRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              79
     C   79              GOTO      STRT
     C                   ENDIF
     C*
     C     SCRMK2        IFNE      *BLANKS
     C                   MOVEL(P)  SCRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              80
     C   80              GOTO      STRT
     C                   ENDIF
     C*
     C     SCRMK3        IFNE      *BLANKS
     C                   MOVEL(P)  SCRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              81
     C   81              GOTO      STRT
     C                   ENDIF
     C*
     C     REDSP4        IFEQ      'Y'
     C                   MOVE      'N'           REDSP4
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ALL EDITS HAVE BEEN PASSED, WRITE THE REFUND PAYMENT
     C*
     C     RRKY          CHAIN     PRR01                              58
     C*
     C                   MOVE      *HIVAL        XXSEQ
     C*
     C     APKY1         SETGT     PPA02
     C     APKY          READPE(N) PPA02                                  60
     C*
     C   60              Z-ADD     *ZERO         PASEQ#
     C     PASEQ#        ADD       1             XXSEQ#
     C                   CLEAR                   PPA02
     C*
     C                   MOVE      XXCO#         PACO#
     C                   MOVE      SCFND         PAFND
     C                   MOVE      SCFYR         PAFYR
     C                   MOVE      SCCASE        PACASE
     C                   MOVE      XXSEQ#        PASEQ#
     C                   Z-ADD     SCCHK#        PACHK#
     C                   Z-ADD     SCSEQ         PABASQ
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C*
     C                   Z-ADD     SCFEIN        PAFEIN
     C                   Z-ADD     SCMOD         PAMOD
     C                   Z-ADD     SCAMT         PAAMT
     C                   MOVE      SCBR2         PABR2
     C                   MOVE      SCBR7         PABR7
     C                   Z-ADD     *ZERO         PAFRDT
     C                   Z-ADD     *ZERO         PATODT
     C                   Z-ADD     SCWKS         PAWKS
     C                   Z-ADD     SCDYWK        PADAYS
     C                   MOVE      SCTRCD        PATRCD
     C                   MOVE      SCTYPE        PATYPE
     C                   MOVE      PCRECD        PARECD
     C                   MOVE      RTTYPE        PARETP
     C                   MOVEL     SCTRDS        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      *BLANK        PAADST
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C                   Z-ADD     SCEOB#        PAEOB#
     C                   Z-ADD     SCRCN         PAEOBS
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
     C*
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVE      ##CKTP        PACKTP
     C                   MOVEL     ##ACCT        PAACCT
     C*
     C                   CLEAR                   PAPGTP
     C                   CLEAR                   PARCVD
     C                   CLEAR                   PADUE
     C                   CLEAR                   PALTCD
     C                   CLEAR                   PADPCD
     C                   CLEAR                   PADPCU
     C                   CLEAR                   PAFNAL
     C                   CLEAR                   PARCDT
     C                   CLEAR                   PARADJ
     C*
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C                   WRITE     PPA02
      /Free
       //*** Begin Add *** 07/10/19 *******************************************

       //IDC processing for refund payment.

        If isModEnabled('IDC') and PAretp = 'C';
           intRRN =DBrrn2;
           WriteTxLog(intRRN);
        Endif;
       //*** End   Add *** 07/10/19 *******************************************
      /End-Free
     C*
     C*  Write a View Check record.
     C*
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C                   Z-ADD     PATRDT        RPTRDT
     C                   MOVEL     SCRMDS        RPRMDS
     C                   MOVEL     SCRMK1        RPRMK1
     C                   MOVEL     SCRMK2        RPRMK2
     C                   MOVEL     SCRMK3        RPRMK3
     C                   MOVE      *BLANKS       RPCKCN
     C                   MOVEL     SCPNAM        RPPNAM
     C                   MOVEL     SCADR1        RPADR1
     C                   MOVEL     SCADR2        RPADR2
     C                   MOVEL     SCCTY         RPCTY
     C                   MOVEL     SCST          RPST
     C                   MOVEL     SCZP1         RPZIP
     C                   MOVE      SCZIP2        RPZIP
     C                   MOVE      *ZEROS        RPBUL1
     C                   MOVEL(P)  SCCTRY        RPCTRY
     C                   MOVEL(P)  SCADR3        RPADR3
     C*
     C                   MOVE      'A'           RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   WRITE     PRP02
     C*
     C*  IF THE CLAIM IS CLOSED, CALL SRPRCC TO UPDATE THE RESERVES
     C*  FOR THIS RECOVERY
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C                   MOVE      'F'           QQTYPE
     C*
     C                   CALL      'SRPRCC'      PRCCPL
     C*
     C                   ENDIF
     C*
     C*  WRITE THE REFUND REQUEST HERE
     C*
     C                   Z-ADD     SCFND         RRFND
     C                   Z-ADD     SCFYR         RRFYR
     C                   Z-ADD     SCREQ#        RRREQ#
     C                   Z-ADD     XXCO#         RRCO#
     C                   Z-ADD     SCCASE        RRCASE
     C                   Z-ADD     SCFEIN        RRFEIN
     C                   Z-ADD     SCMOD         RRMOD
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDTRC
     C*
     C                   SUB       SCAMT         RRRAMT
     C                   ADD       SCAMT         RRRCVD
     C*
     C                   MOVEL     SCTRDS        RRDESC
     C                   MOVEL     Q1USER        RRUSER
     C                   MOVEL     Q1PGM         RRPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDT
     C*
     C                   Z-ADD     DSTIME        RRTIME
     C*
     C     *IN58         IFEQ      *ON
     C                   Z-ADD     SCAMT         RRRCVD
     C                   Z-ADD     *ZERO         RRRAMT
     C                   MOVE      'A'           RRSTAT
     C                   WRITE     PRR01
     C                   ELSE
     C                   UPDATE    PRR01
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     SVCMX         X
     C                   ADD       SCAMT         RR(X)
     C*
     C                   SUB       SCAMT         AR(X)
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     Q1USER        CMUSER
     C                   MOVEL     Q1PGM         CMPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C*
     C                   UPDATE    PCM01
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*
     C*
     C                   SUB       SCAMT         ELTCLP
     C                   ADD       SCAMT         ELOUTS
     C*
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   CLEAR                   SCR04
     C*
     C                   CLEAR                   SCFRDT
     C                   CLEAR                   SCTODT
     C                   CLEAR                   SVFDT
     C                   CLEAR                   SVTDT
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      TOP
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C                   MOVE      *BLANK        Q2RECF
     C*
     C*===============================================================
     C*  SRPREM - PRE-PROCESSING FOR MANUAL PAYMENTS
     C*===============================================================
     C*
     C     SRPREM        BEGSR
      *
      *  Assign 1 line Function Key legend for SCR03
      *
     C                   Z-ADD     1             CK3               3 0
     C                   MOVE(P)   MODE(1)       SCMODE
     C     WCFL10        COMP      'Y'                                    52
     C                   MOVE      FK3(CK3)      SCFK3
     C     ##CLT#        IFEQ      '139'
     C                   MOVE(P)   FK3X(CK3)     SCFK3
     C                   ENDIF
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C*  Initialize Indicators 21 thru 26 - Screen Heading Line 3
     C*
     C                   SETOFF                                       212223
     C                   SETOFF                                       2526
     C*
     C     XXEXRE        COMP      'Y'                                    21
     C     CMCVFL        COMP      'Y'                                    22
     C     CMDNY         COMP      'Y'                                    23
     C     CMSUBR        COMP      'Y'                                    26
     C     CMSUBR        COMP      'R'                                    26
     C*
     C     CMKY          CHAIN     PAC01                              60
     C     *IN60         COMP      *OFF                                   25
     C*
     C     ##RETP        IFNE      'C'
     C                   MOVE      '1'           *IN27
     C                   ELSE
     C                   MOVEA     '1101'        *IN(82)
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN75
     C                   MOVE      *BLANKS       SCRMK1
     C                   MOVE      *BLANKS       SCRMK2
     C                   MOVE      *BLANKS       SCRMK3
     C                   MOVE      *BLANKS       SCRMDS
     C                   MOVE      *BLANKS       SCPNAM
     C                   MOVE      *BLANKS       SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   MOVE      *BLANKS       SCCTY
     C                   MOVE      *BLANKS       SCST
     C                   MOVE      *BLANKS       SCZP1
     C                   MOVE      *BLANKS       SCZIP2
     C                   MOVE      *BLANKS       SCCHK#
     C                   MOVE      *BLANKS       SCFRDT
     C                   MOVE      *BLANKS       SCTODT
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCMOD
     C*
     C                   Z-ADD     *ZEROS        SCWKS
     C                   Z-ADD     *ZEROS        SCDYWK
     C                   Z-ADD     *ZEROS        SVWKS
     C                   Z-ADD     *ZEROS        SVDYWK
     C*
     C                   MOVE      *BLANKS       SCBR2
     C                   MOVE      *BLANKS       SCBR7
     C                   MOVE      *BLANKS       SCAMT
     C                   MOVEL     *BLANKS       SCPOTM            2
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCRCN
     C                   CLEAR                   SVVFLG
      //** Begin Add *** 08/29/19 *********************************************
      /Free
       If isModEnabled('IDC');
          isIDC = *On;
          DsbLmpS = *Off;
       Else;
          isIDC = *Off;
          SClmpS = 'N';
          DsbLmpS = *On;
       Endif;
      /End-Free
      //** End   Add *** 08/29/19 *********************************************
     C*
     C*  If this is a comp pmt, load the claimant info into the payee fields.
     C*
     C     ##RETP        IFEQ      'C'
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C     CDFSN         CAT       CDMID:1       SCPNAM           40
     C                   CAT       CDLSN:1       SCPNAM
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  CDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  CDCTRY        SCCTRY
     C                   MOVE      *ON           *IN05
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C                   ENDIF
     C*
     C                   MOVE      CDADR1        SCADR1
     C                   MOVE      CDADR2        SCADR2
     C                   MOVE      CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVEL     CDZIP         SCZP1
     C                   MOVE      CDZIP         SCZIP2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ##TYPE        IFEQ      'C'
     C                   MOVE      '1'           *IN43
     C                   MOVE      '1'           *IN67
     C                   ELSE
     C                   MOVE      '0'           *IN43
     C                   MOVE      '0'           *IN67
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SRPRER - PRE-PROCESSING FOR REFUNDS
     C*===============================================================
     C*
     C     SRPRER        BEGSR
     C*
     C                   CLEAR                   SCFEIN
     C                   CLEAR                   SCMOD
     C                   CLEAR                   SCPNAM
     C                   CLEAR                   SCADR3
     C                   CLEAR                   SCCTRY
     C                   CLEAR                   SCCTRD
     C                   CLEAR                   SCADR1
     C                   CLEAR                   SCADR2
     C                   CLEAR                   SCCTY
     C                   CLEAR                   SCST
     C                   CLEAR                   SCZP1
     C                   CLEAR                   SCZIP2
     C                   CLEAR                   SVVFLG
     C                   CLEAR                   SCRMK1
     C                   CLEAR                   SCRMK2
     C                   CLEAR                   SCRMK3
     C                   CLEAR                   SCRMDS
     C*
     C*  Initialize Indicators 21 thru 26 - Screen Heading Line 3
     C*
     C                   SETOFF                                       212223
     C                   SETOFF                                       26
     C*
     C     XXEXRE        COMP      'Y'                                    21
     C     CMCVFL        COMP      'Y'                                    22
     C     CMDNY         COMP      'Y'                                    23
     C     CMSUBR        COMP      'Y'                                    26
     C     CMSUBR        COMP      'R'                                    26
     C*
     C                   MOVE(P)   MODE(1)       SCMODE
     C                   CLEAR                   SCFRDT
     C                   CLEAR                   SCTODT
     C                   CLEAR                   SCVFLG
     C*
     C*  GET THE REQUEST INQUIRY IF THERE ARE REQUESTS
     C*
     C                   MOVE      *BLANKS       QQRTCD
     C                   Z-ADD     *ZERO         SCREQ#            5 0
     C*
      *** Beg Chg ***  08/19/14  ***********************************************
     C**** RRKY1         CHAIN     PRR01                              60
     C     RRKY1         CHAIN(N)  PRR01                              60
      *** End Chg ***  08/19/14  ***********************************************
     C     *IN60         IFEQ      *OFF
     C*
     C                   CALL      'W0600R'                             60
     C                   PARM                    QQPRDS
     C                   PARM      ' '           QQCALL
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    SCREQ#
     C                   PARM                    QQRTCD
     C                   PARM                    SCFND
     C*
     C                   ENDIF
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C     SCREQ#        IFEQ      *ZERO
     C*
     C*   FIND NEXT LOGICAL ENTRY FILED
     C*
     C     RRKY1         SETGT     PRR01
     C     RRKY1         READPE(N) PRR01                                  60
     C*
     C   60              Z-ADD     *ZERO         RRREQ#
     C     RRREQ#        ADD       1             SCREQ#
     C*
     C                   MOVE      *ON           *IN27
     C                   Z-ADD     *ZERO         SCFEIN
     C                   Z-ADD     *ZERO         SCMOD
     C                   Z-ADD     *ZERO         RRRAMT
     C                   Z-ADD     *ZERO         RRRCVD
     C                   MOVE      *BLANK        RRDESC
     C                   MOVE      *BLANK        RRAFFL
     C                   MOVE      *BLANK        RREPFL
     C                   MOVE      *BLANK        RRMEFL
     C                   MOVE      *BLANK        RRPEFL
     C                   MOVE      *BLANK        RRCOM1
     C                   MOVE      *BLANK        RRCOM2
     C                   Z-ADD     DSDATE        RRDTRC
     C                   Z-ADD     *ZERO         RRDTRE
     C                   Z-ADD     *ZERO         RRDTRQ
     C*
     C*  GET A RESERVE CODE OF CATEGORY TYPE 'X' OR TYPE 'X'
     C*
     C                   MOVE      *BLANKS       RRSTCD
     C*
     C     TBKY1         CHAIN     PRS01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     RSCTGY        IFEQ      'X'
     C     RSTYPE        OREQ      'X'
     C*
     C     RSSTAT        IFNE      'I'
     C                   MOVEL     RSKEY         RRSTCD
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     TBKY1         READE     PRS01                                  60
     C*
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       RRSTTP
     C                   Z-ADD     *ZERO         RRATTY
     C                   Z-ADD     *ZERO         RRAMOD
     C                   Z-ADD     *ZERO         RREAMT
     C*
     C*       Set flag to indicate no request selected
     C                   MOVE      'N'           RQSTSL            1
     C*
     C                   ELSE
     C*
     C*       Set flag to indicate a request was selected
     C                   MOVE      'Y'           RQSTSL
     C*
      *** Beg Chg ***  08/19/14  ***********************************************
     C***  RRKY          CHAIN     PRR01                              60
     C     RRKY          CHAIN(N)  PRR01                              60
      *** End Chg ***  08/19/14  ***********************************************
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     RRFEIN        SCFEIN
     C                   Z-ADD     RRMOD         SCMOD
     C*
     C                   EXSR      ASGNNM
     C*
     C                   MOVEA     '1111'        *IN(82)
     C*
     C     RRSTAT        IFEQ      'I'
     C*
     C                   MOVEL(P)  ER(4)         MSGALL
     C*
     C                   MOVEL(P)  PACHK#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         MSG4
     C*
     C                   MOVEL(P)  MSGALL        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCDE78
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   CLEAR                   SCFEIN
     C                   CLEAR                   SCMOD
     C                   Z-ADD     *ZERO         RRRAMT
     C                   Z-ADD     *ZERO         RRRCVD
     C                   MOVE      *BLANK        RRDESC
     C                   MOVE      *BLANK        RRAFFL
     C                   MOVE      *BLANK        RREPFL
     C                   MOVE      *BLANK        RRMEFL
     C                   MOVE      *BLANK        RRPEFL
     C                   MOVE      *BLANK        RRCOM1
     C                   MOVE      *BLANK        RRCOM2
     C                   Z-ADD     DSDATE        RRDTRC
     C                   Z-ADD     *ZERO         RRDTRE
     C*
     C*  GET A RESERVE CODE OF CATEGORY TYPE 'X' OR TYPE 'X'
     C*
     C                   MOVE      *BLANKS       RRSTCD
     C*
     C     TBKY1         CHAIN     PRS01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     RSCTGY        IFEQ      'X'
     C     RSTYPE        OREQ      'X'
     C*
     C     RSSTAT        IFNE      'I'
     C                   MOVEL     RSKEY         RRSTCD
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     TBKY1         READE     PRS01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       RRSTTP
     C                   Z-ADD     *ZERO         RRATTY
     C                   Z-ADD     *ZERO         RRAMOD
     C                   Z-ADD     *ZERO         RREAMT
     C*
     C                   MOVE      'N'           RQSTSL
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C                   Z-ADD     RRRAMT        SCAMT
     C                   MOVE      *BLANKS       SCCHK#
     C                   MOVEL     PCDESC        SCTRDS
     C                   MOVE      *BLANK        SCBR2
     C                   MOVE      *BLANK        SCBR7
     C*
     C                   Z-ADD     *ZERO         SCWKS
     C                   Z-ADD     *ZERO         SCDYWK
     C                   Z-ADD     *ZERO         SVWKS
     C                   Z-ADD     *ZERO         SVDYWK
     C*
     C                   MOVE      RRFEIN        SCFEIN
     C                   MOVE      RRMOD         SCMOD
     C                   Z-ADD     0             SCEOB#
     C                   Z-ADD     0             SCRCN
     C*
     C*   DISPLAY SEARCH LIST OF ALL PAYMENTS FOR THIS CLAIM
     C*                              If no request was selected
     C*
     C     RQSTSL        IFEQ      'N'
     C*
     C                   MOVEL     SCTRCD        SVTRCD
     C*
     C                   Z-ADD     DBRRNC        ##RRN
     C                   CALL      'W0620R'                             60
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      'A'           ##TYPE            1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     *INKL         CABEQ     *ON           STRT
     C*
     C                   MOVEL     SVTRCD        SCTRCD
     C                   CLEAR                   SCSEQ
     C*
     C     ##RRN         CHAIN(N)  WDPAP                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     PASEQ#        SCSEQ
     C                   Z-ADD     SCSEQ         XXSEQ
     C     APKY1         CHAIN(N)  PPA02                              60
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C     SCSEQ         IFNE      *ZERO
     C     *IN60         ANDEQ     *OFF
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFRDT
     C                   MOVEL     SRDTO         SVFDT
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCTODT
     C                   MOVEL     SRDTO         SVTDT
     C*
     C                   MOVE      PAAMT         SCAMT
     C*
     C*  GET PROVIDER INFO
     C*
     C                   MOVE      PAFEIN        SCFEIN
     C                   MOVE      PAMOD         SCMOD
     C*
     C                   EXSR      ASGNNM
     C*
     C                   MOVEA     '1111'        *IN(82)
     C                   MOVE      '0'           *IN27
     C*
     C                   MOVE      PABR2         SCBR2
     C                   MOVEL(P)  SCBR2         XXKEY
     C                   MOVE      *BLANK        SCDBR2
     C     TBKY          CHAIN     PB201                              60
     C   60              MOVEL     B2DESC        SCDBR2
     C                   MOVE      PABR7         SCBR7
     C                   MOVEL(P)  SCBR7         XXKEY
     C                   MOVE      *BLANK        SCDBR7
     C     TBKY          CHAIN     PB701                              60
     C   60              MOVEL     B2DESC        SCDBR7
     C*
     C                   Z-ADD     PAWKS         SCWKS
     C                   Z-ADD     PADAYS        SCDYWK
     C                   Z-ADD     PAWKS         SVWKS
     C                   Z-ADD     PADAYS        SVDYWK
     C*
     C                   Z-ADD     PAEOB#        SCEOB#
     C                   Z-ADD     PAEOBS        SCRCN
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SRPREA - PRE-PROCESSING FOR ADJUSTMENTS
     C*===============================================================
     C*
     C     SRPREA        BEGSR
     C*
     C                   Z-ADD     SCSEQ         XXSEQ
     C*
     C     APKY1         CHAIN(N)  PPA02                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   MOVE(P)   MODE(2)       SCMODE
     C                   MOVE      PATYPE        SVTYPE
     C                   MOVE      PAPGTP        SCPGTP
     C                   Z-ADD     XXSEQ         SVSEQ
     C                   MOVE      SCTRCD        SCOFCD
     C*
     C     PCRECD        IFNE      PARECD
     C                   MOVE      *BLANK        SCOFCD
     C                   ENDIF
     C*
     C                   MOVE      SCTRCD        SCCODC
     C                   Z-ADD     PAFYR         SCOFYR
     C                   Z-ADD     PAFYR         SCCOYR
     C                   MOVE      PACASE        SCOFCA
     C                   MOVE      PACASE        SCCOCA
     C                   MOVE      PABR2         SCOFB2
     C                   MOVE      PABR2         SCCOB2
     C                   MOVE      PABR7         SCOFB7
     C                   MOVE      PABR7         SCCOB7
     C                   MOVE      PACHK#        SCOFCH
     C                   MOVE      PACHK#        SCCOCH
     C                   MOVEL     PADESC        SCOFDS
     C                   MOVEL     PADESC        SCCODS
     C                   MOVE      PAFEIN        SCOFFE
     C                   MOVE      PAFEIN        SCCOFE
     C                   MOVE      PAMOD         SCOFMO
     C                   MOVE      PAMOD         SCCOMO
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOFFR
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCOFTO
     C*
     C                   Z-ADD     SCOFFR        SCCOFR
     C                   Z-ADD     SCOFTO        SCCOTO
     C                   Z-ADD     SCOFFR        SVFDT2
     C                   Z-ADD     SCOFTO        SVTDT2
     C*
     C                   Z-ADD     PAWKS         SCCOWK
     C                   Z-ADD     PADAYS        SCCODY
     C                   Z-ADD     PAWKS         SVCOWK
     C                   Z-ADD     PADAYS        SVCODY
     C*
     C                   MOVE      PAWKS         SCOFWK
     C                   MOVE      PADAYS        SCOFDY
     C                   MOVE      PAAMT         SCOFAM
     C                   MOVE      PAAMT         SCCOAM
     C                   MOVE      PCTRTP        SCTYPE
     C                   MOVE      PARETP        SCOFTP
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCOFPO
     C                   MOVEL     SRDTO         SCCOPO
     C*
     C                   Z-ADD     SCCASE        SCCAS
     C                   Z-ADD     PAEOB#        XXEOB#
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SROFAJ - This subroutine writes the offsetting adjustment
     C*  and updates the claim and employer totals.
     C*===============================================================
     C     SROFAJ        BEGSR
     C*
     C                   Z-ADD     SCOFYR        SCFYR
     C                   Z-ADD     SCOFCA        SCCASE
     C     CMKY          CHAIN(N)  PCM01                              60
     C*
     C                   Z-ADD     *HIVAL        XXSEQ
     C     APKY1         SETGT     PPA02
     C     APKY          READPE(N) PPA02                                  60
     C   60              Z-ADD     *ZERO         PASEQ#
     C     PASEQ#        ADD       1             XXSEQ#
     C*
     C                   CLEAR                   PPA02
     C*
     C                   Z-ADD     XXSEQ#        PASEQ#
     C*
     C                   MOVE      XXCO#         PACO#
     C                   MOVE      SCFND         PAFND
     C                   Z-ADD     SCOFYR        PAFYR
     C                   Z-ADD     SCOFCA        PACASE
     C                   Z-ADD     SCOFCH        PACHK#
     C                   Z-ADD     SCSEQ         PABASQ
     C                   Z-ADD     SCCOYR        PAXFYR
     C                   Z-ADD     SCCOCA        PAXCAS
     C*
     C     SVTYPE        IFEQ      'R'
     C                   Z-ADD     SCOFAM        PAAMT
     C                   MOVE      *BLANKS       PAADST
     C                   MOVE      'Y'           PARADJ
     C                   ELSE
     C                   Z-SUB     SCOFAM        PAAMT
     C                   MOVE      SVTRTP        PAADST
     C                   MOVE      *BLANKS       PARADJ
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     SCOFPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C*
     C                   Z-ADD     SCOFFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
     C*
     C                   Z-ADD     SCOFTO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
     C*
     C                   Z-ADD     SCOFFE        PAFEIN
     C                   Z-ADD     SCOFMO        PAMOD
     C                   MOVE      SCOFB2        PABR2
     C                   MOVE      SCOFB7        PABR7
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVEL     SVACCD        PACKTP
     C                   MOVEL     SVACCT        PAACCT
     C                   MOVE      SCOFWK        PAWKS
     C                   MOVE      SCOFDY        PADAYS
     C                   MOVE      SCOFCD        PATRCD
     C                   MOVE      SVTRTP        PATYPE
     C                   MOVE      SV1REC        PARECD
     C                   MOVE      SV1RTP        PARETP
     C                   MOVEL     SCOFDS        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      Q1USER        PARQST
     C                   Z-ADD     SVEOB#        PAEOB#
     C                   Z-ADD     SVEOBS        PAEOBS
     C                   Z-ADD     SVDPCD        PADPCD
     C                   MOVEL     SVDPCU        PADPCU
     C                   MOVE      SCPGTP        PAPGTP
     C                   MOVEL     SVLTCD        PALTCD
     C                   MOVEL     SVFNAL        PAFNAL
     C*
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   Z-ADD     DSTIME        PATIME
     C*
     C                   MOVE      SVDSQ#        PADSQ#
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C                   WRITE     PPA02
      /Free
       //*** Begin Add *** 07/10/19 *******************************************

       //IDC processing offsetting adjustment

        If isModEnabled('IDC') and PAretp = 'C';
           intRRN =DBrrn2;
           WriteTxLog(intRRN);
        Endif;
       //*** End   Add *** 07/10/19 *******************************************
      /End-Free
     C*
     C*  If the claim is closed, and the payment was transferred to
     C*  another claim or reserve, write a Reserve History record.
     C*
     C                   SETOFF                                           97
     C     CMSTAT        COMP      'C'                                    97
     C*
     C     CMSTAT        IFEQ      'C'
     C     SCOFCA        ANDNE     SCCOCA
     C     CMSTAT        OREQ      'C'
     C     SV1REC        ANDNE     SV2REC
     C                   MOVE      'O'           QQTYPE
     C                   CALL      'SRPRCC'      PRCCPL
     C                   SETOFF                                           97
     C                   ENDIF
     C*
     C*  Update the Claim Master file.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X                 2 0
     C     SV1REC        LOOKUP    RT(X)                                  60
     C*
     C  N60              Z-ADD     10            X
     C*
     C     SVTYPE        IFEQ      'R'
     C  N97              SUB       SCOFAM        RR(X)
     C                   ADD       SCOFAM        AR(X)
     C                   ELSE
     C  N97              ADD       SCOFAM        RR(X)
     C                   SUB       SCOFAM        AR(X)
     C                   SUB       1             CMPCTR
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELOUTS        SVOUTS
     C*
     C     SVTYPE        IFEQ      'R'
     C                   ADD       SCOFAM        ELTCLP
     C                   SUB       SCOFAM        ELOUTS
     C                   ELSE
     C                   SUB       SCOFAM        ELTCLP
     C                   ADD       SCOFAM        ELOUTS
     C                   ENDIF
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVOUTS        ELOUTS
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Write a View Check (Pay to) record.
     C*
     C                   Z-ADD     SCSEQ         XXSEQ#
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PAPODT        XXTRDT
     C     RPDKY         CHAIN     PRP02                              99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C*
     C*  Load the payee name and address
     C*
     C     SCOFFE        IFNE      0
     C*
     C*        Access Payee Demographic record
     C*
     C                   Z-ADD     SCOFFE        XXFEIN
     C                   Z-ADD     SCOFMO        XXMOD
     C     PDKY          CHAIN     PPD01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR1        RPPNAM
     C                   MOVEL     PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ELSE
     C                   MOVEL     PDNAME        RPPNAM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         RPPNAM
     C                   CAT       PDFSN:1       RPPNAM
     C                   END
     C                   MOVEL     PDADR1        RPADR1
     C                   MOVEL     PDADR2        RPADR2
     C                   ENDIF
     C*
     C                   MOVEL     PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVE      PDCTRY        RPCTRY
     C                   MOVE      PDADR3        RPADR3
     C                   END
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*        Access Claimant Demographic record
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      *OFF
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C*
     C                   MOVE      CDADR1        RPADR1
     C                   MOVE      CDADR2        RPADR2
     C                   MOVE      CDCTY         RPCTY
     C                   MOVE      CDST          RPST
     C                   MOVE      CDZIP         RPZIP
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C*
     C                   WRITE     PRP02
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SRCOAJ - This subroutine writes the correcting adjustment
     C*  and updates the claim and employer totals.
     C*===============================================================
     C     SRCOAJ        BEGSR
     C*
     C                   Z-ADD     SCCOYR        SCFYR
     C                   Z-ADD     SCCOCA        SCCASE
     C     CMKY          CHAIN(N)  PCM01                              60
     C*
     C                   Z-ADD     *HIVAL        XXSEQ
     C     APKY1         SETGT     PPA02
     C     APKY          READPE(N) PPA02                                  60
     C   60              Z-ADD     *ZERO         PASEQ#
     C     PASEQ#        ADD       1             XXSEQ#
     C*
     C                   CLEAR                   PPA02
     C*
     C                   Z-ADD     XXSEQ#        PASEQ#
     C*
     C                   MOVE      XXCO#         PACO#
     C                   MOVE      SCFND         PAFND
     C                   Z-ADD     SCCOYR        PAFYR
     C                   Z-ADD     SCCOCA        PACASE
     C                   Z-ADD     SCOFYR        PAXFYR
     C                   Z-ADD     SCOFCA        PAXCAS
     C                   Z-ADD     SCOFCH        PACHK#
     C*
     C                   Z-ADD     SCSEQ         PABASQ
     C     SCCOCA        IFNE      SCOFCA
     C                   Z-ADD     *ZERO         PABASQ
     C                   ENDIF
     C*
     C     SVTYPE        IFNE      'R'
     C                   Z-ADD     SCCOAM        PAAMT
     C                   MOVE      *BLANKS       PAADST
     C                   MOVE      *BLANK        PARADJ
     C                   ELSE
     C                   Z-SUB     SCCOAM        PAAMT
     C                   MOVE      'A'           PAADST
     C                   MOVE      'Y'           PARADJ
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     SCCOPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C*
     C                   Z-ADD     SCCOFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
     C*
     C                   Z-ADD     SCCOTO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
     C*
     C                   Z-ADD     SCCOFE        PAFEIN
     C                   Z-ADD     SCCOMO        PAMOD
     C                   MOVE      SCCOB2        PABR2
     C                   MOVE      SCCOB7        PABR7
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVE      SCCOWK        PAWKS
     C                   MOVE      SCCODY        PADAYS
     C                   MOVE      SCCODC        PATRCD
     C                   MOVE      SVTRTP        PATYPE
     C                   MOVE      SV2REC        PARECD
     C                   MOVE      SV2RTP        PARETP
     C                   MOVEL     SCCODS        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     SVACCD        PACKTP
     C                   MOVEL     SVACCT        PAACCT
     C                   MOVEL     SVLTCD        PALTCD
     C                   MOVEL     SVFNAL        PAFNAL
     C                   MOVE      SCPGTP        PAPGTP
     C                   MOVEL     SCPOTM        PALTCD
     C                   Z-ADD     SVEOB#        PAEOB#
     C                   Z-ADD     SVEOBS        PAEOBS
     C                   Z-ADD     SVDPCD        PADPCD
     C                   MOVEL     SVDPCU        PADPCU
     C*
     C                   MOVEL     Q1USER        PAUSER
     C                   MOVEL     Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   Z-ADD     DSTIME        PATIME
     C*
     C                   MOVE      SVDSQ#        PADSQ#
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C                   WRITE     PPA02
      /Free
       //*** Begin Add *** 07/10/19 *******************************************

       //IDC Processing for correcting adjustment.

        If isModEnabled('IDC') and PAretp = 'C';
           intRRN =DBrrn2;
           WriteTxLog(intRRN);
        Endif;
       //*** End   Add *** 07/10/19 *******************************************
      /End-Free
     C*
     C*  If the claim is closed, and the payment was transferred to
     C*  another claim or reserve, write a Reserve History record.
     C*
     C                   SETOFF                                           97
     C     CMSTAT        COMP      'C'                                    97
     C*
     C     CMSTAT        IFEQ      'C'
     C     SCOFCA        ANDNE     SCCOCA
     C     CMSTAT        OREQ      'C'
     C     SV1REC        ANDNE     SV2REC
     C                   MOVE      'C'           QQTYPE
     C                   CALL      'SRPRCC'      PRCCPL
     C                   SETOFF                                           97
     C                   ENDIF
     C*
     C*  Update the Claim Master file
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             X                 2 0
     C     SV2REC        LOOKUP    RT(X)                                  60
     C*
     C  N60              Z-ADD     10            X
     C*
     C     SVTYPE        IFEQ      'R'
     C  N97              ADD       SCCOAM        RR(X)
     C                   SUB       SCCOAM        AR(X)
     C                   ELSE
     C  N97              SUB       SCCOAM        RR(X)
     C                   ADD       SCCOAM        AR(X)
     C                   ADD       1             CMPCTR
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELOUTS        SVOUTS
     C*
     C     SVTYPE        IFEQ      'R'
     C                   SUB       SCCOAM        ELTCLP
     C                   ADD       SCCOAM        ELOUTS
     C                   ELSE
     C                   ADD       SCCOAM        ELTCLP
     C                   SUB       SCCOAM        ELOUTS
     C                   ENDIF
     C*
     C     CMSTAT        IFEQ      'C'
     C                   Z-ADD     SVOUTS        ELOUTS
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  Write a View Check (Pay to) record.
     C*
     C***** Beg Chg ***** 06/15/15 ****************************
     C*****              Z-ADD     SCSEQ         XXSEQ#
     C***** End Chg ***** 06/15/15 ****************************
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PAPODT        XXTRDT
     C     RPDKY         CHAIN     PRP02                              99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *ON
     C     SCCOFE        ORNE      SCOFFE
     C     SCCOMO        ORNE      SCOFMO
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C*
     C*  Load the payee name and address
     C*
     C     SCCOFE        IFNE      0
     C*
     C*        Access Payee Demographic record
     C*
     C                   Z-ADD     SCCOFE        XXFEIN
     C                   Z-ADD     SCCOMO        XXMOD
     C     PDKY          CHAIN     PPD01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR1        RPPNAM
     C                   MOVEL     PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ELSE
     C                   MOVEL     PDNAME        RPPNAM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         RPPNAM
     C                   CAT       PDFSN:1       RPPNAM
     C                   END
     C                   MOVEL     PDADR1        RPADR1
     C                   MOVEL     PDADR2        RPADR2
     C                   ENDIF
     C*
     C                   MOVEL     PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVE      PDCTRY        RPCTRY
     C                   MOVE      PDADR3        RPADR3
     C                   END
     C*
     C                   ELSE
     C*        Access Claimant Demographic record
     C*
     C                   MOVE      SCCOFE        XXSS#
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      *OFF
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C*
     C                   MOVE      CDADR1        RPADR1
     C                   MOVE      CDADR2        RPADR2
     C                   MOVE      CDCTY         RPCTY
     C                   MOVE      CDST          RPST
     C                   MOVE      CDZIP         RPZIP
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*        Access Claimant Demographic record
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      *OFF
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C*
     C                   MOVE      CDADR1        RPADR1
     C                   MOVE      CDADR2        RPADR2
     C                   MOVE      CDCTY         RPCTY
     C                   MOVE      CDST          RPST
     C                   MOVE      CDZIP         RPZIP
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C*
     C                   WRITE     PRP02
     C*
     C                   ENDSR
     C*
     C*
     C*===============================================================
     C*  SRVSZ - PROCESS VOIDS/STOPS/REVERSALS
     C*===============================================================
     C*
     C     SRVSZ         BEGSR
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   MOVE      *BLANKS       SRDFLD
     C     PARCDT        IFNE      *ZEROS
     C                   MOVEL(P)  ER(7)         SRDFLD
     C                   ENDIF
     C*
     C                   Z-ADD     1             X
     C     PARECD        LOOKUP    RT(X)                                  60
     C                   Z-ADD     RR(X)         XXREM
     C*
     C*  IF CLAIM IS OPEN:
     C*
     C     CMSTAT        IFEQ      'O'
     C*
     C*
     C*
     C*  IF REVERSAL TYPE AND RESERVES ARE INADEQUATE, LOAD MESSAGE
     C*
     C     SCTYPE        IFEQ      'Z'
     C*
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PARECD        IFEQ      RPRECD
     C     RPSTAT        ANDEQ     ' '
     C                   SUB       RPAMT         XXREM
     C                   ENDIF
     C*
     C     RPKY          READE     PRP01                                  60
     C                   ENDDO
     C*
     C     PAAMT         IFGT      XXREM
     C                   MOVEL(P)  ER(1)         SRDFLD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  IF VOID OR STOP TYPE AND TOTAL PAYMENTS AGAINST RESERVE CODE
     C*  ARE < SELECTED TRANSACTION, LOAD MESSAGE
     C*
     C     SCTYPE        IFEQ      'V'
     C     SCTYPE        OREQ      'S'
     C*
     C     PAAMT         IFGT      AR(X)
     C                   MOVEL(P)  ER(3)         SRDFLD
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  CENTER MESSAGE AND RETURN TO SCR01
     C*
     C     SRDFLD        IFNE      *BLANKS
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCDE78
     C                   GOTO      STRT
     C*
     C                   ENDIF
     C*
     C*
     C*  Process V/S/Z reason window.
     C*
     C                   EXSR      SRWDW2
     C*
     C*  TEST TRANSACTION CODE VALUE ENTERED.
     C*
     C                   MOVEL(P)  SCTRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C*
     C*  GET THE NEXT AVAILABLE SEQUENCE NUMBER
     C*
     C                   MOVE      *HIVAL        XXSEQ
     C     APKY1         SETGT     PPA02
     C     APKY          READPE(N) PPA02                                  60
     C*
     C   60              Z-ADD     *ZERO         PASEQ#
     C     PASEQ#        ADD       1             SVSEQ#
     C*
     C*  GET THE SELECTED TRANSACTION
     C*
     C                   Z-ADD     SCSEQ         XXSEQ
     C     ##RRN         CHAIN     WDPAP                              60
     C*
     C                   MOVE      PADSQ#        SVDSQ#
     C*  SET THE BASE SEQ NUMBER UP
     C*
     C     PABASQ        IFEQ      *ZERO
     C                   Z-ADD     SCSEQ         SVBASQ
     C                   ELSE
     C                   Z-ADD     PABASQ        SVBASQ
     C                   END
     C*
     C                   MOVEL     PACKTP        SVACCD
     C                   MOVEL     PAACCT        SVACCT
     C                   MOVEL     PALTCD        SVLTCD
     C                   MOVEL     PAFNAL        SVFNAL
     C*
     C*  UPDATE THE SELECTED RECORD.
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C                   MOVE      SCTYPE        PAADST
     C*
     C                   UPDATE    PPA05
     C*
     C                   Z-ADD     SVSEQ#        PASEQ#
     C                   Z-ADD     SVBASQ        PABASQ
     C                   MOVE      SVTRTP        PATYPE
     C                   MOVEL     PCDESC        PADESC
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   MOVEL     SRDTO         PATRDT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C                   MOVE      SCTRCD        PATRCD
     C*
     C     SCTYPE        IFEQ      'Z'
     C                   MOVE      SCTYPE        PAADST
     C                   ELSE
     C                   MOVE      *BLANK        PAADST
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         PARCVD
     C                   Z-ADD     *ZERO         PADUE
     C                   MOVE      *BLANK        PAPGTP
     C                   MOVEL     CMOFF#        PAOFF#
     C                   MOVEL     SVACCD        PACKTP
     C                   MOVEL     SVACCT        PAACCT
     C                   MOVEL     Q1USER        PARQST
     C                   MOVEL     SVLTCD        PALTCD
     C                   Z-ADD     *ZERO         PADPCD
     C                   MOVEL     *BLANK        PADPCU
     C                   MOVEL     SVFNAL        PAFNAL
     C                   Z-ADD     *ZERO         PARCDT
     C                   MOVEL     *BLANK        PARADJ
     C*
     C*  If bank account number is blank
     C*
     C     PAACCT        IFEQ      *BLANKS
     C*
     C                   MOVE      FNCO#         ##CO#
     C                   MOVE      FNFND         ##FND
     C*
     C*  Get Type of Payment from Group Auxilary File
     C*
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F2CTYP        ##CALL
     C                   ENDIF
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         ##SRDT
      *
      *  Call API to access "Current" Bank Account based on ##CALL value
      *
      *******Beg Chg********04/17/12**********************************
     C*****              CALL      'WA023R'
     C*****              PARM                    ##CO#             3
     C*****              PARM                    ##FND             3
     C*****              PARM                    XXAPPL
     C*****              PARM                    ##SRDT            8
     C*****              PARM                    ##ACCT           20
     C*****              PARM                    ##CALL            1
     C*****              PARM      *BLANK        ##HDCY            1
     C*****              PARM      *BLANK        ##CHKQ           10
     C*****              PARM      *BLANK        ##FTYP           10
     C*****              PARM      *BLANK        ##RTCD            1
     C*****              PARM      *ZERO         ##EFDT            7
      *
      /Free
       //Call the Procedure to retrieve Bank Account.
       ##eftc = '  ';
       ##coid = '  ';
       ##acct = GetBankAcc(##co#: ##fnd: xxappl: ##srdt: ##call:
                 ##hdcy: ##chkq: ##ftyp: ##efdt: ##cpyq:
                 ##cktp: ##eftc: ##coid: ##rtcd);
      /End-Free
      *******End Chg********04/17/12**********************************
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      ##ACCT        PAACCT
     C                   ELSE
     C                   MOVE      *BLANKS       PAACCT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   MOVE      SVDSQ#        PADSQ#
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C                   WRITE     PPA02
      /Free
       //*** Begin Add *** 07/10/19 *******************************************

       //IDC processing for voids/stops/reversals

        If isModEnabled('IDC') and PAretp = 'C';
           intRRN =DBrrn2;
           WriteTxLog(intRRN);
        Endif;
       //*** End   Add *** 07/10/19 *******************************************
      /End-Free
      *
     C*  Write a View Check record with the V/S/Z reason.
     C*
     C                   Z-ADD     PABASQ        XXSEQ#
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PAPODT        XXTRDT
     C     RPDKY         CHAIN     PRP02                              99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C                   MOVE      *BLANKS       RPRMDS
     C                   MOVE      *BLANKS       RPRMK1
     C                   MOVE      *BLANKS       RPRMK2
     C                   MOVE      *BLANKS       RPRMK3
     C                   MOVE      *BLANKS       RPCKCN
     C*
     C     PAFEIN        IFNE      0
      *
      *  Access Payee Demographic record
      *
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     PDNAME        RPPNAM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         RPPNAM
     C                   CAT       PDFSN:1       RPPNAM
     C                   END
     C*
     C                   MOVEL     PDADR1        RPADR1
     C                   MOVEL     PDADR2        RPADR2
     C                   MOVEL     PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
      *
     C     PDDBA1        IFEQ      'Y'
     C                   MOVE      PDADR1        RPPNAM
     C                   MOVE      PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ENDIF
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVE      PDCTRY        RPCTRY
     C                   MOVE      PDADR3        RPADR3
     C                   END
      *
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*        Access Claimant Demographic record
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      *OFF
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C*
     C                   MOVE      CDADR1        RPADR1
     C                   MOVE      CDADR2        RPADR2
     C                   MOVE      CDCTY         RPCTY
     C                   MOVE      CDST          RPST
     C                   MOVE      CDZIP         RPZIP
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C*
     C                   MOVE      SCRMDS        RPRMDS
     C                   MOVE      SCRMK1        RPRMK1
     C                   MOVE      SCRMK2        RPRMK2
     C                   MOVE      SCRMK3        RPRMK3
     C                   MOVE      'A'           RPSTAT
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   WRITE     PRP02
     C*
     C*  IF THE CLAIM IS CLOSED, CALL SRPRCC TO ALLOCATE THE RESERVES
     C*  FOR THIS PAYMENT.
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      SCTYPE        QQTYPE
     C                   CALL      'SRPRCC'      PRCCPL
     C                   ENDIF
     C*
     C*  IF THE REQUEST IS TO REVERSE A TRANSACTION-GET THE BASE RECORD
     C*
     C     SCTYPE        IFEQ      'Z'
     C*
     C                   MOVE      SVBASQ        XXSEQ
     C     APKY1         CHAIN     PPA02                              60
     C*
     C*  UPDATE THE SELECTED RECORD
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C*
     C                   Z-ADD     DSTIME        PATIME
     C                   MOVE      *BLANK        PAADST
     C*
     C                   UPDATE    PPA02
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER
     C*
     C                   Z-ADD     1             X                 2 0
     C     PARECD        LOOKUP    RT(X)                                  60
     C  N60              Z-ADD     10            X
     C*
     C     SCTYPE        IFEQ      'Z'
     C                   SUB       PAAMT         RR(X)
     C                   ADD       PAAMT         AR(X)
     C                   ADD       1             CMPCTR
     C                   ELSE
     C                   ADD       PAAMT         RR(X)
     C                   SUB       PAAMT         AR(X)
     C                   SUB       1             CMPCTR
     C                   ENDIF
     C*
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   UPDATE    PCM01
     C*
     C                   ENDIF
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     ELOUTS        SVOUTS
     C*
     C     SCTYPE        IFEQ      'V'
     C     SCTYPE        OREQ      'S'
     C                   SUB       PAAMT         ELTCLP
     C                   ADD       PAAMT         ELOUTS
     C                   ELSE
     C                   ADD       PAAMT         ELTCLP
     C                   SUB       PAAMT         ELOUTS
     C                   ENDIF
     C*
     C*  DO NOT UPDATE ELOUTS IF THE CLAIM IS CLOSED
     C*
     C*
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C*
     C                   END
     C*
     C                   CLEAR                   MSGALL
     C                   MOVEL     MS(1)         MSG1
     C*
     C                   MOVEL(P)  PACHK#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         MSG2
     C*
     C                   SELECT
     C     SCTYPE        WHENEQ    'Z'
     C                   MOVEL     MS(3)         MSG3
     C     SCTYPE        WHENEQ    'V'
     C                   MOVEL     MS(4)         MSG3
     C     SCTYPE        WHENEQ    'S'
     C                   MOVEL     MS(5)         MSG3
     C                   ENDSL
     C*
     C                   MOVEL(P)  MSGALL        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCDE78
     C*
     C                   CLEAR                   SCFYR
     C                   CLEAR                   SCCASE
     C                   CLEAR                   SCTRCD
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  SRWKDY - Subroutine to calculate weeks and days
     C*===============================================================
     C*
     C     SRWKDY        BEGSR
     C*
     C                   Z-ADD     *ZERO         XXDYS
     C                   MOVE      *OFF          QQERR
     C                   Z-ADD     *ZERO         XXWK
     C                   Z-ADD     *ZERO         XXDY
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQBEG             8 0
     C                   PARM                    QQEND             8 0
     C                   PARM                    XXDYS             5 0
     C                   PARM                    QQERR             1
     C*
     C     QQERR         IFEQ      *OFF
     C                   ADD       1             XXDYS
     C     XXDYS         DIV       7             XXWK
     C                   MVR                     XXDY
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C*  ASGNCS - Subroutine to Assign Screen Group, Year, and Case
     C*===============================================================
     C*
     C     ASGNCS        BEGSR
      *
     C     ##RRN         CHAIN     WMCMP                              62
      *
     C     *IN62         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C                   Z-ADD     CMCO#         KKCO#
     C                   Z-ADD     CMFND         KKFND
     C                   Z-ADD     CMFND         XXFND
     C                   Z-ADD     CMFYR         KKFYR
     C                   Z-ADD     CMCASE        KKCASE
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
      *
     C                   CALL      'WA006R'
     C                   PARM                    KKCO#
     C                   PARM                    KKFND
     C                   PARM                    KKFYR
     C                   PARM                    KKCASE
     C                   PARM                    SCCS01
     C                   PARM                    SCCS02
      *
     C                   MOVEL     S2NAME        SCCSNM
      *
     C     FNKY2         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
     C*===============================================================
     C*  ASGNNM - Subroutine to Assign "Pay To" Name/Address
     C*===============================================================
      *
     C     ASGNNM        BEGSR
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
      *
      *  Determine if "Pay To" is Claimant or Payee
      *
     C                   MOVE      SCFEIN        CCFEIN            9
      *
     C     SCFEIN        IFEQ      *ZERO
     C     CCFEIN        OREQ      CMSS#
      *
      *  Access Claimant Demographic record
      *
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCCNAM           40
     C     CDFSN         CAT       CDMID:1       SCCNAM
     C                   CAT       CDLSN:1       SCCNAM
     C                   MOVEL     SCCNAM        SCPNAM
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C*** End add ***** 01/11/06 **************************************
     C*** BEG ADD *** 10/20/03 *******************************************  ****
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  CDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  CDCTRY        SCCTRY
     C                   MOVE      *ON           *IN05
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   ENDIF
     C*
     C*** END ADD *** 10/20/03 *******************************************  ****
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVEL     CDZIP         SCZP1
     C                   MOVE      CDZIP         SCZIP2
     C                   ENDIF
     C                   ELSE
      *
      *  Access Payee Demographic record
      *
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
      *
     C     PDKY          CHAIN     PPD01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   EXSR      ASGNGP
      *
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C                   MOVEL     XXLAST        SCPNAM
     C                   MOVEL     PDADR1        SCADR1
     C                   MOVEL     PDADR2        SCADR2
     C                   MOVEL     PDCTY         SCCTY
     C                   MOVE      PDST          SCST
     C                   MOVEL     PDZIP         SCZP1
     C                   MOVE      PDZIP         SCZIP2
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVE      PDADR1        SCPNAM
     C                   MOVE      PDADR2        SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   ENDIF
     C*** END ADD ***** 04/25/00 **************************************
     C*** Begin add *** 01/11/06 **************************************
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C*** End add ***** 01/11/06 **************************************
     C*** Begin add *** 03/23/05 **************************************
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  PDADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  PDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  PDCTRY        SCCTRY
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C                   END
     C*** End add ***** 03/23/05 **************************************
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     SCFEIN        SVFEIN
     C                   Z-ADD     SCMOD         SVMOD
      *
     C                   ENDSR
      *
     C*===============================================================
      *  ASGNGP - Subroutine to Assign "Group" Provider Indicator
     C*===============================================================
      *
     C     ASGNGP        BEGSR
      *
     C     NRKY          CHAIN     PNR01                              60
      *
     C     *IN60         IFEQ      '0'
     C     NRSTAT        ANDEQ     'A'
     C                   SETON                                        24
     C                   ENDIF
      *
     C                   ENDSR
     C*
     C*===============================================================*
     C* PERFORM SCREEN THREE EDITS     ** Subroutine **   S C R 0 3 E *
     C*===============================================================*
     C*
     C     SCR03E        BEGSR
     C*
     C*  EDIT REMARK CODE 1
     C*
     C     SCRMK1        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              70
     C  N70RCSTAT        COMP      'I'                                    70
     C     *IN70         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT REMARK CODE 2
     C*
     C     SCRMK2        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              71
     C  N71RCSTAT        COMP      'I'                                    71
     C     *IN71         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT REMARK CODE 3
     C*
     C     SCRMK3        IFNE      *BLANK
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              72
     C  N72RCSTAT        COMP      'I'                                    72
     C     *IN72         IFEQ      '1'
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  EDIT REMARK CODE 4
     C*
     C*          SCRMK4    IFNE *BLANK
     C*                    MOVE *BLANKS   XXKEY
     C*                    MOVELSCRMK4    XXKEY
     C*          TBKY      CHAINPRC01                78
     C* N78      RCSTAT    COMP 'I'                      78
     C*          *IN78     IFEQ '1'
     C*                    GOTO STRT
     C*                    END
     C*                    END
     C*
     C*  IF A VALID PAYEE HAS BEEN ENTERED BUT NO PAYEE INFORMATION
     C*  HAS BEEN ENTERED, RETRIEVE DEMOGRAPHICS FROM FILE
     C*
     C     *IN43         IFEQ      '0'
     C     SCFEIN        ANDNE     *ZEROS
     C     SCADR1        ANDEQ     *BLANKS
     C*
     C*  LOAD PROPER PAYEE NAME INTO SCREEN FIELDS
     C*
     C                   Z-ADD     SCFEIN        XXFEIN
     C                   Z-ADD     SCMOD         XXMOD
     C     PDKY          CHAIN     PPD01                              60
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C     SCPNAM        IFEQ      *BLANKS
     C                   MOVEL     XXLAST        SCPNAM
     C                   END
     C                   MOVEL     PDADR1        SCADR1
     C                   MOVEL     PDADR2        SCADR2
     C                   MOVEL     PDCTY         SCCTY
     C                   MOVE      PDST          SCST
     C                   MOVEL     PDZIP         SCZP1
     C                   MOVE      PDZIP         SCZIP2
     C*** BEGIN ADD *** 04/25/00 **************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVE      PDADR1        SCPNAM
     C                   MOVE      PDADR2        SCADR1
     C                   MOVE      *BLANKS       SCADR2
     C                   ENDIF
     C*** END ADD ***** 04/25/00 **************************************
     C*** Begin add *** 03/23/05 **************************************
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  PDADR3        SCADR3
     C                   MOVEL(P)  'CTRY'        KKTBID
     C                   MOVEL(P)  PDCTRY        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCCTRD
     C                   ELSE
     C                   MOVEL(P)  NF            SCCTRD
     C                   END
     C                   MOVEL(P)  PDCTRY        SCCTRY
     C*
     C                   MOVE      *ON           *IN05
     C                   ELSE
     C                   MOVE      *OFF          *IN05
     C                   MOVE      *BLANKS       SCADR3
     C                   MOVE      *BLANKS       SCCTRD
     C                   MOVE      *BLANKS       SCCTRY
     C                   END
     C*** End add ***** 03/23/05 **************************************
     C*
     C                   END
     C*
     C                   ENDSR
     C**
     C*** BEGIN ADD *** 06/03/99 ***************************************
     C*===============================================================*
     C*  This routine accesses the work history file - WMWHL - to
     C*  edit payment service dates against Left Work and Returned
     C*  to Work history records.
     C*
     C*  The two possible errors are:
     C*          o prior record is Return to Work
     C*          o prior record is Left Work, but date is greater
     C*            than the FROM service date of the payment
     C*
     C*  If no work history record is found prior to the TO service
     C*  date, the claim master Return to Work date is used to edit
     C*  only the TO service date.
     C*===============================================================*
     C     CKRTW         BEGSR
     C*
     C*   Position work history file at TO service date; read prior.
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWKDT
     C     WHKY          SETGT     PWH01
     C     WHKY2         READPE    PWH01                                  17
     C*
     C     *IN17         IFEQ      *ON
     C                   Z-ADD     CMRTW         XXRTW
     C                   ELSE
     C*** BEGIN ADD *** 05/04/09 **************************************
     C     *IN17         DOWEQ     *OFF
     C*
     C* SKIP E TYPE ENTRIES
     C*
     C     WHWKST        IFNE      'E'
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C*   Set error indicators.
     C     WHWKST        IFEQ      'R'
     C                   Z-ADD     WHDATE        XXRTW
     C                   MOVE      *ON           *IN43
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   LEAVE
     C*** END   ADD *** 05/04/09 **************************************
     C                   ELSE
     C*
     C     WHWKST        IFEQ      'L'
     C                   Z-ADD     *ZERO         XXRTW
     C*
     C     WHDATE        IFGE      XXFRDT
     C                   Z-ADD     SCFRDT        SCFDT1
     C                   MOVE      *ON           *IN57
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   LEAVE
     C*** END   ADD *** 05/04/09 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 05/04/09 **************************************
     C                   ENDIF
     C     WHKY2         READPE    PWH01                                  17
     C                   ENDDO
     C*** END   ADD *** 05/04/09 **************************************
     C*
     C                   ENDIF
     C                   MOVE      *ON           WHFLAG
     C*
     C                   ENDSR
     C*** END ADD ***** 06/03/99 ***************************************
     C**
     C*===============================================================*
     C*  THIS SUBROUTINE EDITS THE RESERVES OF THE 'FROM' CLAIM FOR   *
     C*  EOMB PAYMENTS BEING TRANSFERRED.                             *
     C*===============================================================*
     C     CHKRS1        BEGSR
     C*
     C                   MOVE      '1'           *IN95
     C                   MOVE      *BLANKS       RECD
     C                   MOVE      *BLANKS       RETP
     C                   Z-ADD     0             REM
     C                   Z-ADD     0             SEQ
     C                   Z-ADD     SCOFYR        XXFYR
     C                   Z-ADD     SCOFCA        XXCASE
     C*
     C     PAKY          CHAIN     PPA03                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     1             Z                 5 0
     C     SVSEQ         IFEQ      P3SEQ#
     C     SV1REC        LOOKUP    RECD(Z)                                65
     C                   ELSE
     C     P3RECD        LOOKUP    RECD(Z)                                65
     C                   END
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    RECD(Z)                                65
     C     SVSEQ         IFEQ      P3SEQ#
     C   65              MOVE      SV1REC        RECD(Z)
     C   65              MOVE      SV1RTP        RETP(Z)
     C                   ELSE
     C   65              MOVE      P3RECD        RECD(Z)
     C   65              MOVE      P3RETP        RETP(Z)
     C                   END
     C                   END
     C   65              ADD       P3AMT         REM(Z)
     C*
     C*  SAVE THE PAYMENT SEQ#
     C*
     C                   Z-ADD     1             Z
     C     *ZERO         LOOKUP    SEQ(Z)                                 65
     C   65              Z-ADD     P3SEQ#        SEQ(Z)
     C*
     C     PAKY          READE     PPA03                                  60
     C                   END
     C*
     C                   Z-ADD     1             Z
     C     RECD(Z)       DOWNE     *BLANKS
     C                   Z-ADD     1             Y                 2 0
     C     RECD(Z)       LOOKUP    RT(Y)                                  65
     C                   MOVE      RETP(Z)       SVRETP            1
     C     *IN65         IFEQ      '0'
     C     REM(Z)        ORGT      AR(Y)
     C                   MOVE      '1'           *IN94
     C*
     C*  GET THE RESERVE TYPE TO CONDITION WHICH MESSAGE WILL APPEAR.
     C*
     C                   MOVEA     '0000'        *IN(90)
     C     SVRETP        COMP      'M'                                    90
     C     SVRETP        COMP      'C'                                    91
     C     SVRETP        COMP      'L'                                    92
     C     SVRETP        COMP      'E'                                    93
     C*
     C*  DISPLAY THE AMOUNT BY WHICH THE RESERVES MUST BE INCREASED.
     C*
     C   90              MOVEL(P)  MSG(1)        SRDFLD
     C   91              MOVEL(P)  MSG(2)        SRDFLD
     C   92              MOVEL(P)  MSG(3)        SRDFLD
     C   93              MOVEL(P)  MSG(4)        SRDFLD
     C*
     C   65REM(Z)        SUB       AR(Y)         SCRR
     C  N65              Z-ADD     REM(Z)        SCRR
     C*
     C                   MOVEL(P)  SCRR          XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRR             12
     C*
     C     SRDFLD        CAT       '$':1         SRDFLD
     C     SRDFLD        CAT       XXRR:0        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCMSG
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   ADD       1             Z
     C                   END
     C*
     C                   MOVE      '0'           *IN(95)
     C*
     C                   ENDSR
     C*===============================================================*
     C*  THIS SUBROUTINE EDITS THE RESERVES OF THE 'TO' CLAIM FOR EOMB*
     C*  PAYMENTS BEING TRANSFERRED.                                  *
     C*===============================================================*
     C     CHKRS2        BEGSR
     C*
     C                   MOVE      '1'           *IN96
     C                   MOVE      *BLANKS       RECD
     C                   MOVE      *BLANKS       RETP
     C                   Z-ADD     0             REM
     C                   Z-ADD     SCOFYR        XXFYR
     C                   Z-ADD     SCOFCA        XXCASE
     C*
     C     PAKY          CHAIN     PPA03                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     1             Z
     C     SVSEQ         IFEQ      P3SEQ#
     C     SV2REC        LOOKUP    RECD(Z)                                65
     C                   ELSE
     C     P3RECD        LOOKUP    RECD(Z)                                65
     C                   END
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     1             Z
     C     *BLANK        LOOKUP    RECD(Z)                                65
     C     SVSEQ         IFEQ      P3SEQ#
     C   65              MOVE      SV2REC        RECD(Z)
     C   65              MOVE      SV2RTP        RETP(Z)
     C                   ELSE
     C   65              MOVE      P3RECD        RECD(Z)
     C   65              MOVE      P3RETP        RETP(Z)
     C                   END
     C                   END
     C   65              ADD       P3AMT         REM(Z)
     C     PAKY          READE     PPA03                                  60
     C                   END
     C*
     C                   Z-ADD     SCCOYR        XXFYR
     C                   Z-ADD     SCCOCA        XXCASE
     C*
     C*** BEGIN ADD *** 04/22/99 **************************************
     C     CMSTAT        IFNE      'C'
     C*** END ADD ***** 04/22/99 **************************************
     C*
     C     RPKY1         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     1             Z
     C     RPRECD        LOOKUP    RECD(Z)                                65
     C   65              ADD       RPAMT         REM(Z)
     C                   END
     C     RPKY1         READE     PRP01                                  60
     C                   END
     C*
     C*** BEGIN ADD *** 04/22/99 **************************************
     C                   END
     C*** END ADD ***** 04/22/99 **************************************
     C*
     C                   Z-ADD     1             Z
     C     RECD(Z)       DOWNE     *BLANKS
     C                   Z-ADD     1             Y                 2 0
     C     RECD(Z)       LOOKUP    RT(Y)                                  65
     C                   MOVE      RETP(Z)       SVRETP            1
     C*
     C*** BEGIN ADD *** 04/22/99 **************************************
     C*  If the claim is closed, check the reserve cushion.
     C*
     C     CMSTAT        IFEQ      'C'
     C*
     C                   Z-ADD     0             XXCUSH
     C                   CALL      'SRCUSH'
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    RECD(Z)
     C                   PARM                    CMLTFL
     C                   PARM                    XXCUSH
     C*
     C                   ADD       XXCUSH        RR(Y)
     C*
     C                   END
     C*** END ADD ***** 04/22/99 **************************************
     C*
     C     *IN65         IFEQ      '0'
     C     REM(Z)        ORGT      RR(Y)
     C                   MOVE      '1'           *IN94
     C*
     C*  GET THE RESERVE TYPE TO CONDITION WHICH MESSAGE WILL APPEAR.
     C*
     C                   MOVEA     '0000'        *IN(90)
     C     SVRETP        COMP      'M'                                    90
     C     SVRETP        COMP      'C'                                    91
     C     SVRETP        COMP      'L'                                    92
     C     SVRETP        COMP      'E'                                    93
     C*
     C*  DISPLAY THE AMOUNT BY WHICH THE RESERVES MUST BE INCREASED.
     C*
     C   90              MOVEL(P)  MSG(5)        SRDFLD
     C   91              MOVEL(P)  MSG(6)        SRDFLD
     C   92              MOVEL(P)  MSG(7)        SRDFLD
     C   93              MOVEL(P)  MSG(8)        SRDFLD
     C*
     C   65REM(Z)        SUB       RR(Y)         SCRR
     C  N65              Z-ADD     REM(Z)        SCRR
     C*
     C                   MOVEL(P)  SCRR          XXNUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXRR             12
     C*
     C     SRDFLD        CAT       '$':1         SRDFLD
     C     SRDFLD        CAT       XXRR:0        SRDFLD
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      78            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCMSG
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C                   ADD       1             Z
     C                   END
     C*
     C                   MOVE      '0'           *IN(96)
     C*
     C                   ENDSR
     C*===============================================================*
     C*  THIS SUBROUTINE TRANSFERS THE EOMB PAYMENTS.                 *
     C*===============================================================*
     C     TRNSFR        BEGSR
     C*
     C                   Z-ADD     1             Z
     C     SEQ(Z)        DOWNE     0
     C*
     C                   Z-ADD     SCOFYR        XXFYR
     C                   Z-ADD     SCOFCA        XXCASE
     C                   Z-ADD     SEQ(Z)        XXSEQ
     C     PAKY1         CHAIN     PPA03                              60
     C     *IN60         IFEQ      '0'
     C*
     C     SVSEQ         COMP      P3SEQ#                                 97
     C*** BEGIN ADD *** 04/04/00 **************************************
     C     P3TYPE        IFEQ      'R'
     C                   MULT      -1            P3AMT
     C                   ENDIF
     C*** END ADD ***** 04/04/00 **************************************
     C*
     C*   UPDATE THE ORIGINAL PAYMENT RECORD
     C*
     C                   Z-ADD     SCOFYR        SCFYR
     C                   Z-ADD     SCOFCA        SCCASE
     C     APKY1         CHAIN     PPA02                              60
     C     *IN60         IFEQ      '0'
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      PADSQ#        SVDSQ#
     C*** END   ADD *** 08/09/07 **************************************
     C*** BEGIN ADD *** 10/10/02 **************************************
     C                   Z-ADD     SCCOYR        PAXFYR
     C                   Z-ADD     SCCOCA        PAXCAS
     C*** END   ADD *** 10/10/02 **************************************
     C                   MOVE      SCTYPE        PAADST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PADT
     C                   Z-ADD     DSTIME        PATIME
     C                   UPDATE    PPA02
     C                   END
     C*
     C*   WRITE THE OFFSETTING ADJUSTMENT RECORD
     C*
     C                   Z-ADD     99999         XXSEQ
     C     APKY1         SETGT     PPA02
     C     APKY          READPE(N) PPA02                                  60
     C   60              Z-ADD     *ZERO         PASEQ#
     C*** Begin dlt *** 12/07/01 **************************************
     C*******************  ADD  1         PASEQ#
     C*** End dlt ***** 12/07/01 **************************************
     C*** Begin add *** 12/07/01 **************************************
     C     PASEQ#        ADD       1             XXSEQ#
     C                   CLEAR                   PPA02
     C*** End add ***** 12/07/01 **************************************
     C*
     C                   Z-ADD     XXCO#         PACO#
     C                   Z-ADD     SCFND         PAFND
     C                   Z-ADD     P3FYR         PAFYR
     C                   Z-ADD     P3CASE        PACASE
     C*** BEGIN ADD *** 10/10/02 **************************************
     C                   Z-ADD     SCCOYR        PAXFYR
     C                   Z-ADD     SCCOCA        PAXCAS
     C*** END   ADD *** 10/10/02 **************************************
     C*** Begin add *** 12/07/01 **************************************
     C                   Z-ADD     XXSEQ#        PASEQ#
     C*** End add ***** 12/07/01 **************************************
     C                   Z-ADD     P3CHK#        PACHK#
     C                   Z-ADD     *ZERO         PABASQ
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C                   Z-ADD     SCOFPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C                   Z-ADD     P3FEIN        PAFEIN
     C                   Z-ADD     P3MOD         PAMOD
     C                   Z-SUB     P3AMT         PAAMT
     C                   MOVE      P3BR2         PABR2
     C                   MOVE      P3BR7         PABR7
     C                   Z-ADD     P3FRDT        PAFRDT
     C                   Z-ADD     P3TODT        PATODT
     C                   Z-ADD     P3WKS         PAWKS
     C                   Z-ADD     P3DAYS        PADAYS
     C                   MOVE      SCOFCD        PATRCD
     C                   MOVE      SVTRTP        PATYPE
     C                   MOVEL     SCOFDS        PADESC
     C                   MOVE      P3RECD        PARECD
     C                   MOVE      P3RETP        PARETP
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      SVTRTP        PAADST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   Z-ADD     PATRDT        PADT
     C                   Z-ADD     DSTIME        PATIME
     C                   Z-ADD     P3RCVD        PARCVD
     C                   Z-ADD     P3DUE         PADUE
     C                   Z-ADD     P3EOB#        PAEOB#
     C                   MOVE      P3PGTP        PAPGTP
     C*** Begin add *** 12/07/01 **************************************
     C                   MOVEL     P3CKTP        PACKTP
     C                   MOVEL     P3ACCT        PAACCT
     C                   MOVE      P3OFF#        PAOFF#
     C                   MOVEL     P3LTCD        PALTCD
     C                   MOVEL     P3DPCD        PADPCD
     C                   MOVEL     P3DPCU        PADPCU
     C                   MOVEL     P3RQST        PARQST
     C                   MOVEL     P3FNAL        PAFNAL
     C                   MOVEL     P3RCDT        PARCDT
     C                   MOVEL     P3RADJ        PARADJ
     C                   MOVEL     P3EOBS        PAEOBS
     C*** End add ***** 12/07/01 **************************************
     C*
     C     *IN97         IFEQ      '1'
     C                   MOVE      SV1REC        PARECD
     C                   MOVE      SV1RTP        PARETP
     C                   END
     C*
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      SVDSQ#        PADSQ#
     C*** END   ADD *** 08/09/07 **************************************
     C*** BEGIN ADD *** 08/29/11 **************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 ****************************************
     C                   WRITE     PPA02
      /Free
       //*** Begin Add *** 07/10/19 *******************************************

       //IDC processing for transfers (Offsetting Adj)        //Pending
        //If isModEnabled('IDC') and PAretp = 'C';
          //   intRRN =DBrrn2;
          //   WriteTxLog(intRRN);
        //Endif;
       //*** End   Add *** 07/10/19 *******************************************
      /End-Free
     C*** BEGIN ADD *** 12/15/11 **************************************
     C*
     C*  Write a View Check (Pay to) record.
     C*
     C*** Beg Chg *** 09/18/14 *********************************************
     C                   Z-ADD     SEQ(Z)        XXSEQ#
     C***********        Z-ADD     SCSEQ         XXSEQ#
     C*** End Chg *** 09/18/14 *********************************************
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PAPODT        XXTRDT
     C     RPDKY         CHAIN     PRP02                              99
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *ON
     C     SCCOFE        ORNE      SCOFFE
     C     SCCOMO        ORNE      SCOFMO
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C*
     C*  Load the payee name and address
     C*
     C     SCCOFE        IFNE      0
     C*
     C*        Access Payee Demographic record
     C*
     C                   Z-ADD     SCCOFE        XXFEIN
     C                   Z-ADD     SCCOMO        XXMOD
     C     PDKY          CHAIN     PPD01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR1        RPPNAM
     C                   MOVEL     PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ELSE
     C                   MOVEL     PDNAME        RPPNAM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         RPPNAM
     C                   CAT       PDFSN:1       RPPNAM
     C                   END
     C                   MOVEL     PDADR1        RPADR1
     C                   MOVEL     PDADR2        RPADR2
     C                   ENDIF
     C*
     C                   MOVEL     PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVE      PDCTRY        RPCTRY
     C                   MOVE      PDADR3        RPADR3
     C                   END
     C*
     C                   ELSE
     C*        Access Claimant Demographic record
     C*
     C                   MOVE      SCCOFE        XXSS#
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      *OFF
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C*
     C                   MOVE      CDADR1        RPADR1
     C                   MOVE      CDADR2        RPADR2
     C                   MOVE      CDCTY         RPCTY
     C                   MOVE      CDST          RPST
     C                   MOVE      CDZIP         RPZIP
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*        Access Claimant Demographic record
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      *OFF
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C*
     C                   MOVE      CDADR1        RPADR1
     C                   MOVE      CDADR2        RPADR2
     C                   MOVE      CDCTY         RPCTY
     C                   MOVE      CDST          RPST
     C                   MOVE      CDZIP         RPZIP
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C*
     C                   WRITE     PRP02
     C*** END   ADD *** 12/15/11 **************************************
     C*
     C*** BEGIN DLT *** 04/04/00 **************************************
     C*** BEGIN ADD *** 04/22/99 **************************************
     C*  IF THE OFFSETTING ADJUSTMENT CLAIM RECORD IS CLOSED USE
     C*  SRPRCC TO UPDATE THE RESERVES
     C**********
     C********** CMSTAT    IFEQ 'C'
     C**********           MOVE 'O'       QQTYPE
     C**********           CALL 'SRPRCC'  PRCCPL
     C**********           ENDIF                           CMSTAT=C
     C*** END ADD ***** 04/22/99 **************************************
     C*** END DLT ***** 04/04/00 **************************************
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C*** BEGIN CHG *** 04/04/00 **************************************
     C********** CMKY      CHAINPCM01                60
     C     CMKY          CHAIN(N)  PCM01                              60
     C*** END CHG ***** 04/04/00 **************************************
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN ADD *** 04/04/00 **************************************
     C*  IF THE OFFSETTING ADJUSTMENT CLAIM RECORD IS CLOSED USE
     C*  SRPRCC TO UPDATE THE RESERVES
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'O'           QQTYPE
     C                   CALL      'SRPRCC'      PRCCPL
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*** END ADD ***** 04/04/00 **************************************
     C*
     C*  TEST TRANSACTION CODE VALUE ENTERED.
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PATRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C*
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER.
     C*
     C                   Z-ADD     1             X                 2 0
     C   97PCRECD        LOOKUP    RT(X)                                  60
     C  N97PARECD        LOOKUP    RT(X)                                  60
     C  N60              Z-ADD     10            X
     C*
     C                   ADD       P3AMT         RR(X)
     C                   SUB       P3AMT         AR(X)
     C                   SUB       1             CMPCTR
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   UPDATE    PCM01
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   SUB       P3AMT         ELTCLP
     C                   ADD       P3AMT         ELOUTS
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C                   END
     C*
     C                   END
     C*
     C*   WRITE THE CORRECTING ADJUSTMENT RECORDS.
     C*
     C                   Z-ADD     99999         XXSEQ
     C                   Z-ADD     SCCOYR        SCFYR
     C                   Z-ADD     SCCOCA        SCCASE
     C     APKY1         SETGT     PPA02
     C     APKY          READPE(N) PPA02                                  60
     C   60              Z-ADD     *ZERO         PASEQ#
     C*** Begin dlt *** 12/07/01 **************************************
     C*******************  ADD  1         PASEQ#
     C*** End dlt ***** 12/07/01 **************************************
     C*** Begin add *** 12/07/01 **************************************
     C     PASEQ#        ADD       1             XXSEQ#
     C                   CLEAR                   PPA02
     C*** End add ***** 12/07/01 **************************************
     C*
     C                   Z-ADD     XXCO#         PACO#
     C                   Z-ADD     SCFND         PAFND
     C                   Z-ADD     SCCOYR        PAFYR
     C                   Z-ADD     SCCOCA        PACASE
     C*** BEGIN ADD *** 10/10/02 **************************************
     C                   Z-ADD     SCOFYR        PAXFYR
     C                   Z-ADD     SCOFCA        PAXCAS
     C*** END   ADD *** 10/10/02 **************************************
     C*** Begin add *** 12/07/01 **************************************
     C                   Z-ADD     XXSEQ#        PASEQ#
     C*** End add ***** 12/07/01 **************************************
     C                   Z-ADD     P3CHK#        PACHK#
     C                   Z-ADD     *ZERO         PABASQ
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C                   Z-ADD     SCCOPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAPODT
     C                   Z-ADD     P3FEIN        PAFEIN
     C                   Z-ADD     P3MOD         PAMOD
     C                   Z-ADD     P3AMT         PAAMT
     C                   MOVE      P3BR2         PABR2
     C                   MOVE      SCCOB7        PABR7
     C*
     C                   Z-ADD     SCCOFR        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAFRDT
     C*
     C                   Z-ADD     SCCOTO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATODT
     C*
     C                   MOVE      SCCOWK        PAWKS
     C                   MOVE      SCCODY        PADAYS
     C                   MOVE      SCCODC        PATRCD
     C                   MOVE      SVTRTP        PATYPE
     C                   MOVEL     SCCODS        PADESC
     C                   MOVE      P3RECD        PARECD
     C                   MOVE      P3RETP        PARETP
     C                   MOVE      *BLANK        PASTAT
     C                   MOVE      *BLANKS       PAADST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   Z-ADD     PATRDT        PADT
     C                   Z-ADD     DSTIME        PATIME
     C                   Z-ADD     P3RCVD        PARCVD
     C                   Z-ADD     P3DUE         PADUE
     C                   Z-ADD     P3EOB#        PAEOB#
     C                   MOVE      P3PGTP        PAPGTP
     C*** Begin add *** 12/07/01 **************************************
     C                   MOVEL     P3CKTP        PACKTP
     C                   MOVEL     P3ACCT        PAACCT
     C                   MOVEL     P3LTCD        PALTCD
     C                   MOVEL     P3DPCD        PADPCD
     C                   MOVEL     P3DPCU        PADPCU
     C                   MOVEL     P3RQST        PARQST
     C                   MOVEL     P3FNAL        PAFNAL
     C                   MOVEL     P3RCDT        PARCDT
     C                   MOVEL     P3RADJ        PARADJ
     C                   MOVEL     P3EOBS        PAEOBS
     C*
     C     CMKY          CHAIN(N)  PCM01                              60
     C  N60              MOVE      CMOFF#        PAOFF#
     C*** End add ***** 12/07/01 **************************************
     C*
     C     *IN97         IFEQ      '1'
     C                   Z-ADD     SCCOFE        PAFEIN
     C                   Z-ADD     SCCOMO        PAMOD
     C                   MOVE      SCCOB2        PABR2
     C                   MOVE      SV2REC        PARECD
     C                   MOVE      SV2RTP        PARETP
     C                   END
     C*
     C*** BEGIN ADD *** 08/09/07 **************************************
     C                   MOVE      SVDSQ#        PADSQ#
     C*** END   ADD *** 08/09/07 **************************************
     C*** BEGIN ADD *** 08/29/11 **************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** END ADD *** 08/29/11 ****************************************
     C                   WRITE     PPA02
      /Free
       //*** Begin Add *** 07/10/19 *******************************************

       //IDC processing for transfers (Correcting Adj)      //Pending

        //If isModEnabled('IDC') and PAretp = 'C';
          // intRRN =DBrrn2;
          // WriteTxLog(intRRN);
        //Endif;
       //*** End   Add *** 07/10/19 *******************************************
      /End-Free
     C*** BEGIN ADD *** 12/15/11 **************************************
     C*** Beg Chg *** 09/18/14 *********************************************
     C                   Z-ADD     SEQ(Z)        XXSEQ#
     C********           Z-ADD     SCSEQ         XXSEQ#
     C*** End Chg *** 09/18/14 *********************************************
     C     SCOFYR        IFEQ      SCCOYR
     C     SCOFCA        ANDEQ     SCCOCA
     C     R3KY          CHAIN     PRP03                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PAPODT        XXTRDT
     C     RPDKY         CHAIN     PRP02                              99
     C                   ENDIF
     C                   ELSE
     C     R3KY2         CHAIN     PRP03                              99
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PAPODT        XXTRDT
     C     RPDKY2        CHAIN     PRP02                              99
     C                   ENDIF
     C                   ENDIF
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     PACO#         RPCO#
     C                   Z-ADD     PAFND         RPFND
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PACHK#        RPCHK#
     C*
     C*  Load the payee name and address
     C*
     C     SCOFFE        IFNE      0
     C*
     C*        Access Payee Demographic record
     C*
     C                   Z-ADD     SCOFFE        XXFEIN
     C                   Z-ADD     SCOFMO        XXMOD
     C     PDKY          CHAIN     PPD01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL     PDADR1        RPPNAM
     C                   MOVEL     PDADR2        RPADR1
     C                   MOVE      *BLANKS       RPADR2
     C                   ELSE
     C                   MOVEL     PDNAME        RPPNAM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         RPPNAM
     C                   CAT       PDFSN:1       RPPNAM
     C                   END
     C                   MOVEL     PDADR1        RPADR1
     C                   MOVEL     PDADR2        RPADR2
     C                   ENDIF
     C*
     C                   MOVEL     PDCTY         RPCTY
     C                   MOVE      PDST          RPST
     C                   MOVE      PDZIP         RPZIP
     C     PDCTRY        IFNE      *BLANK
     C     PDCTRY        ANDNE     'USA'
     C                   MOVE      PDCTRY        RPCTRY
     C                   MOVE      PDADR3        RPADR3
     C                   END
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*        Access Claimant Demographic record
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKY          CHAIN     PCD01                              98
     C     *IN98         IFEQ      *OFF
     C     CDFSN         CAT(P)    CDMID:1       RPPNAM
     C                   CAT       CDLSN:1       RPPNAM
     C*
     C                   MOVE      CDADR1        RPADR1
     C                   MOVE      CDADR2        RPADR2
     C                   MOVE      CDCTY         RPCTY
     C                   MOVE      CDST          RPST
     C                   MOVE      CDZIP         RPZIP
     C     CDCTRY        IFNE      *BLANK
     C     CDCTRY        ANDNE     'USA'
     C                   MOVEL(P)  CDCTRY        RPCTRY
     C                   MOVEL(P)  CDADR3        RPADR3
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     PAFYR         RPFYR
     C                   Z-ADD     PACASE        RPCASE
     C                   Z-ADD     PATRDT        RPTRDT
     C                   Z-ADD     PASEQ#        RPSEQ#
     C*
     C                   MOVEL     Q1USER        RPUSER
     C                   MOVEL     Q1PGM         RPPGM
     C                   Z-ADD     PADT          RPDT
     C                   Z-ADD     PATIME        RPTIME
     C*
     C                   WRITE     PRP02
     C*** END   ADD *** 12/15/11 **************************************
     C*
     C*** BEGIN DLT *** 04/04/00 **************************************
     C*** BEGIN ADD *** 04/22/99 **************************************
     C*  IF THE CORRECTING ADJUSTMENT CLAIM RECORD IS CLOSED USE
     C*  SRPRCC TO UPDATE THE RESERVES
     C**********
     C********** CMSTAT    IFEQ 'C'
     C**********           MOVE 'C'       QQTYPE
     C**********           CALL 'SRPRCC'  PRCCPL
     C**********           ENDIF                           CMSTAT=C
     C*** END ADD ***** 04/22/99 **************************************
     C*** END DLT ***** 04/04/00 **************************************
     C*
     C* PROCESS THE CLAIM MASTER RECORDS EFFECTED.
     C*
     C*** BEGIN CHG *** 04/04/00 **************************************
     C********** CMKY      CHAINPCM01                60
     C     CMKY          CHAIN(N)  PCM01                              60
     C*** END CHG ***** 04/04/00 **************************************
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN ADD *** 04/04/00 **************************************
     C*  IF THE OFFSETTING ADJUSTMENT CLAIM RECORD IS CLOSED USE
     C*  SRPRCC TO UPDATE THE RESERVES
     C*
     C     CMSTAT        IFEQ      'C'
     C                   MOVE      'O'           QQTYPE
     C                   CALL      'SRPRCC'      PRCCPL
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*** END ADD ***** 04/04/00 **************************************
     C*
     C*  TEST TRANSACTION CODE VALUE ENTERED.
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     PATRCD        XXKEY
     C     TBKY          CHAIN     PPC01                              60
     C*
     C*  VALIDATE THE RESERVE CODE AGAINST THE CLAIM MASTER.
     C*
     C                   Z-ADD     1             X                 2 0
     C   97PCRECD        LOOKUP    RT(X)                                  60
     C  N97PARECD        LOOKUP    RT(X)                                  60
     C  N60              Z-ADD     10            X
     C*
     C                   SUB       P3AMT         RR(X)
     C                   ADD       P3AMT         AR(X)
     C                   ADD       1             CMPCTR
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C                   UPDATE    PCM01
     C*
     C*  UPDATE THE EMPLOYER DETAIL FILE WITH THE OUTSTANDING
     C*  RESERVE & TOTAL CLAIMS PAID.
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C                   ADD       P3AMT         ELTCLP
     C                   SUB       P3AMT         ELOUTS
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   ADD       1             Z
     C                   END
     C*** BEGIN ADD *** 10/10/02 **************************************
     C     ##MCIF        IFEQ      'Y'
     C     PAEOB#        IFNE      *ZEROS
     C*
     C                   CALL      'FA736R'
     C                   PARM      PACO#         $$CO#             3 0
     C                   PARM      PAFND         $$WCF#            3 0
     C                   PARM      SCOFYR        $$FFYR            3 0
     C                   PARM      SCCOYR        $$TFYR            3 0
     C                   PARM      SCOFCA        $$FCAS            7 0
     C                   PARM      SCCOCA        $$TCAS            7 0
     C                   PARM      PAEOB#        $$EOB#            9 0
     C*
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 10/10/02 **************************************
     C*
     C                   ENDSR
     C*
     C*===============================================================*
     C*  LATE REASON DETAIL EXPLANATION - REQUIRED   **  S U B R      *
     C*===============================================================*
     C     SCR06T        BEGSR
      *                                                                *
     C                   Z-ADD     DSDATE        SCDRDT
     C                   MOVEL     LRDART        SCRMCD
     C                   MOVE      *BLANKS       XXCOMM
     C     STR6          TAG
     C                   EXFMT     SCR06
     C*
     C*  F3=Exit
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C                   END
      *
     C     SCDS01        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN79
     C                   GOTO      STR6
     C                   ELSE
     C                   MOVE      '0'           *IN79
     C                   END
      *
     C     LRDART        IFNE      *BLANK
      *
      *  Late Reason code has DARTS Remark Code.
      *
     C*** BEGIN DEL *** 04/26/00 **************************************
     C********             MOVE *BLANK    C3COMM
     C********   XXCOMM    IFNE *BLANK
     C********             MOVELXXCOMM    C3COMM
     C********             ELSE
     C********   LRDART    CHAINPCT01                60
     C**N60***             MOVELCTDESC    C3COMM
     C********             ENDIF
      ********
     C********             MOVE *BLANKS   QCKEY
     C********             Z-ADDXXCO#     QCCO#
     C********             Z-ADDXXFND     QCFND
     C********             Z-ADDSCFYR     QCFYR
     C********             Z-ADDSCCASE    QCCASE
     C********             MOVELQCKEY     C3KEY
     C********             MOVE *BLANKS   C3CSID
     C********             TIME           DSDTTM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     C3CDAT
     C********             MOVE *BLANKS   C3DCID
     C********             MOVEL'W0070R'  C3PNAM
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     C3TRDT
     C********             Z-ADDDSTIME    C3TRTM
     C********             MOVE *ZEROS    C3SECL
     C********             MOVE 'N'       C3SBYL
     C********             MOVE 'A'       C3STAT
     C********             MOVELQ1USER    C3USER
     C********             MOVELQ1PGM     C3PGM
     C********             Z-ADDDSTIME    C3TIME
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     C3DT
     C********             MOVE Q1USER    C3IUSR
     C********             MOVE *BLANKS   C3FOLD
     C********             MOVE *ZEROS    C3DCDT
     C********             MOVE *BLANKS   C3DIFL
     C********             MOVE *BLANKS   C3APID
     C********             MOVE *BLANKS   C3IDOC
     C********             MOVE *BLANKS   C3IQUE
     C********             MOVE *BLANKS   C3IMGF
     C********             MOVE *BLANKS   C3IPTH
     C********             MOVE *ZEROS    C3PIDX
     C********             MOVE *BLANKS   C3QPTY
     C********             MOVE *BLANKS   C3DCTP
     C********             MOVELQ1PGM     C3IPGM
     C********                                                         *********
     C****BEGIN DEL *** 02/23/98 ***********************************************
     C**********           MOVE *BLANKS   C3SMCD
     C****END*  DEL *** 02/23/98 ***********************************************
     C********                                                         *********
     C********             MOVE *BLANKS   C3FL01
     C********             MOVE *BLANKS   C3FL02
     C********             MOVE *BLANKS   C3FL03
     C********             MOVE *BLANKS   C3FL04
     C********             MOVE *BLANKS   C3FL05
     C********             Z-ADDDSDATE    SRDTI
     C********             CALL 'SRCVTDAT'DTCYMD
     C********             MOVELSRDTO     C3IDT
     C********
     C********             MOVELLRDART    C3RMCD
     C********                                                         *********
     C****BEGIN ADD *** 02/23/98 ***********************************************
     C********                                                         *********
     C********   C3RMCD    CHAINPCT01                60
     C********                                                         *********
     C********   *IN60     IFEQ *OFF
     C********             MOVE CTSMCD    C3SMCD
     C********             ENDIF                           *IN60=OFF
     C********                                                         *********
     C****END*  ADD *** 02/23/98 ***********************************************
     C********
     C********
     C****GET*LAST SEQUENCE USED
     C********
     C********             MOVE *HIVAL    XXDSEQ  50
     C********   C2KY      SETGTPCM0I
     C********             READPPCM0I                    60
     C********   *IN60     IFEQ '0'
     C********   QCKEY     ANDEQPIKEY
     C********   PISEQ#    ADD  1         C3SEQ#
     C********             ELSE
     C********             Z-ADD0         C3SEQ#
     C********             ENDIF
      ********
     C********             WRITEPCM03
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
     C*** END DEL *** 04/26/00 ****************************************
     C*** BEGIN ADD *** 04/26/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     SCFYR         QCFYR
     C                   Z-ADD     SCCASE        QCCASE
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C     XXCOMM        IFNE      *BLANK
     C                   MOVEL(P)  XXCOMM        Q#COMM          256
     C                   ELSE
     C     LRDART        CHAIN     PCT01                              60
     C  N60              MOVEL(P)  CTDESC        Q#COMM
     C                   ENDIF
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  LRDART        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANKS       Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANKS       Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
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
     C*** END ADD *** 04/26/00 ****************************************
     C                   ENDIF
     C                   ENDSR
     C*                                                                *
     C*===============================================================*
     C*  SCRNNM - Centering screen names
     C*===============================================================*
     C*                                                                *
     C     SCRNNM        BEGSR
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C*
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 10/15/97 **************************************
     C*===============================================================*
     C*  SRCHK# - This subroutine will retrieve the next available
     C*    check# for a manual payment.
     C*===============================================================*
     C     SRCHK#        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C*
     C                   Z-ADD     0             SCCHK#
     C     CKKY          CHAIN     PCK01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     CKLCHK        SCCHK#
     C                   ADD       1             CKLCHK
     C                   MOVEL     Q1USER        CKUSER
     C                   MOVEL     Q1USER        CKUSER
     C                   Z-ADD     DSTIME        CKTIME
     C                   MOVEL     SRDTO         CKDT
     C                   UPDATE    PCK01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END ADD ***** 10/15/97 **************************************
     C*** Begin add *** 02/27/02 **************************************
     C*===============================================================*
     C*  SRWDW2 - This subroutine will process the V/S/Z reason entry
     C*    window.
     C*===============================================================*
     C     SRWDW2        BEGSR
     C*
     C*** Begin add *** 04/29/02 **************************************
     C                   CLEAR                   SCRMK1
     C                   CLEAR                   SCRMK2
     C                   CLEAR                   SCRMK3
     C                   CLEAR                   SCRMDS
     C*** End add ***** 04/29/02 **************************************
     C*
     C*  Display window for user to enter reason for V/S/Z.
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   SETOFF                                           90
     C                   EXFMT     WDW02
     C*-----------------------------
     C*  Process F10-Search
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     HFLD          IFEQ      'SCRMK1'
     C     HFLD          OREQ      'SCRMK2'
     C     HFLD          OREQ      'SCRMK3'
     C*
     C                   CALL      'W2171R'
     C                   PARM      '2'           ##CALL
     C     *INKC         PARM      *INKC         ##KC
     C                   PARM      *INKL         ##KL
     C                   PARM      *BLANKS       ##KEY
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     ##KEY         IFNE      *BLANK
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCRMK1'
     C                   MOVEL     ##KEY         SCRMK1
     C                   SETON                                        31
     C     HFLD          WHENEQ    'SCRMK2'
     C                   MOVEL     ##KEY         SCRMK2
     C                   SETON                                        32
     C     HFLD          WHENEQ    'SCRMK3'
     C                   MOVEL     ##KEY         SCRMK3
     C                   SETON                                        33
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCRMK2'
     C                   SETON                                        31
     C     HFLD          WHENEQ    'SCRMK3'
     C                   SETON                                        32
     C                   ENDSL
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   SETON                                            90
     C                   ENDIF
     C*-----------------------------
     C*  Process F12-Cancel
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*-----------------------------
     C*  Process edits
     C*
     C     SCRMK1        IFNE      *BLANKS
     C                   MOVEL(P)  SCRMK1        XXKEY
     C     TBKY          CHAIN     PRC01                              21
     C   21              SETON                                        90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRMK2        ANDNE     *BLANKS
     C                   MOVEL(P)  SCRMK2        XXKEY
     C     TBKY          CHAIN     PRC01                              22
     C   22              SETON                                        90
     C                   ENDIF
     C*
     C     *IN90         IFEQ      *OFF
     C     SCRMK3        ANDNE     *BLANKS
     C                   MOVEL(P)  SCRMK3        XXKEY
     C     TBKY          CHAIN     PRC01                              23
     C   23              SETON                                        90
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*** End add ***** 02/27/02 **************************************
     C*** BEGIN ADD ****** 11/19/09 ************************************
      *******Beg Del********04/17/12**********************************
      ******************************************************************
      *  SRUEFT - Update EFT Expiration date to expired
      ******************************************************************
     C*****SRUEFT        BEGSR
     C* RETRIEVE EFFECTIVE EFT SETUP RECORD
     C* Build ##KEY to access API for EFT record
     C*****Q1FEIN        IFEQ      0
     C*****              MOVEL(P)  SCFYR         XXWK10           10
     C*****              MOVE      SCCASE        XXWK10
     C*****Q1DSQ#        IFEQ      0
     C**Claimant
     C*****              MOVE      'C'           XXTYPE
     C*****              MOVEL(P)  XXWK10        XXEKY
     C*****              ELSE
     C* dependent
     C*****              MOVE      'D'           XXTYPE
     C*****              MOVEL(P)  XXWK10        XXWK15           15
     C*****              MOVE      Q1DSQ#        XXWK15
     C*****              MOVEL(P)  XXWK15        XXEKY
     C*****              ENDIF
     C*****              ELSE
     C* Provider
     C*****              MOVEL(P)  Q1FEIN        XXWK12           12
     C*****              MOVE      Q1MOD         XXWK12
     C*****              MOVEL(P)  XXWK12        XXEKY
     C*****              MOVE      'P'           XXTYPE
     C*****              ENDIF
      *****
      *** BEG ADD ***** 04/26/11 **********************************
     C*****              MOVEL(P)  XXTYPE        ##EKEY
     C*****              MOVE      XXEKY         ##EKEY
      *** END ADD ***** 04/26/11 **********************************
     *****
     C*****EFKY          SETGT     PEF01
     C*****EFKY1         READPE    PEF01                                  60
     C**** *IN60         DOWEQ     *OFF
     C**LOOP THRU FILE TO GET EFFECTIVE RECORD
     ******XXDATE        IFGE      EFEFDT
     C*****XXDATE        ANDLT     EFEXDT
     C*****XXDATE        ORGE      EFEFDT
     C*****EFEXDT        ANDEQ     0
     C**VALID EFT RECORD FOUND, LOAD TODAY'S DATE AS EXPIRATION DATE
     C**Store existing expiration date to reset later
     C*****              Z-ADD     EFEXDT        SVEXDT            7 0
     C*****              MOVE      'EFT'         SVEFTS            3
     C*****              Z-ADD     EFEFDT        XXDATE
     C*****              MOVEL     Q1USER        EFUSER
     C*****              MOVEL     Q1PGM         EFPGM
     C*****              TIME                    DSDTTM
      *****
     C*****              Z-ADD     DSDATE        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              MOVEL     SRDTO         EFEXDT
     C*****              MOVEL     SRDTO         EFDT
     C*****              MOVEL     DSTIME        EFTIME
     C*****              UPDATE    PEF01
      *****
     C*****              LEAVE
     C*****              ENDIF
     C*****EFKY1         READPE    PEF01                                  60
     C*****              ENDDO
      *****
     C*****              ENDSR
      ******************************************************************
      *  SRREFT - Reset EFT expiration date to previous status
      ******************************************************************
     C*****SRREFT        BEGSR
     C**Reset Expiration date in WMEFP
     C*****SVEFTS        IFEQ      'EFT'
     C*****EFKY          CHAIN     PEF01                              60
     C******IN60         IFEQ      *OFF
     C*****              MOVEL     Q1USER        EFUSER
     C*****              MOVEL     Q1PGM         EFPGM
     C*****              TIME                    DSDTTM
      *****
     C*****              Z-ADD     DSDATE        SRDTI
     C*****              CALL      'SRCVTDAT'    DTCYMD
     C*****              Z-ADD     *ZEROS        EFEXDT
     C*****              Z-ADD     SVEXDT        EFEXDT
     C*****              MOVEL     SRDTO         EFDT
     C*****              MOVEL     DSTIME        EFTIME
     C*****
     C*****              UPDATE    PEF01
     C*****              ENDIF
     C*****              MOVE      *BLANKS       SVEFTS
     C*****              ENDIF
     C*****              ENDSR
      *******End Del********04/17/12**********************************
     C*** END   ADD ****** 11/19/09 ************************************
     C*===============================================================*
      /Free
       //******************************************************************;
       // Clear function key lines for web display;
       //******************************************************************;
       Begsr ClearFKeys;
      /If Defined (WEB)
       Clear scFK1a;
       Clear scFK1b;
       Clear scFK2;
       Clear scFK3;
       Clear scFK4;
      /Endif
       Endsr;

       //******************************************************************;
       // Load function key lines for web display;
       //******************************************************************;
       Begsr LoadFKeys;
       fk1# = 0;
       fk2# = 0;
       fk3# = 0;
       fk4# = 0;
       Select;
       When *in04;
          fk1# = 4;
       Endsl;
       Select;
       When ##clt# = '139';
          fk1# = 0;
          fl1(1) = fk1x(fk1# + 1);
          fl1(2) = fk1x(fk1# + 2);
          fl1(3) = fk1x(fk1# + 3);
          fl1(4) = fk1x(fk1# + 4);
          fl2(1) = fk2x(fk2# + 1);
          fl3(1) = fk3x(fk3# + 1);
          fl3(2) = fk3x(fk3# + 2);
          fl3(3) = fk3x(fk3# + 3);
          fl3(4) = fk3x(fk3# + 4);
          fl4(1) = fk4x(fk4# + 1);
       Other;
          fl1(1) = fk1(fk1# + 1);
          fl1(2) = fk1(fk1# + 2);
          fl1(3) = fk1(fk1# + 3);
          fl1(4) = fk1(fk1# + 4);
          fl2(1) = fk2(fk2# + 1);
          fl3(1) = fk3(fk3# + 1);
          fl3(2) = fk3(fk3# + 2);
          fl3(3) = fk3(fk3# + 3);
          fl3(4) = fk3(fk3# + 4);
          fl4(1) = fk4(fk4# + 1);
       Endsl;
       Endsr;
      /End-free
     C*
**  MS   1         2         3         4
CHECK NUMBER XXXXXXX HAS BEEN SUCCESSFULLY

REVERSED
VOIDED
STOPPED
**  ER   1         2         3         4         5         6         7
REVERSAL NOT SUCCESSFUL.  RESERVES NOT ADEQUATE FOR CLAIM.
RESERVE CODE ON SELECTED TRANSACTION IS NOT VALID FOR CLAIM.
PYMT TOTAL FOR RSRV CODE LESS THAN SELECTED AMT.  NEGATIVE PMT WILL RESULT.
REQEST # SCREQ IS INACTIVE.  PRESS ENTER TO SELECT ANOTHER REQUEST.
MESSAGE: COMPENSATION PAYMENT REQUESTED & CLAIM DATE OF DISABILTY NOT ENTERED
THE RESERVE CODES FOR THE SELECTED PAYMENT AND ADJUSTMENT CODE DO NOT MATCH.
THIS CHECK HAS ALREADY BEEN CLEARED, UNABLE TO PROCESS THIS REQUEST
**  MSG
THE MEDICAL PAYMENTS WILL BE NEGATIVE
THE COMPENSATION PAYMENTS WILL BE NEGATIVE
THE LEGAL PAYMENTS WILL BE NEGATIVE
THE EXPENSE PAYMENTS WILL BE NEGATIVE
THE MEDICAL RESERVES MUST BE INCREASED BY
THE COMPENSATION RESERVES MUST BE INCREASED BY
THE LEGAL RESERVES MUST BE INCREASED BY
THE EXPENSE RESERVES MUST BE INCREASED BY
**  FK1  1         2         3         4         5         6         7         8
F3=Exit      F7=Financial summary    F8=Accident info     F9=Management info
F10=Search   F11=Benefit info        F13=Chg user parms   F24=More keys
F15=Remaining reserve   F24=More keys

F3=Exit      F7=Financial summary    F8=Accident info   F9=Management info
F10=Search   F11=Benefit info        F12=Cancel         F24=More keys
F15=Remaining reserve   F24=More keys

**  FK3 - Function Keys  SCR03
F3=Exit   F4=Additional Address   F7=Financial summary   F24=More keys
F8=Accident info   F9=Management info   F10=Search   F24=More keys
F11=Benefit info   F12=Cancel   F15=Remaining reserve   F24=More keys
F21=Change group provider   F24=More keys
**  MODE
 -- ADD --
- CHANGE -
**  FK2 - Function Keys  SCR02
F3=Exit   F10=Search   F12=Cancel   F15=Remaining Reserves
**  FK4 - Function Keys  SCR04
F3=Exit   F7=Financial summary   F10=Search   F12=Cancel  F15=Remaining rsv
**  FK1X (Clt139)
F3=Exit      F7=Financial summary    F8=Accident info   F9=Management info
F10=Search   F11=Benefit info        F12=Cancel
F3=Exit      F7=Financial summary    F8=Accident info     F9=Management info
F10=Search   F11=Benefit info        F13=Chg user parms
**  FK2X - Function Keys  SCR02 (Clt139)
F3=Exit   F10=Search   F12=Cancel
**  FK3X - Function Keys SCR03 (Clt139)
F3=Exit   F4=Additional Address   F7=Financial summary   F24=More keys
F8=Accident info   F9=Management info   F10=Search   F24=More keys
F11=Benefit info   F12=Cancel   F21=Change group provider   F24=More keys
**  FK4X - Function Keys  SCR04 (Clt139)
F3=Exit   F7=Financial summary   F10=Search   F12=Cancel
** VOUCHER HEADING
Process mthd (P/V/X):
Voucher        (Y/N):
** VOUCHER CODE DESCRITION
(P=Paper Checks, V=Voucher, X=XML File)
