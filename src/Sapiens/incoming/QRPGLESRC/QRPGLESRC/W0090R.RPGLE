     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0090R - EMPLOYER BILLING MAINTENANCE
     H*****************************************************************
     H* CALLED FROM:                                                  *
     H*    MENU                                                       *
     H*    W0051R - WORK WITH EMPLOYER POLICY YEARS                   *
     H* CALLS TO:                                                     *
     H*    W8001C                                                     *
     H*    W0051R10 - EMPLOYER PREMIUM                                *
     H*    W7412C - NCCI POLICY INFORMATION PAGE SUBMITTER            *
     H*                                                               *
     H* NOTE:  THIS PROGRAM WILL NO LONGER BE USING SCR02, INSTEAD IT *
     H*        WILL CALL W0051R10 TO DISPLAY PREMIUM INFORMATION.     *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 07/02/13 C:136 Project: 147WAI                                *
     H*                Implement changes for Dividend Plan            *
     H*                                                               *
     H* 08/30/12 C:136 SUPPORT: WA025 80122                           *
     H*                ADD OVER-RIDE FOR LARGE DEDUCTIBLE FILE        *
     H*                (WALDP) SO THAT LARGE DEDUCTIBLE CHANGES       *
     H*                CAN BE EVALUATED WITHOUT IMPACTING             *
     H*                PRODUCTION                                     *
     H*                                                               *
     H* 04/17/12 C:136 SUPPORT: WA146 79682                           *
     H*                CORRECT INSTALLMENT LOGIC ASSOCIATED WITH      *
     H*                BILL ON EFFECTIVE DATE FLAG (ELBLFL). USE      *
     H*                FULL DATE INSTEAD OF YYYMM TO COLLECT          *
     H*                PRIOR BILLINGS WITH.                           *
     H*                                                               *
     H* 08/23/11 C:180 SUPPORT 77730                                  *
     H*                Qualify incoming parms added on 11/29/10.
     H*                                                               *
     H* 06/28/11 C:136 SUPPORT: WA114 76929                           *
     H*               -POST LUMP SUM ASST ADJUSTMENTS TO NEXT         *
     H*                INSTALLMENT DATE IF FLAGGED TO DO SO           *
     H*               -DO NOT LET ASSESSMENT VALUES CARRY OVER        *
     H*                ON MULTI DIVISION POLICIES                     *
     H*                                                               *
     H* 06/27/11 C:136 SUPPORT: WA132 77213                           *
     H*                PREVENT ZERO POST DATE ON LUMP SUM IF DIV      *
     H*                HAS NEG ADJUSTMENT                             *
     H*                                                               *
     H* 04/20/11 C:136 SUPPORT: WA132 76619                           *
     H*                PREVENT ZERO POST DATE ON LUMP SUM IF EMP      *
     H*                DETAIL IS FLAGGED TO SPREAD BILLING ADJ        *
     H*                                                               *
     H* 01/31/11 C:136 SUPPORT: WA109 75652                           *
     H*                SAVE INSTALLMENT POST DATES AND WRITE TO       *
     H*                EACH TYPE OF INSTALLMENT BILLING AS IT IS      *
     H*                WRITTEN TO PREVENT ZERO POST DATES FROM        *
     H*                BEING WRITTEN TO ASSESSMENTS                   *
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP FOR POLICY PAGE, ITEM 3C.       *
     H*                                                               *
     H* 11/29/10 C:180 SUPPORT  74221                                 *
     H*                Check for record in use to prevent a system
     H*                error when 2 or more users are trying to
     H*                do a revised bill endorsement to the same
     H*                policy number.
     H*
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Add overrides for HCN Program.  ADD A NEW      *
     H*                FUNCTION KEY TO CALL THE HCN SETUP PROGRAM AS  *
     H*                WELL, FOLLOWING SAME RULES AS F6 AND F9 TO     *
     H*                RECALCULATE THE PREMIUM AFTER RETURNING.       *
     H*                                                               *
     H* 10/14/10 C:061 SUPPORT: WA132 72597                           *
     H*                Correct issue of WAEAP record being written    *
     H*                with zero post date and non-zero amount.       *
     H*                                                               *
     H* 08/09/10 C:191 PROJECT: 71WKU                                 *
     H*                Comment out all changes associated with current*
     H*                processing of RDP dated 03/05/07 and 04/21/06  *
     H*                                                               *
     H* 07/14/10 C:220 SUPPORT: 73542                                 *
     H*                Lum sum adjustment window appears again and    *
     H*                again.                                         *
     H*                                                               *
     H* 07/12/10 C:220 SUPPORT: 71730                                 *
     H*                Edit on concurrent revised billing run need to *
     H*                change a hard edit.                            *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELDS IN WDELP FOR SPLIT DEVIATIONS.          *
     H*                                                               *
     H* 06/08/10 C:61  SUPPORT WA139 72906                            *
     H*                Always produce output for Mid-Term Revised     *
     H*                Bill endorsement.                              *
     H*                                                               *
     H* 04/19/10 C:255 Project: 114WWI                                *
     H*               -Auto resubmit policy page on revised billing   *
     H*                                                               *
     H* 04/15/10 C:220 SUPPORT: 71250                                 *
     H*                ADD EDITS FOR EACH ADJUSTMENT SCREEN FOR       *
     H*                MUTUALLY INCLUSIVE FIELDS.                     *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Prior to deleting A/R, write the information   *
     H*                 to the A/R Holding file                       *
     H*                                                               *
     H* 10/27/09 C:190 PROJECT: WC1473                                *
     H*                OVERRIDE TO NEW LOGICALS FROM THIS PROJECT.    *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP.                                *
     H*                                                               *
     H* 07/23/09 C:65  PROJECT: 139WJU                                *
     H*                DISABLE PROJECT BY CHANGING CLTID FOR 139 TO   *
     H*                999 AFTER CLTID IS SET.                        *
     H*                                                               *
     H* 06/15/09 C:65  PROJECT WC1473  (SUPP#68829)                   *
     H*                OVERRIDE TO NEW LOGICALS FROM THIS PROJECT.    *
     H*                                                               *
     H* 05/01/09 C:180 SUPPORT 66957
     H*                Bill deposit if no previous deposit received
     H*
     H* 04/30/09 C:189 SUPPORT: 68248                                 *
     H*                MID TERM ENDORSEMNT SCREEN BEING DISPLAYED     *
     H*                ON ADDTION OF A PAYROLL FROM REVISE BILLING    *
     H*                OPTION(31) ON W/W POLICY YEARS                 *
     H*                                                               *
     H*                                                               *
     H* 04/17/09 C:180 SUPPORT 68079 Prevent DECIMAL DATA ERROR
     H*                when LDA not populated.
     H*
     H* 04/07/09 C:220 SUPPORT: 66684                                 *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP             *
     H*                - JUST COMPILED                                *
     H*                                                               *
     H* 02/12/09 C:65  PROJECT: 132WBW/SUPPORT                        *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES FOR NEW SECTIONS OF CODE.     *
     H*                                                               *
     H* 02/03/09 C:136 PROJECT: 146WBW (3RD PARTY BILLING)            *
     H*                DO NOT DEFAULT DARTS REMARK CODE IF 3RD        *
     H*                PARTY BILLING IS ACTIVATED.                    *
     H*                                                               *
     H* 12/11/08 C:65  PROJECT: WC1769                                *
     H*                ADD IN CODING TO BILL/ADJUST 6TH AND 7TH       *
     H*                ASSESSMENTS. STATUS VALUES TO INDICATE A/R     *
     H*                TYPE HAS BEEN CHANGED.  SUMMARIZED ASSESSMENTS *
     H*                ARE NOW LISTED WITH A STATUS OF D THRU K (NO I)*
     H*                                                               *
     H* 11/12/08 C:65  PROJECT: 139WJU                                *
     H*                BILL ASSESSMENTS BY INSTALLMENT FOR S/R        *
     H*                POLICIES FOR GIC.  THE ASSESSMENT WILL BE      *
     H*                BILLED IN EQUAL AMOUNTS FOR EACH MONTH THAT    *
     H*                GETS REPORTED ON THE POLICY, BILLED TO THE     *
     H*                LAST DAY OF THE MONTH.                         *
     H*                                                               *
     H* 10/07/08 C:65  PROJECT: WC1568/SUPP#65382                     *
     H*                DO NOT SPREAD ASSESSMENTS INTO FUTURE AS THE   *
     H*                LUMP SUM ASSESSMENTS ARE NOT TO BE BILLED INTO *
     H*                INSTALLMENTS.                                  *
     H*                                                               *
     H* 07/09/08 C:197 SUPPORT: 64026                                 *
     H*                ISSUING MIDTERM END FOR REVISED BILLING        *
     H*                                                               *
     H* 01/11/08 C:65  PROJECT: WC1568                                *
     H*                REWIRE THE ASSESSMENT BILLING TO PROCESS BOTH  *
     H*                ASSESSMENTS BY INSTALLMENT AND ASSESSMENTS IN  *
     H*                LUMP SUM AND POST ALL ADJUSTMENTS TO THE       *
     H*                ADJUSTMENT SCREENS.  LUMP SUM ASSESSMENTS WILL *
     H*                ALWAYS BE POSTED TO THE FIRST (CATCH-UP) SLOT. *
     H*                ALL CODING TO ADJUST ASSESSMENTS WILL THEN     *
     H*                BE REMOVED FROM W8011R BACK END PROCESING.     *
     H*                                                               *
     H*                ASSESSMENT FIELD DEFINITIONS:                  *
     H*                                                               *
     H*                XX1STB - 1ST ASSESSMENT AMOUNT TO BILL INTO    *
     H*                         INSTALLMENTS.                         *
     H*                XU1STB - 1ST ASSESSMENT AMOUNT TO BILL IN      *
     H*                         LUMP SUM.                             *
     H*                XT1STB - 1ST ASSESSMENT TOTAL FOR INSTALLMENTS.*
     H*                                                               *
     H*                XL1STB - 1ST ASSESSMENT TOTAL FOR LUMP SUM.    *
     H*                                                               *
     H*                SV1STB - 1ST ASSESSMENT SAVE AMOUNT FOR        *
     H*                         INSTALLMENTS.                         *
     H*                SU1STB - 1ST ASSESSMENT SAVE AMOUNT FOR        *
     H*                         LUMP SUM.                             *
     H*                ST1STB - 1ST ASSESSMENT SAVE TOTAL FOR         *
     H*                         INSTALLMENTS.                         *
     H*                SL1STB - 1ST ASSESSMENT SAVE TOTAL FOR         *
     H*                         LUMP SUM.                             *
     H*                (REPEATED FOR EACH ASSESSMENT 2ND THRU 5TH)    *
     H*                                                               *
     H* 12/10/07 C:190 SUPP# 61394                                    *
     H*                REVISED BILL ABENDS ON WEEKLY BILLING CODE     *
     H*                                                               *
     H* 10/11/07 C:183 PROJECT: 146WBC                                *
     H*                Added code to invoke the same processing that  *
     H*                occurs on a billing schedule code change if    *
     H*                the user changes the "Monthly Billings on      *
     H*                Policy Effective Day" from BLANK to 'Y' or 'Y' *
     H*                to blank.                                      *
     H*                                                               *
     H* 10/04/07 C:136 SUPPORT: WA045 60448                           *
     H*                CORRECTION TO DATE FOR LUMP SUM ADJUSTMENTS    *
     H*                                                               *
     H* 09/17/07 C:136 PROJECT: WC1568                                *
     H*                CORRECTIONS TO ORIGINAL PROJECT                *
     H*                                                               *
     H* 09/18/07  C:65  PROJECT: SUPP# 60174                          *
     H*                 FOR SINGLE DIVISION CALL, LIMIT THE W8010R    *
     H*                 CALLS TO PROCESSING THE ONE DIVISION ONLY.    *
     H*                                                               *
     H* 09/14/07  C:65  PROJECT: SUPP# 59352                          *
     H*                 DO NOT SPREAD CREDIT ADJUSTMENTS INTO THE     *
     H*                 FUTURE AS THIS DEFERS PREMIUM THEY DO NOT OWE.*
     H*                                                               *
     H* 05/15/07 C:184 PROJECT WC1568                                 *
     H*                Charge assessments on the same basis as premium*
     H*                installments                                   *
     H*                                                               *
     H* 03/06/07 C:61  PROJECT 71WIF                                  *
     H*                Change conditions for "Spread revised billing  *
     H*                adjustments in the future".                    *
     H*                                                               *
     H* 03/05/07  C:65  PROJECT: 71WGH/SUPP# 57383                    *
     H*                 MOVE RETRO CREDIT CODE WITHIN EMPLR MASTER    *
     H*                 LOOP SO ITS SET PROPERLY FOR EACH DIVISION.   *
     H*                                                               *
     H* 01/11/07 C:183 PROJECT: 139WGA                                *
     H*                RETRO RATING WITHOUT PAID LOSS                 *
     H*                                                               *
     H* 01/08/07  C:65  PROJECT: SUPP# 56522                          *
     H*                 REMOVE EXPENSE CONSTANT FROM ANNUAL LEVEL     *
     H*                 DEPOSIT CALC, SINCE ITS BILLED UP FRONT.      *
     H*                                                               *
     H* 10/18/06 C:136 SUPPORT: WA071 55383                           *
     H*                REMOVE ELFFL1 FLAG AS CONDITION FOR SPREADING  *
     H*                ADJ FOR ECM (CLT071)                           *
     H*                                                               *
     H*  10/11/06  C:33  Seq#: WA099/41801                            *
     H*                  Use SC305R instead of SC580R.                *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY POLICY YEAR MODE                *
     H*                                                               *
     H* 08/07/06 C:61  PROJECT: 132WDR                                *
     H*                Default "Endorsement Effective Date" when      *
     H*                processing mid-term endorsement.               *
     H*                                                               *
     H* 07/11/06 C:65  PROJECT: 132WDD                                *
     H*                CLIENT SPECIFIC: ENABLE SPREAD ADJUSTMENTS     *
     H*                INTO FUTURE CODING FOR CLIENT 132.             *
     H*                                                               *
     H* 07/07/06 C:65  SUPPORT: #52894                                *
     H*                MOVE THE A/R PROCESSING ON SCREEN 4 PAST THE   *
     H*                PRINT TAG SO THE PROCESSING WILL ONLY BE DONE  *
     H*                ONCE, IN THE CASE WHERE A USER CAN BACK UP     *
     H*                FROM THE ENDORSEMENT DESCRIPTION SCREEN.       *
     H*                                                               *
     H* 05/26/06 C:65  SUPPORT: #52933                                *
     H*                ENSURE A/R ADJUSTMENTS AREN'T SKIPPED FOR      *
     H*                A/R SUMMARIZED SINGLE DIVISION POLICIES.       *
     H*                                                               *
     H* 04/26/06  C:65  PROJECT: 71WGH                                *
     H*                 ENABLE THE SPREAD ADJUSTMENTS INTO FUTURE     *
     H*                 CODE FOR CLIENT 71 SINCE THEY'VE GONE TO BASE.*
     H*                                                               *
     H* 04/21/06  C:65  PROJECT: 71WGH(REIMPLEMENT 71FDN)             *
     H*                 ADD RETRO CREDIT/DEBIT CODE TO HANDLE ADJS.   *
     H*                                                               *
     H* 02/28/06 C:65  PROJECT: WC1327                                *
     H*                ALLOW PROGRAM TO BE CALLED IN SINGLE DIVISION  *
     H*                MODE.  THIS MODE WILL ALLOW A BILL TO BE RUN   *
     H*                ON A SINGLE DIVISION ON A POLICY.  IF CALLED   *
     H*                FOR A SINGLE DIVISION, CHECK FOR A/R IN A      *
     H*                PREVIOUS MONTH ON THE DIVISION AND ON THE      *
     H*                STATE REPORTING DIVISION AS WELL IF SUMMARIZED.*
     H*                                                               *
     H* 02/27/06 C:136 PROJECT: WC1298                                *
     H*                ADD OVERRIDE FOR PREMIUM PERIODS FILE WDPPP.   *
     H*                                                               *
     H* 02/23/06 C:136 SUPPORT: WA136 51298                           *
     H*                DEFAULT DEPOSIT DUE DATE TO SYSTEM DATE +      *
     H*                REVISED INTERIM DAYS.                          *
     H*                                                               *
     H* 02/20/06 C:65  PROJECT: 132WDP                                *
     H*                CHECK THE FLAG AT THE ADMIN/DARTS LEVEL FOR    *
     H*                ROUNDING INSTALLMENTS (PREMIUM AND TAX) TO     *
     H*                THE WHOLE DOLLAR.                              *
     H*                                                               *
     H* 02/15/06 C:136 SUPPORT: WA129 51113                           *
     H*                CHANGE EDIT ON ENDORSEMENT EFFECTIVE DATE      *
     H*                TO PREVENT DATE EQUAL TO EXP DATE POLICY       *
     H*                FROM BEING ENTERED.                            *
     H*                                                               *
     H* 01/24/06 C:65  PROJECT: SUPP#51088                            *
     H*                FOR 109 AND 136, ONLY ACCUMULATE PRIOR DEPOSIT *
     H*                BILLED FROM CURRENT YEAR TEMPORARILY.          *
     H*                                                               *
     H* 08/12/05 C:136 SUPPORT: WA132 48249                           *
     H*                CONDITION LOGIC FOR LUMP SUM ADJ NOT TO        *
     H*                EXECUTE IF PREM ADJ DON'T EQUAL ANNUAL PREM    *
     H*                TOTAL OR IF F-12 IS PRESSED.                   *
     H*                                                               *
     H* 07/11/05 C:65  SUPPORT: WA099/48024                           *
     H*                RUN A DATE CONVERSION ON SCDATE TO ENSURE A    *
     H*                PROPER DATE IS BEING PASSED ONTO W8001C.       *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109BQ}                                        *
     H*                Do not bill deposit adjustments for an account *
     H*                 that has transferred its deposit.             *
     H*                {109BX}                                        *
     H*               -Do not spread the A/R adjustment for BSCD chg. *
     H*               -Spread all adjustments for A/R into the future *
     H*                 for estimating employers.                     *
     H*                                                               *
     H* 06/13/05 C:136 SUPPORT: WA099 46477                           *
     H*                WRITE STATE REPORTING DIVISION'S NAME TO       *
     H*                WMENP                                          *
     H*                                                               *
     H* 04/21/05 C:65  SUPPORT: WA129/46030                           *
     H*                DELETE FUTURE TAXES AND INSTALLMENT FEES FOR   *
     H*                S/R EMPLOYERS AS WELL.                         *
     H*                                                               *
     H* 04/07/05 C:65  PROJECT: WC1273                                *
     H*                ALLOW DEPOSIT TO BE APPLIED TO AN ANNUAL LEVEL.*
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                ADD PROCESSING TO POPULATE EXTENDED EMPLR NAMES*
     H*                                                               *
     H* 03/03/05 C:132 PROJECT: WC1260                                *
     H*                Recompile for changes to WDELL and WAELP for   *
     H*                new Installment Fees Exemption Flag in these   *
     H*                files. NO CODE CHANGE IN THIS PROGRAM          *
     H*                                                               *
     H*  02/15/05 C:65 PROJECT: WC1167                                *
     H*                ADD THREE FREE FORM COMMENT FIELDS.  THESE     *
     H*                FIELDS WILL BE MUTUALLY EXCLUSIVE WITH THE     *
     H*                COMMENT CODE FIELDS.                           *
     H*                                                               *
     H* 01/25/05 C:65  SUPPORT: WA129/45449                           *
     H*                CHECK THE DELETED A/R FILE FOR EMPLOYERS WITH  *
     H*                A FUTURE CANCELLATION DATE.                    *
     H*                                                               *
     H* 01/03/05 C:136 PROJECT: 132WBN                                *
     H*                ADD LUMP SUM ADJUSTMENT                        *
     H*                                                               *
     H* 11/30/04 C:65  PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*                                                               *
     H* 09/30/04 C:136 PROJECT: WC1167                                *
     H*                ADDED 3 PARMS TO CALL FOR W8001C. (JH WILL     *
     H*                ADD THE CODE TO POPULATE THESE NEW PARMS       *
     H*                FROM THE PROMPT SCREEN.)                       *
     H*                                                               *
     H* 06/23/04 C:65  SUPPORT: WA109/42066                           *
     H*                ENSURE ENDORSEMENT EFFECTIVE DATE CANNOT BE    *
     H*                ZERO, AS THE DATE IS USED FOR EMPLR HISTORY.   *
     H*                                                               *
     H* 06/21/04 C:65  SUPPORT: WA129/42959/WC1115/WC1115             *
     H*                LIMIT # OF DAYS IN POLICY TO 365 FOR THE       *
     H*                WEEKLY BSCD SINCE THERE IS NO LIMIT ON BILLING *
     H*                START DATE.                                    *
     H*                                                               *
     H* 03/11/04 C:65  SUPPORT: WA102/41696                           *
     H*                MOVE CONDITION FOR 1/29/4 CHANGE INSIDE THE    *
     H*                EASTAT CONDITION TO PREVENT DELETION OF NON-   *
     H*                STATE TAX RECORDS.                             *
     H*                                                               *
     H* 01/29/04 C:65  SUPPORT: PRISS/18                              *
     H*          C:136 CONDITION WRITE/UPDATE FOR WAEAP FOR STATE TAX *
     H*                WHEN THE SYSTEM HAS NO A/R TRANSACTION CODES.  *
     H*                                                               *
     H* 01/20/04 C:09  PROJECT: CM027                                 *
     H*                We will no longer generate a separate NCCI     *
     H*                midterm endorsement form (WC890600A) if        *
     H*                payroll changes are made.  One form will       *
     H*                be created with Item 4.*Class, Rate, Other     *
     H*                (WC890615) marked or Interim Adjustment of     *
     H*                Premium (WC890616) marked or both, as appr.    *
     H*                (see W0060R, SR89600 for other changes).       *
     H*                                                               *
     H* 01/06/04 C:65  PROJECT: SUPP#40496                            *
     H*                ADD IN CODE TO ADVANCE INSTALLMENTS UP A SLOT  *
     H*                ON WEEKLY/TWICE MONTHLY IF THE FIRST INSTALL-  *
     H*                MENT ENDS UP BEING ZERO.                       *
     H*                                                               *
     H* 11/11/03 C:65  PROJECT: WC936                                 *
     H*                ADD IN CODE TO ALLOW A/R SUMMARY ON S/R EMPLRS *
     H*                                                               *
     H* 10/27/03 C:136 SUPPORT: 39695                                 *
     H*                PREVENT DECIMAL DATA ERROR IN SRWASB IF NEW    *
     H*                BILLING PLAN IS WEEKLY.                        *
     H*                                                               *
     H* 10/15/03 C:65  PROJECT: SUPPORT                               *
     H*                REFINE CODE ADDED ON 10/02/03 TO PREVENT       *
     H*                ACCUMULATING ALL A/R UP TO CURRENT DATE WHEN   *
     H*                ENSURING PAST MONTHS ARE IN BALANCE.           *
     H*                                                               *
     H* 10/13/03 C:65  PROJECT: SUPPORT                               *
     H*                ADD A POPUP WINDOW WHEN REVISED BILLING FIRST  *
     H*                STARTS.  THIS WINDOW WILL POP UP IF THERE IS   *
     H*                A/R SITTING OUT IN WAEAP WAITING TO BE PROCE-  *
     H*                SSED.  THIS MEANS A REVISED BILLING JOB HAS    *
     H*                ALREADY BEEN SUBMITTED.  ALLOW OVERRIDING TO   *
     H*                PROMPT A NEW BILL IF CHOSEN.                   *
     H*                                                               *
     H* 10/10/03 C:65  PROJECT: SUPP/#35908                           *
     H*                ADD IN CODE FOR S/R POLICIES TO CALCULATE HOW  *
     H*                MUCH STATE TAX ON DEPOSIT SHOULD BE BILLED IN  *
     H*                THE CURRENT YEAR BASED ON HOW MUCH DEPOSIT IS  *
     H*                BILLED WITHIN THAT YEAR.                       *
     H*                                                               *
     H* 10/07/03 C:65  PROJECT: SUPP/#35908                           *
     H*                ADD IN CODE FOR ESTIMATED POLICIES TO ALLOW    *
     H*                FOR PROPER POSTING OF TAX CHANGES WHEN THERE   *
     H*                ARE NO PREMIUM CHANGES.  ADDITIONALLY, DISPLAY *
     H*                UNAPPLIED BILLING CHANGES WHEN THE STATE TAX   *
     H*                CHANGES BUT THE PREMIUM DOESN'T AS WELL.       *
     H*                                                               *
     H* 10/06/03 C:65  PROJECT: SUPP/#39165                           *
     H*                RECODED SECTION WHICH ELIMINATED POSTING DATES *
     H*                WHEN THERE WAS NO MATCHING INSTALLMENT.        *
     H*                                                               *
     H* 10/03/03 C:65  PROJECT: SUPP/#38651                           *
     H*                ALLOW THE PROGRAM TO RECOGNIZE ANY A/R POSTED  *
     H*                AFTER THE LAST INSTALLMENT DATE.  THIS WILL    *
     H*                ALLOW USERS TO POST ADJUSTMENTS AS FAR OUT AS  *
     H*                THEY WANT AND STILL BE RECOGNIZED BY THE PGM.  *
     H*                THIS WILL BE DONE IN TWO STEPS:  IF THERE ARE  *
     H*                NO MORE INSTALLMENTS PAST THE CURRENT MONTH,   *
     H*                PROCESS AN ADDITIONAL CHECK FOR A/R RECORDS.   *
     H*                SECOND, ALTER THE POSTING DATE ARRAY TO ALLOW  *
     H*                LAST INSTALLMENT TO CHECK OUT TO INFINITY.     *
     H*                                                               *
     H* 10/02/03 C:65  PROJECT: SUPPORT                               *
     H*                IF A REVISED BILL IS RUN BEFORE THE BILLING    *
     H*                START DATE AND THE BILLING SCHEDULE CODE       *
     H*                CHANGES THEN DO NOT INCLUDE ANY A/R AMOUNTS    *
     H*                FROM CURRENT DATE + INTERIM DAYS FOR BALANCE   *
     H*                SINCE THEY WILL BE DELETED IN W8011R.          *
     H*                                                               *
     H* 06/12/03 C:65  PROJECT: SUPP#37551                            *
     H*                CORRECT DATE/AMOUNT CHECK WHICH WAS ALLOWING   *
     H*                ZERO POSTING DATES TO SLIDE THROUGH.           *
     H*                                                               *
     H* 04/25/03 C:65  PROJECT: WC1116                                *
     H*                ADD IN CODE TO RECOGNIZE AND BILL TO A/R A     *
     H*                TWICE A MONTH BILLING SCHEDULE CODE.           *
     H*                                                               *
     H* 04/21/03 C:65  PROJECT: WC1115                                *
     H*                ADD IN CODE TO RECOGNIZE AND BILL TO A/R A     *
     H*                WEEKLY ESTIMATED BILLING SCHEDULE CODE.        *
     H*                ALSO CHECK FOR DEPOSIT IN PERIOD #2.           *
     H*                                                               *
     H* 02/28/02 C:65  PROJECT: SUPP#31442                            *
     H*                REMOVE CHANGE DONE 1/28/02 FOR CX DATE.        *
     H*                COPIED IN BY C:136                             *
     H*                                                               *
     H* 02/28/02 C:136 PROJECT: WC1015                                *
     H*                IF CALLING EMP DETAIL (W0051R02), PASS PARM    *
     H*                'R' IN ##MODE.                                 *
     H*                                                               *
     H* 01/28/02 C:65  PROJECT: SUPP#30330                            *
     H*                DO NOT RUN A REVISED BILL ON A DIVISION IF     *
     H*                IT HAS ANY CANCELLATION DATE.                  *
     H*                                                               *
     H* 01/08/02 C:65  PROJECT: SUPP#28090                            *
     H*                IF THE AMOUNT FIELD ON ADJUSTMENT SCREEN IS    *
     H*                ZERO THEN THE DATE FIELD SHOULD BE ZERO TOO.   *
     H*                                                               *
     H* 01/07/02 C:65  PROJECT: SUPP#25626                            *
     H*                WHEN CHECKING THE BILL ADJUSTMENT FLAG FOR     *
     H*                DEPOSIT ON REVISED BILL, IGNORE IF THE BILLING *
     H*                SCHEDULE CODE CHANGES.                         *
     H*                                                               *
     H* 10/01/01 C:65  PROJECT: WC1045                                *
     H*                RUN THE BILLING PROGRAM TWICE IN ORDER TO      *
     H*                PROPERLY UPDATE ANNUAL PREM FOR LOSS CONSTANT. *
     H*                                                               *
     H* 06/01/01 C:90  PROJECT: CM027                                 *
     H*                Change midterm end. description from one 60A   *
     H*                line to three 40A lines. Move midterm          *
     H*                endorsement entry before submit to W8001C.     *
     H*                                                               *
     H* 02/20/01 C:65  PROJECT: SUPP#26331                            *
     H*                SET *IN81 TO MATCH THE DIVISION BILL WAS       *
     H*                SUBMITTED FROM FOR SCR04 TO PROCESS CORRECTLY. *
     H*                                                               *
     H* 02/12/01 C:65  PROJECT: SUPP#26204                            *
     H*                RESET EMPLR INFO USING THE WAELL FILE.         *
     H*                                                               *
     H* 01/16/01 C:65  PROJECT: SUPP#25855                            *
     H*                CHECK EFFECTIVE DATE RANGE FROM STATE RPT DIV  *
     H*                FOR THE ENDORSEMENT DATE CHECK.                *
     H*                                                               *
     H* 12/26/00 C:65  PROJECT: SUPP#24906                            *
     H*                IF INITIAL POSTING DATE IS LESS THAN BILLING   *
     H*                START DATE THEN CHANGE IT TO EQUAL BILL ST DT. *
     H*                                                               *
     H*  08/25/00 C:65 PROJECT: SUPP#24112                            *
     H*                RESET EMPLOYER MASTER RECORD FOR SUMMARY RUN.  *
     H*                                                               *
     H* 05/19/00 C:61  PROJECT WC577                                  *
     H*                Correct cursor position after selection SCR04. *
     H*                                                               *
     H*  04/05/00 C:65 PROJECT: SUPPORT                               *
     H*                ADD A HALF ADJUST TO A TAX CALCULATION.        *
     H*                                                               *
     H*  11/15/99 C:65 PROJECT: WC936                                 *
     H*                PROCESS ALL DIVISIONS ON A SINGLE POLICY RUN.  *
     H*                IF A POLICY IS SUMMARY A/R, THEN STORE ALL     *
     H*                A/R TO BE WRITTEN TO THE STATE RPT DIVISION    *
     H*                AFTER ALL DIVISIONS HAVE PROCESSED.            *
     H*                ADD OVERRIDES TO FILES WDILP AND WDMPP AS WELL *
     H*                                                               *
     H* 12/17/99  C:09  SUPPORT: WC577  Y2K # 74                      *
     H*                 YEAR/MONTH VARIABLE USED IN CALCULATING       *
     H*                 ESTIMATING INSTALLMENT ADJUSTMENTS WAS ONLY   *
     H*                 DEFINED TO BE 4.0 INSTEAD OF 5.0 (CYYMM).     *
     H*                                                               *
     H* 07/22/99  C:65  SUPPORT: 18929                                *
     H*                 OVERRIDE FILE WDELL1 FOR THE BILLING CALCS.   *
     H*                                                               *
     H* 04/12/99  C:65  PROJECT WC875                                 *
     H*                 BILL STATE TAX ON DEPOSIT WITH ITS OWN A/R    *
     H*                 CODE.  ACCUMULATE A/R ON THE 'D' CATEGORY.    *
     H*                                                               *
     H* 04/01/99  C:09   PROJECT: SUPPORT #13455/#15369               *
     H*                  CORRECT MIDTERM REVISED BILL CHANGES AND     *
     H*                  MULTIPLE ENDORSEMENTS BEING PROCESSED.       *
     H*                                                               *
     H* 03/23/99  C:65   PROJECT: SUPPORT #17280                      *
     H*                  WHEN RUNNING REVISED BILLS PRIOR TO BILLING  *
     H*                  START DATE, CHECK PRIOR BILLED TO BE         *
     H*                  THE BILLING START DATE + 1 DAY.              *
     H*                                                               *
     H* 03/16/99  C:65   PROJECT: WC888                               *
     H*                  WHEN RUNNING REVISED BILLS AFTER THE POLICY  *
     H*                  EXPIRES, CHECK FOR ALL A/R POSTED AFTER      *
     H*                  THE POLICY EXPIRATION DATE.                  *
     H*                                                               *
     H* 03/16/99  C:65   PROJECT: SUPPORT #17098                      *
     H*                  MOVE THE OFFICE CHECK CODE TO TOP OF PROGRAM *
     H*                  CHECK THE OFFICE FLAG WITHIN THE SYSTEM FILE *
     H*                  CHANGE INDICATOR FOR DARTS FROM 46 TO 66.    *
     H*                                                               *
     H* 03/03/99  C:65   PROJECT: WC705                               *
     H*                  IF A REVISED BILL RESULTS IN A CREDIT, POST  *
     H*                  TO THE CURRENT DAY.                          *
     H*                                                               *
     H* 01/29/99  C:33  SEQ#: WA081/15499                             *
     H*                 CHANGE ##CALL PARM ON F9=PAYROLLS TO INDICATE *
     H*                 BEING CALLED FROM REVISED BILLING.            *
     H*                                                               *
     H* 01/15/99  C:65   PROJECT: SUPPORT WA045 16015                 *
     H*                  CORRECT DEPOSIT ADJUSTMENT SECTION FOR 0%    *
     H*                  DEPOSIT BILLING SCHEDULE CODES.              *
     H*                                                               *
     H* 01/14/99  C:09   PROJECT: SUPPORT WA033 13709                 *
     H*                  INITIALIZE SVLDA (MIDTERM ENDORSEMENT LOCAL  *
     H*                  DATA AREA) TO PREVENT DECIMAL DATA ERRORS.   *
     H*                                                               *
     H* 11/23/98  C:35   PROJECT: SUPPORT WA090 15125                 *
     H*                  INITIALIZE POLICY EFFECTIVE DATE IN THE      *
     H*                  ENDORSEMENT RECORDS FROM THE EMPLOYER DETAIL *
     H*                  FILE.                                        *
     H*                                                               *
     H*  11/19/98  C:33  SEQ#: WC048/14689                            *
     H*                  DO NOT ADD 1 TO THE TIME WHEN WRITING THE    *
     H*                  EMPLOYER DETAIL & PAYROLL CHANGE RECORDS.    *
     H*                                                               *
     H* 10/14/98  C:35   PROJECT: SUPPORT WA091 13352                 *
     H*                  INITIALIZE CARRIER NUMBER IN THE ENDORSEMENT *
     H*                  RECORDS FROM THE VALUE IN FUND AUXILIARY.    *
     H*                                                               *
     H*  10/12/98  C:65  PROJECT: WC705                               *
     H*                  ENDORSEMENT EFFECTIVE DATE MUST BE WITHIN    *
     H*                  THE POLICY EFFECTIVE DATE RANGE.             *
     H*                                                               *
     H*  09/24/98  C:65  PROJECT: WC705                               *
     H*                  IF THE FIRST ADJUSTMENT DATE IS CHANGED,     *
     H*                  CHANGE THE DATE PASSED TO THE CALLS.         *
     H*                                                               *
     H*  08/27/98  C:65  PROJECT: SUPPORT                             *
     H*                  ADD PARM FOR QUOTE TO CALL TO W0051R10.      *
     H*                                                               *
     H*  08/07/98  C:65  PROJECT: WC705                               *
     H*                  WHEN CHECKING FOR BACK POSTING, USE CURRENT  *
     H*                  DATE, NOT CURRENT DATE + INTERIM DAYS.       *
     H*                                                               *
     H* 07/30/98  C:35  PROJECT: SUPPORT WA091 12864                  *
     H*                 LOAD ENDORSEMENT DESCRIPTION FROM SCREEN TO   *
     H*                 ENDORSEMENT RECORD.                           *
     H*                                                               *
     H*  07/24/98  C:78  WC577                                        *
     H*                  CORRECT F10 SEARCH. IF DEFAULT DARTS EQUALS  *
     H*                  BLANKS, RESOLVE FLAG TO 'N'.                 *
     H*                                                               *
     H*  07/15/98  C:65  SEQ#: WA033/12645                            *
     H*                  USE BILLING START DATE FOR ALL A/R CHECKS.   *
     H*                                                               *
     H*  06/24/98  C:33  SEQ#: WA093/12385                            *
     H*                  DON'T WRITE RECORDS TO BILL TRANSACTION FILE.*
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H*  06/04/98  C:65  PROJECT: WC705                               *
     H*                  DISPLAY '-' SIGN ON THE A/R ADJ SCREEN.      *
     H*                                                               *
     H*  05/01/98  C:65  PROJECT: WC705                               *
     H*                  USE SYSTEM DATE + INTERIM DAYS FOR QQDATE.   *
     H*                                                               *
     H*  03/09/98  C:65  PROJECT: WC705                               *
     H*                  CALCULATE STATE TAX ON DEPOSIT FOR SELF-RPT. *
     H*                                                               *
     H*  03/09/98  C:65  PROJECT: 91WBO                               *
     H*                  CHECK DEPOSIT ADJ THRESHOLD BEFORE ADJ SCRN. *
     H*                                                               *
     H*  03/03/98  C:65  PROJECT: WC705                               *
     H*                  DO NOT REQURE DUE DATE IF PRINT BALANCE = N  *
     H*                                                               *
     H*  02/26/98  C:65  PROJECT: WC705                               *
     H*                  DO NOT DEFAULT ENDORSEMENT EFFECTIVE DATE.   *
     H*                                                               *
     H*  02/19/98  C:65  PROJECT: WC705                               *
     H*               1) USE POSTING DATE FOR SCANNING A/R INSTEAD OF *
     H*                  CURRENT DATE.                                *
     H*               2) ALSO REMOVED CHECK FOR WAELP RECORD HIT FOR  *
     H*                  FUNCTION 7 - TO ALWAYS RUN W8010R.           *
     H*                                                               *
     H*  01/21/98  C:65  PROJECT: WC705                               *
     H*                  CHECK FOR MULTIPLE DIVISIONS FOR SCR05.      *
     H*                                                               *
     H*  01/01/98  C:65  PROJECT: WC705                               *
     H*                  DEFAULT PRINT BALANCE INFO FLAG TO 'Y'.      *
     H*                  (FORMERLY PRINT DEPOSIT INFO FLAG)           *
     H*****************************************************************
     FW0090F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS3)
     FWDF1L     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEMP     IF   E             DISK
     F**** BEG ADD *** 07/09/08  ****************************************
     FWMEPL3    IF   E           K DISK
     F                                     RENAME(PEP01:PEP03)
     F**** END ADD *** 07/09/08  ****************************************
     FWMEML     IF   E           K DISK
     F                                     RENAME(PEM01:PEM02)
     F                                     INFDS(DBFDS2)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PRS01)
     FWTLML1    IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FSTCTL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWMEAL4    UF   E           K DISK
     FWMEAL3    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     F*** BEGIN ADD ***  11/15/99  ************************************
     FWAEAL1    UF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     F*** END ADD *****  11/15/99  ************************************
     FWAEAL     O  A E           K DISK
     F                                     RENAME(PEA01:PEA02)
     F***  Begin Add  ***  11/19/09  **************************************
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     F***  End   Add  ***  11/19/09  **************************************
     F*** BEGIN ADD ***  01/25/05  ************************************
     FWMXAL1    UF   E           K DISK
     F*** END ADD *****  01/25/05  ************************************
     FWMEPL     IF   E           K DISK    USROPN
     FWAEPL     IF A E           K DISK
     F                                     RENAME(PEP01:PEP02)
     F*                                                             ******
     FWDELL     IF   E           K DISK    USROPN
     F                                     INFDS(DBFDS)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL10)
     FWAELL     IF A E           K DISK
     F                                     RENAME(PEL01:PEL02)
     F                                     INFDS(DBFDS4)
     F***  Begin Add  11/10/10  *******************************************
     FWDHCL     IF A E           K DISK    USROPN
     F                                     RENAME(PHC01:PHC00)
     FWDHCLR    IF   E           K DISK
     F                                     RENAME(PHC01:PHC0R)
     F***  End   Add  11/10/10  *******************************************
     F*** BEG ADD ***** 02/09/98 ***********************************************
     FWTMYP     IF   E             DISK
     FWMDXL     IF   E           K DISK
     FWMENL3    IF A E           K DISK
     F*** END ADD ***** 02/09/98 ***********************************************
     F*** BEGIN ADD ***  10/01/01  ************************************
     FWDSCL     IF   E           K DISK
     FWDELL1    IF   E           K DISK    USROPN
     F                                     RENAME(PEL01:PEL03)
     F*** END ADD *****  10/01/01  ************************************
     F*** BEGIN ADD ***  04/21/05  ************************************
     FWMIFL     IF   E           K DISK
     F*** END ADD *****  04/21/05  ************************************
     F*** BEG   ADD ***  04/19/10  ************************************
     FWMSHL1    UF   E           K DISK
     F*** END   ADD ***  04/19/10  ************************************
     F***** BEGIN ADD ****** 11/29/10 *********************************
     FWXRIL1    IF A E           K DISK
     F***** END ADD   ****** 11/29/10 *********************************

      *** beg add ***** 07/02/13 **************************************************
     d/copy wprDivPlan
      *** end add ***** 07/02/13 **************************************************

     D AB1             S              9  8 DIM(12)
     D AB2             S              9  8 DIM(12)
     D BD              S              7  0 DIM(13)
     D*** BEGIN CHG ***  11/12/08  ************************************
     D*********           DA         12  8 0
     D*********           PA         12  9 2
     D*** END CHG *****  11/12/08  ************************************
      ***** BEG ADD ******* 10/14/10 **********************************
      ***** END ADD ******* 10/14/10 **********************************
      *** BEG ADD ***** 01/31/11 **************************************
     D J2D             S              7  0 DIM(12)
      *** END ADD ***** 01/31/11 **************************************
     D KEYF            S              8  0 DIM(2)
     D KEYM            S              5  0 DIM(2)
     D X0              S              1    DIM(5)
     D XB              S              1    DIM(5)
     D*** BEGIN CHG ***  07/22/99  ************************************
     D******              QCA     1  14 55               CMD EXEC/QCAEXEC
     D*** BEGIN CHG ***  11/15/99  ************************************
     D**********          QCA     1  16 55               CMD EXEC/QCAEXEC
     D*** BEGIN CHG ***  02/27/06  ************************************
     D*****************   QCA     1  20 55               CMD EXEC/QCAEXEC
     D*** BEGIN CHG ***  06/15/09  ************************************
     D*************       QCA     1  22 55               CMD EXEC/QCAEXEC
     D*** BEGIN CHG ***  10/27/09  ************************************
     D*************       QCA     1  24 55               CMD EXEC/QCAEXEC
     D***  Begin Chg  ***  11/10/10  **************************************
     D**********          QCA     1  26 55               CMD EXEC/QCAEXEC
     D QCA             S             55    DIM(50) CTDATA PERRCD(1)             CMD EXEC/QCAEXEC
     D***  End   Chg  ***  11/10/10  **************************************
     D*** END   CHG ***  10/27/09  ************************************
     D*** END CHG *****  06/15/09  ************************************
     D*** END CHG *****  02/27/06  ************************************
     D*** END CHG *****  11/15/99  ************************************
     D*** END CHG *****  07/22/99  ************************************
     D TMP             S              1    DIM(22)
     D NUM             S              1    DIM(5)
     D*** BEGIN CHG ***  11/12/08  ************************************
     D*********           P1         12  9 2
     D*** END CHG *****  11/12/08  ************************************
     D*** BEGIN ADD ***  05/15/07  ************************************
     D*** BEGIN CHG ***  11/12/08  ************************************
     D*********           A1         12  9 2
     D*********           A2         12  9 2
     D*********           A3         12  9 2
     D*********           A4         12  9 2
     D*********           A5         12  9 2
     D*** BEGIN ADD ***  12/11/08  ************************************
     D*** END ADD *****  12/11/08  ************************************
     D*** END CHG *****  11/12/08  ************************************
     D*** END ADD *****  05/15/07  ************************************
     D*** BEGIN ADD ***  11/15/99  ************************************
     D*** END ADD *****  11/15/99  ************************************
     D*** BEGIN ADD ***  04/21/03  ************************************
     D*** BEGIN CHG ***  01/06/04  ************************************
     D*********           WKD        53  7 0
     D*** BEG CHG *****  12/10/07  ************************************
     D***************     WKD        53  7 0A
     D*** END CHG *****  01/06/04  ************************************
     D***************     WKA        53  9 2
     D*** BEGIN ADD ***  05/15/07  ************************************
     D WKD             S              7  0 DIM(60) ASCEND
     D WKA             S              9  2 DIM(60)
     D*** END CHG *****  12/10/07  ************************************
     D WK1             S              9  2 DIM(53)
     D WK2             S              9  2 DIM(53)
     D WK3             S              9  2 DIM(53)
     D WK4             S              9  2 DIM(53)
     D WK5             S              9  2 DIM(53)
     D*** BEGIN ADD ***  12/11/08  ************************************
     D WK6             S              9  2 DIM(53)
     D WK7             S              9  2 DIM(53)
     D*** END ADD *****  12/11/08  ************************************
     D*** END ADD *****  05/15/07  ************************************
     D*** BEG CHG *****  12/10/07  ************************************
     D***********         WKT        53  9 2
     D WKT             S              9  2 DIM(60)
     D*** END CHG *****  12/10/07  ************************************
     D*** BEGIN ADD ***  02/28/06  ************************************
     D SKA             S              9  2 DIM(53)
     D*** BEGIN ADD ***  05/15/07  ***********************************
     D SK1             S              9  2 DIM(53)
     D SK2             S              9  2 DIM(53)
     D SK3             S              9  2 DIM(53)
     D SK4             S              9  2 DIM(53)
     D SK5             S              9  2 DIM(53)
     D*** BEGIN ADD ***  12/11/08  ************************************
     D SK6             S              9  2 DIM(53)
     D SK7             S              9  2 DIM(53)
     D*** END ADD *****  12/11/08  ************************************
     D*** END ADD *****  05/15/07  ************************************
     D SKT             S              9  2 DIM(53)
     D*** END ADD *****  02/28/06  ************************************
     D DAY             S              2    DIM(13) CTDATA PERRCD(13)              DAYS OF WEEK    %
     D*** END ADD *****  04/21/03  ************************************
     D*** BEG ADD ****** 01/03/05 *************************************
     D*** END ADD ****** 01/03/05 *************************************
     D*** BEG ADD *****  09/17/07  ************************************
     D*** BEGIN CHG ***  12/11/08  ************************************
     D*********           CA      1   5 14               DEFAULT ASST HEADINGS
     D CA              S             14    DIM(7) CTDATA PERRCD(1)              DEFAULT ASST HEADING
     D*** END CHG *****  12/11/08  ************************************
     D*** END ADD *****  09/17/07  ************************************
     D*** BEGIN ADD ***  11/12/08  ************************************
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     D*** END ADD ***** 10/01/01  *************************************
     D*
     D OPTNM           C                   CONST('Revised Billing')
     D*** BEG ADD ***** 02/09/98 ***********************************************
     D AGTAPI          C                   CONST('WA002R')
     D*** END ADD ***** 02/09/98 ***********************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D DBFDS4          DS
     D  DBRRN4               397    400B 0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  YYMM                   1      5  0
     D  YY                     1      3  0
     D  MM                     4      5  0
     D*** BEGIN ADD ***  11/12/08  ************************************
     D                 DS
     D  X1PFDT                 1      7  0
     D  YY1                    1      3  0
     D  MM1                    4      5  0
     D  DD1                    6      7  0
     D  YYMM1                  1      5  0
     D*** END ADD *****  11/12/08  ************************************
     D*** BEGIN ADD ***  11/15/99  ************************************
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*** END ADD *****  11/15/99  ************************************
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 108 P1
     D  P1                     1    117  2
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCP101                 1      9  2
     D  SCP102                10     18  2
     D  SCP103                19     27  2
     D  SCP104                28     36  2
     D  SCP105                37     45  2
     D  SCP106                46     54  2
     D  SCP107                55     63  2
     D  SCP108                64     72  2
     D  SCP109                73     81  2
     D  SCP110                82     90  2
     D  SCP111                91     99  2
     D  SCP112               100    108  2
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCP113               109    117  2
     D*** END CHG *****  11/12/08  ************************************
     D*** BEGIN ADD****  05/15/07  ************************************
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 108 A1
     D  A1                     1    117  2
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCA101                 1      9  2
     D  SCA102                10     18  2
     D  SCA103                19     27  2
     D  SCA104                28     36  2
     D  SCA105                37     45  2
     D  SCA106                46     54  2
     D  SCA107                55     63  2
     D  SCA108                64     72  2
     D  SCA109                73     81  2
     D  SCA110                82     90  2
     D  SCA111                91     99  2
     D  SCA112               100    108  2
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCA113               109    117  2
     D*** END CHG *****  11/12/08  ************************************
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 108 A2
     D  A2                     1    117  2
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCA201                 1      9  2
     D  SCA202                10     18  2
     D  SCA203                19     27  2
     D  SCA204                28     36  2
     D  SCA205                37     45  2
     D  SCA206                46     54  2
     D  SCA207                55     63  2
     D  SCA208                64     72  2
     D  SCA209                73     81  2
     D  SCA210                82     90  2
     D  SCA211                91     99  2
     D  SCA212               100    108  2
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCA213               109    117  2
     D*** END CHG *****  11/12/08  ************************************
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 108 A3
     D  A3                     1    117  2
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCA301                 1      9  2
     D  SCA302                10     18  2
     D  SCA303                19     27  2
     D  SCA304                28     36  2
     D  SCA305                37     45  2
     D  SCA306                46     54  2
     D  SCA307                55     63  2
     D  SCA308                64     72  2
     D  SCA309                73     81  2
     D  SCA310                82     90  2
     D  SCA311                91     99  2
     D  SCA312               100    108  2
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCA313               109    117  2
     D*** END CHG *****  11/12/08  ************************************
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 108 A4
     D  A4                     1    117  2
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCA401                 1      9  2
     D  SCA402                10     18  2
     D  SCA403                19     27  2
     D  SCA404                28     36  2
     D  SCA405                37     45  2
     D  SCA406                46     54  2
     D  SCA407                55     63  2
     D  SCA408                64     72  2
     D  SCA409                73     81  2
     D  SCA410                82     90  2
     D  SCA411                91     99  2
     D  SCA412               100    108  2
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCA413               109    117  2
     D*** END CHG *****  11/12/08  ************************************
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 108 A5
     D  A5                     1    117  2
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCA501                 1      9  2
     D  SCA502                10     18  2
     D  SCA503                19     27  2
     D  SCA504                28     36  2
     D  SCA505                37     45  2
     D  SCA506                46     54  2
     D  SCA507                55     63  2
     D  SCA508                64     72  2
     D  SCA509                73     81  2
     D  SCA510                82     90  2
     D  SCA511                91     99  2
     D  SCA512               100    108  2
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCA513               109    117  2
     D*** END CHG *****  11/12/08  ************************************
    *D***END ADD *******05/15/07***************************
     D*** BEGIN ADD ***  12/11/08  ************************************
     D                 DS
     D  A6                     1    117  2
     D                                     DIM(13)
     D  SCA601                 1      9  2
     D  SCA602                10     18  2
     D  SCA603                19     27  2
     D  SCA604                28     36  2
     D  SCA605                37     45  2
     D  SCA606                46     54  2
     D  SCA607                55     63  2
     D  SCA608                64     72  2
     D  SCA609                73     81  2
     D  SCA610                82     90  2
     D  SCA611                91     99  2
     D  SCA612               100    108  2
     D  SCA613               109    117  2
     D                 DS
     D  A7                     1    117  2
     D                                     DIM(13)
     D  SCA701                 1      9  2
     D  SCA702                10     18  2
     D  SCA703                19     27  2
     D  SCA704                28     36  2
     D  SCA705                37     45  2
     D  SCA706                46     54  2
     D  SCA707                55     63  2
     D  SCA708                64     72  2
     D  SCA709                73     81  2
     D  SCA710                82     90  2
     D  SCA711                91     99  2
     D  SCA712               100    108  2
     D  SCA713               109    117  2
     D*** END ADD *****  12/11/08  ************************************
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
     D  XXDATA                 1     80
     D  XXTYPE                 1      1
     D  ZZCO#                  2      4
     D  XXUSER                 5     14
     D  XXTIME                15     20
     D  XXDATE                21     28
     D  UPOUTQ                29     38
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 096 DA
     D  DA                     1    104  0
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCDAT1               001    008  0
     D  SCDAT2               009    016  0
     D  SCDAT3               017    024  0
     D  SCDAT4               025    032  0
     D  SCDAT5               033    040  0
     D  SCDAT6               041    048  0
     D  SCDAT7               049    056  0
     D  SCDAT8               057    064  0
     D  SCDAT9               065    072  0
     D  SCDA10               073    080  0
     D  SCDA11               081    088  0
     D  SCDA12               089    096  0
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCDA13               097    104  0
     D*** END CHG *****  11/12/08  ************************************
     D                 DS
     D*** BEGIN CHG****  11/12/08  ************************************
     D*********                               1 108 PA
     D  PA                     1    117  2
     D                                     DIM(13)
     D*** END CHG *****  11/12/08  ************************************
     D  SCPAY1                 1      9  2
     D  SCPAY2                10     18  2
     D  SCPAY3                19     27  2
     D  SCPAY4                28     36  2
     D  SCPAY5                37     45  2
     D  SCPAY6                46     54  2
     D  SCPAY7                55     63  2
     D  SCPAY8                64     72  2
     D  SCPAY9                73     81  2
     D  SCPA10                82     90  2
     D  SCPA11                91     99  2
     D  SCPA12               100    108  2
     D*** BEGIN CHG****  11/12/08  ************************************
     D  SCPA13               109    117  2
     D*** END CHG *****  11/12/08  ************************************
     D                 DS
     D  DT                     1    080  0
     D                                     DIM(10)
     D  SCDT01               001    008  0
     D  SCDT02               009    016  0
     D  SCDT03               017    024  0
     D  SCDT04               025    032  0
     D  SCDT05               033    040  0
     D  SCDT06               041    048  0
     D  SCDT07               049    056  0
     D  SCDT08               057    064  0
     D  SCDT09               065    072  0
     D  SCDT10               073    080  0
      ***** BEG ADD ******* 10/14/10 **********************************
      *
     D                 DS
     D  J1D                    1     84  0
     D                                     DIM(12)
     D  EAPO01                 1     07  0
     D  EAPO02                 8     14  0
     D  EAPO03                15     21  0
     D  EAPO04                22     28  0
     D  EAPO05                29     35  0
     D  EAPO06                36     42  0
     D  EAPO07                43     49  0
     D  EAPO08                50     56  0
     D  EAPO09                57     63  0
     D  EAPO10                64     70  0
     D  EAPO11                71     77  0
     D  EAPO12                78     84  0
      *
      ***** END ADD ******* 10/14/10 **********************************
     D                 DS
     D  CL                     1     60
     D                                     DIM(10)
     D  SCCL01                 1      6
     D  SCCL02                 7     12
     D  SCCL03                13     18
     D  SCCL04                19     24
     D  SCCL05                25     30
     D  SCCL06                31     36
     D  SCCL07                37     42
     D  SCCL08                43     48
     D  SCCL09                49     54
     D  SCCL10                55     60
     D                 DS
     D  TP                     1     10
     D                                     DIM(10)
     D  SCTP01                 1      1
     D  SCTP02                 2      2
     D  SCTP03                 3      3
     D  SCTP04                 4      4
     D  SCTP05                 5      5
     D  SCTP06                 6      6
     D  SCTP07                 7      7
     D  SCTP08                 8      8
     D  SCTP09                 9      9
     D  SCTP10                10     10
     D                 DS
     D  SS                     1     10
     D                                     DIM(10)
     D  SCST01                 1      1
     D  SCST02                 2      2
     D  SCST03                 3      3
     D  SCST04                 4      4
     D  SCST05                 5      5
     D  SCST06                 6      6
     D  SCST07                 7      7
     D  SCST08                 8      8
     D  SCST09                 9      9
     D  SCST10                10     10
     D                 DS
     D  AM                     1     90  0
     D                                     DIM(10)
     D  SCAM01                 1      9  0
     D  SCAM02                10     18  0
     D  SCAM03                19     27  0
     D  SCAM04                28     36  0
     D  SCAM05                37     45  0
     D  SCAM06                46     54  0
     D  SCAM07                55     63  0
     D  SCAM08                64     72  0
     D  SCAM09                73     81  0
     D  SCAM10                82     90  0
     D                 DS
     D  HDG                    1    418
     D                                     DIM(19)
     D  SCHD01                 1     22
     D  SCHD02                23     44
     D  SCHD03                45     66
     D  SCHD04                67     88
     D  SCHD05                89    110
     D  SCHD06               111    132
     D  SCHD07               133    154
     D  SCHD08               155    176
     D  SCHD09               177    198
     D  SCHD10               199    220
     D  SCHD11               221    242
     D  SCHD12               243    264
     D  SCHD13               265    286
     D  SCHD14               287    308
     D  SCHD15               309    330
     D  SCHD16               331    352
     D  SCHD17               353    374
     D  SCHD18               375    396
     D  SCHD19               397    418
     D                 DS
     D  TAM                    1    209  2
     D                                     DIM(19)
     D  SCTA01                 1     11  2
     D  SCTA02                12     22  2
     D  SCTA03                23     33  2
     D  SCTA04                34     44  2
     D  SCTA05                45     55  2
     D  SCTA06                56     66  2
     D  SCTA07                67     77  2
     D  SCTA08                78     88  2
     D  SCTA09                89     99  2
     D  SCTA10               100    110  2
     D  SCTA11               111    121  2
     D  SCTA12               122    132  2
     D  SCTA13               133    143  2
     D  SCTA14               144    154  2
     D  SCTA15               155    165  2
     D  SCTA16               166    176  2
     D  SCTA17               177    187  2
     D  SCTA18               188    198  2
     D  SCTA19               199    209  2
     D*** BEG ADD ***** 01/03/05 ***********************************************
     D                 DS
     D  AR                     1    108  2
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
     D*** END ADD ***** 01/03/05 ***********************************************
     D*** BEG ADD ***** 02/09/98 ***********************************************
     D*** BEGIN CHG *** 01/14/99 ***********************************************
     D****SVLDA       DS                           1024
     D SVLDA           DS          1024    INZ
     D*** END CHG ***** 01/14/99 ***********************************************
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     D*** END ADD ***** 02/09/98 ***********************************************
     D*** BEGIN ADD ***  04/25/03  ************************************
     D                 DS
     D  XXWKDT                 1      7  0
     D  WYY                    1      3  0
     D  WMM                    4      5  0
     D  WDD                    6      7  0
     D  WYYMM                  1      5  0
     D*** END ADD *****  04/25/03  ************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*** END ADD *****  11/12/08  ************************************
     I*** BEGIN ADD *** 10/01/01  *************************************
     IPSC01
     I              SCCO#                       S2CO#
     I              SCFND                       S2FND
     I              SCST                        S2ST
     I              SCEFDT                      S2EFDT
     I              SCPRBS                      S2PRBS
     I              SCSTAT                      S2STAT
     I              SCUSER                      S2USER
     I              SCPGM                       S2PGM
     I              SCTIME                      S2TIME
     I              SCDT                        S2DT
     I              SCIUSR                      S2IUSR
     I              SCIDT                       S2IDT

     C*
     C*** BEGIN ADD ***  11/29/10  ************************************
     C     RIUKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*** END   ADD ***  11/29/10  ************************************
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  EMPLOYER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  08/25/00  ************************************
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C*** END ADD *****  08/25/00  ************************************
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFD1
     C                   KFLD                    XXCLAS            5 0
     C*
     F**** BEG ADD *** 07/09/08  ****************************************
     C     EPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C***BEG DLT *** 07/09/08 *****************************************
     C***********          KFLD           XXEFD1
     C***END DLT *** 07/09/08 *****************************************
     C*
     C     EPKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X@CLAS            5 0
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFD1
     F**** END ADD *** 07/09/08  ****************************************
     C*
     C*  TABLE FILE MAINTENANCE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*** END ADD *****  11/15/99  ************************************
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C*
     C     HCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    HCIN
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
      *** BEG ADD ***** 08/09/06 *************************************
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
      *** END ADD ***** 08/09/06 *************************************
     C*
     C*
     C*  EMPLOYER A/R TRANSACTION
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*** BEGIN CHG ***  11/15/99  ************************************
     C************         KFLD           SCDIV
     C                   KFLD                    XXDIV
     C*** END CHG *****  11/15/99  ************************************
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C                   KFLD                    XXPODT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*** BEGIN CHG ***  11/15/99  ************************************
     C************         KFLD           SCDIV
     C                   KFLD                    XXDIV
     C*** END CHG *****  11/15/99  ************************************
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C*
     C     EAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C*** BEGIN CHG ***  11/15/99  ************************************
     C************         KFLD           SCDIV
     C                   KFLD                    XXDIV
     C*** END CHG *****  11/15/99  ************************************
     C                   KFLD                    XXARTY            1
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C     EAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C*** END ADD *****  11/15/99  ************************************
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C     EAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     F*** BEG   ADD ***  04/19/10  ************************************
     C*  RATE SET CODE FILE
     C*
     C     RSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XX1RTC            3
     F*** END   ADD ***  04/19/10  ************************************
     C*
     C*  STAT RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     F*** BEG   ADD ***  04/19/10  ************************************
     C     RRKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C*
     C     RRKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTAT
     C                   KFLD                    XX1EFD
     C     RRKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTAT
     F*** END   ADD ***  04/19/10  ************************************
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXPFDT
     C*
     C     LMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C*
     C*  KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*** END ADD ***** 02/09/98 ***********************************************
     C*** BEGIN ADD ***  10/01/01  ************************************
     C*
     C*  LOSS CONSTANT FILE
     C*
     C     SCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     SCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C*** END ADD *****  10/01/01  ************************************
     C*** BEG   ADD *****  04/19/10  ************************************
     C     SHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
     C*** END   ADD *****  04/19/10  ************************************
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ELANPR        XXANPR
     C*** BEGIN ADD ***  10/07/03  ************************************
     C     *LIKE         DEFINE    ELTXDU        X1TXDU
     C*** END ADD *****  10/07/03  ************************************
     C     *LIKE         DEFINE    ELENMP        XXENMP
     C     *LIKE         DEFINE    ELSTPR        XXSTPR
     C     *LIKE         DEFINE    ELMAPR        XXMAPR
     C     *LIKE         DEFINE    ELBSCD        XXBSCD
     C     *LIKE         DEFINE    ELASP2        XXASP2
     C     *LIKE         DEFINE    ELCO#         XKCO#
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    EADT          XXEFDT
      *** BEG ADD ***** 04/19/10 **********************************
     C     *LIKE         DEFINE    RSJRST        XXSTAT
     C     *LIKE         DEFINE    EADT          XX1EFD
     C     *LIKE         DEFINE    ELPTDT        XXTPFD
      *** END ADD ***** 04/19/10 **********************************
     C     *LIKE         DEFINE    EADT          XXEFD1
     C     *LIKE         DEFINE    EADT          XXPODT
     C     *LIKE         DEFINE    EADT          XXPFDT
     C     *LIKE         DEFINE    EADT          XXWK60
     C     *LIKE         DEFINE    EADT          XXCURD
     C     *LIKE         DEFINE    EADT          XXCUR2
     C     *LIKE         DEFINE    EADT          XXDTE
     C     *LIKE         DEFINE    EADT          XXDTE1
     C     *LIKE         DEFINE    EADT          XXDTE2
      *** BEG ADD ***** 06/13/05 **********************************
     C     *LIKE         DEFINE    EMNAME        SVNAME
     C     *LIKE         DEFINE    EMFDNM        SVFDNM
     C     *LIKE         DEFINE    EMNAM2        SVNAM2
     C     *LIKE         DEFINE    EMFDN2        SVFDN2
      *** END ADD ***** 06/13/05 **********************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C                   PARM                    @@RRN             9 0
     C*** BEGIN ADD ***  11/29/10  ************************************
     C                   PARM                    @@RRN2            9 0
     C                   PARM                    @@RTCD            1
     C*** END   ADD ***  11/29/10  ************************************
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
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM      XXCO#         ##CO#             3 0
     C                   PARM      XXFND         ##FND             3 0
     C                   PARM      XXEMP#        ##EMP#            9 0
     C                   PARM      XXDIV         ##DIV             5 0
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*** END ADD ***** 02/09/98 ***********************************************
     C*
     C*** BEGIN ADD ***  02/28/06  ************************************
     C*
     C* SINGLE DIVISION MODE.
     C*
     C     @@CALL        IFEQ      'S'
     C                   MOVE      *ON           *IN67
     C                   MOVE      ' '           @@CALL
     C                   ELSE
     C                   MOVE      *OFF          *IN67
     C                   ENDIF                                                  @@CALL IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  06/24/05  **************************************
     C*** BEGIN ADD ***  07/23/09  ****************************************
     C     CLTID         IFEQ      '139'
     C                   MOVE      '999'         CLTID
     C                   ENDIF
     C*** END ADD *****  07/23/09  ****************************************
     C*
     C                   MOVE      '0'           XXOPN             1
     C                   MOVE      '0'           XXOPN2            1
     C***  Begin Add  11/10/10  *******************************************
     C                   MOVE      '0'           XXOPNH            1
     C***  End   Add  11/10/10  *******************************************
     C*** BEGIN CHG ***  01/20/04  ************************************
     C                   MOVE      'N'           XXPYCG            1
     C*** END CHG *****  01/20/04  ************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         UPCO#
     C                   Z-ADD     *ZERO         UPFND
     C                   Z-ADD     *ZERO         UPFYR
     C                   Z-ADD     *ZERO         UPEMP#
     C                   Z-ADD     *ZERO         UPDIV
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      UPCO#         ZZCO#
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C                   Z-ADD     UPFYR         XXFYR
     C                   Z-ADD     UPFND         SCFND
     C                   Z-ADD     UPFYR         SCYR
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
     C*
     C     Q1PRMS        IFGT      1
     C     @@RRN         CHAIN     WDELP                              60
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELFYR         SCYR
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C*** BEGIN ADD ***  10/01/01  ************************************
     C                   MOVE      *BLANK        XXLOSS            1
     C                   OPEN      WDELL1
     C                   EXSR      CHKLC
     C                   CLOSE     WDELL1
     C     @@RRN         CHAIN     WDELP                              60
     C*** END ADD *****  10/01/01  ************************************
     C*** BEGIN ADD **** 08/23/11 *************************************
     C     Q1PRMS        IFGT      4
     C*** END   ADD **** 08/23/11 *************************************
     C***** BEGIN ADD ****** 11/29/10 *********************************
     C                   EXSR      CHKRIU
     C***** END ADD   ****** 11/29/10 *********************************
     C                   ENDIF                                                  Q1PRMS>1
     C*** BEGIN ADD **** 08/23/11 *************************************
     C                   ENDIF                                                  Q1PRMS>1
     C*** END   ADD **** 08/23/11 *************************************
     C*** BEGIN ADD ***  11/30/04  ************************************
     C                   TIME                    DSDTTM
     C***  Begin Add  ***  11/19/09  **************************************
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C***  End   Add  ***  11/19/09  **************************************
     C                   Z-ADD     0             XXACDT            8 0
     C*
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     SCEMP#        @@EMP#
     C                   Z-ADD     SCDIV         @@DIV
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
     C*** END ADD *****  11/30/04  ************************************
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C                   MOVE      *BLANK        XX1ST             1
     C                   Z-ADD     0             XXDIVC            5 0
     C*
     C     EMKY1         CHAIN     PEM02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   ADD       1             XXDIVC
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV             5 0
      *** BEG ADD ***** 07/07/06 **************************************
     C                   MOVEL     EMNAME        SVNAME
     C                   MOVEL     EMFDNM        SVFDNM
     C                   MOVEL     EMNAM2        SVNAM2
     C                   MOVEL     EMFDN2        SVFDN2
      *** END ADD ***** 07/07/06 **************************************
     C                   ENDIF
     C*
     C     EMKY1         READE     PEM02                                  60
     C                   ENDDO
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
      *** BEG ADD ***** 02/03/09 **************************************
     C     EX3RDB        ANDNE     'Y'
      *** END ADD ***** 02/03/09 **************************************
     C                   MOVE      EXREV         SCRMCD
      *** BEG ADD **** 01/03/05 ***************************************
     C                   MOVE      EXLUMP        XXLSFL            1            LUMP SUM FLAG
      *** END ADD **** 01/03/05 ***************************************
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
      *** BEG ADD **** 01/03/05 ***************************************
     C                   MOVE      *BLANKS       XXLSFL
      *** END ADD **** 01/03/05 ***************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C****** BEGIN ADD ***** 07/24/98 *********************************
     C     SCRMCD        IFEQ      *BLANKS
     C                   MOVE      'N'           SCDART
     C                   ENDIF                                                  SCRMCD
     C****** END   ADD ***** 07/24/98 *********************************
     C*** BEGIN ADD  ***  03/16/99  **********************************
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     SYOFIF        ORNE      'Y'
     C*
     C                   MOVE      *ON           *IN66
     C                   MOVE      'N'           SCDART
     C*
     C                   ELSE
     C*
     C*  DEFAULT RESOLVE TO DARTS FLAG TO 'Y' INITIALLY
     C*
     C                   MOVE      *OFF          *IN66
     C*
     C*  RESOLVE TO DARTS FLAG SET TO 'N' IF REMARK CODE FIELD = *BLANK
     C*
     C     SCRMCD        IFEQ      *BLANKS
     C                   MOVE      'N'           SCDART
     C                   ELSE
     C                   MOVE      'Y'           SCDART
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD ******  03/16/99  **********************************
     C*
     C     FNKY          CHAIN     PFN01                              60
     C     FNKY          CHAIN     PF201                              60
     C*
     C   60              CLEAR                   PF201
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  OPTNM         SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
     C     @@CALL        COMP      '1'                                    09
     C*** BEGIN ADD ***  10/13/03  ************************************
     C     EAKY4         CHAIN(N)  PEA04                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  EAUSER        SCERRU
     C                   Z-ADD     EATIME        SCERRT
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCERRD
     C*** BEG DEL *****  07/12/10  ************************************
     C*******              MOVE 'N'       SCEOVR
     C*** END DEL *****  07/12/10  ************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDWMSG'      Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  10/13/03  ************************************
     C*
     C*  LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     Q1PRMS        IFGT      1
     C                   GOTO      SCR01T
     C                   ENDIF                                                  Q1PRMS>1
     C*
     C*  DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C*** BEGIN ADD ***  10/13/03  ************************************
     C*
     C     Q2RECF        IFEQ      'WDWMSG'
     C                   EXFMT     WDWMSG
     C*
     C*** BEGIN DEL ***  07/12/10  ************************************
     C******     SCEOVR    IFEQ 'Y'
     C******               MOVE *BLANK    Q2RECF
     C******               MOVEL'SCR01'   Q2RECF
     C******               GOTO SCR01T
     C******               ELSE
     C*** END   DEL ***  07/12/10  ************************************
     C                   MOVE      *ON           *INLR
     C                   GOTO      TERM
     C*** BEGIN DEL ***  07/12/10  ************************************
     C******               ENDIF                           SCEOVR IFEQ
     C*** END   DEL ***  07/12/10  ************************************
     C*
     C                   ENDIF                                                  Q2RECF IFEQ
     C*** END ADD *****  10/13/03  ************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*  NEXT LINE IS NOT SUPPOSED TO BE EXECUTED
     C*
     C  N09
     CAN 09              EXFMT     SCR02
     C*
     C                   MOVE      *OFF          *INKE
     C*
     C     *IN59         IFEQ      *OFF
     C                   MOVE      'U'           ##MODE
     C                   Z-ADD     DBRRN4        ##RRN
     C                   EXSR      LDOVRF
     C                   ELSE
     C                   Z-ADD     DBRRN         ##RRN
     C                   MOVE      'R'           ##MODE
     C                   ENDIF                                                  *IN59 IFEQ
     C*
     C                   CALL      'W0051R10'                           99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      SCPRDS        ##PRDS           40
     C     *INKF         PARM                    ##KF              1
     C     *INKG         PARM                    ##KG              1
     C     *INKI         PARM                    ##KI              1
     C*** BEGIN ADD ***  08/27/98  ************************************
     C                   PARM      ' '           ##QTE             1
     C*** END ADD *****  08/27/98  ************************************
     C***  Begin Add  ***  11/10/10  **************************************
     C     *INKS         PARM                    ##KS              1
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C     *IN59         IFEQ      *OFF
     C                   EXSR      DLTOVR
     C                   ENDIF                                                  *IN59 IFEQ
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C     *INKF         OREQ      *ON
     C     *INKG         OREQ      *ON
     C     *INKI         OREQ      *ON
     C***  Begin Add  ***  11/10/10  **************************************
     C     *INKS         OREQ      *ON
     C***  End   Add  ***  11/10/10  **************************************
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  *INKC/KL/KG=*ON
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C     Q2RECF        IFEQ      'SCR03'
      *** BEG ADD ***** 01/03/05 **************************************
      *
      *   PROMPT TO LUMP SUM ADJUSTMENTS OR NOT
      *
     C     XXLSFL        IFEQ      'Y'
      *** BEG ADD ***** 08/12/05 **************************************
     C     *IN32         ANDEQ     *OFF
     C     *INKL         ANDEQ     *OFF
      *** END ADD ***** 08/12/05 **************************************
      *** BEG ADD ***** 07/14/10 **************************************
     C     XLMFLG        ANDNE     'Y'
      *** END ADD ***** 07/14/10 **************************************
     C                   EXSR      SRCADJ
      *
     C     SCPRAJ        IFNE      *ZERO
     C                   WRITE     BLANK
      *
     C                   MOVE      *ON           *IN90
     C     *IN90         DOWEQ     *ON
     C                   EXFMT     WDLUMP
      *
     C     SCLUMP        IFNE      'Y'
     C     SCLUMP        ANDNE     *BLANK
     C                   MOVE      *ON           *IN88
     C                   ELSE
     C                   MOVE      *OFF          *IN90
      *** BEG ADD ***** 07/14/10 **************************************
     C                   MOVE      'Y'           XLMFLG            1
      *** END ADD ***** 07/14/10 **************************************
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF                                                  SCPRAJ <> 0
      *
     C     SCLUMP        IFEQ      'Y'
     C                   EXSR      SRLUMP
     C                   ENDIF
      *
     C                   ENDIF                                                  XXLSFL = 'Y'
      *** END ADD ***** 01/03/05 **************************************
     C*
     C                   EXFMT     SCR03
     C                   END
     C*** BEG ADD ***** 05/15/07 ***********************************************
     C*
     C     Q2RECF        IFEQ      'SCR03A'
     C                   EXFMT     SCR03A
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03B'
     C                   EXFMT     SCR03B
     C                   END
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     Q2RECF        IFEQ      'SCR03C'
     C                   EXFMT     SCR03C
     C                   END
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD ***** 05/15/07 ***********************************************
     C*
     C     Q2RECF        IFEQ      'SCR04'
      ***** BEG ADD ******* 08/07/06 **********************************
      *
      * Default Effective Date if called as Mid-Term Endorsement
      *
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
     C     SCEFDT        ANDEQ     *ZERO
     C                   Z-ADD     LDEEDT        SCEFDT
     C                   ENDIF
      *
      ***** END ADD ******* 08/07/06 **********************************
      *** BEG ADD ***** 02/23/06 **********************************************
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCDATE            8 0
     C                   ENDIF
     C*** BEGIN ADD ***  02/12/09  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SCDT
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  02/12/09  ************************************
      *** END ADD ***** 02/23/06 **********************************************
     C                   EXFMT     SCR04
     C                   END
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C*
     C     Q2RECF        IFEQ      'SCR04A'
     C                   EXFMT     SCR04A
     C                   END
     C*** END ADD ***** 02/09/98 ***********************************************
     C*
     C     Q2RECF        IFEQ      'SCR05'
     C                   EXFMT     SCR05
     C                   END
     C*
     C                   MOVE      '0'           *IN30
     C*
     C*  PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*  F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  11/15/99  ************************************
     C     Q1PRMS        IFGT      1
     C                   MOVE      *INKC         @@KC
     C                   ENDIF                                                  Q1PRMS>1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F12=CANCEL
     C*
     C     *INKL         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C*
     C     *IN86         IFEQ      *ON
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C     Q1PRMS        IFGT      1
     C                   MOVE      *INKL         @@KL
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF                                                  Q1PRMS>1
     C*
     C**
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEL     'SCR02'       Q2RECF
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C     Q2RECF        WHENEQ    'SCR03A'
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C*
     C     Q2RECF        WHENEQ    'SCR03B'
     C     XX3A          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03A'      Q2RECF
     C                   ELSE
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     Q2RECF        WHENEQ    'SCR03C'
     C     XX3B          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03B'      Q2RECF
     C                   ELSE
     C     XX3A          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03A'      Q2RECF
     C                   ELSE
     C                   MOVEL(P)  'SCR03'       Q2RECF
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*
     C**
     C     Q2RECF        WHENEQ    'SCR04'
     C     *IN80         IFEQ      '1'
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C     XXDIVC        IFGT      1
     C*
     C     EAKY4         CHAIN(N)  PEA04                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C*
     C                   MOVE      ' '           Q1CALL            1
     C                   MOVE      XXCO#         Q1CO#             3
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      SCYR          Q1FYR             3
     C                   MOVE      SCEMP#        Q1EMP#            9
     C                   MOVE      XSDIV         Q1DIV             5
     C                   MOVE      *BLANK        Q1KC
     C                   MOVE      *BLANK        Q1KL
     C*
     C                   CALL      'W0091R'
     C                   PARM                    Q1CALL            1
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1FYR             3
     C     *INKC         PARM                    Q1KC              1
     C     *INKL         PARM                    Q1KL              1
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *INKC IFEQ
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ELSE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANK        SCDEP
     C                   MOVE      *BLANKS       SCAR
     C                   Z-ADD     0             SCEFDT
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ENDIF                                                  *INKL IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C*** END ADD *****  11/15/99  ************************************
     C*
     C                   MOVEL     'SCR03'       Q2RECF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XX3C          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03C'      Q2RECF
     C                   ELSE
     C*** END ADD *****  12/11/08  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     XX3B          IFEQ      'Y'
      *** BEG CHG ***** 09/17/07 **************************************
     C*****************    MOVE 'SCR03B'  Q2RECF    P
     C                   MOVEL(P)  'SCR03B'      Q2RECF
      *** END CHG ***** 09/17/07 **************************************
     C                   ELSE
     C     XX3A          IFEQ      'Y'
      *** BEG CHG ***** 09/17/07 **************************************
     C*****************    MOVE 'SCR03A'  Q2RECF    P
     C                   MOVEL(P)  'SCR03A'      Q2RECF
      *** END CHG ***** 09/17/07 **************************************
     C                   ENDIF
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD   ***  05/15/07  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   ENDIF                                                  XXDIVC IFGT
     C*** END ADD *****  11/15/99  ************************************
     C                   END
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C     Q2RECF        WHENEQ    'SCR04A'
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C*** END ADD ***** 02/09/98 ***********************************************
     C*
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKL=*ON
     C*
     C*   ROLL UP REQUEST
     C*
     C     *IN18         CABEQ     '1'           SCR02A
     C*
     C*   ROLL DOWN REQUEST
     C*
     C     *IN19         CABEQ     '1'           SCR02A
     C*
     C*   COMMAND 05 = SEARCH EMPLOYER FILE
     C*
     C     *INKE         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN3        ##RRN2            9 0
     C                   PARM                    ##QTE             1
     C                   PARM      1             ##VIEW            1 0
     C                   PARM                    ##RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     SCYR          OREQ      *ZERO
     C     ##RRN         OREQ      *ZERO
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*
     C                   WRITE     SCR01
     C*
     C     ##RRN         CHAIN     WMEMP                              60
     C*
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   GOTO      SCR01T
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   ENDIF                                                  SCR01
     C*
     C*  F5=PAYROLLS
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   EXSR      LDOVRF
     C*
     C                   CALL      'W0060R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN4        ##RRN2            9 0
     C***  BEGIN ADD  ***  06/24/98  *********************************
     C                   PARM      'N'           @@BILL            1
     C***  END   ADD  ***  06/24/98  *********************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C*  WRITE TRANSACTION FILE RECORD IF NOT EXISTING
     C*
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  CALL PROGRAM TO COMPUTE NEW BILLING TOTALS
     C*
     C                   EXSR      BILL
     C*
     C                   ENDIF                                                   *INKL=*OFF
     C*
     C*  DELETE OVERRIDES
     C*
     C                   EXSR      DLTOVR
     C*
     C*  RELOAD BILLING DETAIL SCREEN
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF                                                  SCR02
     C*
     C*  COMMAND 05 = A/R INQUIRY
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      *BLANKS       QQDATE            8
     C                   MOVE      *BLANKS       QQAM              9
     C                   MOVE      *BLANKS       QQCHK#            7
     C                   MOVE      '2'           XXCALL            1
     C                   MOVE      'R'           QQRTC             1
     C                   CALL      'W0612R'
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    XXCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQDATE
     C                   PARM                    QQAM
     C                   PARM                    QQCHK#
     C                   PARM                    QQRTC             1
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  SCR03
     C*
     C                   ENDIF                                                  *INKE
     C*
     C*  F6=EMPLOYER DETAIL INFORMATION
     C*
     C     *INKF         IFEQ      *ON
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*  LOAD OVERRIDE FILES
     C*
     C                   EXSR      LDOVRF
     C*
     C                   CALL      'W0051R02'
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
      *** BEG CHG **** 02/28/02 **********************************************
     C******************** PARM 'C'       ##MODE  1
     C                   PARM      'R'           ##MODE            1
      *** END CHG **** 02/28/02 **********************************************
     C                   PARM      DBRRN4        ##RRN             9 0
     C                   PARM      DBRRN2        ##RRN2            9 0
     C                   PARM      ' '           ##QTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  CALL PROGRAM TO COMPUTE NEW BILLING TOTALS
     C*
     C                   EXSR      BILL
     C*
     C                   END
     C*
     C*  DELETE OVERRIDES
     C*
     C                   EXSR      DLTOVR
     C*
     C*  RELOAD BILLING DETAIL SCREEN
     C*
     C                   GOTO      SCR01T
     C                   END
     C                   END
     C*
     C*  F7=SUBMIT BILLING REVISION SCREEN
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*** BEGIN ADD ***  07/14/10  ************************************
     C*
     C*  BLANK THE LUMSUM ADJUSTMENT WINDOW FLAG FOR SECOND TIME
     C*
     C                   MOVE      ' '           XLMFLG
     C*** END   ADD ***  07/14/10  ************************************
     C*
     C     *IN86         IFEQ      '1'
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C                   END
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      LDOVRF
     C                   EXSR      BILL
     C                   EXSR      DLTOVR
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  04/21/06  ************************************
     C*** BEGIN DEL ***  03/05/07  ************************************
     C*********
     C*********            Z-ADD0         XXRTCR 112
     C*********  F1ORCR    IFEQ 'Y'
     C*********  CLTID     ANDEQ'071'
     C*********
     C*********            MOVE 'P'       XXARTY
     C*********  EAKY2     CHAINPEA01                60
     C*********  *IN60     DOWEQ'0'
     C*********
     C*********            EXCPTPEA01E
     C*********
     C*********  EAARCD    IFEQ F1RCCD
     C*********            ADD  EAAMT     XXRTCR
     C*********            ENDIF                           EAARCD IFEQ
     C*********
     C*********  EAKY2     READEPEA01                    60
     C*********            ENDDO
     C*********
     C*********            ENDIF                           *IN60 IFEQ
     C*** END DEL *****  03/05/07  ************************************
     C*** END ADD *****  04/21/06  ************************************
     C*
     C*  LOAD PREVIOUS BILLING SCHEDULE
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C                   MOVE      *BLANK        XXSUM             1
     C                   MOVE      *BLANK        XXWAR             1
     C                   Z-ADD     0             XTANPR           11 2
     C                   Z-ADD     0             XTTXDU           11 2
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   MOVE      *BLANK        XX3A              1
     C                   MOVE      *BLANK        XX3B              1
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   MOVE      *BLANK        XX3C              1
     C*** END ADD *****  12/11/08  ************************************
     C                   Z-ADD     0             XT1STB           11 2
     C                   Z-ADD     0             XT2NDB           11 2
     C                   Z-ADD     0             XT3RDB           11 2
     C                   Z-ADD     0             XT4THB           11 2
     C                   Z-ADD     0             XT5THB           11 2
     C                   Z-ADD     0             ST1STB           11 2
     C                   Z-ADD     0             ST2NDB           11 2
     C                   Z-ADD     0             ST3RDB           11 2
     C                   Z-ADD     0             ST4THB           11 2
     C                   Z-ADD     0             ST5THB           11 2
     C*** BEGIN ADD ***  01/11/08  ************************************
     C                   Z-ADD     0             XL1STB           11 2
     C                   Z-ADD     0             XL2NDB           11 2
     C                   Z-ADD     0             XL3RDB           11 2
     C                   Z-ADD     0             XL4THB           11 2
     C                   Z-ADD     0             XL5THB           11 2
     C                   Z-ADD     0             SL1STB           11 2
     C                   Z-ADD     0             SL2NDB           11 2
     C                   Z-ADD     0             SL3RDB           11 2
     C                   Z-ADD     0             SL4THB           11 2
     C                   Z-ADD     0             SL5THB           11 2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             XT6THB           11 2
     C                   Z-ADD     0             XT7THB           11 2
     C                   Z-ADD     0             ST6THB           11 2
     C                   Z-ADD     0             ST7THB           11 2
     C                   Z-ADD     0             XL6THB           11 2
     C                   Z-ADD     0             XL7THB           11 2
     C                   Z-ADD     0             SL6THB           11 2
     C                   Z-ADD     0             SL7THB           11 2
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  01/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     0             STANPR           11 2
     C                   Z-ADD     0             STTXDU           11 2
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD ***  04/21/03  ************************************
     C                   MOVE      *OFF          *IN62
     C                   MOVE      *OFF          *IN63
     C*** BEGIN ADD ***  04/25/03  ************************************
     C                   MOVE      *OFF          *IN64
     C*** END ADD *****  04/25/03  ************************************
     C*** END ADD *****  04/21/03  ************************************
     C*
     C*** BEGIN CHG ***  02/28/06  ************************************
     C*********  EMKY1     CHAINPEM02                60
     C  N67EMKY1         CHAIN     PEM02                              60
     C   67EMKY          CHAIN     PEM02                              60
     C*** END CHG *****  02/28/06  ************************************
     C*
     C     *IN60         DOWEQ     *OFF
     C     *IN60         OREQ      *ON
     C     XXSUM         ANDEQ     'Y'
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EMDIV         XXDIV
     C                   ELSE
     C                   Z-ADD     XSDIV         XXDIV
     C                   MOVE      'Y'           XXWAR
     C*** BEGIN ADD ***  08/25/00  ************************************
     C     EMKY2         CHAIN     PEM02                              60
     C*** END ADD *****  08/25/00  ************************************
     C                   ENDIF
     C*
     C     XXOPN2        IFEQ      '0'
     C                   OPEN      WDELL
     C                   MOVE      '1'           XXOPN2
     C                   END
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C*
     C     XXOPN2        IFEQ      '1'
     C                   CLOSE     WDELL
     C                   MOVE      '0'           XXOPN2
     C                   END
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE            7 0
     C*
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
     C*** BEGIN DEL ***  01/28/02  ************************************
     C*********  CD,Y      ANDLTX1DATE
     C*********  Y         ORNE *ZERO
     C*********  CD,Y      ANDNE*ZERO
     C*********  CD,Y      ANDLTELPFDT
     C*** END DEL *****  01/28/02  ************************************
     C*** BEGIN ADD ***  02/28/02  ************************************
     C     CD(Y)         ANDLT     X1DATE
     C     Y             ORNE      *ZERO
     C     CD(Y)         ANDNE     *ZERO
     C     CD(Y)         ANDLT     ELPFDT
     C*** END ADD *****  02/28/02  ************************************
     C                   MOVE      'Y'           XXCNCL            1
     C                   END
     C*
     C*  ONLY PROCESS DIVISION IF NOT CANCELLED.
     C*
     C     XXCNCL        IFNE      'Y'
     C*** BEGIN ADD ***  03/05/07  ************************************
     C*** BEGIN DEL ****** 08/09/10 ************************************
     C**********           Z-ADD0         XXRTCR 112
     C********** F1ORCR    IFEQ 'Y'
     C********** CLTID     ANDEQ'071'
     C**********
     C**********           MOVE 'P'       XXARTY
     C********** EAKY2     CHAINPEA01                60
     C********** *IN60     DOWEQ'0'
     C**********
     C**********           EXCPTPEA01E
     C**********
     C********** EAARCD    IFEQ F1RCCD
     C**********           ADD  EAAMT     XXRTCR
     C**********           ENDIF                           EAARCD IFEQ
     C**********
     C********** EAKY2     READEPEA01                    60
     C**********           ENDDO
     C**********
     C**********           ENDIF                           *IN60 IFEQ
     C*** END   DEL ****** 08/09/10 ************************************
     C*** END ADD *****  03/05/07  ************************************
     C*
     C                   MOVEL     ELBSCD        XXBSCD
     C*** BEG ADD *****  10/11/07  ************************************
     C                   MOVEL     ELBLFL        XXBLFL            1
     C*** END ADD *****  10/11/07  ************************************
     C     ELANPR        SUB       ELPRTD        SVANPR           11 2
     C                   Z-ADD     ELSTPR        SVSTPR           11 2
     C                   Z-ADD     ELMAPR        SVMAPR           11 2
     C                   Z-ADD     ELENMP        SVNMPR           11 2
     C*** BEGIN ADD ***  04/07/05  ************************************
     C*** BEGIN CHG ***  01/08/07  ************************************
     C***********          Z-ADDELANPR    S2ANPR 112
     C     ELANPR        SUB       ELPRTD        S2ANPR           11 2
     C*** END CHG *****  01/08/07  ************************************
     C*** END ADD *****  04/07/05  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     ELTXDU        SVTXDU           11 2
     C                   MOVE      ELARSM        SVARSM            1
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEG CHG ***** 09/17/07 **************************************
     C**********
     C********** RR1STB    IFEQ 'Y'
     C**********           Z-ADDELTXDU    SV1STB 112
     C**********           ENDIF
     C********** RR2NDB    IFEQ 'Y'
     C**********           Z-ADDELTXDU    SV2NDB 112
     C**********           ENDIF
     C********** RR3RDB    IFEQ 'Y'
     C**********           Z-ADDELTXDU    SV3RDB 112
     C**********           ENDIF
     C********** RR4THB    IFEQ 'Y'
     C**********           Z-ADDELTXDU    SV4THB 112
     C**********           ENDIF
     C********** RR5THB    IFEQ 'Y'
     C**********           Z-ADDELTXDU    SV5THB 112
     C**********           ENDIF
     C*** BEGIN CHG ***  01/11/08  ************************************
     C*******    RR1STB    IFEQ 'Y'
     C*******              Z-ADDEL1AAM    SV1STB 112
     C*******              ENDIF
     C*******    RR2NDB    IFEQ 'Y'
     C*******              Z-ADDEL2AAM    SV2NDB 112
     C*******              ENDIF
     C*******    RR3RDB    IFEQ 'Y'
     C*******              Z-ADDEL3AAM    SV3RDB 112
     C*******              ENDIF
     C*******    RR4THB    IFEQ 'Y'
     C*******              Z-ADDEL4AAM    SV4THB 112
     C*******              ENDIF
     C*******    RR5THB    IFEQ 'Y'
     C*******              Z-ADDEL5AAM    SV5THB 112
     C*******              ENDIF
     C     RR1STB        IFEQ      'Y'
     C                   Z-ADD     EL1AAM        SV1STB           11 2
     C                   ELSE
     C                   Z-ADD     EL1AAM        SU1STB           11 2
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   Z-ADD     EL2AAM        SV2NDB           11 2
     C                   ELSE
     C                   Z-ADD     EL2AAM        SU2NDB           11 2
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   Z-ADD     EL3AAM        SV3RDB           11 2
     C                   ELSE
     C                   Z-ADD     EL3AAM        SU3RDB           11 2
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   Z-ADD     EL4AAM        SV4THB           11 2
     C                   ELSE
     C                   Z-ADD     EL4AAM        SU4THB           11 2
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   Z-ADD     EL5AAM        SV5THB           11 2
     C                   ELSE
     C                   Z-ADD     EL5AAM        SU5THB           11 2
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     RR6THB        IFEQ      'Y'
     C                   Z-ADD     EL6AAM        SV6THB           11 2
     C                   ELSE
     C                   Z-ADD     EL6AAM        SU6THB           11 2
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   Z-ADD     EL7AAM        SV7THB           11 2
     C                   ELSE
     C                   Z-ADD     EL7AAM        SU7THB           11 2
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  01/11/08  ************************************
     C*** END CHG ***** 09/17/07 **************************************
     C*** END ADD *****  05/15/07  ************************************
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*** BEG ADD ***** 09/17/07 **************************************
     C     RR1STH        IFNE      *BLANKS
     C                   MOVEL(P)  RR1STH        SC1STH
     C                   ELSE
     C                   MOVEL(P)  CA(1)         SC1STH
     C                   ENDIF
     C*
     C     RR2NDH        IFNE      *BLANKS
     C                   MOVEL(P)  RR2NDH        SC2NDH
     C                   ELSE
     C                   MOVEL(P)  CA(2)         SC2NDH
     C                   ENDIF
     C*
     C     RR3RDH        IFNE      *BLANKS
     C                   MOVEL(P)  RR3RDH        SC3RDH
     C                   ELSE
     C                   MOVEL(P)  CA(3)         SC3RDH
     C                   ENDIF
     C*
     C     RR4THH        IFNE      *BLANKS
     C                   MOVEL(P)  RR4THH        SC4THH
     C                   ELSE
     C                   MOVEL(P)  CA(4)         SC4THH
     C                   ENDIF
     C*
     C     RR5THH        IFNE      *BLANKS
     C                   MOVEL(P)  RR5THH        SC5THH
     C                   ELSE
     C                   MOVEL(P)  CA(5)         SC5THH
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     RR6THH        IFNE      *BLANKS
     C                   MOVEL(P)  RR6THH        SC6THH
     C                   ELSE
     C                   MOVEL(P)  CA(6)         SC6THH
     C                   ENDIF
     C*
     C     RR7THH        IFNE      *BLANKS
     C                   MOVEL(P)  RR7THH        SC7THH
     C                   ELSE
     C                   MOVEL(P)  CA(7)         SC7THH
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD ***** 09/17/07 **************************************
     C*
     C*** END ADD *****  11/15/99  ************************************
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     XXBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C                   MOVEA     AB            AB1
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C     ELKY1         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*** BEGIN DEL ***  11/11/03  ************************************
     C*********  ELARSM    IFEQ 'Y'
     C*********  XXWAR     ANDNE'Y'
     C*********            MOVE 'Y'       XXSUM
     C*********  ELANPR    SUB  ELPRTD    X1ANPR 112
     C*********            ADD  X1ANPR    XTANPR
     C*********            ADD  ELTXDU    XTTXDU
     C*
     C*********            ELSE
     C*** END DEL *****  11/11/03  ************************************
     C*
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*  LOAD NEW BILLING SCHEDULE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C                   MOVEL     ELBSCD        SCBSCD
     C     TBKY          CHAIN     PBS01                              60
     C                   MOVEA     AB            AB2
     C*
     C*** BEGIN DEL ***  11/15/99  ************************************
     C********   ELKY      CHAINPEL02                60
     C*** END DEL *****  11/15/99  ************************************
     C*** BEGIN DEL ***  02/19/98  ************************************
     C*********  *IN60     IFEQ '1'
     C*** END DEL *****  02/19/98  ************************************
     C*** BEGIN DEL ***  11/15/99  ************************************
     C********             EXSR LDOVRF
     C********             EXSR BILL
     C********             EXSR DLTOVR
     C********   ELKY      CHAINPEL02                60
     C*** END DEL *****  11/15/99  ************************************
     C*** BEGIN DEL ***  02/19/98  ************************************
     C*********            END
     C*** END DEL *****  02/19/98  ************************************
     C*
     C*  CLEAR DATE AND AMOUNT ARRAYS TO BE USED ON ADJUSTMENT SCREEN
     C*
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C                   MOVEA     *ZEROS        P1
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD   ***  05/15/07  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     SVARSM        IFNE      'Y'
     C     *IN67         ANDEQ     *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     0             SVMAPR
     C                   Z-ADD     0             SVSTPR
     C                   Z-ADD     0             SVNMPR
     C                   Z-ADD     0             SVANPR
     C                   Z-ADD     0             SVTXDU
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     0             SV1STB
     C                   Z-ADD     0             SV2NDB
     C                   Z-ADD     0             SV3RDB
     C                   Z-ADD     0             SV4THB
     C                   Z-ADD     0             SV5THB
     C*** BEGIN ADD ***  01/11/08  ************************************
     C                   Z-ADD     0             SU1STB
     C                   Z-ADD     0             SU2NDB
     C                   Z-ADD     0             SU3RDB
     C                   Z-ADD     0             SU4THB
     C                   Z-ADD     0             SU5THB
     C*** END ADD *****  01/11/08  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             SV6THB
     C                   Z-ADD     0             SV7THB
     C                   Z-ADD     0             SU6THB
     C                   Z-ADD     0             SU7THB
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C                   ENDIF                                                  SVARSM IFNE
     C*** END ADD *****  02/28/06  ************************************
     C*
     C*  COMPUTE ADJUSTMENT IF NEW BILLING SCHEDULE DESIGNATES A
     C*  SELF-REPORTING ACCOUNT
     C*
     C     BSSEFL        IFEQ      'S'
     C*** BEGIN CHG ***  02/20/01  ************************************
     C*********            MOVE *ON       *IN81
     C     XXDIV         IFEQ      SCDIV
     C                   MOVE      *ON           *IN81
     C                   ENDIF
     C*** END CHG *****  02/20/01  ************************************
     C*
     C                   Z-ADD     *ZEROS        XXADJ            11 2
     C                   Z-ADD     *ZEROS        XXADJ2           11 2
     C     F1DPS         IFEQ      'N'
     C     ELENMP        SUB       SVNMPR        XXADJ
     C                   ELSE
     C     F1DPS         IFEQ      'M'
     C     ELMAPR        SUB       SVMAPR        XXADJ
     C                   ELSE
     C     F1DPS         IFEQ      'S'
     C     ELSTPR        SUB       SVSTPR        XXADJ
     C*** BEGIN ADD ***  04/07/05  ************************************
     C                   ELSE
     C*** BEGIN CHG ***  01/08/07  ************************************
     C*********  ELANPR    SUB  S2ANPR    XXADJ
     C     ELANPR        SUB       ELPRTD        XXAM12           11 2
     C     XXAM12        SUB       S2ANPR        XXADJ
     C*** END CHG *****  01/08/07  ************************************
     C*** END ADD *****  04/07/05  ************************************
     C                   END
     C                   END
     C                   END
     C*
     C*** BEG ADD *****  08/09/06  ************************************
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDMFL) IF THE POLICY YEAR
      *  IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      *ON           XXDPYM            1
     C                   ELSE
     C                   MOVE      *OFF          XXDPYM            1
     C                   ENDIF
     C*** END ADD *****  08/09/06  ************************************
     C*
     C                   Z-ADD     *ZEROS        XXARPR           11 2
     C                   MOVE      'H'           XXARTY
     C     EAKY3         CHAIN     PEA03                              50           ALL FYRS
     C     *IN50         DOWEQ     '0'
     C     EAFYR         ANDLE     XXFYR
     C*** BEGIN ADD ***  01/24/06  ************************************
     C*** BEG CHG *****  08/09/06  ************************************
      **********
     C********** CLTID     IFEQ '109'
     C********** CLTID     OREQ '136'
     C**********
     C********** EAFYR     IFEQ XXFYR
     C**********           ADD  EAAMT     XXARPR
     C**********           ENDIF
     C**********
     C**********           ELSE
     C****END*ADD *****  01/24/06  ************************************
     C**********           ADD  EAAMT     XXARPR
     C****BEGIN*ADD ***  01/24/06  ************************************
     C**********           ENDIF                           CLTID  IFEQ
     C*** END ADD *****  01/24/06  ************************************
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C     XXDPYM        OREQ      *ON
     C     EAFYR         IFEQ      XXFYR
     C                   ADD       EAAMT         XXARPR
     C                   ENDIF
     C                   OTHER
     C     EAFYR         IFLE      XXFYR
      *
      * DO NOT INCLUDE PRIOR YEAR DEPOSIT TRANSACTIONS IF THAT YEAR IS FLAGGED
      * AS A PER YEAR DEPOSIT MODE
      *
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C     F1DPYM        IFEQ      *BLANK
     C     EAFYR         ANDLT     XXFYR
     C                   ADD       EAAMT         XXARPR
     C                   ENDIF
      *
     C     EAFYR         IFEQ      XXFYR
     C                   ADD       EAAMT         XXARPR
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDSL
     C*** END CHG *****  08/09/06  ************************************
     C     EAKY3         READE     PEA03                                  50
     C                   END                                                    *IN50
     C*
     C****** BEGIN ADD ******* 05/01/09 *******************************
     C                   Z-ADD     XXARPR        XXDEPB           11 2
     C****** BEGIN ADD ******* 05/01/09 *******************************
     C*** BEG ADD *****  08/09/06  ************************************
     C*
     C* RESET GROUP DETAIL
     C*
     C     F1KY          SETLL     PF101
     C     F1KY          READE     PF101                                  60
     C*** END ADD *****  08/09/06  ************************************
     C*** BEGIN CHG ***  04/21/03  ************************************
     C*********  BSAB01    IFNE *ZEROS
     C*********  XXARPR    DIV  BSAB01    XXARPR
     C*
     C                   Z-ADD     BSAB01        XXAB01            7 6
     C     BSAB01        IFEQ      *ZERO
     C                   Z-ADD     BSAB02        XXAB01
     C                   ENDIF
     C*
     C     XXAB01        IFNE      *ZEROS
     C     XXARPR        DIV       XXAB01        XXARPR
     C*** END CHG *****  04/21/03  ************************************
     C*** BEGIN DEL ***  01/15/99  ************************************
     C*****                ELSE
     C*****                Z-ADD*ZEROS    XXARPR
     C*** END DEL *****  01/15/99  ************************************
     C                   END
     C*
     C     F1DPS         IFEQ      'N'
     C     SVNMPR        SUB       XXARPR        XXADJ2
     C                   ELSE
     C     F1DPS         IFEQ      'M'
     C     SVMAPR        SUB       XXARPR        XXADJ2
     C                   ELSE
     C     F1DPS         IFEQ      'S'
     C     SVSTPR        SUB       XXARPR        XXADJ2
     C*** BEGIN ADD ***  04/07/05  ************************************
     C                   ELSE
     C     S2ANPR        SUB       XXARPR        XXADJ2
     C*** END ADD *****  04/07/05  ************************************
     C                   END                                                    F1DPS = 'N'
     C                   END                                                    F1DPS = 'M'
     C                   END                                                    F1DPS = 'S'
     C*** BEGIN ADD ***  02/28/06  ************************************
     C*
     C*  SET ADJUSTMENT TO DIFFERENCE IN OLD AND NEW PREMIUM ONLY IF
     C*  RUNNING FOR SINGLE DIVISION BILL.
     C*
     C     *IN67         IFEQ      *OFF
     C     ELARSM        ORNE      'Y'
     C*** END ADD *****  02/28/06  ************************************
     C                   ADD       XXADJ2        XXADJ
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ENDIF                                                  *IN67 IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     XXADJ         XXSAVE           11 2
     C     XXADJ         IFLT      *ZERO
     C                   MULT      -1            XXADJ
     C                   END
     C                   MOVE      XXADJ         XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXADJ
     C                   END
     C                   MOVE      '00'          XXADJ
     C     XXSAVE        IFLT      *ZERO
     C     XXADJ         MULT      -1            XXADJ
     C                   END
     C*
     C*** BEGIN CHG ***  04/21/03  ************************************
     C*********  XXADJ     MULT BSAB01    XXWK90  90H
     C*** BEGIN ADD ***  01/15/99  ************************************
     C*********  BSAB01    IFEQ *ZERO
     C     XXADJ         MULT(H)   XXAB01        XXWK90            9 0
     C     XXAB01        IFEQ      *ZERO
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         ANDEQ     *OFF
     C     XXAB01        OREQ      *ZERO
     C     ELARSM        ANDNE     'Y'
     C*** END ADD *****  02/28/06  ************************************
     C*** END CHG *****  04/21/03  ************************************
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  BSAB01 IFEQ
     C*** END ADD *****  01/15/99  ************************************
     C                   Z-ADD     XXWK90        SCTOT
     C*** BEGIN ADD ***  03/09/98  ************************************
     C                   MOVE      *BLANK        XXTHRS            1
     C     F1BVFL        IFNE      'Y'
     C*** BEGIN ADD ***  01/07/02  ************************************
     C     ELBSCD        ANDEQ     XXBSCD
     C*** END ADD *****  01/07/02  ************************************
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ELSE
     C*
     C     XXWK90        IFGE      0
     C*
     C* DO NOT POST ADJ IF LESS THAN DEBIT THRESHOLD
     C*
     C     XXWK90        IFLT      F1DADJ
     C**** BEGIN ADD ******* 05/01/09 *********************************
     C     XXDEPB        ANDNE     *ZERO
     C**** END ADD   ******* 05/01/09 *********************************
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ENDIF                                                  XXWK90 IFLT
     C*
     C                   ELSE
     C*
     C* DO NOT POST ADJ IF GREATER THAN CREDIT THRESHOLD
     C*
     C     XXWK90        MULT      -1            XXTMP             9 0
     C*
     C     XXTMP         IFLT      F1CADJ
     C**** BEGIN ADD ******* 05/01/09 *********************************
     C     XXDEPB        ANDNE     *ZERO
     C**** END ADD   ******* 05/01/09 *********************************
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ENDIF                                                  XXTMP IFLT
     C*
     C                   ENDIF                                                  XXWK90 IFGE
     C*
     C                   ENDIF                                                  F1BVFL IFNE
     C*** END ADD *****  03/09/98  ************************************
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C*  DO NOT BILL DEPOSIT ADJUSTMENTS IF THE ACCOUNT HAS TRANSFERED
     C*  ITS DEPOSIT.
     C*
     C                   MOVE      *BLANK        ##TDFL            1
     C*
     C     EAKY5         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     EAARCD        IFEQ      F1SHTR
     C     EAARCD        OREQ      F1SITR
     C                   MOVE      'Y'           ##TDFL
     C                   ENDIF                                                  EAARCD IFEQ
     C*
     C     EAKY5         READE     PEA01                                  50
     C                   ENDDO                                                  *IN50
     C*
      *
     C     ##TDFL        IFEQ      'Y'
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ENDIF
     C*
     C                   ENDSL
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C*** BEGIN ADD ***  11/11/03  ************************************
     C                   Z-ADD     SCTOT         XXDEPP           11 2
     C*** END ADD *****  11/11/03  ************************************
     C*
     C*  CALCULATE THE STATE TAX ADJUSTMENT
     C*
     C*** BEGIN ADD ***  10/10/03  ************************************
     C                   Z-ADD     0             XXDEPA           11 2
     C                   MOVE      'H'           XXARTY
     C     EAKY2         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C                   ADD       EAAMT         XXDEPA
     C     EAKY2         READE     PEA01                                  50
     C                   ENDDO                                                  *IN50
     C*** END ADD *****  10/10/03  ************************************
     C                   Z-ADD     0             XXTXDU           11 2
     C                   MOVE      'F'           XXARTY
     C     EAKY2         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C*** BEGIN ADD ***  04/12/99  ************************************
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'S'
     C*** END ADD *****  04/12/99  ************************************
     C                   ADD       EAAMT         XXTXDU
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C                   END                                                    *IN50
     C*
     C*** BEGIN ADD ***  03/09/98  ************************************
     C*****      ELTXDU    SUB  XXTXDU    XXTXDU
     C     RRTDFL        IFNE      'Y'
     C     XXTHRS        OREQ      'Y'
     C*** BEGIN CHG ***  10/10/03  ************************************
     C*********            Z-ADD0         XXTXDU
     C     RRTDFL        IFNE      'Y'
     C                   MULT      -1            XXTXDU
     C                   ELSE
     C     XXDEPA        MULT(H)   RRTXPC        XXWK90
     C                   MULT(H)   .01           XXWK90
     C     XXWK90        SUB       XXTXDU        XXTXDU
     C                   ENDIF                                                  RRTDFL IFNE
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     0             XXTXDU
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C*** END CHG *****  10/10/03  ************************************
     C                   ELSE
     C*
     C*** BEGIN CHG ***  04/21/03  ************************************
     C*********  BSAB01    MULT XXADJ     XXWK90    H
     C*** BEGIN ADD ***  01/15/99  ************************************
     C*********  BSAB01    IFEQ *ZERO
     C     XXAB01        MULT(H)   XXADJ         XXWK90
     C     XXAB01        IFEQ      *ZERO
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         ANDEQ     *OFF
     C     XXAB01        OREQ      *ZERO
     C     ELARSM        ANDNE     'Y'
     C*** END ADD *****  02/28/06  ************************************
     C*** END CHG *****  04/21/03  ************************************
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  BSAB01 IFEQ
     C*** END ADD *****  01/15/99  ************************************
     C*** BEGIN ADD ***  10/10/03  ************************************
     C                   ADD       XXWK90        XXDEPA
     C*** END ADD *****  10/10/03  ************************************
     C                   MULT(H)   RRTXPC        XXWK90
     C                   MULT(H)   .01           XXWK90
     C*** BEGIN CHG ***  10/10/03  ************************************
     C*********            Z-ADDXXWK90    XXTXDU
     C     XXDEPA        MULT(H)   RRTXPC        XXTEMP           11 0
     C                   MULT(H)   .01           XXTEMP
     C     XXTEMP        SUB       XXTXDU        XXTXDU
     C*** END CHG *****  10/10/03  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     XXWK90        XXTXDU
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C*
     C                   ENDIF                                                  RRTDFL IFNE
     C*
     C*** END ADD *****  03/09/98  ************************************
     C                   ADD       XXTXDU        SCTOT
     C                   MOVE      '0'           *IN80
     C*** BEGIN ADD ***  11/11/03  ************************************
     C     ELARSM        IFEQ      'Y'
     C*
     C     XXWAR         IFNE      'Y'
     C                   MOVE      'Y'           XXSUM
     C                   ADD       XXDEPP        XTANPR
     C                   ADD       XXTXDU        XTTXDU
     C                   Z-ADD     0             SCTOT
     C                   ELSE
     C                   Z-ADD     XTANPR        XXDEPP
     C                   Z-ADD     XTTXDU        XXTXDU
     C     XXDEPP        ADD       XXTXDU        SCTOT
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*** END ADD *****  11/11/03  ************************************
     C*** BEGIN ADD ***  11/12/08  ************************************
     C                   Z-ADD     0             X
     C                   MOVE      *OFF          *IN89
     C*
     C* BILL THE ASSESSMENTS INTO INSTALLMENTS IF FLAGGED TO DO SO,
     C* OTHERWISE BILL IN LUMP SUM.  ASSESSMENT CODING WILL BE HANDLED
     C* BY W8011R FOR S/R POLICIES FOR CLIENTS NOT FLAGGED TO BILL HERE.
     C*
     C     CLTID         IFEQ      '139'
     C*
     C                   MOVE      *ON           *IN89
     C*
     C* STORE UP ASSESSMENTS FOR SUMMARY.
     C*
     C     ELARSM        IFEQ      'Y'
     C     XXWAR         ANDNE     'Y'
     C*
     C     RR1STB        IFEQ      'Y'
     C                   ADD       EL1AAM        XT1STB
     C                   ELSE
     C                   ADD       EL1AAM        XL1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   ADD       EL2AAM        XT2NDB
     C                   ELSE
     C                   ADD       EL2AAM        XL2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   ADD       EL3AAM        XT3RDB
     C                   ELSE
     C                   ADD       EL3AAM        XL3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   ADD       EL4AAM        XT4THB
     C                   ELSE
     C                   ADD       EL4AAM        XL4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   ADD       EL5AAM        XT5THB
     C                   ELSE
     C                   ADD       EL5AAM        XL5THB
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     RR6THB        IFEQ      'Y'
     C                   ADD       EL6AAM        XT6THB
     C                   ELSE
     C                   ADD       EL6AAM        XL6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   ADD       EL7AAM        XT7THB
     C                   ELSE
     C                   ADD       EL7AAM        XL7THB
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*
     C     RR1STB        IFEQ      'Y'
     C                   ADD       SV1STB        ST1STB
     C                   ELSE
     C                   ADD       SU1STB        SL1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   ADD       SV2NDB        ST2NDB
     C                   ELSE
     C                   ADD       SU2NDB        SL2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   ADD       SV3RDB        ST3RDB
     C                   ELSE
     C                   ADD       SU3RDB        SL3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   ADD       SV4THB        ST4THB
     C                   ELSE
     C                   ADD       SU4THB        SL4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   ADD       SV5THB        ST5THB
     C                   ELSE
     C                   ADD       SU5THB        SL5THB
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     RR6THB        IFEQ      'Y'
     C                   ADD       SV6THB        ST6THB
     C                   ELSE
     C                   ADD       SU6THB        SL6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   ADD       SV7THB        ST7THB
     C                   ELSE
     C                   ADD       SU7THB        SL7THB
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ELSE
     C*
     C* PROCESS CALCULATIONS OF ADJUSTMENTS TO THE ASSESSMENTS.
     C*
     C     XXWAR         IFEQ      'Y'
     C*
     C* SET SUMMARY TOTALS INTO WORKING ASSESSMENT FIELDS.
     C*
     C                   Z-ADD     XT1STB        XX1STB           11 2
     C                   Z-ADD     ST1STB        SV1STB
     C                   Z-ADD     XT2NDB        XX2NDB           11 2
     C                   Z-ADD     ST2NDB        SV2NDB
     C                   Z-ADD     XT3RDB        XX3RDB           11 2
     C                   Z-ADD     ST3RDB        SV3RDB
     C                   Z-ADD     XT4THB        XX4THB           11 2
     C                   Z-ADD     ST4THB        SV4THB
     C                   Z-ADD     XT5THB        XX5THB           11 2
     C                   Z-ADD     ST5THB        SV5THB
     C                   Z-ADD     XL1STB        XU1STB           11 2
     C                   Z-ADD     SL1STB        SU1STB
     C                   Z-ADD     XL2NDB        XU2NDB           11 2
     C                   Z-ADD     SL2NDB        SU2NDB
     C                   Z-ADD     XL3RDB        XU3RDB           11 2
     C                   Z-ADD     SL3RDB        SU3RDB
     C                   Z-ADD     XL4THB        XU4THB           11 2
     C                   Z-ADD     SL4THB        SU4THB
     C                   Z-ADD     XL5THB        XU5THB           11 2
     C                   Z-ADD     SL5THB        SU5THB
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     XT6THB        XX6THB           11 2
     C                   Z-ADD     ST6THB        SV6THB
     C                   Z-ADD     XT7THB        XX7THB           11 2
     C                   Z-ADD     ST7THB        SV7THB
     C                   Z-ADD     XL6THB        XU6THB           11 2
     C                   Z-ADD     SL6THB        SU6THB
     C                   Z-ADD     XL7THB        XU7THB           11 2
     C                   Z-ADD     SL7THB        SU7THB
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ELSE
     C*
     C* SET CURRENT AMOUNT FIELDS FOR EACH ASSESSMENT FOR NON-SUMMARY.
     C*
     C     RR1STB        IFEQ      'Y'
     C                   Z-ADD     EL1AAM        XX1STB
     C                   ELSE
     C                   Z-ADD     EL1AAM        XU1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   Z-ADD     EL2AAM        XX2NDB
     C                   ELSE
     C                   Z-ADD     EL2AAM        XU2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   Z-ADD     EL3AAM        XX3RDB
     C                   ELSE
     C                   Z-ADD     EL3AAM        XU3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   Z-ADD     EL4AAM        XX4THB
     C                   ELSE
     C                   Z-ADD     EL4AAM        XU4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   Z-ADD     EL5AAM        XX5THB
     C                   ELSE
     C                   Z-ADD     EL5AAM        XU5THB
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     RR6THB        IFEQ      'Y'
     C                   Z-ADD     EL6AAM        XX6THB
     C                   ELSE
     C                   Z-ADD     EL6AAM        XU6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   Z-ADD     EL7AAM        XX7THB
     C                   ELSE
     C                   Z-ADD     EL7AAM        XU7THB
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  XXWAR IFEQ 'Y'
     C*
     C* COUNT THE NUMBER OF MONTHS ON THE POLICY TO BE REPORTED.
     C*
     C                   MOVE      ELPFDT        X1PFDT
     C                   MOVEL     ELPTDT        XXYMX             5 0
     C                   Z-ADD     0             XXCNT
     C*
     C     YYMM1         DOWLT     XXYMX
     C                   ADD       1             XXCNT
     C                   ADD       1             MM1
     C*
     C     MM1           IFEQ      13
     C                   Z-ADD     1             MM1
     C                   ADD       1             YY1
     C                   ENDIF
     C*
     C                   ENDDO                                                  YYMM1 DOWLT XXYMX
     C*
     C                   MOVE      '01'          DD1
     C*
     C* IF POLICY DOES NOT END ON 1ST OF MONTH, THEN ADD IN EXTRA MONTH.
     C*
     C     X1PFDT        IFLT      ELPTDT
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*
     C* IF NO REPORTINGS HAVE BEEN SET THEN BILL ALL TO ONE AMOUNT.
     C*
     C     XXCNT         IFEQ      0
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*
     C* SET UP ARRAY OF POSTING DATES FOR S/R ASSESSMENT INSTALLMENTS.
     C*
     C                   CLEAR                   WKD
     C                   MOVE      ELPFDT        X1PFDT
     C*
     C     1             DO        XXCNT         L                 2 0
     C                   MOVE      X1PFDT        WKD(L)
     C                   MOVE      MD(MM1)       WKD(L)
     C*
     C* CHECK IF FEB 29TH IS A VALID DATE AND USE FOR THAT MONTH.
     C*
     C     MM1           IFEQ      2
     C                   Z-ADD     WKD(L)        XXDTE
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVE      29            WKD(L)
     C                   ENDIF
     C*
     C                   ENDIF                                                  MM1 IFEQ 2
     C*
     C                   ADD       1             MM1
     C*
     C     MM1           IFEQ      13
     C                   Z-ADD     1             MM1
     C                   ADD       1             YY1
     C                   ENDIF
     C*
     C                   ENDDO                                                  1 DO XXCNT
     C*
     C                   SUB       1             L
     C*
     C     WKD(L)        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        WKD(L)
     C                   ENDIF
     C*
     C                   ADD       1             L
     C                   MOVE      *HIVAL        WKD(L)
     C*
     C* CLEAR WORKING FIELDS.
     C*
     C                   Z-ADD     *ZEROS        W
     C                   Z-ADD     *ZEROS        Y
     C                   Z-ADD     *ZEROS        Z
     C                   Z-ADD     *ZEROS        XXDTE
     C                   Z-ADD     *ZEROS        XXDTE1
     C                   Z-ADD     *ZEROS        XXDTE2
     C                   Z-ADD     *ZEROS        XXDIFF
     C                   Z-ADD     *ZEROS        YRMO
     C                   Z-ADD     *ZEROS        YYMM
     C                   MOVE      '0'           XXFUT
     C                   Z-ADD     *ZEROS        X1ATOT
     C                   Z-ADD     *ZEROS        X1AA1
     C                   Z-ADD     *ZEROS        X1AA2
     C                   Z-ADD     *ZEROS        X1AA3
     C                   Z-ADD     *ZEROS        X1AS3
     C                   Z-ADD     *ZEROS        X2ATOT
     C                   Z-ADD     *ZEROS        X2AA1
     C                   Z-ADD     *ZEROS        X2AA2
     C                   Z-ADD     *ZEROS        X2AA3
     C                   Z-ADD     *ZEROS        X2AS3
     C                   Z-ADD     *ZEROS        X3ATOT
     C                   Z-ADD     *ZEROS        X3AA1
     C                   Z-ADD     *ZEROS        X3AA2
     C                   Z-ADD     *ZEROS        X3AA3
     C                   Z-ADD     *ZEROS        X3AS3
     C                   Z-ADD     *ZEROS        X4ATOT
     C                   Z-ADD     *ZEROS        X4AA1
     C                   Z-ADD     *ZEROS        X4AA2
     C                   Z-ADD     *ZEROS        X4AA3
     C                   Z-ADD     *ZEROS        X4AS3
     C                   Z-ADD     *ZEROS        X5ATOT
     C                   Z-ADD     *ZEROS        X5AA1
     C                   Z-ADD     *ZEROS        X5AA2
     C                   Z-ADD     *ZEROS        X5AA3
     C                   Z-ADD     *ZEROS        X5AS3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     *ZEROS        X6ATOT
     C                   Z-ADD     *ZEROS        X6AA1
     C                   Z-ADD     *ZEROS        X6AA2
     C                   Z-ADD     *ZEROS        X6AA3
     C                   Z-ADD     *ZEROS        X6AS3
     C                   Z-ADD     *ZEROS        X7ATOT
     C                   Z-ADD     *ZEROS        X7AA1
     C                   Z-ADD     *ZEROS        X7AA2
     C                   Z-ADD     *ZEROS        X7AA3
     C                   Z-ADD     *ZEROS        X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C*
     C* CALCULATE MONTHLY CHARGE FOR ASSESSMENT.
     C*
     C     XX1STB        DIV(H)    XXCNT         X1AMT            11 2
     C     SV1STB        DIV(H)    XXCNT         S1AMT            11 2
     C*
     C     XX2NDB        DIV(H)    XXCNT         X2AMT            11 2
     C     SV2NDB        DIV(H)    XXCNT         S2AMT            11 2
     C*
     C     XX3RDB        DIV(H)    XXCNT         X3AMT            11 2
     C     SV3RDB        DIV(H)    XXCNT         S3AMT            11 2
     C*
     C     XX4THB        DIV(H)    XXCNT         X4AMT            11 2
     C     SV4THB        DIV(H)    XXCNT         S4AMT            11 2
     C*
     C     XX5THB        DIV(H)    XXCNT         X5AMT            11 2
     C     SV5THB        DIV(H)    XXCNT         S5AMT            11 2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     XX6THB        DIV(H)    XXCNT         X6AMT            11 2
     C     SV6THB        DIV(H)    XXCNT         S6AMT            11 2
     C*
     C     XX7THB        DIV(H)    XXCNT         X7AMT            11 2
     C     SV7THB        DIV(H)    XXCNT         S7AMT            11 2
     C*** END ADD *****  12/11/08  ************************************
     C*
     C* ROUND INSTALLMENTS TO WHOLE DOLLAR IF FLAGGED.
     C*
     C     EXRNDI        IFEQ      'Y'
     C*
     C                   Z-ADD(H)  X1AMT         XXWK10
     C                   Z-ADD     XXWK10        X1AMT
     C                   Z-ADD(H)  S1AMT         XXWK10
     C                   Z-ADD     XXWK10        S1AMT
     C*
     C                   Z-ADD(H)  X2AMT         XXWK10
     C                   Z-ADD     XXWK10        X2AMT
     C                   Z-ADD(H)  S2AMT         XXWK10
     C                   Z-ADD     XXWK10        S2AMT
     C*
     C                   Z-ADD(H)  X3AMT         XXWK10
     C                   Z-ADD     XXWK10        X3AMT
     C                   Z-ADD(H)  S3AMT         XXWK10
     C                   Z-ADD     XXWK10        S3AMT
     C*
     C                   Z-ADD(H)  X4AMT         XXWK10
     C                   Z-ADD     XXWK10        X4AMT
     C                   Z-ADD(H)  S4AMT         XXWK10
     C                   Z-ADD     XXWK10        S4AMT
     C*
     C                   Z-ADD(H)  X5AMT         XXWK10
     C                   Z-ADD     XXWK10        X5AMT
     C                   Z-ADD(H)  S5AMT         XXWK10
     C                   Z-ADD     XXWK10        S5AMT
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C                   Z-ADD(H)  X6AMT         XXWK10
     C                   Z-ADD     XXWK10        X6AMT
     C                   Z-ADD(H)  S6AMT         XXWK10
     C                   Z-ADD     XXWK10        S6AMT
     C*
     C                   Z-ADD(H)  X7AMT         XXWK10
     C                   Z-ADD     XXWK10        X7AMT
     C                   Z-ADD(H)  S7AMT         XXWK10
     C                   Z-ADD     XXWK10        S7AMT
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C* SET THE POSTING DATE FOR THE DEPOSIT/TAX ADJUSTMENT.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDT
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             EXRVID
     C                   ENDIF
     C*
     C     SCTOT         IFGT      *ZERO
     C     EXRVID        ANDNE     *ZERO
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
     C                   MOVE      QQDT          SCDT              8 0
     C                   ENDIF
     C*
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SCDT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C* DO NOT ALLOW DEP/TAX POSTING DATE TO BE PRIOR TO BILLING START DATE.
     C*
     C     XXDT          IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   ENDIF                                                  XXDT IFLT
     C*
     C* MOVE DEPOSIT/TAX ADJUSTMENT INTO ARRAY.
     C*
     C     SCTOT         IFNE      *ZERO
     C                   Z-ADD     XXDEPP        PA(1)
     C                   Z-ADD     XXTXDU        P1(1)
     C                   Z-ADD     SCDT          DA(1)
     C                   ENDIF
     C*
     C* SET CURRENT DATE.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT
     C                   PARM      EXRVID        QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C*** BEGIN ADD ***  02/12/09  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  02/12/09  ************************************
     C*
     C* SET CURRENT DATE TO THE END OF THE MONTH TO MATCH INSTALLMENT DATE.
     C*
     C                   Z-ADD     XXCURD        X1PFDT
     C                   MOVE      MD(MM1)       X1PFDT
     C                   Z-ADD     X1PFDT        XXCURD
     C*
     C     MM1           IFEQ      2
     C                   Z-ADD     XXCURD        XXDTE
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVE      29            XXCURD
     C                   ENDIF
     C*
     C                   ENDIF                                                  MM1 IFEQ 2
     C*
     C                   Z-ADD     XXCURD        XXCUR2
     C*
     C* SET BEGINNING DATE TO THE END OF THE MONTH TO MATCH INSTALLMENT DATE.
     C*
     C                   Z-ADD     ELPFDT        X1PFDT
     C                   MOVE      MD(MM1)       X1PFDT
     C                   Z-ADD     X1PFDT        XXBSDT            7 0
     C*
     C     MM1           IFEQ      2
     C                   Z-ADD     XXBSDT        XXDTE
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVE      29            XXBSDT
     C                   ENDIF
     C*
     C                   ENDIF                                                  MM1 IFEQ 2
     C*
     C                   MOVEL     XXCURD        YRMO1
     C                   MOVE      YRMO1         YRMO
     C                   MOVEL     ELPFDT        YYMM
     C                   Z-ADD     1             Y
     C                   Z-ADD     *ZEROS        XXTOT
     C*
     C* LOOP THROUGH ALL MONTHS ON POLICY PRIOR TO CURRENT POSTING DATE.
     C*
     C     Y             DOWLE     XXCNT
     C     YYMM          ANDLE     YRMO
     C*
     C     Y             ADD       1             W
     C*
     C                   Z-ADD     WKD(Y)        XXDTE1
     C*
     C     WKD(W)        IFGT      XXCURD
     C*
     C*  WE NEED TO INCLUDE ANY BILLINGS OR ADJUSTMENTS WITH TODAYS DATE
     C*
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C*  COMPUTE TODAYS DATE + 1 SO THAT SRWASB WHICH LOOKS FOR 'A'
     C*  OR 'B' A/R RECORDS WITH EAPODT >= XXDTE1 AND EAPODT < XXDTE2
     C*  WILL INCLUDE TODAYS DATE.
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     XXCURD        XXDTE2
     C*
     C     SRRTCD        IFEQ      '0'
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C                   END
     C*
     C                   Z-ADD     XXDTE2        XXCUR2
     C*
     C                   ELSE
     C                   Z-ADD     WKD(W)        XXDTE2
     C                   END                                                    WKD,W >= XXCURD
     C*
     C* IF THE END DATE IS BEFORE THE FIRST POSTING DATE, MOVE IT UP TO THE
     C* FIRST POSTING DATE.
     C*
     C     XXDTE2        IFLT      XXBSDT
     C                   Z-ADD     XXBSDT        XXDTE2
     C*
     C* USE THE FIRST POSTING DATE + 1 DAY
     C*
     C                   Z-ADD     XXBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C     SRRTCD        IFEQ      '0'
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C*
     C                   ENDIF                                                  SRRTCD IFEQ
     C*
     C                   ENDIF                                                  XXDTE2 IFLT
     C*
     C* CALL SUBROUTINE TO ACCUMULATE PRIOR BILLED FROM A/R.
     C*
     C                   EXSR      SRWASB
     C*
     C* 1ST ASSESSMENT
     C*
     C                   Z-ADD(H)  X1AMT         X1AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S1AMT         X1AA1
     C*
     C     Y             IFEQ      1
     C                   ADD       SU1STB        X1AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X1AA2         SUB       X1AA1         XXDIFF
     C                   ADD       XXDIFF        X1ATOT
     C                   ADD       X1AA2         X1AA3
     C                   ADD       X1AA1         X1AS3
     C*
     C* 2ND ASSESSMENT
     C*
     C                   Z-ADD(H)  X2AMT         X2AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S2AMT         X2AA1
     C*
     C     Y             IFEQ      1
     C                   ADD       SU2NDB        X2AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X2AA2         SUB       X2AA1         XXDIFF
     C                   ADD       XXDIFF        X2ATOT
     C                   ADD       X2AA2         X2AA3
     C                   ADD       X2AA1         X2AS3
     C*
     C* 3RD ASSESSMENT
     C*
     C                   Z-ADD(H)  X3AMT         X3AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S3AMT         X3AA1
     C*
     C     Y             IFEQ      1
     C                   ADD       SU3RDB        X3AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X3AA2         SUB       X3AA1         XXDIFF
     C                   ADD       XXDIFF        X3ATOT
     C                   ADD       X3AA2         X3AA3
     C                   ADD       X3AA1         X3AS3
     C*
     C* 4TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X4AMT         X4AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S4AMT         X4AA1
     C*
     C     Y             IFEQ      1
     C                   ADD       SU4THB        X4AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X4AA2         SUB       X4AA1         XXDIFF
     C                   ADD       XXDIFF        X4ATOT
     C                   ADD       X4AA2         X4AA3
     C                   ADD       X4AA1         X4AS3
     C*
     C* 5TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X5AMT         X5AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S5AMT         X5AA1
     C*
     C     Y             IFEQ      1
     C                   ADD       SU5THB        X5AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X5AA2         SUB       X5AA1         XXDIFF
     C                   ADD       XXDIFF        X5ATOT
     C                   ADD       X5AA2         X5AA3
     C                   ADD       X5AA1         X5AS3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X6AMT         X6AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S6AMT         X6AA1
     C*
     C     Y             IFEQ      1
     C                   ADD       SU6THB        X6AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X6AA2         SUB       X6AA1         XXDIFF
     C                   ADD       XXDIFF        X6ATOT
     C                   ADD       X6AA2         X6AA3
     C                   ADD       X6AA1         X6AS3
     C*
     C* 7TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X7AMT         X7AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S7AMT         X7AA1
     C*
     C     Y             IFEQ      1
     C                   ADD       SU7THB        X7AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X7AA2         SUB       X7AA1         XXDIFF
     C                   ADD       XXDIFF        X7ATOT
     C                   ADD       X7AA2         X7AA3
     C                   ADD       X7AA1         X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C*
     C* INCREMENT TO NEXT MONTH.
     C*
     C                   ADD       1             Y
     C                   ADD       1             MM
     C*
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*                                                     &  YYMM <= YRMO
     C                   ENDDO                                                  DOW Y <= 12
     C*                                                     &  YYMM <= YRMO
     C* MOVE CATCH UP ADJUSTMENT AMOUNT INTO ADJUSTMENT ARRAY.
     C*
     C     X1ATOT        IFNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXDT8             8 0
     C*
     C     X1ATOT        IFGT      *ZEROS
     C     X2ATOT        ORGT      *ZEROS
     C     X3ATOT        ORGT      *ZEROS
     C     X4ATOT        ORGT      *ZEROS
     C     X5ATOT        ORGT      *ZEROS
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     X6ATOT        ORGT      *ZEROS
     C     X7ATOT        ORGT      *ZEROS
     C*** END ADD *****  12/11/08  ************************************
     C*
     C     XX1STB        IFNE      *ZERO
     C     XX2NDB        ORNE      *ZERO
     C     XX3RDB        ORNE      *ZERO
     C     XX4THB        ORNE      *ZERO
     C     XX5THB        ORNE      *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XX6THB        ORNE      *ZERO
     C     XX7THB        ORNE      *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   ELSE
     C                   Z-ADD     SCDT          XXDT8
     C                   ENDIF                                                  XX1STB IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* IF THE POSTING DATE IS PRIOR TO THE BILLING START DATE,
     C* CHANGE IT.
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C*
     C     XXTDT         IFLT      XXBSDT
     C*
     C     X1ATOT        IFGT      *ZEROS
     C     X2ATOT        ORGT      *ZEROS
     C     X3ATOT        ORGT      *ZEROS
     C     X4ATOT        ORGT      *ZEROS
     C     X5ATOT        ORGT      *ZEROS
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     X6ATOT        ORGT      *ZEROS
     C     X7ATOT        ORGT      *ZEROS
     C*** END ADD *****  12/11/08  ************************************
     C                   Z-ADD     XXBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   ELSE
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   ENDIF                                                  X1ATOT IFGT
     C*
     C                   ENDIF                                                  XXTDT IFLT
     C*
     C* SET POSTING DATE FOR ASSESSMENT ADJUSTMENT.  IF THE DATE DOES
     C* NOT MATCH THE DEP/TAX ADJUSTMENT DATE THEN PUT IN SLOT #2.
     C*
     C     SCTOT         IFNE      *ZERO
     C     XXDT8         ANDNE     SCDT
     C                   ADD       2             X
     C                   ELSE
     C                   ADD       1             X
     C                   ENDIF
     C*
     C                   Z-ADD     XXDT8         DA(X)
     C*
     C     XX1STB        IFNE      *ZERO
     C                   Z-ADD     X1ATOT        A1(X)
     C                   ELSE
     C                   Z-ADD     X1ATOT        A1(1)
     C                   ENDIF
     C*
     C     XX2NDB        IFNE      *ZERO
     C                   Z-ADD     X2ATOT        A2(X)
     C                   ELSE
     C                   Z-ADD     X2ATOT        A2(1)
     C                   ENDIF
     C*
     C     XX3RDB        IFNE      *ZERO
     C                   Z-ADD     X3ATOT        A3(X)
     C                   ELSE
     C                   Z-ADD     X3ATOT        A3(1)
     C                   ENDIF
     C*
     C     XX4THB        IFNE      *ZERO
     C                   Z-ADD     X4ATOT        A4(X)
     C                   ELSE
     C                   Z-ADD     X4ATOT        A4(1)
     C                   ENDIF
     C*
     C     XX5THB        IFNE      *ZERO
     C                   Z-ADD     X5ATOT        A5(X)
     C                   ELSE
     C                   Z-ADD     X5ATOT        A5(1)
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     XX6THB        IFNE      *ZERO
     C                   Z-ADD     X6ATOT        A6(X)
     C                   ELSE
     C                   Z-ADD     X6ATOT        A6(1)
     C                   ENDIF
     C*
     C     XX7THB        IFNE      *ZERO
     C                   Z-ADD     X7ATOT        A7(X)
     C                   ELSE
     C                   Z-ADD     X7ATOT        A7(1)
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  X1ATOT <> 0
     C*
     C* PROCESS THE REMAINING MONTHS ON THE POLICY.
     C*
     C     Y             DO        XXCNT         Z
     C*
     C     Z             ADD       1             W
     C*
     C*  THE FOLLOWING MUST ONLY BE DONE FOR THE FIRST BILLING PERIOD
     C*  (I.E. FROM WKD,Y TO WKD,Y+1; ONLY THE FIRST TIME THIS CODE IS EXECUTED)
     C*  NEED TO BE SURE TO INCLUDE ANY 'B' OR 'A' A/R RECORDS THAT MAY
     C*  HAVE BEEN MANUALLY POSTED BETWEEN THE CURRENT DATE (OR CURRENT
     C*  DATE + 1) AND THE NEXT BILLING PERIOD (WKD,W)
     C*
     C     XXCUR2        IFNE      *ZEROS
     C                   Z-ADD     XXCUR2        XXDTE1
     C                   Z-ADD     *ZEROS        XXCUR2
     C                   ELSE
     C                   Z-ADD     WKD(Z)        XXDTE1
     C                   ENDIF                                                  XXCUR2 <> 0
     C*
     C                   Z-ADD     WKD(W)        XXDTE2
     C*
     C* IF THE START DATE IS PRIOR TO THE BILLING START DATE,
     C* CHANGE IT.
     C*
     C     XXDTE1        IFLT      XXBSDT
     C                   Z-ADD     WKD(Z)        XXDTE1
     C                   ENDIF                                                  XXDTE1 IFLT
     C*
     C                   EXSR      SRWASB
      *
     C* 1ST ASSESSMENT
      *
     C                   Z-ADD(H)  X1AMT         X1AA2
     C     Z             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S1AMT         X1AA1
     C*
     C     Z             IFEQ      1
     C                   ADD       SU1STB        X1AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X1AA2         SUB       X1AA1         X1ATOT
     C                   ADD       X1AA2         X1AA3
     C                   ADD       X1AA1         X1AS3
     C*
     C* 2ND ASSESSMENT
      *
     C                   Z-ADD(H)  X2AMT         X2AA2
     C     Z             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S2AMT         X2AA1
     C*
     C     Z             IFEQ      1
     C                   ADD       SU2NDB        X2AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X2AA2         SUB       X2AA1         X2ATOT
     C                   ADD       X2AA2         X2AA3
     C                   ADD       X2AA1         X2AS3
     C*
     C* 3RD ASSESSMENT
      *
     C                   Z-ADD(H)  X3AMT         X3AA2
     C     Z             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S3AMT         X3AA1
     C*
     C     Z             IFEQ      1
     C                   ADD       SU3RDB        X3AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X3AA2         SUB       X3AA1         X3ATOT
     C                   ADD       X3AA2         X3AA3
     C                   ADD       X3AA1         X3AS3
     C*
     C* 4TH ASSESSMENT
      *
     C                   Z-ADD(H)  X4AMT         X4AA2
     C     Z             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S4AMT         X4AA1
     C*
     C     Z             IFEQ      1
     C                   ADD       SU4THB        X4AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X4AA2         SUB       X4AA1         X4ATOT
     C                   ADD       X4AA2         X4AA3
     C                   ADD       X4AA1         X4AS3
     C*
     C* 5TH ASSESSMENT
      *
     C                   Z-ADD(H)  X5AMT         X5AA2
     C     Z             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S5AMT         X5AA1
     C*
     C     Z             IFEQ      1
     C                   ADD       SU5THB        X5AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X5AA2         SUB       X5AA1         X5ATOT
     C                   ADD       X5AA2         X5AA3
     C                   ADD       X5AA1         X5AS3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
      *
     C                   Z-ADD(H)  X6AMT         X6AA2
     C     Z             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S6AMT         X6AA1
     C*
     C     Z             IFEQ      1
     C                   ADD       SU6THB        X6AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X6AA2         SUB       X6AA1         X6ATOT
     C                   ADD       X6AA2         X6AA3
     C                   ADD       X6AA1         X6AS3
     C*
     C* 7TH ASSESSMENT
      *
     C                   Z-ADD(H)  X7AMT         X7AA2
     C     Z             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C                   Z-ADD(H)  S7AMT         X7AA1
     C*
     C     Z             IFEQ      1
     C                   ADD       SU7THB        X7AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C     X7AA2         SUB       X7AA1         X7ATOT
     C                   ADD       X7AA2         X7AA3
     C                   ADD       X7AA1         X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C*
     C* MOVE NEXT ADJUSTMENT INTO ARRAY.
     C*
     C     XXTOT         IFNE      *ZEROS
     C     X1TOT         ORNE      *ZEROS
     C     X1ATOT        ORNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C*** END ADD *****  12/11/08  ************************************
     C                   ADD       1             X
     C*
     C                   Z-ADD     WKD(Z)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C*
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
     C*** END ADD *****  12/11/08  ************************************
     C                   ENDIF                                                  XXTOT <> 0
     C*
     C                   ENDDO                                                  Y DO XXCNT Z
     C*
     C*  CHECK FOR ANY ROUNDING DISCREPANCIES BETWEEN CURRENT A/R AND
     C*  NEW ADJUSTMENTS THAT WILL BE POSTED TO A/R; BOTH SHOULD EQUAL
     C*  THE NEW PREMIUM AMOUNT TO THE PENNY
     C*
     C     XX1STB        SUB       X1AA3         X1AA3
     C     XX2NDB        SUB       X2AA3         X2AA3
     C     XX3RDB        SUB       X3AA3         X3AA3
     C     XX4THB        SUB       X4AA3         X4AA3
     C     XX5THB        SUB       X5AA3         X5AA3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XX6THB        SUB       X6AA3         X6AA3
     C     XX7THB        SUB       X7AA3         X7AA3
     C*** END ADD *****  12/11/08  ************************************
     C                   ADD       XU1STB        X1AA3
     C                   ADD       XU2NDB        X2AA3
     C                   ADD       XU3RDB        X3AA3
     C                   ADD       XU4THB        X4AA3
     C                   ADD       XU5THB        X5AA3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       XU6THB        X6AA3
     C                   ADD       XU7THB        X7AA3
     C*** END ADD *****  12/11/08  ************************************
     C*
     C* ROUNDING FOR PRIOR PREMIUM ON A/R SUMMARIZED SINGLE DIVISION.
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     SV1STB        SUB       X1AS3         X1AS3
     C     SV2NDB        SUB       X2AS3         X2AS3
     C     SV3RDB        SUB       X3AS3         X3AS3
     C     SV4THB        SUB       X4AS3         X4AS3
     C     SV5THB        SUB       X5AS3         X5AS3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     SV6THB        SUB       X6AS3         X6AS3
     C     SV7THB        SUB       X7AS3         X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C                   ADD       SU1STB        X1AS3
     C                   ADD       SU2NDB        X2AS3
     C                   ADD       SU3RDB        X3AS3
     C                   ADD       SU4THB        X4AS3
     C                   ADD       SU5THB        X5AS3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       SU6THB        X6AS3
     C                   ADD       SU7THB        X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C*
     C     X1AS3         IFLT      1.00
     C     X1AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A1(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A1(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X1AS3         A1(1)
     C                   ELSE
     C                   SUB       X1AS3         A1(2)
     C                   ENDIF
     C                   END
     C*
     C     X2AS3         IFLT      1.00
     C     X2AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A2(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A2(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X2AS3         A2(1)
     C                   ELSE
     C                   SUB       X2AS3         A2(2)
     C                   ENDIF
     C                   END
     C*
     C     X3AS3         IFLT      1.00
     C     X3AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A3(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A3(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X3AS3         A3(1)
     C                   ELSE
     C                   SUB       X3AS3         A3(2)
     C                   ENDIF
     C                   END
     C*
     C     X4AS3         IFLT      1.00
     C     X4AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A4(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A4(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X4AS3         A4(1)
     C                   ELSE
     C                   SUB       X4AS3         A4(2)
     C                   ENDIF
     C                   END
     C*
     C     X5AS3         IFLT      1.00
     C     X5AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A5(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A5(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X5AS3         A5(1)
     C                   ELSE
     C                   SUB       X5AS3         A5(2)
     C                   ENDIF
     C                   END
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     X6AS3         IFLT      1.00
     C     X6AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A6(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A6(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X6AS3         A6(1)
     C                   ELSE
     C                   SUB       X6AS3         A6(2)
     C                   ENDIF
     C                   END
     C*
     C     X7AS3         IFLT      1.00
     C     X7AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A7(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A7(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X7AS3         A7(1)
     C                   ELSE
     C                   SUB       X7AS3         A7(2)
     C                   ENDIF
     C                   END
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*
     C*  DIFFERENCE SHOULD NEVER BE GREATER THAN OR LESS THAN $1.00
     C*
     C     X1AA3         IFLT      1.00
     C     X1AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A1(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A1(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X1AA3         A1(1)
     C                   ELSE
     C                   ADD       X1AA3         A1(2)
     C                   ENDIF
     C                   END
     C*
     C     X2AA3         IFLT      1.00
     C     X2AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A2(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A2(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X2AA3         A2(1)
     C                   ELSE
     C                   ADD       X2AA3         A2(2)
     C                   ENDIF
     C                   END
     C*
     C     X3AA3         IFLT      1.00
     C     X3AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A3(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A3(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X3AA3         A3(1)
     C                   ELSE
     C                   ADD       X3AA3         A3(2)
     C                   ENDIF
     C                   END
     C*
     C     X4AA3         IFLT      1.00
     C     X4AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A4(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A4(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X4AA3         A4(1)
     C                   ELSE
     C                   ADD       X4AA3         A4(2)
     C                   ENDIF
     C                   END
     C*
     C     X5AA3         IFLT      1.00
     C     X5AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A5(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A5(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X5AA3         A5(1)
     C                   ELSE
     C                   ADD       X5AA3         A5(2)
     C                   ENDIF
     C                   END
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     X6AA3         IFLT      1.00
     C     X6AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A6(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A6(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X6AA3         A6(1)
     C                   ELSE
     C                   ADD       X6AA3         A6(2)
     C                   ENDIF
     C                   END
     C*
     C     X7AA3         IFLT      1.00
     C     X7AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A7(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A7(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X7AA3         A7(1)
     C                   ELSE
     C                   ADD       X7AA3         A7(2)
     C                   ENDIF
     C                   END
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  ELARSM IFEQ 'Y'
     C*
     C                   XFOOT     A1            SCTOT2
     C                   XFOOT     A2            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A3            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A4            SCTOT3
     C                   XFOOT     A5            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   XFOOT     A6            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A7            SCTOT4
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   Z-ADD     13            X
     C     X             DOWGT     0
     C     PA(X)         IFNE      *ZERO
     C     P1(X)         ORNE      *ZERO
     C     A1(X)         ORNE      *ZERO
     C     A2(X)         ORNE      *ZERO
     C     A3(X)         ORNE      *ZERO
     C     A4(X)         ORNE      *ZERO
     C     A5(X)         ORNE      *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(X)         ORNE      *ZERO
     C     A7(X)         ORNE      *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C                   LEAVE
     C                   ELSE
     C                   Z-ADD     0             DA(X)
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  CLTID IFEQ
     C*** END ADD *****  11/12/08  ************************************
     C*** BEGIN ADD ***  05/26/06  ************************************
     C     ELARSM        IFNE      'Y'
     C     XXWAR         OREQ      'Y'
     C*** END ADD *****  05/26/06  ************************************
     C*
     C*  PRINT BILL & RETURN TO SCREEN 1 IF THERE IS NO BILLING CHANGE
     C*
     C     SCTOT         IFEQ      *ZERO
     C*** BEGIN ADD ***  11/12/08  ************************************
     C     CLTID         ANDNE     '139'
     C     SCTOT         OREQ      *ZERO
     C     X             ANDEQ     0
     C     CLTID         ANDEQ     '139'
     C*** END ADD *****  11/12/08  ************************************
     C     F1UPAR        OREQ      'N'
     C*
     C*  DUE TO CHANGE MADE ON 01/19/92 (TO ALWAYS CREATE PLACE REVISED
     C*  RECORDS IN FILES WAELP AND WAEPP) WE MUST STILL GO TO
     C*  SCR04 TO PROCESS BILLING REVISION EVEN THOUGH NO ADJUSTMENTS
     C*  ARE TO BE POSTED.  THE BILLING REVISION FILES STILL NEED TO
     C*  BE MOVED INTO THE LIVE ENVIRONMENT. IN ADDITION, IF THERE WAS
     C*  A BILLING SCHEDULE CHANGE, FUTURE BILLINGS AND ADJUSTMENTS NEED
     C*  TO BE DELETED
     C*
     C                   MOVE      '1'           *IN80
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C*** BEGIN ADD ***  11/12/08  ************************************
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD   ***  11/12/08  ************************************
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANK        SCDEP
     C                   MOVE      *BLANKS       SCAR
     C*
     C*** BEGIN CHG ***  02/26/98  ************************************
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTMDY8
     C*****                MOVELSRDTO     SCEFDT
     C                   Z-ADD     0             SCEFDT
     C*** END CHG *****  02/26/98  ************************************
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     XXDIVC        IFEQ      1
     C*** END ADD *****  11/15/99  ************************************
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   ENDIF
     C*** END ADD *****  11/15/99  ************************************
     C*
     C                   ELSE
     C*
     C*** BEGIN CHG ***  02/26/98  ************************************
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTMDY8
     C*****                MOVELSRDTO     SCEFDT
     C                   Z-ADD     0             SCEFDT
     C*** END CHG *****  02/26/98  ************************************
     C*** BEGIN ADD ***  01/01/98  ************************************
     C*
     C*** BEGIN CHG ***  02/26/98  ************************************
     C*****                Z-ADD0         SCDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDT
     C*** END CHG *****  02/26/98  ************************************
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD ***  03/03/99  ************************************
     C     SCTOT         ANDGT     *ZERO
     C*** END ADD *****  03/03/99  ************************************
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
     C                   MOVE      QQDT          SCDT              8 0
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SCDT
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** BEGIN ADD ***  12/26/00  ************************************
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     XXDT          IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   ENDIF                                                  XXDT IFLT
     C*** END ADD *****  12/26/00  ************************************
     C*** BEGIN ADD ***  11/12/08  ************************************
     C     CLTID         IFNE      '139'
     C*** END ADD *****  11/12/08  ************************************
     C     SCDT          IFNE      *ZERO
     C                   Z-ADD     SCDT          DA(1)
     C                   ELSE
     C*** END ADD *****  01/01/98  ************************************
     C                   Z-ADD     SCEFDT        DA(1)
     C*** BEGIN ADD ***  01/01/98  ************************************
     C                   ENDIF                                                  SCDT IFNE
     C*** END ADD *****  01/01/98  ************************************
     C*** BEGIN CHG ***  04/21/03  ************************************
     C*********  XXADJ     MULT BSAB01    XXWK90  90H
     C*** BEGIN ADD ***  01/15/99  ************************************
     C*********  BSAB01    IFEQ *ZERO
     C     XXADJ         MULT(H)   XXAB01        XXWK90            9 0
     C     XXAB01        IFEQ      *ZERO
     C*** END CHG *****  04/21/03  ************************************
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  BSAB01 IFEQ
     C*** END ADD *****  01/15/99  ************************************
     C                   Z-ADD     XXWK90        XXADJ
     C                   Z-ADD     XXADJ         PA(1)
     C*** BEGIN ADD ***  11/11/03  ************************************
     C     XXWAR         IFEQ      'Y'
     C                   Z-ADD     XXDEPP        PA(1)
     C                   ENDIF
     C*** END ADD *****  11/11/03  ************************************
     C                   Z-ADD     XXTXDU        P1(1)
     C*** BEGIN ADD ***  11/12/08  ************************************
     C                   ENDIF                                                  CLTID IFNE
     C*** END ADD *****  11/12/08  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     XXDIVC        IFGT      1
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DA(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA(3)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA(4)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA(5)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA(7)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA(8)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA(9)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA(10)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA(11)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA(12)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
      *** BEG ADD ***** 01/31/11 **************************
     C                   MOVEA     J1D           J2D
      *** END ADD ***** 01/31/11 **************************
     C*
     C                   Z-ADD     PA(1)         EAAM01
     C                   Z-ADD     PA(2)         EAAM02
     C                   Z-ADD     PA(3)         EAAM03
     C                   Z-ADD     PA(4)         EAAM04
     C                   Z-ADD     PA(5)         EAAM05
     C                   Z-ADD     PA(6)         EAAM06
     C                   Z-ADD     PA(7)         EAAM07
     C                   Z-ADD     PA(8)         EAAM08
     C                   Z-ADD     PA(9)         EAAM09
     C                   Z-ADD     PA(10)        EAAM10
     C                   Z-ADD     PA(11)        EAAM11
     C                   Z-ADD     PA(12)        EAAM12
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C*
     C                   MOVE      ELCO#         EACO#
     C                   MOVE      ELFND         EAFND
     C                   Z-ADD     ELFYR         EAFYR
     C                   MOVE      ELEMP#        EAEMP#
     C                   MOVE      ELDIV         EADIV
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'C'           EASTAT
     C                   ELSE
     C                   MOVE      'B'           EASTAT
     C                   ENDIF
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'U'           EASTAT
     C                   ELSE
     C                   MOVE      'T'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     P1(1)         EAAM01
     C                   Z-ADD     P1(2)         EAAM02
     C                   Z-ADD     P1(3)         EAAM03
     C                   Z-ADD     P1(4)         EAAM04
     C                   Z-ADD     P1(5)         EAAM05
     C                   Z-ADD     P1(6)         EAAM06
     C                   Z-ADD     P1(7)         EAAM07
     C                   Z-ADD     P1(8)         EAAM08
     C                   Z-ADD     P1(9)         EAAM09
     C                   Z-ADD     P1(10)        EAAM10
     C                   Z-ADD     P1(11)        EAAM11
     C                   Z-ADD     P1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** BEGIN ADD ***  11/12/08  ************************************
     C                   Z-ADD     EAPO01        SVDT7             7 0
     C*
     C* WRITE 1ST ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '6'       EASTAT
     C                   MOVE      'D'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '1'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A1(1)         EAAM01
     C                   Z-ADD     A1(2)         EAAM02
     C                   Z-ADD     A1(3)         EAAM03
     C                   Z-ADD     A1(4)         EAAM04
     C                   Z-ADD     A1(5)         EAAM05
     C                   Z-ADD     A1(6)         EAAM06
     C                   Z-ADD     A1(7)         EAAM07
     C                   Z-ADD     A1(8)         EAAM08
     C                   Z-ADD     A1(9)         EAAM09
     C                   Z-ADD     A1(10)        EAAM10
     C                   Z-ADD     A1(11)        EAAM11
     C                   Z-ADD     A1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A1(13)        IFNE      *ZERO
     C                   Z-ADD     A1(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 2ND ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '7'       EASTAT
     C                   MOVE      'E'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '2'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A2(1)         EAAM01
     C                   Z-ADD     A2(2)         EAAM02
     C                   Z-ADD     A2(3)         EAAM03
     C                   Z-ADD     A2(4)         EAAM04
     C                   Z-ADD     A2(5)         EAAM05
     C                   Z-ADD     A2(6)         EAAM06
     C                   Z-ADD     A2(7)         EAAM07
     C                   Z-ADD     A2(8)         EAAM08
     C                   Z-ADD     A2(9)         EAAM09
     C                   Z-ADD     A2(10)        EAAM10
     C                   Z-ADD     A2(11)        EAAM11
     C                   Z-ADD     A2(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A2(13)        IFNE      *ZERO
     C                   Z-ADD     A2(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 3RD ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '8'       EASTAT
     C                   MOVE      'F'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '3'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A3(1)         EAAM01
     C                   Z-ADD     A3(2)         EAAM02
     C                   Z-ADD     A3(3)         EAAM03
     C                   Z-ADD     A3(4)         EAAM04
     C                   Z-ADD     A3(5)         EAAM05
     C                   Z-ADD     A3(6)         EAAM06
     C                   Z-ADD     A3(7)         EAAM07
     C                   Z-ADD     A3(8)         EAAM08
     C                   Z-ADD     A3(9)         EAAM09
     C                   Z-ADD     A3(10)        EAAM10
     C                   Z-ADD     A3(11)        EAAM11
     C                   Z-ADD     A3(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A3(13)        IFNE      *ZERO
     C                   Z-ADD     A3(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 4TH ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '9'       EASTAT
     C                   MOVE      'G'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '4'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A4(1)         EAAM01
     C                   Z-ADD     A4(2)         EAAM02
     C                   Z-ADD     A4(3)         EAAM03
     C                   Z-ADD     A4(4)         EAAM04
     C                   Z-ADD     A4(5)         EAAM05
     C                   Z-ADD     A4(6)         EAAM06
     C                   Z-ADD     A4(7)         EAAM07
     C                   Z-ADD     A4(8)         EAAM08
     C                   Z-ADD     A4(9)         EAAM09
     C                   Z-ADD     A4(10)        EAAM10
     C                   Z-ADD     A4(11)        EAAM11
     C                   Z-ADD     A4(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A4(13)        IFNE      *ZERO
     C                   Z-ADD     A4(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 5TH ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '0'       EASTAT
     C                   MOVE      'H'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '5'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A5(1)         EAAM01
     C                   Z-ADD     A5(2)         EAAM02
     C                   Z-ADD     A5(3)         EAAM03
     C                   Z-ADD     A5(4)         EAAM04
     C                   Z-ADD     A5(5)         EAAM05
     C                   Z-ADD     A5(6)         EAAM06
     C                   Z-ADD     A5(7)         EAAM07
     C                   Z-ADD     A5(8)         EAAM08
     C                   Z-ADD     A5(9)         EAAM09
     C                   Z-ADD     A5(10)        EAAM10
     C                   Z-ADD     A5(11)        EAAM11
     C                   Z-ADD     A5(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A5(13)        IFNE      *ZERO
     C                   Z-ADD     A5(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*** END ADD *****  11/12/08  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* WRITE 6TH ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'J'           EASTAT
     C                   ELSE
     C                   MOVE      '6'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A6(1)         EAAM01
     C                   Z-ADD     A6(2)         EAAM02
     C                   Z-ADD     A6(3)         EAAM03
     C                   Z-ADD     A6(4)         EAAM04
     C                   Z-ADD     A6(5)         EAAM05
     C                   Z-ADD     A6(6)         EAAM06
     C                   Z-ADD     A6(7)         EAAM07
     C                   Z-ADD     A6(8)         EAAM08
     C                   Z-ADD     A6(9)         EAAM09
     C                   Z-ADD     A6(10)        EAAM10
     C                   Z-ADD     A6(11)        EAAM11
     C                   Z-ADD     A6(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A6(13)        IFNE      *ZERO
     C                   Z-ADD     A6(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C* WRITE 7TH ASSESSMENT ADJUSTMENTS
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'K'           EASTAT
     C                   ELSE
     C                   MOVE      '7'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A7(1)         EAAM01
     C                   Z-ADD     A7(2)         EAAM02
     C                   Z-ADD     A7(3)         EAAM03
     C                   Z-ADD     A7(4)         EAAM04
     C                   Z-ADD     A7(5)         EAAM05
     C                   Z-ADD     A7(6)         EAAM06
     C                   Z-ADD     A7(7)         EAAM07
     C                   Z-ADD     A7(8)         EAAM08
     C                   Z-ADD     A7(9)         EAAM09
     C                   Z-ADD     A7(10)        EAAM10
     C                   Z-ADD     A7(11)        EAAM11
     C                   Z-ADD     A7(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A7(13)        IFNE      *ZERO
     C                   Z-ADD     A7(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*** END ADD *****  11/15/99  ************************************
     C                   END
     C*** BEGIN ADD ***  05/26/06  ************************************
     C                   ENDIF                                                  ELARSM IFNE
     C*** END ADD *****  05/26/06  ************************************
     C*
     C                   END                                                    BSSEFL = 'S'
     C*
     C*  COMPUTE ADJUSTMENT IF NEW BILLING SCHEDULE DESIGNATES AN
     C*  ESTIMATING ACCOUNT
     C*
     C     BSSEFL        IFEQ      'E'
     C*
     C*** BEGIN ADD ***  11/11/03  ************************************
     C     ELARSM        IFEQ      'Y'
     C     XXWAR         ANDNE     'Y'
     C                   MOVE      'Y'           XXSUM
     C     ELANPR        SUB       ELPRTD        X1ANPR           11 2
     C*** BEGIN ADD ***  04/21/06  ************************************
     C*** BEGIN DEL ****** 08/09/10 ************************************
     C**********           SUB  XXRTCR    X1ANPR
     C*** END   DEL ****** 08/09/10 ************************************
      *** beg add ***** 07/02/13 **************************************
      /free
       // Reduce annual premium (X1ANPR) by the amount of dividend premium
       //  returned from the CalcDividend procedure
       x1anpr = x1anpr - CalcDividend(ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr);
      /end-free
      *** end add ***** 07/02/13 **************************************
     C*** END ADD *****  04/21/06  ************************************
     C                   ADD       X1ANPR        XTANPR
     C                   ADD       ELTXDU        XTTXDU
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEGIN CHG ***  01/11/08  ************************************
     C*******    RR1STB    IFEQ 'Y'
     C*******              ADD  EL1AAM    XT1STB
     C*******              ENDIF
     C*******    RR2NDB    IFEQ 'Y'
     C*******              ADD  EL2AAM    XT2NDB
     C*******              ENDIF
     C*******    RR3RDB    IFEQ 'Y'
     C*******              ADD  EL3AAM    XT3RDB
     C*******              ENDIF
     C*******    RR4THB    IFEQ 'Y'
     C*******              ADD  EL4AAM    XT4THB
     C*******              ENDIF
     C*******    RR5THB    IFEQ 'Y'
     C*******              ADD  EL5AAM    XT5THB
     C*******              ENDIF
     C     RR1STB        IFEQ      'Y'
     C                   ADD       EL1AAM        XT1STB
     C                   ELSE
     C                   ADD       EL1AAM        XL1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   ADD       EL2AAM        XT2NDB
     C                   ELSE
     C                   ADD       EL2AAM        XL2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   ADD       EL3AAM        XT3RDB
     C                   ELSE
     C                   ADD       EL3AAM        XL3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   ADD       EL4AAM        XT4THB
     C                   ELSE
     C                   ADD       EL4AAM        XL4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   ADD       EL5AAM        XT5THB
     C                   ELSE
     C                   ADD       EL5AAM        XL5THB
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     RR6THB        IFEQ      'Y'
     C                   ADD       EL6AAM        XT6THB
     C                   ELSE
     C                   ADD       EL6AAM        XL6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   ADD       EL7AAM        XT7THB
     C                   ELSE
     C                   ADD       EL7AAM        XL7THB
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  01/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       SVANPR        STANPR
     C                   ADD       SVTXDU        STTXDU
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEGIN CHG ***  01/11/08  ************************************
     C*******    RR1STB    IFEQ 'Y'
     C*******              ADD  SV1STB    ST1STB
     C*******              ENDIF
     C*******    RR2NDB    IFEQ 'Y'
     C*******              ADD  SV2NDB    ST2NDB
     C*******              ENDIF
     C*******    RR3RDB    IFEQ 'Y'
     C*******              ADD  SV3RDB    ST3RDB
     C*******              ENDIF
     C*******    RR4THB    IFEQ 'Y'
     C*******              ADD  SV4THB    ST4THB
     C*******              ENDIF
     C*******    RR5THB    IFEQ 'Y'
     C*******              ADD  SV5THB    ST5THB
     C*******              ENDIF
     C     RR1STB        IFEQ      'Y'
     C                   ADD       SV1STB        ST1STB
     C                   ELSE
     C                   ADD       SU1STB        SL1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   ADD       SV2NDB        ST2NDB
     C                   ELSE
     C                   ADD       SU2NDB        SL2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   ADD       SV3RDB        ST3RDB
     C                   ELSE
     C                   ADD       SU3RDB        SL3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   ADD       SV4THB        ST4THB
     C                   ELSE
     C                   ADD       SU4THB        SL4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   ADD       SV5THB        ST5THB
     C                   ELSE
     C                   ADD       SU5THB        SL5THB
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     RR6THB        IFEQ      'Y'
     C                   ADD       SV6THB        ST6THB
     C                   ELSE
     C                   ADD       SU6THB        SL6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   ADD       SV7THB        ST7THB
     C                   ELSE
     C                   ADD       SU7THB        SL7THB
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  01/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C*
     C                   ELSE
     C*** END ADD *****  11/11/03  ************************************
     C*
     C*** BEGIN CHG ***  02/20/01  ************************************
     C*********            MOVE *OFF      *IN81
     C     XXDIV         IFEQ      SCDIV
     C                   MOVE      *OFF          *IN81
     C                   ENDIF
     C*** END CHG *****  02/20/01  ************************************
     C*
     C                   Z-ADD     *ZEROS        W                 3 0
     C                   Z-ADD     *ZEROS        X                 3 0
     C                   Z-ADD     *ZEROS        Y                 3 0
     C                   Z-ADD     *ZEROS        Z                 3 0
     C                   Z-ADD     *ZEROS        XXDTE
     C                   Z-ADD     *ZEROS        XXDTE1
     C                   Z-ADD     *ZEROS        XXDTE2
     C                   Z-ADD     *ZEROS        XXDIFF           11 2
     C                   Z-ADD     *ZEROS        XXTOT            11 2
     C                   Z-ADD     *ZEROS        XXAMT1           11 2
     C                   Z-ADD     *ZEROS        XXAMT2           11 2
     C*** BEGIN CHG ***  12/17/99  ************************************
     C*********************Z-ADD*ZEROS    YRMO    40
     C                   Z-ADD     *ZEROS        YRMO              5 0
     C*** END CHG *****  12/17/99  ************************************
     C*** BEG ADD *****  10/27/03  ************************************
     C                   Z-ADD     *ZEROS        YYMM
     C*** END ADD *****  10/27/03  ************************************
     C                   Z-ADD     *ZEROS        XXAMT3           11 2
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     *ZEROS        XSAMT3           11 2
     C                   Z-ADD     *ZEROS        X2AMT3           11 2
     C*** END ADD *****  02/28/06  ************************************
     C                   Z-ADD     *ZEROS        X1TOT            11 2
     C                   Z-ADD     *ZEROS        X1AMT1           11 2
     C                   Z-ADD     *ZEROS        X1AMT2           11 2
     C                   Z-ADD     *ZEROS        X1AMT3           11 2
     C                   MOVE      '0'           XXFUT             1
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     *ZEROS        X1ATOT           11 2
     C                   Z-ADD     *ZEROS        X1AA1            11 2
     C                   Z-ADD     *ZEROS        X1AA2            11 2
     C                   Z-ADD     *ZEROS        X1AA3            11 2
     C                   Z-ADD     *ZEROS        X1AS3            11 2
     C                   Z-ADD     *ZEROS        X2ATOT           11 2
     C                   Z-ADD     *ZEROS        X2AA1            11 2
     C                   Z-ADD     *ZEROS        X2AA2            11 2
     C                   Z-ADD     *ZEROS        X2AA3            11 2
     C                   Z-ADD     *ZEROS        X2AS3            11 2
     C                   Z-ADD     *ZEROS        X3ATOT           11 2
     C                   Z-ADD     *ZEROS        X3AA1            11 2
     C                   Z-ADD     *ZEROS        X3AA2            11 2
     C                   Z-ADD     *ZEROS        X3AA3            11 2
     C                   Z-ADD     *ZEROS        X3AS3            11 2
     C                   Z-ADD     *ZEROS        X4ATOT           11 2
     C                   Z-ADD     *ZEROS        X4AA1            11 2
     C                   Z-ADD     *ZEROS        X4AA2            11 2
     C                   Z-ADD     *ZEROS        X4AA3            11 2
     C                   Z-ADD     *ZEROS        X4AS3            11 2
     C                   Z-ADD     *ZEROS        X5ATOT           11 2
     C                   Z-ADD     *ZEROS        X5AA1            11 2
     C                   Z-ADD     *ZEROS        X5AA2            11 2
     C                   Z-ADD     *ZEROS        X5AA3            11 2
     C                   Z-ADD     *ZEROS        X5AS3            11 2
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     *ZEROS        X6ATOT           11 2
     C                   Z-ADD     *ZEROS        X6AA1            11 2
     C                   Z-ADD     *ZEROS        X6AA2            11 2
     C                   Z-ADD     *ZEROS        X6AA3            11 2
     C                   Z-ADD     *ZEROS        X6AS3            11 2
     C                   Z-ADD     *ZEROS        X7ATOT           11 2
     C                   Z-ADD     *ZEROS        X7AA1            11 2
     C                   Z-ADD     *ZEROS        X7AA2            11 2
     C                   Z-ADD     *ZEROS        X7AA3            11 2
     C                   Z-ADD     *ZEROS        X7AS3            11 2
     C*** END ADD *****  12/11/08  ************************************
     C*** BEG ADD ***** 06/28/11 **************************************
     C                   Z-ADD     *ZEROS        XX1STB
     C                   Z-ADD     *ZEROS        XU1STB
     C                   Z-ADD     *ZEROS        XX2NDB
     C                   Z-ADD     *ZEROS        XU2NDB
     C                   Z-ADD     *ZEROS        XX3RDB
     C                   Z-ADD     *ZEROS        XU3RDB
     C                   Z-ADD     *ZEROS        XX4THB
     C                   Z-ADD     *ZEROS        XU4THB
     C                   Z-ADD     *ZEROS        XX5THB
     C                   Z-ADD     *ZEROS        XU5THB
     C                   Z-ADD     *ZEROS        XX6THB
     C                   Z-ADD     *ZEROS        XU6THB
     C                   Z-ADD     *ZEROS        XX7THB
     C                   Z-ADD     *ZEROS        XU7THB
     C*** END ADD ***** 06/28/11 **************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     XXWAR         IFEQ      'Y'
     C                   Z-ADD     XTANPR        X1ANPR
     C                   Z-ADD     XTTXDU        ELTXDU
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     STANPR        SVANPR
     C                   Z-ADD     STTXDU        SVTXDU
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     XT1STB        XX1STB           11 2
     C                   Z-ADD     ST1STB        SV1STB
     C                   Z-ADD     XT2NDB        XX2NDB           11 2
     C                   Z-ADD     ST2NDB        SV2NDB
     C                   Z-ADD     XT3RDB        XX3RDB           11 2
     C                   Z-ADD     ST3RDB        SV3RDB
     C                   Z-ADD     XT4THB        XX4THB           11 2
     C                   Z-ADD     ST4THB        SV4THB
     C                   Z-ADD     XT5THB        XX5THB           11 2
     C                   Z-ADD     ST5THB        SV5THB
     C*** BEGIN ADD ***  01/11/08  ************************************
     C                   Z-ADD     XL1STB        XU1STB           11 2
     C                   Z-ADD     SL1STB        SU1STB
     C                   Z-ADD     XL2NDB        XU2NDB           11 2
     C                   Z-ADD     SL2NDB        SU2NDB
     C                   Z-ADD     XL3RDB        XU3RDB           11 2
     C                   Z-ADD     SL3RDB        SU3RDB
     C                   Z-ADD     XL4THB        XU4THB           11 2
     C                   Z-ADD     SL4THB        SU4THB
     C                   Z-ADD     XL5THB        XU5THB           11 2
     C                   Z-ADD     SL5THB        SU5THB
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     XT6THB        XX6THB           11 2
     C                   Z-ADD     ST6THB        SV6THB
     C                   Z-ADD     XT7THB        XX7THB           11 2
     C                   Z-ADD     ST7THB        SV7THB
     C                   Z-ADD     XL6THB        XU6THB           11 2
     C                   Z-ADD     SL6THB        SU6THB
     C                   Z-ADD     XL7THB        XU7THB           11 2
     C                   Z-ADD     SL7THB        SU7THB
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  01/11/08  ************************************
     C*** END ADD  ****  05/15/07  ************************************
     C*** END ADD *****  02/28/06  ************************************
     C                   ELSE
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEGIN CHG ***  01/11/08  ************************************
     C*********            Z-ADDEL1AAM    XX1STB
     C*********            Z-ADDEL2AAM    XX2NDB
     C*********            Z-ADDEL3AAM    XX3RDB
     C*********            Z-ADDEL4AAM    XX4THB
     C*********            Z-ADDEL5AAM    XX5THB
     C     RR1STB        IFEQ      'Y'
     C                   Z-ADD     EL1AAM        XX1STB
     C                   ELSE
     C                   Z-ADD     EL1AAM        XU1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   Z-ADD     EL2AAM        XX2NDB
     C                   ELSE
     C                   Z-ADD     EL2AAM        XU2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   Z-ADD     EL3AAM        XX3RDB
     C                   ELSE
     C                   Z-ADD     EL3AAM        XU3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   Z-ADD     EL4AAM        XX4THB
     C                   ELSE
     C                   Z-ADD     EL4AAM        XU4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   Z-ADD     EL5AAM        XX5THB
     C                   ELSE
     C                   Z-ADD     EL5AAM        XU5THB
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     RR6THB        IFEQ      'Y'
     C                   Z-ADD     EL6AAM        XX6THB
     C                   ELSE
     C                   Z-ADD     EL6AAM        XU6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   Z-ADD     EL7AAM        XX7THB
     C                   ELSE
     C                   Z-ADD     EL7AAM        XU7THB
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  01/11/08  ************************************
     C*** END ADD  ****  05/15/07  ************************************
     C     ELANPR        SUB       ELPRTD        X1ANPR           11 2
      *** beg add ***** 07/02/13 **************************************
      /free
       // Reduce annual premium (X1ANPR) by the amount of dividend premium
       //  returned from the CalcDividend procedure
       x1anpr = x1anpr - CalcDividend(ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr);
      /end-free
      *** end add ***** 07/02/13 **************************************
     C*** BEGIN ADD ***  04/21/06  ************************************
     C*** BEGIN DEL ****** 08/09/10 ************************************
     C**********           SUB  XXRTCR    X1ANPR
     C*** END   DEL ****** 08/09/10 ************************************
     C*** END ADD *****  04/21/06  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   ENDIF
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  04/21/03  ************************************
     C*
     C*  PROCESS NON-WEEKLY BILLING SCHEDULE CODES.
     C*
     C     BSDOWK        IFEQ      *BLANKS
     C*** BEGIN ADD ***  04/25/03  ************************************
     C     BSFREQ        ANDNE     'T'
     C*** END ADD *****  04/25/03  ************************************
     C*** END ADD *****  04/21/03  ************************************
     C*
     C*  FIRST LOAD BILLING SCHEDULE DATES INTO AN ARRAY FOR LATER
     C*  USE
     C*
     C                   Z-ADD     ELBSDT        XXDTE
     C                   MOVEL     XXDTE         YYMM
     C**** BEG ADD ****** 10/11/07 *********************************************
     C                   MOVE      XXDTE         DD                2 0
     C**** END ADD ****** 10/11/07 *********************************************
     C                   DO        12            X
     C**** BEG ADD ****** 10/11/07 *********************************************
     C     ELBLFL        IFEQ      'Y'
      *
     C                   MOVE      DD            XXDTE
      *
     C     MM            IFEQ      2
     C     DD            ANDGE     29
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      28            XXDTE
     C                   ENDIF                                                  SRERR IFEQ *ON
      *
     C                   ELSE
      *
     C     DD            IFGT      30
      *
     C     MM            IFEQ      4
     C     MM            OREQ      6
     C     MM            OREQ      9
     C     MM            OREQ      11
     C                   MOVE      30            XXDTE
     C                   ENDIF                                                  MM IFEQ 4
      *
     C                   ENDIF                                                  DD IFGT 30
      *
     C                   ENDIF                                                  MM IFEQ 2
      *
     C                   ENDIF                                                  ELBLFL IFEQ 'Y'
     C**** END ADD ****** 10/11/07 *********************************************
     C                   Z-ADD     XXDTE         BD(X)
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   END
     C                   MOVEL     YYMM          XXDTE
     C*** BEG ADD *****  10/11/07  ************************************
     C     ELBLFL        IFNE      'Y'
     C*** END ADD *****  10/11/07  ************************************
     C                   MOVE      '01'          XXDTE
     C*** BEG ADD *****  10/11/07  ************************************
     C                   ENDIF                                                  ELBLFL IFNE
     C*** END ADD *****  10/11/07  ************************************
     C                   END
     C*** BEGIN CHG ***  03/16/99  ************************************
     C*******              Z-ADDELPTDT    BD,13
     C                   Z-ADD     *HIVAL        BD(13)
     C*** END CHG *****  03/16/99  ************************************
     C*** BEGIN ADD ***  10/03/03  ************************************
     C                   Z-ADD     12            X
     C     X             DOWGT     0
     C     AB(X)         IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        BD(X)
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
     C*** END ADD *****  10/03/03  ************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*** BEGIN ADD ***  02/19/98  ************************************
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
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** END ADD *****  02/19/98  ************************************
     C*
     C                   Z-ADD     XXCURD        XXCUR2
     C*
     C                   MOVEL     XXCURD        YRMO1             5 0
     C                   MOVE      YRMO1         YRMO
     C                   MOVEL     ELBSDT        YYMM
     C                   Z-ADD     1             Y
     C                   Z-ADD     0             X
     C                   Z-ADD     *ZEROS        XXTOT
     C     Y             DOWLE     12
     C     YYMM          ANDLE     YRMO
      *** BEG ADD ***** 04/17/12 *********************************
     C     ELBLFL        ANDEQ     *BLANK
     C     Y             ORLE      12
     C     BD(Y)         ANDLE     XXCURD
     C     ELBLFL        ANDEQ     'Y'
      *** END ADD ***** 04/17/12 *********************************
     C     Y             ADD       1             W
     C                   Z-ADD     BD(Y)         XXDTE1
     C     BD(W)         IFGT      XXCURD
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C*** BEG ADD *****  10/11/07  ************************************
     C     ELBLFL        ANDEQ     XXBLFL
     C*** END ADD *****  10/11/07  ************************************
     C*
     C*  IF THE BILLING SCHEDULE CODE CHANGED WE WILL BE DELETING
     C*  ALL BILLINGS AND ADJUSTMENTS WITH TODAY'S DATE OR GREATER SO
     C*  WHEN CALCULATING WHAT WAS BILLED DO NOT INCLUDE TODAYS DATE.
     C*  IF THE BILLING SCHEDULE DOES NOT CHANGE, HOWEVER, WE NEED TO
     C*  INCLUDE ANY BILLINGS OR ADJUSTMENTS WITH TODAYS DATE
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     1             SRFUTR            5 0
     C*** BEGIN ADD ***  02/19/98  ************************************
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   ADD       EXRVID        SRFUTR
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   Z-ADD     1             SRFUTR
     C                   ADD       EXAOID        SRFUTR
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C                   ENDIF
     C*** END ADD *****  02/19/98  ************************************
     C                   MOVE      ' '           SRRTCD            1
     C*
     C*  COMPUTE TODAYS DATE + 1 SO THAT SRWASB WHICH LOOKS FOR 'A'
     C*  OR 'B' A/R RECORDS WITH EAPODT >= XXDTE1 AND EAPODT < XXDTE2
     C*  WILL INCLUDE TODAYS DATE IF THE BILLING SCHEDULE CODE HAS
     C*  NOT CHANGED
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     XXCURD        XXDTE2
     C     SRRTCD        IFEQ      '0'
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C*
     C                   END
     C                   Z-ADD     XXDTE2        XXCUR2
     C                   ELSE                                                   ELBSCD <> XXBSCD
     C                   Z-ADD     XXCURD        XXDTE2
     C                   END                                                    ELBSCD = XXBSCD
     C*
     C                   ELSE
     C                   Z-ADD     BD(W)         XXDTE2
     C                   END                                                    BD,W >= XXCURD
     C*
     C* IF THE END DATE IS BEFORE THE POLICY STARTS, MOVE IT UP TO THE
     C* EFFECTIVE DATE.
     C*
     C*** BEGIN CHG ***  07/15/98  ************************************
     C*****      XXDTE2    IFLT ELPFDT
     C*****                Z-ADDELPFDT    XXDTE2
     C*****                ENDIF                           XXDTE2 IFLT
     C     XXDTE2        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        XXDTE2
     C*** BEGIN ADD ***  03/23/99  ************************************
     C*
     C* USE THE BILLING START DATE + 1 DAY
     C*
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C     SRRTCD        IFEQ      '0'
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C*
     C                   ENDIF                                                  SRRTCD IFEQ
     C*** END ADD *****  03/23/99  ************************************
     C                   ENDIF                                                  XXDTE2 IFLT
     C*** END CHG *****  07/15/98  ************************************
     C*
     C                   EXSR      SRWASB
     C     AB2(Y)        MULT(H)   X1ANPR        XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SVANPR        XXAMT1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT1        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C     XXAMT2        SUB       XXAMT1        XXDIFF
     C                   ADD       XXDIFF        XXTOT
     C*
     C*  CALCULATE THE STATE TAX DUE FOR THIS PERIOD
     C*
     C*** BEGIN CHG ***  04/05/00  ************************************
     C*********  AB2,Y     MULT ELTXDU    X1AMT2
     C     AB2(Y)        MULT(H)   ELTXDU        X1AMT2
     C*** END CHG *****  04/05/00  ************************************
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SVTXDU        X1AMT1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT1        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C     X1AMT2        SUB       X1AMT1        XXDIFF
     C                   ADD       XXDIFF        X1TOT
     C                   ADD       X1AMT2        X1AMT3
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       X1AMT1        X2AMT3
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********  RR1STB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C     AB2(Y)        MULT(H)   XX1STB        X1AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV1STB        X1AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       SU1STB        X1AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X1AA2         SUB       X1AA1         XXDIFF
     C                   ADD       XXDIFF        X1ATOT
     C                   ADD       X1AA2         X1AA3
     C                   ADD       X1AA1         X1AS3
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR2NDB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C     AB2(Y)        MULT(H)   XX2NDB        X2AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV2NDB        X2AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       SU2NDB        X2AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X2AA2         SUB       X2AA1         XXDIFF
     C                   ADD       XXDIFF        X2ATOT
     C                   ADD       X2AA2         X2AA3
     C                   ADD       X2AA1         X2AS3
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR3RDB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C     AB2(Y)        MULT(H)   XX3RDB        X3AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV3RDB        X3AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       SU3RDB        X3AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X3AA2         SUB       X3AA1         XXDIFF
     C                   ADD       XXDIFF        X3ATOT
     C                   ADD       X3AA2         X3AA3
     C                   ADD       X3AA1         X3AS3
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR4THB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C     AB2(Y)        MULT(H)   XX4THB        X4AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV4THB        X4AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       SU4THB        X4AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X4AA2         SUB       X4AA1         XXDIFF
     C                   ADD       XXDIFF        X4ATOT
     C                   ADD       X4AA2         X4AA3
     C                   ADD       X4AA1         X4AS3
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR5THB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C     AB2(Y)        MULT(H)   XX5THB        X5AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV5THB        X5AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Y             IFEQ      1
     C                   ADD       SU5THB        X5AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X5AA2         SUB       X5AA1         XXDIFF
     C                   ADD       XXDIFF        X5ATOT
     C                   ADD       X5AA2         X5AA3
     C                   ADD       X5AA1         X5AS3
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*** END DEL *****  01/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
     C*
     C     AB2(Y)        MULT(H)   XX6THB        X6AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X6AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X6AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Y             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV6THB        X6AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X6AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X6AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Y             IFEQ      1
     C                   ADD       SU6THB        X6AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X6AA2         SUB       X6AA1         XXDIFF
     C                   ADD       XXDIFF        X6ATOT
     C                   ADD       X6AA2         X6AA3
     C                   ADD       X6AA1         X6AS3
     C*
     C* 7TH ASSESSMENT
     C*
     C     AB2(Y)        MULT(H)   XX7THB        X7AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X7AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X7AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Y             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Y)        MULT(H)   SV7THB        X7AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X7AA1         XXWK10           11 0
     C                   Z-ADD     XXWK10        X7AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Y             IFEQ      1
     C                   ADD       SU7THB        X7AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X7AA2         SUB       X7AA1         XXDIFF
     C                   ADD       XXDIFF        X7ATOT
     C                   ADD       X7AA2         X7AA3
     C                   ADD       X7AA1         X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       XXAMT2        XXAMT3
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       XXAMT1        XSAMT3
     C*** END ADD *****  02/28/06  ************************************
     C                   ADD       1             Y
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   END
     C                   END                                                    DOW Y <= 12
     C*                                                     &  YYMM <= YRMO
     C*** BEGIN ADD ***  10/03/03  ************************************
     C                   MOVE      *BLANK        XXFLAG            1
     C     Y             DO        12            Z
     C     AB2(Z)        IFNE      *ZEROS
     C                   MOVE      'Y'           XXFLAG
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     XXFLAG        IFEQ      *BLANK
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         ANDEQ     *OFF
     C     XXFLAG        OREQ      *BLANK
     C     ELARSM        ANDNE     'Y'
     C*** END ADD *****  02/28/06  ************************************
     C*
     C                   Z-ADD     XXDTE2        XXDTE1
     C                   Z-ADD     *HIVAL        XXDTE2
     C                   EXSR      SRWASB
     C                   SUB       XXAMT1        XXTOT
     C                   SUB       X1AMT1        X1TOT
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEGIN CHG ***  01/11/08  ************************************
     C*******    RR1STB    IFEQ 'Y'
     C*******              SUB  X1AA1     X1ATOT
     C*******              ENDIF
     C*******    RR2NDB    IFEQ 'Y'
     C*******              SUB  X2AA1     X2ATOT
     C*******              ENDIF
     C*******    RR3RDB    IFEQ 'Y'
     C*******              SUB  X3AA1     X3ATOT
     C*******              ENDIF
     C*******    RR4THB    IFEQ 'Y'
     C*******              SUB  X4AA1     X4ATOT
     C*******              ENDIF
     C*******    RR5THB    IFEQ 'Y'
     C*******              SUB  X5AA1     X5ATOT
     C*******              ENDIF
     C                   SUB       X1AA1         X1ATOT
     C                   SUB       X2AA1         X2ATOT
     C                   SUB       X3AA1         X3ATOT
     C                   SUB       X4AA1         X4ATOT
     C                   SUB       X5AA1         X5ATOT
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   SUB       X6AA1         X6ATOT
     C                   SUB       X7AA1         X7ATOT
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  01/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*
     C                   ENDIF                                                  XXFLAG IFEQ
     C*** END ADD *****  10/03/03  ************************************
     C*
     C     XXTOT         IFNE      *ZEROS
     C*** BEGIN ADD ***  10/07/03  ************************************
     C     X1TOT         ORNE      *ZEROS
     C*** END ADD *****  10/07/03  ************************************
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     X1ATOT        ORNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C*** END ADD *****  01/11/08  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C*** END ADD *****  12/11/08  ************************************
     C                   ADD       1             X
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         DA(X)
     C*** BEGIN ADD ***  01/01/98  ************************************
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*** BEGIN ADD ***  03/03/99  ************************************
     C     XXTOT         ANDGT     *ZERO
     C*** BEGIN ADD ***  10/07/03  ************************************
     C     XXTOT         OREQ      *ZERO
     C     X1TOT         ANDGT     *ZEROS
     C*** END ADD *****  10/07/03  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     XXTOT         OREQ      *ZERO
     C     X1ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X2ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X3ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X4ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X5ATOT        ANDGT     *ZEROS
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XXTOT         OREQ      *ZERO
     C     X6ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X7ATOT        ANDGT     *ZEROS
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C*** END ADD *****  03/03/99  ************************************
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
     C                   MOVE      QQDT          DA(X)
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        DA(X)
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  01/01/98  ************************************
     C*
     C* IF THE POSTING DATE IS PRIOR TO THE POLICY EFFECTIVE DATE,
     C* CHANGE IT.
     C*
     C                   Z-ADD     DA(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C*** BEGIN CHG ***  07/15/98  ************************************
     C*****      XXTDT     IFLT ELPFDT
     C*****                Z-ADDELPFDT    SRDTI
     C     XXTDT         IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C*** END CHG *****  07/15/98  ************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C                   ENDIF                                                  XXTDT IFLT
     C*
     C                   Z-ADD     XXTOT         PA(X)
     C                   Z-ADD     X1TOT         P1(X)
     C*** BEGIN ADD*****  05/15/07  ***********************************
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD*****  05/15/07  ***********************************
     C                   END                                                    XXTOT <> 0
     C*
     C*** BEG ADD *****  10/11/07  ************************************
     C     ELBLFL        IFNE      'Y'
     C*** END ADD *****  10/11/07  ************************************
     C                   MOVE      '01'          XXDTE
     C*** BEG ADD *****  10/11/07  ************************************
     C                   ENDIF                                                  ELBLFL IFNE
     C*** END ADD *****  10/11/07  ************************************
     C     Y             DO        12            Z
     C     AB2(Z)        IFNE      *ZEROS
     C*
     C*  IF THE BILLING SCHEDLUE CODE DID NOT CHANGE WE MUST COMPARE
     C*  WHAT WAS BILLED FOR A GIVEN MONTH TO WHAT SHOULD BE BILLED
     C*  THAT MONTH UNDER THE NEW BILLING SCHEDULE CODE
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C*
     C     Z             ADD       1             W
     C*
     C*  THE FOLLOWING MUST ONLY BE DONE FOR THE FIRST BILLING PERIOD
     C*  (I.E. FROM BD,Y TO BD,Y+1; ONLY THE FIRST TIME THIS CODE IS EXECUTED)
     C*  NEED TO BE SURE TO INCLUDE ANY 'B' OR 'A' A/R RECORDS THAT MAY
     C*  HAVE BEEN MANUALLY POSTED BETWEEN THE CURRENT DATE (OR CURRENT
     C*  DATE + 1) AND THE NEXT BILLING PERIOD (BD,W)
     C*
     C     XXCUR2        IFNE      *ZEROS
     C                   Z-ADD     XXCUR2        XXDTE1
     C                   Z-ADD     *ZEROS        XXCUR2
     C                   ELSE
     C                   Z-ADD     BD(Z)         XXDTE1
     C                   END                                                    XXCUR2 <> 0
     C                   Z-ADD     BD(W)         XXDTE2
     C*
     C* IF THE START DATE IS PRIOR TO THE POLICY EFFECTIVE DATE,
     C* CHANGE IT.
     C*
     C*** BEGIN CHG ***  07/15/98  ************************************
     C*****      XXDTE1    IFLT ELPFDT
     C     XXDTE1        IFLT      ELBSDT
     C*** END CHG *****  07/15/98  ************************************
     C                   Z-ADD     BD(Z)         XXDTE1
     C                   ENDIF                                                  XXDTE1 IFLT
     C*
     C                   EXSR      SRWASB
     C     AB2(Z)        MULT(H)   X1ANPR        XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SVANPR        XXAMT1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT1        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C     XXAMT2        SUB       XXAMT1        XXTOT
     C     AB2(Z)        MULT(H)   ELTXDU        X1AMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SVTXDU        X1AMT1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT1        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C     X1AMT2        SUB       X1AMT1        X1TOT
     C                   ADD       X1AMT2        X1AMT3
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       X1AMT1        X2AMT3
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD *****  05/15/07  **********************************
      *
     C* 1ST ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX1STB        X1AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV1STB        X1AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       SU1STB        X1AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X1AA2         SUB       X1AA1         X1ATOT
     C                   ADD       X1AA2         X1AA3
     C                   ADD       X1AA1         X1AS3
     C*
     C* 2ND ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX2NDB        X2AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV2NDB        X2AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       SU2NDB        X2AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X2AA2         SUB       X2AA1         X2ATOT
     C                   ADD       X2AA2         X2AA3
     C                   ADD       X2AA1         X2AS3
     C*
     C* 3RD ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX3RDB        X3AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV3RDB        X3AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       SU3RDB        X3AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X3AA2         SUB       X3AA1         X3ATOT
     C                   ADD       X3AA2         X3AA3
     C                   ADD       X3AA1         X3AS3
     C*
     C* 4TH ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX4THB        X4AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV4THB        X4AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       SU4THB        X4AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X4AA2         SUB       X4AA1         X4ATOT
     C                   ADD       X4AA2         X4AA3
     C                   ADD       X4AA1         X4AS3
     C*
     C* 5TH ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX5THB        X5AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV5THB        X5AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** BEGIN ADD ***  01/11/08  ************************************
     C     Z             IFEQ      1
     C                   ADD       SU5THB        X5AA1
     C                   ENDIF
     C*** END ADD *****  01/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X5AA2         SUB       X5AA1         X5ATOT
     C                   ADD       X5AA2         X5AA3
     C                   ADD       X5AA1         X5AS3
     C*
     C*** END ADD *****  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX6THB        X6AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X6AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X6AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Z             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV6THB        X6AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X6AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X6AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Z             IFEQ      1
     C                   ADD       SU6THB        X6AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X6AA2         SUB       X6AA1         X6ATOT
     C                   ADD       X6AA2         X6AA3
     C                   ADD       X6AA1         X6AS3
     C*
     C* 7TH ASSESSMENT
      *
     C     AB2(Z)        MULT(H)   XX7THB        X7AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X7AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X7AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Z             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     AB2(Z)        MULT(H)   SV7THB        X7AA1
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X7AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X7AA1
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     Z             IFEQ      1
     C                   ADD       SU7THB        X7AA1
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C     X7AA2         SUB       X7AA1         X7ATOT
     C                   ADD       X7AA2         X7AA3
     C                   ADD       X7AA1         X7AS3
     C*
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       XXAMT2        XXAMT3
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       XXAMT1        XSAMT3
     C*** END ADD *****  02/28/06  ************************************
     C     XXTOT         IFNE      *ZEROS
     C*** BEGIN ADD ***  10/07/03  ************************************
     C     X1TOT         ORNE      *ZEROS
     C*** END ADD *****  10/07/03  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     X1ATOT        ORNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C                   ADD       1             X
     C*
     C                   Z-ADD     BD(Z)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C*
     C                   Z-ADD     XXTOT         PA(X)
     C                   Z-ADD     X1TOT         P1(X)
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C                   END                                                    XXTOT <> 0
     C*
     C                   ELSE                                                   ELBSCD <> XXBSCD
     C*
     C*  IF THE BILLING CODE WAS CHANGED ALL FUTURE BILLINGS AND
     C*  ADJUSTMENTS WILL BE DELETED SO THE ADUSTMENT AMOUNT IS
     C*  SIMPLY DETERMINED BY THE NEW BILLING SCHEDULE CODE
     C*
     C                   ADD       1             X
     C*
     C                   Z-ADD     BD(Z)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C*
     C     AB2(Z)        MULT(H)   X1ANPR        PA(X)
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  PA(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        PA(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C     AB2(Z)        MULT(H)   ELTXDU        P1(X)
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  P1(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        P1(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C                   ADD       P1(X)         X1AMT3
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C* 1ST ASSESSMENT
     C*
     C     AB2(Z)        MULT(H)   XX1STB        A1(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A1(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A1(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A1(X)         X1AA3
     C*
     C* 2ND ASSESSMENT
     C*
     C     AB2(Z)        MULT(H)   XX2NDB        A2(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A2(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A2(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A2(X)         X2AA3
     C*
     C* 3RD ASSESSMENT
     C*
     C     AB2(Z)        MULT(H)   XX3RDB        A3(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A3(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A3(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A3(X)         X3AA3
     C*
     C* 4TH ASSESSMENT
     C*
     C     AB2(Z)        MULT(H)   XX4THB        A4(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A4(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A4(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A4(X)         X4AA3
     C*
     C* 5TH ASSESSMENT
     C*
     C     AB2(Z)        MULT(H)   XX5THB        A5(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A5(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A5(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A5(X)         X5AA3
     C*
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
     C*
     C     AB2(Z)        MULT(H)   XX6THB        A6(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A6(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A6(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A6(X)         X6AA3
     C*
     C* 7TH ASSESSMENT
     C*
     C     AB2(Z)        MULT(H)   XX7THB        A7(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A7(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A7(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A7(X)         X7AA3
     C*
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       PA(X)         XXAMT3
     C*
     C                   END                                                    ELBSCD = XXBSCD
     C                   END                                                    AB2,Z <> 0
     C                   END                                                    Y DO 12 Z
     C*
      ***** BEG DEL ******* 03/06/07 **********************************
      *********
     C*****Begin Add  ***  06/24/05  **************************************
     C*********            SELEC
     C*********  CLTID     WHEQ '109'
     C*****BEGIN ADD  ***  04/26/06  **************************************
     C*********  CLTID     OREQ '071'
     C*****END*ADD ******  04/26/06  **************************************
     C*****BEGIN ADD  ***  07/11/06  **************************************
     C*********  CLTID     OREQ '132'
     C*****END*ADD ******  07/11/06  **************************************
     C*********
     C**TAKE*THE FIRST ADJUSTMENT, WHICH SHOULD BE OF ALL PAST AND
     C**CURRENT ADJUSTMENTS, AND SPREAD IT ACROSS ALL FUTURE THE
     C**FUTURE*INSTALLMENTS.
     C*********
     C*********  PA,1      IFGT *ZERO
     C*****BEGIN ADD  ***  04/26/06  **************************************
     C*********  CLTID     ANDEQ'109'
     C*********  PA,1      ORGT *ZERO
     C*****BEG*DEL ******  10/18/06  **************************************
     C*********  ELFFL1    ANDEQ'Y'
     C*****END*DEL ******  10/18/06  **************************************
     C*********  CLTID     ANDEQ'071'
     C*********  PA,1      ORLT *ZERO
     C*********  ELBSCD    ANDEQXXBSCD
     C*****BEG*DEL ******  10/18/06  **************************************
     C*********  ELFFL1    ANDEQ'Y'
     C*****END*DEL ******  10/18/06  **************************************
     C*********  CLTID     ANDEQ'071'
     C*****BEGIN ADD  ***  07/11/06  **************************************
     C*********  PA,1      ORGT *ZERO
     C*********  ELFFL1    ANDEQ'Y'
     C*********  CLTID     ANDEQ'132'
     C*********  PA,1      ORLT *ZERO
     C*********  ELBSCD    ANDEQXXBSCD
     C*********  ELFFL1    ANDEQ'Y'
     C*********  CLTID     ANDEQ'132'
     C***  END ADD ******  07/11/06  **************************************
     C***  END ADD ******  04/26/06  **************************************
      ***** END DEL ******* 03/06/07 **********************************
      ***** BEG ADD ******* 03/06/07 **********************************
      *
      * TAKE THE FIRST ADJUSTMENT, WHICH SHOULD BE OF ALL PAST AND
      * CURRENT ADJUSTMENTS, AND SPREAD IT ACROSS ALL FUTURE THE
      * FUTURE INSTALLMENTS.
      *
     C                   MOVE      ELFFL1        XXFFL1            1
      *
     C     CLTID         IFEQ      '109'
     C                   MOVE      'Y'           XXFFL1
     C                   ENDIF
      *
     C     PA(1)         IFGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     P1(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A1(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A2(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A3(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A4(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A5(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A7(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  10/07/08  ************************************
     C*** BEGIN DEL ***  09/14/07  ************************************
     C*********  PA,1      ORLT *ZERO
     C*********  ELBSCD    ANDEQXXBSCD
     C*********  XXFFL1    ANDEQ'Y'
     C*********  CLTID     ANDNE'109'
     C*** END DEL *****  09/14/07  ************************************
      *
      ***** END ADD ******* 03/06/07 **********************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
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
     C                   MOVEL     SRDTO         XXDT
     C                   ENDIF
     C*** BEGIN ADD ***  02/12/09  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  02/12/09  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     DA(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT1             7 0
     C*
     C     XXDT1         IFLE      XXDT
     C     XXDT1         ANDNE     *ZERO
     C*
     C                   Z-ADD     0             XXINST            2 0
     C     2             DO        12            X
     C     PA(X)         IFNE      *ZEROS
     C                   ADD       1             XXINST
     C                   ENDIF
     C                   ENDDO
     C*
     C     XXINST        IFGE      1
     C*
     C                   SUB       PA(1)         XXAMT3
     C                   SUB       P1(1)         X1AMT3
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEGIN CHG ***  10/07/08  ************************************
     C*********            SUB  A1,1      X1AA3
     C*********            SUB  A2,1      X2AA3
     C*********            SUB  A3,1      X3AA3
     C*********            SUB  A4,1      X4AA3
     C*********            SUB  A5,1      X5AA3
     C     XU1STB        IFEQ      0
     C                   SUB       A1(1)         X1AA3
     C                   ENDIF
     C     XU2NDB        IFEQ      0
     C                   SUB       A2(1)         X2AA3
     C                   ENDIF
     C     XU3RDB        IFEQ      0
     C                   SUB       A3(1)         X3AA3
     C                   ENDIF
     C     XU4THB        IFEQ      0
     C                   SUB       A4(1)         X4AA3
     C                   ENDIF
     C     XU5THB        IFEQ      0
     C                   SUB       A5(1)         X5AA3
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XU6THB        IFEQ      0
     C                   SUB       A6(1)         X6AA3
     C                   ENDIF
     C     XU7THB        IFEQ      0
     C                   SUB       A7(1)         X7AA3
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  10/07/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C     1             DO        XXINST        X
     C     X             ADD       1             W
     C     PA(1)         DIV(H)    XXINST        XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C                   ADD       XXAMT2        PA(W)
     C                   ADD       XXAMT2        XXAMT3
     C     P1(1)         DIV(H)    XXINST        XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C                   ADD       XXAMT2        P1(W)
     C                   ADD       XXAMT2        X1AMT3
     C*** BEGIN ADD ***  05/15/07  ************************************
     C* 1ST ASSESSMENT
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     XU1STB        IFEQ      0
     C*** END ADD *****  10/07/08  ************************************
     C     A1(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A1(W)
     C                   ADD       XXAMT2        X1AA3
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ENDIF                                                  XU1STB IFEQ
     C*** END ADD *****  10/07/08  ************************************
     C*
     C* 2ND ASSESSMENT
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     XU2NDB        IFEQ      0
     C*** END ADD *****  10/07/08  ************************************
     C     A2(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A2(W)
     C                   ADD       XXAMT2        X2AA3
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ENDIF                                                  XU2NDB IFEQ
     C*** END ADD *****  10/07/08  ************************************
     C*
     C* 3RD ASSESSMENT
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     XU3RDB        IFEQ      0
     C*** END ADD *****  10/07/08  ************************************
     C     A3(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A3(W)
     C                   ADD       XXAMT2        X3AA3
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ENDIF                                                  XU3RDB IFEQ
     C*** END ADD *****  10/07/08  ************************************
     C*
     C* 4TH ASSESSMENT
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     XU4THB        IFEQ      0
     C*** END ADD *****  10/07/08  ************************************
     C     A4(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A4(W)
     C                   ADD       XXAMT2        X4AA3
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ENDIF                                                  XU4THB IFEQ
     C*** END ADD *****  10/07/08  ************************************
     C*
     C* 5TH ASSESSMENT
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     XU5THB        IFEQ      0
     C*** END ADD *****  10/07/08  ************************************
     C     A5(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A5(W)
     C                   ADD       XXAMT2        X5AA3
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ENDIF                                                  XU5THB IFEQ
     C*** END ADD *****  10/07/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
     C     XU6THB        IFEQ      0
     C     A6(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A6(W)
     C                   ADD       XXAMT2        X6AA3
     C                   ENDIF                                                  XU6THB IFEQ
     C*
     C* 7TH ASSESSMENT
     C     XU7THB        IFEQ      0
     C     A7(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A7(W)
     C                   ADD       XXAMT2        X7AA3
     C                   ENDIF                                                  XU7THB IFEQ
     C*** END ADD *****  12/11/08  ************************************
     C                   ENDDO
     C*
     C*** BEGIN ADD ***  10/07/08  ************************************
     C*** BEG DEL ***** 06/28/11 **************************************
     C***********XU1STB    IFEQ 0
     C***********XU2NDB    ANDEQ0
     C***********XU3RDB    ANDEQ0
     C***********XU4THB    ANDEQ0
     C***********XU5THB    ANDEQ0
     C****BEGIN*ADD ***  12/11/08  ************************************
     C***********XU6THB    ANDEQ0
     C***********XU7THB    ANDEQ0
     C*** END DEL ***** 06/28/11 **************************************
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  10/07/08  ************************************
     C     2             DO        12            X
     C     X             SUB       1             W
     C                   Z-ADD     PA(X)         PA(W)
     C                   Z-ADD     DA(X)         DA(W)
     C                   Z-ADD     0             PA(X)
     C                   Z-ADD     0             DA(X)
     C                   Z-ADD     P1(X)         P1(W)
     C                   Z-ADD     0             P1(X)
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*** BEGIN CHG ***  10/07/08  ************************************
     C********             Z-ADDA1,X      A1,W
     C********             Z-ADD0         A1,X
     C********             Z-ADDA2,X      A2,W
     C********             Z-ADD0         A2,X
     C********             Z-ADDA3,X      A3,W
     C********             Z-ADD0         A3,X
     C********             Z-ADDA4,X      A4,W
     C********             Z-ADD0         A4,X
     C********             Z-ADDA5,X      A5,W
     C********             Z-ADD0         A5,X
     C     XU1STB        IFEQ      0
     C                   Z-ADD     A1(X)         A1(W)
     C                   Z-ADD     0             A1(X)
     C                   ENDIF
     C     XU2NDB        IFEQ      0
     C                   Z-ADD     A2(X)         A2(W)
     C                   Z-ADD     0             A2(X)
     C                   ENDIF
     C     XU3RDB        IFEQ      0
     C                   Z-ADD     A3(X)         A3(W)
     C                   Z-ADD     0             A3(X)
     C                   ENDIF
     C     XU4THB        IFEQ      0
     C                   Z-ADD     A4(X)         A4(W)
     C                   Z-ADD     0             A4(X)
     C                   ENDIF
     C     XU5THB        IFEQ      0
     C                   Z-ADD     A5(X)         A5(W)
     C                   Z-ADD     0             A5(X)
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XU6THB        IFEQ      0
     C                   Z-ADD     A6(X)         A6(W)
     C                   Z-ADD     0             A6(X)
     C                   ENDIF
     C     XU7THB        IFEQ      0
     C                   Z-ADD     A7(X)         A7(W)
     C                   Z-ADD     0             A7(X)
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  10/07/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C                   ENDDO
     C*** BEGIN ADD ***  10/07/08  ************************************
     C*** BEG DEL ***** 06/28/11 **************************************
     C***********          ELSE
     C***********
     C***********          Z-ADD0         PA,1
     C***********          Z-ADD0         P1,1
     C***********
     C***********XU1STB    IFEQ 0
     C***********          Z-ADD0         A1,1
     C***********          ENDIF
     C***********XU2NDB    IFEQ 0
     C***********          Z-ADD0         A2,1
     C***********          ENDIF
     C***********XU3RDB    IFEQ 0
     C***********          Z-ADD0         A3,1
     C***********          ENDIF
     C***********XU4THB    IFEQ 0
     C***********          Z-ADD0         A4,1
     C***********          ENDIF
     C***********XU5THB    IFEQ 0
     C***********          Z-ADD0         A5,1
     C***********          ENDIF
     C****BEGIN*ADD ***  12/11/08  ************************************
     C***********XU6THB    IFEQ 0
     C***********          Z-ADD0         A6,1
     C***********          ENDIF
     C***********XU7THB    IFEQ 0
     C***********          Z-ADD0         A7,1
     C***********          ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C***********          ENDIF                           XU1STB IFEQ 0
     C*** END DEL ***** 06/28/11 **************************************
     C*** END ADD *****  10/07/08  ************************************
     C*
     C                   ENDIF                                                  XXINST IFGE
     C*
     C                   ENDIF                                                  XXDT1 IFLE
     C*
     C                   ENDIF                                                  PA,1 IFGT
     C*
      ***** BEG DEL ******* 03/06/07 **********************************
     C*********            ENDSL
      ***** BEG DEL ******* 03/06/07 **********************************
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C*  CHECK FOR ANY ROUNDING DISCREPANCIES BETWEEN CURRENT A/R AND
     C*  NEW ADJUSTMENTS THAT WILL BE POSTED TO A/R; BOTH SHOULD EQUAL
     C*  THE NEW PREMIUM AMOUNT TO THE PENNY
     C*
     C     X1ANPR        SUB       XXAMT3        XXAMT3
     C     ELTXDU        SUB       X1AMT3        X1AMT3
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     XX1STB        SUB       X1AA3         X1AA3
     C     XX2NDB        SUB       X2AA3         X2AA3
     C     XX3RDB        SUB       X3AA3         X3AA3
     C     XX4THB        SUB       X4AA3         X4AA3
     C     XX5THB        SUB       X5AA3         X5AA3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XX6THB        SUB       X6AA3         X6AA3
     C     XX7THB        SUB       X7AA3         X7AA3
     C*** END ADD *****  12/11/08  ************************************
     C*** BEGIN ADD ***  01/11/08  ************************************
     C                   ADD       XU1STB        X1AA3
     C                   ADD       XU2NDB        X2AA3
     C                   ADD       XU3RDB        X3AA3
     C                   ADD       XU4THB        X4AA3
     C                   ADD       XU5THB        X5AA3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       XU6THB        X6AA3
     C                   ADD       XU7THB        X7AA3
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  01/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C*
     C* ROUNDING FOR PRIOR PREMIUM ON A/R SUMMARIZED SINGLE DIVISION.
     C*
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     SVANPR        SUB       XSAMT3        XSAMT3
     C     SVTXDU        SUB       X2AMT3        X2AMT3
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     SV1STB        SUB       X1AS3         X1AS3
     C     SV2NDB        SUB       X2AS3         X2AS3
     C     SV3RDB        SUB       X3AS3         X3AS3
     C     SV4THB        SUB       X4AS3         X4AS3
     C     SV5THB        SUB       X5AS3         X5AS3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     SV6THB        SUB       X6AS3         X6AS3
     C     SV7THB        SUB       X7AS3         X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C*** BEGIN ADD ***  01/11/08  ************************************
     C                   ADD       SU1STB        X1AS3
     C                   ADD       SU2NDB        X2AS3
     C                   ADD       SU3RDB        X3AS3
     C                   ADD       SU4THB        X4AS3
     C                   ADD       SU5THB        X5AS3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       SU6THB        X6AS3
     C                   ADD       SU7THB        X7AS3
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  01/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*
     C     XSAMT3        IFLT      1.00
     C     XSAMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     PA(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     PA(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   SUB       XSAMT3        PA(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   SUB       XSAMT3        PA(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X2AMT3        IFLT      1.00
     C     X2AMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     P1(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     P1(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   SUB       X2AMT3        P1(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   SUB       X2AMT3        P1(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C     X1AS3         IFLT      1.00
     C     X1AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A1(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A1(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   SUB       X1AS3         A1(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   SUB       X1AS3         A1(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X2AS3         IFLT      1.00
     C     X2AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A2(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A2(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   SUB       X2AS3         A2(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   SUB       X2AS3         A2(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X3AS3         IFLT      1.00
     C     X3AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A3(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A3(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   SUB       X3AS3         A3(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   SUB       X3AS3         A3(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X4AS3         IFLT      1.00
     C     X4AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A4(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A4(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   SUB       X4AS3         A4(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   SUB       X4AS3         A4(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X5AS3         IFLT      1.00
     C     X5AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A5(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A5(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   SUB       X5AS3         A5(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   SUB       X5AS3         A5(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     X6AS3         IFLT      1.00
     C     X6AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A6(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A6(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X6AS3         A6(1)
     C                   ELSE
     C                   SUB       X6AS3         A6(2)
     C                   ENDIF
     C                   END
     C*
     C     X7AS3         IFLT      1.00
     C     X7AS3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A7(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A7(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   SUB       X7AS3         A7(1)
     C                   ELSE
     C                   SUB       X7AS3         A7(2)
     C                   ENDIF
     C                   END
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  *IN67 IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C*
     C*  DIFFERENCE SHOULD NEVER BE GREATER THAN OR LESS THAN $1.00
     C*
     C     XXAMT3        IFLT      1.00
     C     XXAMT3        ANDGT     -1.00
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        OREQ      'Y'
     C*** END ADD *****  02/20/06  ************************************
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     PA(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     PA(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   ADD       XXAMT3        PA(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   ADD       XXAMT3        PA(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C     X1AMT3        IFLT      1.00
     C     X1AMT3        ANDGT     -1.00
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        OREQ      'Y'
     C*** END ADD *****  02/20/06  ************************************
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     P1(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     P1(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   ADD       X1AMT3        P1(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   ADD       X1AMT3        P1(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C     X1AA3         IFLT      1.00
     C     X1AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A1(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A1(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   ADD       X1AA3         A1(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   ADD       X1AA3         A1(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X2AA3         IFLT      1.00
     C     X2AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A2(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A2(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   ADD       X2AA3         A2(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   ADD       X2AA3         A2(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X3AA3         IFLT      1.00
     C     X3AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A3(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A3(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   ADD       X3AA3         A3(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   ADD       X3AA3         A3(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X4AA3         IFLT      1.00
     C     X4AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A4(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A4(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   ADD       X4AA3         A4(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   ADD       X4AA3         A4(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*
     C     X5AA3         IFLT      1.00
     C     X5AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A5(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A5(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C*** END ADD *****  10/07/08  ************************************
     C                   ADD       X5AA3         A5(1)
     C*** BEGIN ADD ***  10/07/08  ************************************
     C                   ELSE
     C                   ADD       X5AA3         A5(2)
     C                   ENDIF
     C*** END ADD *****  10/07/08  ************************************
     C                   END
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     X6AA3         IFLT      1.00
     C     X6AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A6(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A6(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X6AA3         A6(1)
     C                   ELSE
     C                   ADD       X6AA3         A6(2)
     C                   ENDIF
     C                   END
     C*
     C     X7AA3         IFLT      1.00
     C     X7AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A7(1)         IFNE      *ZERO
      ***** BEG ADD ******* 10/14/10 **********************************
     C     A7(2)         OREQ      *ZERO
      ***** END ADD ******* 10/14/10 **********************************
     C                   ADD       X7AA3         A7(1)
     C                   ELSE
     C                   ADD       X7AA3         A7(2)
     C                   ENDIF
     C                   END
     C*** END ADD *****  12/11/08  ************************************
     C*** BEGIN ADD ***  04/21/03  ************************************
     C                   ELSE                                                   BSDOWK IFEQ
     C*** BEGIN ADD ***  04/25/03  ************************************
     C     BSFREQ        IFEQ      'T'
      *
      * PROCESS AND BILL TO A/R TWICE A MONTH BILLLING INSTALLMENTS.
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     0             WK1
     C                   Z-ADD     0             WK2
     C                   Z-ADD     0             WK3
     C                   Z-ADD     0             WK4
     C                   Z-ADD     0             WK5
     C*
     C                   Z-ADD     0             SK1
     C                   Z-ADD     0             SK2
     C                   Z-ADD     0             SK3
     C                   Z-ADD     0             SK4
     C                   Z-ADD     0             SK5
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             WK6
     C                   Z-ADD     0             WK7
     C*
     C                   Z-ADD     0             SK6
     C                   Z-ADD     0             SK7
     C*** END ADD *****  12/11/08  ************************************
     C                   Z-ADD     0             WKD
     C                   Z-ADD     0             WKA
     C                   Z-ADD     0             WKT
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     0             SKA
     C                   Z-ADD     0             SKT
     C*** END ADD *****  02/28/06  ************************************
     C*
     C                   MOVE      ELBSDT        XXWKDT
     C*
     C* CHECK FOR FIRST INSTALLMENT ON/AFTER BILLING START DATE.
     C*
     C     WDD           IFEQ      1
     C     WDD           OREQ      15
     C                   Z-ADD     ELBSDT        WKD(1)
     C                   ELSE
     C*
     C     WDD           IFLT      15
     C                   Z-ADD     15            WDD
     C                   Z-ADD     XXWKDT        WKD(1)
     C                   ELSE
     C*
     C                   Z-ADD     1             WDD
     C                   ADD       1             WMM
     C     WMM           IFEQ      13
     C                   Z-ADD     1             WMM
     C                   ADD       1             WYY
     C                   ENDIF
     C*
     C                   Z-ADD     XXWKDT        WKD(1)
     C                   ENDIF                                                  DD IFLT
     C*
     C                   ENDIF                                                  DD IFEQ
     C*
     C                   Z-ADD     1             T                 3 0
     C*
     C     WKD(1)        IFGE      ELPTDT
     C                   Z-ADD     ELBSDT        WKD(1)
     C                   ELSE
     C*
     C* BUILD DATE ARRAY WITH EACH TWICE MONTHLY INSTALLMENT DATE.
     C*
     C     XXWKDT        DOWLT     ELPTDT
     C*
     C     WDD           IFEQ      1
     C                   Z-ADD     15            WDD
     C                   ELSE
     C*
     C                   Z-ADD     1             WDD
     C                   ADD       1             WMM
     C     WMM           IFEQ      13
     C                   Z-ADD     1             WMM
     C                   ADD       1             WYY
     C                   ENDIF
     C*
     C                   ENDIF                                                  WDD IFEQ
     C*
     C     XXWKDT        IFLT      ELPTDT
     C                   ADD       1             T
     C                   Z-ADD     XXWKDT        WKD(T)
     C                   ENDIF                                                  XXWKDT IFLT
     C*
     C                   ENDDO                                                  XXWKDT DOWLT
     C*
     C                   ENDIF                                                  WKD,1 IFGE
     C*
     C* CALCULATE EACH INSTALLMENT AMOUNT.
     C*
     C     X1ANPR        DIV(H)    T             XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     SVANPR        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     SVAMT2        SKA(U)
     C*** END ADD *****  02/28/06  ************************************
     C                   ENDDO
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C     XXCOMP        IFNE      X1ANPR
     C     X1ANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   XFOOT     SKA           XXCOMP           15 4
     C     XXCOMP        IFNE      SVANPR
     C     SVANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKA(1)
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C*
     C* STATE TAX
     C*
     C     ELTXDU        DIV(H)    T             XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     SVTXDU        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKT(U)
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     SVAMT2        SKT(U)
     C*** END ADD *****  02/28/06  ************************************
     C                   ENDDO
     C*
     C                   XFOOT     WKT           XXCOMP           15 4
     C     XXCOMP        IFNE      ELTXDU
     C     ELTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKT(1)
     C                   ENDIF
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   XFOOT     SKT           XXCOMP           15 4
     C     XXCOMP        IFNE      SVTXDU
     C     SVTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKT(1)
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C*** BEG ADD *****  05/15/07  ************************************
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********  RR1STB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX1STB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV1STB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK1(U)
     C*
     C                   Z-ADD     SVAMT2        SK1(U)
     C*
     C                   ENDDO
     C*
     C                   XFOOT     WK1           XXCOMP           15 4
     C     XXCOMP        IFNE      XX1STB
     C     XX1STB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK1(1)
     C                   ENDIF
     C*
     C                   XFOOT     SK1           XXCOMP           15 4
     C     XXCOMP        IFNE      SV1STB
     C     SV1STB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK1(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR2NDB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX2NDB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV2NDB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK2(U)
     C*
     C                   Z-ADD     SVAMT2        SK2(U)
     C*
     C                   ENDDO
     C*
     C                   XFOOT     WK2           XXCOMP           15 4
     C     XXCOMP        IFNE      XX2NDB
     C     XX2NDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK2(1)
     C                   ENDIF
     C*
     C                   XFOOT     SK2           XXCOMP           15 4
     C     XXCOMP        IFNE      SV2NDB
     C     SV2NDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK2(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR3RDB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX3RDB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV3RDB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK3(U)
     C*
     C                   Z-ADD     SVAMT2        SK3(U)
     C*
     C                   ENDDO
     C*
     C                   XFOOT     WK3           XXCOMP           15 4
     C     XXCOMP        IFNE      XX3RDB
     C     XX3RDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK3(1)
     C                   ENDIF
     C*
     C                   XFOOT     SK3           XXCOMP           15 4
     C     XXCOMP        IFNE      SV3RDB
     C     SV3RDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK3(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR4THB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX4THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV4THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK4(U)
     C*
     C                   Z-ADD     SVAMT2        SK4(U)
     C*
     C                   ENDDO
     C*
     C                   XFOOT     WK4           XXCOMP           15 4
     C     XXCOMP        IFNE      XX4THB
     C     XX4THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK4(1)
     C                   ENDIF
     C*
     C                   XFOOT     SK4           XXCOMP           15 4
     C     XXCOMP        IFNE      SV4THB
     C     SV4THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK4(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR5THB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX5THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV5THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK5(U)
     C*
     C                   Z-ADD     SVAMT2        SK5(U)
     C*
     C                   ENDDO
     C*
     C                   XFOOT     WK5           XXCOMP           15 4
     C     XXCOMP        IFNE      XX5THB
     C     XX5THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK5(1)
     C                   ENDIF
     C*
     C                   XFOOT     SK5           XXCOMP           15 4
     C     XXCOMP        IFNE      SV5THB
     C     SV5THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK5(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*** END DEL *****  01/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
     C*
     C     XX6THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV6THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK6(U)
     C*
     C                   Z-ADD     SVAMT2        SK6(U)
     C*
     C                   ENDDO
     C*
     C                   XFOOT     WK6           XXCOMP           15 4
     C     XXCOMP        IFNE      XX6THB
     C     XX6THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK6(1)
     C                   ENDIF
     C*
     C                   XFOOT     SK6           XXCOMP           15 4
     C     XXCOMP        IFNE      SV6THB
     C     SV6THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK6(1)
     C                   ENDIF
     C*
     C* 7TH ASSESSMENT
     C*
     C     XX7THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV7THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK7(U)
     C*
     C                   Z-ADD     SVAMT2        SK7(U)
     C*
     C                   ENDDO
     C*
     C                   XFOOT     WK7           XXCOMP           15 4
     C     XXCOMP        IFNE      XX7THB
     C     XX7THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK7(1)
     C                   ENDIF
     C*
     C                   XFOOT     SK7           XXCOMP           15 4
     C     XXCOMP        IFNE      SV7THB
     C     SV7THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK7(1)
     C                   ENDIF
     C*
     C*** END ADD *****  12/11/08  ************************************
     C*
     C* BILL INSTALLMENTS INTO A/R, CATCHING UP TO CURRENT DATE
     C* IF NECESSARY.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
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
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* CALCULATE A/R ADJUSTMENTS.
     C*
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     0             SVAMT2
     C                   Z-ADD     0             S1AMT2           11 2
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C*
     C                   Z-ADD     0             SV1AA2           11 2
     C                   Z-ADD     0             SV2AA2           11 2
     C                   Z-ADD     0             SV3AA2           11 2
     C                   Z-ADD     0             SV4AA2           11 2
     C                   Z-ADD     0             SV5AA2           11 2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C*
     C                   Z-ADD     0             SV6AA2           11 2
     C                   Z-ADD     0             SV7AA2           11 2
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*** BEGIN ADD ***  01/11/08  ************************************
     C                   ADD       XU1STB        WK1(1)
     C                   ADD       XU2NDB        WK2(1)
     C                   ADD       XU3RDB        WK3(1)
     C                   ADD       XU4THB        WK4(1)
     C                   ADD       XU5THB        WK5(1)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       XU6THB        WK6(1)
     C                   ADD       XU7THB        WK7(1)
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ADD       SU1STB        SK1(1)
     C                   ADD       SU2NDB        SK2(1)
     C                   ADD       SU3RDB        SK3(1)
     C                   ADD       SU4THB        SK4(1)
     C                   ADD       SU5THB        SK5(1)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       SU6THB        SK6(1)
     C                   ADD       SU7THB        SK7(1)
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  01/11/08  ************************************
     C*** END   ADD **** 05/15/07  ************************************
     C                   Z-ADD     0             Y
     C                   Z-ADD     0             Z
     C*
     C     1             DO        53            U
     C*
     C     WKA(U)        IFNE      *ZERO
     C*
     C* CATCH UP TO NEXT INSTALLMENT DUE.
     C*
     C     WKD(U)        IFLT      XXCURD
     C                   ADD       WKA(U)        XXAMT2
     C                   ADD       WKT(U)        X1AMT2
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD **** 05/15/07  ************************************
     C*
     C                   ADD       WK1(U)        X1AA2
     C                   ADD       WK2(U)        X2AA2
     C                   ADD       WK3(U)        X3AA2
     C                   ADD       WK4(U)        X4AA2
     C                   ADD       WK5(U)        X5AA2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       WK6(U)        X6AA2
     C                   ADD       WK7(U)        X7AA2
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ADD       SK1(U)        SV1AA2
     C                   ADD       SK2(U)        SV2AA2
     C                   ADD       SK3(U)        SV3AA2
     C                   ADD       SK4(U)        SV4AA2
     C                   ADD       SK5(U)        SV5AA2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*** END   ADD **** 05/15/07  ************************************
     C                   ELSE
     C*
     C     Y             IFLT      2
     C*
     C                   ADD       1             Y
     C     Y             IFEQ      2
     C                   ADD       1             Z
     C                   ENDIF
     C     WKA(U)        ADD       XXAMT2        PA(Y)
     C     WKT(U)        ADD       X1AMT2        P1(Y)
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C     WK1(U)        ADD       X1AA2         A1(Y)
     C     WK2(U)        ADD       X2AA2         A2(Y)
     C     WK3(U)        ADD       X3AA2         A3(Y)
     C     WK4(U)        ADD       X4AA2         A4(Y)
     C     WK5(U)        ADD       X5AA2         A5(Y)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     WK6(U)        ADD       X6AA2         A6(Y)
     C     WK7(U)        ADD       X7AA2         A7(Y)
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ADD       SK1(U)        SV1AA2
     C                   ADD       SK2(U)        SV2AA2
     C                   ADD       SK3(U)        SV3AA2
     C                   ADD       SK4(U)        SV4AA2
     C                   ADD       SK5(U)        SV5AA2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*** END ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
     C*** END ADD *****  02/28/06  ************************************
     C     XXAMT2        IFNE      *ZERO
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C*** BEGIN ADD**  05/15/07  ************************************
     C*
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*** END   ADD**  05/15/07  ************************************
     C                   ENDIF
     C*
     C* SET DATE RANGE FOR PRIOR BILLED A/R.
     C*
     C                   Z-ADD     WKD(U)        XXDTE1
     C     U             ADD       1             T
     C                   Z-ADD     WKD(T)        XXDTE2
     C*
     C* IF CHANGING BSCD, DO NOT INCLUDE NEXT INSTALLMENT IN THE CATCH
     C* UP PRIOR A/R SINCE IT WILL BE DELETED.
     C*
     C     ELBSCD        IFNE      XXBSCD
     C     Y             ANDEQ     1
     C                   Z-ADD     XXCURD        XXDTE2
     C                   ENDIF                                                  ELBSCD IFNE
     C*
     C                   EXSR      SRWASB
     C*
     C* BILL FULL AMOUNT IF CHANGING BILLING SCHEDULE CODE.
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C     Y             OREQ      1
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     SVAMT2        XXAMT1
     C                   Z-ADD     S1AMT2        X1AMT1
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C                   ADD       SV1AA2        X1AA1
     C                   ADD       SV2AA2        X2AA1
     C                   ADD       SV3AA2        X3AA1
     C                   ADD       SV4AA2        X4AA1
     C                   ADD       SV5AA2        X5AA1
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       SV6AA2        X6AA1
     C                   ADD       SV7AA2        X7AA1
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*** END   ADD ***  05/15/07  ************************************
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C                   SUB       XXAMT1        PA(Y)
     C                   SUB       X1AMT1        P1(Y)
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C                   SUB       X1AA1         A1(Y)
     C                   SUB       X2AA1         A2(Y)
     C                   SUB       X3AA1         A3(Y)
     C                   SUB       X4AA1         A4(Y)
     C                   SUB       X5AA1         A5(Y)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   SUB       X6AA1         A6(Y)
     C                   SUB       X7AA1         A7(Y)
     C*** END ADD *****  12/11/08  ************************************
     C*
     C*** END   ADD ***  05/15/07  ************************************
     C                   ENDIF                                                  ELBSCD IFEQ
     C*
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C                   ELSE
     C                   ADD       1             Z
     C                   ENDIF                                                  Y IFLT 2
     C*
     C                   ENDIF                                                  WKD,U IFLT
     C*
     C                   ENDIF                                                  WKA,U IFNE
     C*
     C                   ENDDO                                                  1 DO 53
     C*
     C                   MOVE      *ON           *IN62
     C                   MOVE      *ON           *IN64
     C*** BEGIN ADD ***  01/06/04  ************************************
     C     PA(1)         IFEQ      *ZERO
     C     P1(1)         ANDEQ     *ZERO
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     A1(1)         ANDEQ     *ZERO
     C     A2(1)         ANDEQ     *ZERO
     C     A3(1)         ANDEQ     *ZERO
     C     A4(1)         ANDEQ     *ZERO
     C     A5(1)         ANDEQ     *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(1)         ANDEQ     *ZERO
     C     A7(1)         ANDEQ     *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*
     C     PA(2)         IFNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     A1(2)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(2)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C                   Z-ADD     PA(2)         PA(1)
     C                   Z-ADD     P1(2)         P1(1)
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   Z-ADD     A1(2)         A1(1)
     C                   Z-ADD     A2(2)         A2(1)
     C                   Z-ADD     A3(2)         A3(1)
     C                   Z-ADD     A4(2)         A4(1)
     C                   Z-ADD     A5(2)         A5(1)
     C                   Z-ADD     A6(2)         A6(1)
     C                   Z-ADD     A7(2)         A7(1)
      ***** END ADD ******* 10/14/10 **********************************
     C                   Z-ADD     DA(2)         DA(1)
     C                   SUB       1             Z
     C*
     C     Z             IFGT      0
     C                   Z-ADD     1             U
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2DATE            7 0
     C     X2DATE        LOOKUP    WKD(U)                             60
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(2)
     C                   ELSE
     C                   Z-ADD     0             PA(2)
     C                   Z-ADD     0             P1(2)
     C***BEGIN ADD*****  05/15/07  ************************************
     C                   Z-ADD     0             A1(2)
     C                   Z-ADD     0             A2(2)
     C                   Z-ADD     0             A3(2)
     C                   Z-ADD     0             A4(2)
     C                   Z-ADD     0             A5(2)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             A6(2)
     C                   Z-ADD     0             A7(2)
     C*** END ADD *****  12/11/08  ************************************
     C***END   ADD*****  05/15/07  ************************************
     C                   Z-ADD     0             DA(2)
     C                   ENDIF                                                  Z IFGT
     C*
     C                   ENDIF                                                  PA,2 IFNE
     C*
     C                   ENDIF                                                  PA,1 IFEQ
     C*** END ADD *****  01/06/04  ************************************
     C*
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         ORNE      *ZERO
     C     P1(1)         ORNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C***BEGIN ADD*****  05/15/07  ************************************
     C     A1(1)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*********  A1,3      ORNE *ZERO
     C*********  A1,4      ORNE *ZERO
     C*********  A1,5      ORNE *ZERO
     C*********  A2,1      ORNE *ZERO
     C*********  A2,2      ORNE *ZERO
     C*********  A2,3      ORNE *ZERO
     C*********  A2,4      ORNE *ZERO
     C*********  A2,5      ORNE *ZERO
     C     A2(1)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(1)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(1)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(1)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(1)         ORNE      *ZERO
     C     A6(2)         ORNE      *ZERO
     C     A7(1)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C*** END CHG *****  12/11/08  ************************************
     C***END   ADD*****  05/15/07  ************************************
     C                   Z-ADD     2             X
     C                   ENDIF
     C*
     C                   ELSE                                                   BSFREQ IFEQ
     C*** END ADD *****  04/25/03  ************************************
     C*
     C*  PROCESS WEEKLY BILLING SCHEDULE CODES.
     C*
     C                   Z-ADD     0             WKD
     C                   Z-ADD     0             WKA
     C                   Z-ADD     0             WKT
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     0             SKA
     C                   Z-ADD     0             SKT
     C*** END ADD *****  02/28/06  ************************************
     C***BEGIN ADD*****  05/15/07  ************************************
     C                   Z-ADD     0             WK1
     C                   Z-ADD     0             WK2
     C                   Z-ADD     0             WK3
     C                   Z-ADD     0             WK4
     C                   Z-ADD     0             WK5
     C                   Z-ADD     0             SK1
     C                   Z-ADD     0             SK2
     C                   Z-ADD     0             SK3
     C                   Z-ADD     0             SK4
     C                   Z-ADD     0             SK5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             WK6
     C                   Z-ADD     0             WK7
     C                   Z-ADD     0             SK6
     C                   Z-ADD     0             SK7
     C*** END ADD *****  12/11/08  ************************************
     C***END   ADD*****  05/15/07  ************************************
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     ELPTDT        SRDTI
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
     C* TOTAL DAYS IN POLICY
     C*
     C                   Z-ADD     QQDAYS        TDAYS             5 0
     C*** BEGIN ADD ***  06/21/04  ************************************
     C     TDAYS         IFGT      365
     C                   Z-ADD     365           TDAYS
     C                   ENDIF
     C*** END ADD *****  06/21/04  ************************************
     C*
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTE
     C                   MOVE      *BLANK        SRSDAY
     C                   MOVE      *BLANK        SRLDAY
     C                   MOVE      *BLANK        SRERR
     C                   CALL      'SRDAY8'
     C                   PARM                    SRDTE             8 0          M/D/YY
     C                   PARM                    SRSDAY            4            Abbreviation
     C                   PARM                    SRLDAY            9            Full Day Name
     C                   PARM                    SRERR                          Error Flag
     C*
     C                   MOVEL     SRSDAY        XXDAY             2
     C                   Z-ADD     0             XXCNT             3 0
     C*
     C* CHECK FOR FIRST DAY OF WEEK ON/AFTER BILLING START DATE.
     C*
     C     XXDAY         IFEQ      BSDOWK
     C                   Z-ADD     ELBSDT        WKD(1)
     C                   ELSE
     C*
     C                   Z-ADD     0             X1                3 0
     C                   Z-ADD     0             X2                3 0
     C                   Z-ADD     1             T                 3 0
     C*
     C     XXDAY         LOOKUP    DAY(T)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     T             X1
     C                   ENDIF                                                  *IN60
     C*
     C     BSDOWK        LOOKUP    DAY(T)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     T             X2
     C                   ENDIF                                                  *IN60
     C*
     C     X2            SUB       X1            XXCNT
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      SRDTE         QQDT              8 0
     C                   PARM      XXCNT         QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTE
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKD(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDAY IFEQ
     C*
     C     WKD(1)        IFGE      ELPTDT
     C                   Z-ADD     ELBSDT        WKD(1)
     C                   ENDIF
     C*
     C                   Z-ADD     1             T                 3 0
     C                   ADD       7             XXCNT
     C*
     C* BUILD DATE ARRAY WITH EACH WEEKLY INSTALLMENT DATE.
     C*
     C     XXCNT         DOWLT     TDAYS
     C*
     C                   ADD       1             T
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      SRDTE         QQDT              8 0
     C                   PARM      7             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTE
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKD(T)
     C                   ENDIF
     C*
     C                   ADD       7             XXCNT
     C                   ENDDO                                                  XXCNT IFLT
     C*
     C* CALCULATE EACH WEEKLY INSTALLMENT AMOUNT.
     C*
     C     X1ANPR        DIV(H)    T             XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     SVANPR        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     SVAMT2        SKA(U)
     C*** END ADD *****  02/28/06  ************************************
     C                   ENDDO
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C     XXCOMP        IFNE      X1ANPR
     C     X1ANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   XFOOT     SKA           XXCOMP           15 4
     C     XXCOMP        IFNE      SVANPR
     C     SVANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKA(1)
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C*
     C* WEEKLY STATE TAX
     C*
     C     ELTXDU        DIV(H)    T             XXAMT2
     C*** BEGIN ADD ***  02/20/06  ************************************
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/20/06  ************************************
     C*
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     SVTXDU        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*** END ADD *****  02/28/06  ************************************
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKT(U)
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     SVAMT2        SKT(U)
     C*** END ADD *****  02/28/06  ************************************
     C                   ENDDO
     C*
     C                   XFOOT     WKT           XXCOMP           15 4
     C     XXCOMP        IFNE      ELTXDU
     C     ELTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKT(1)
     C                   ENDIF
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   XFOOT     SKT           XXCOMP           15 4
     C     XXCOMP        IFNE      SVTXDU
     C     SVTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SKT(1)
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD *****  05/15/07  **********************************
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********  RR1STB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX1STB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV1STB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK1(U)
     C                   Z-ADD     SVAMT2        SK1(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK1           XXCOMP           15 4
     C     XXCOMP        IFNE      XX1STB
     C     XX1STB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK1(1)
     C                   ENDIF
     C                   XFOOT     SK1           XXCOMP           15 4
     C     XXCOMP        IFNE      SV1STB
     C     SV1STB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK1(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR2NDB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX2NDB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV2NDB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK2(U)
     C                   Z-ADD     SVAMT2        SK2(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK2           XXCOMP           15 4
     C     XXCOMP        IFNE      XX2NDB
     C     XX2NDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK2(1)
     C                   ENDIF
     C                   XFOOT     SK2           XXCOMP           15 4
     C     XXCOMP        IFNE      SV2NDB
     C     SV2NDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK2(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR3RDB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX3RDB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV3RDB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK3(U)
     C                   Z-ADD     SVAMT2        SK3(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK3           XXCOMP           15 4
     C     XXCOMP        IFNE      XX3RDB
     C     XX3RDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK3(1)
     C                   ENDIF
     C                   XFOOT     SK3           XXCOMP           15 4
     C     XXCOMP        IFNE      SV3RDB
     C     SV3RDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK3(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR4THB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX4THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV4THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK4(U)
     C                   Z-ADD     SVAMT2        SK4(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK4           XXCOMP           15 4
     C     XXCOMP        IFNE      XX4THB
     C     XX4THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK4(1)
     C                   ENDIF
     C                   XFOOT     SK4           XXCOMP           15 4
     C     XXCOMP        IFNE      SV4THB
     C     SV4THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK4(1)
     C                   ENDIF
     C*
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*
     C*********  RR5THB    IFEQ 'Y'
     C*** END DEL *****  01/11/08  ************************************
     C*
     C     XX5THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV5THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK5(U)
     C                   Z-ADD     SVAMT2        SK5(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK5           XXCOMP           15 4
     C     XXCOMP        IFNE      XX5THB
     C     XX5THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK5(1)
     C                   ENDIF
     C                   XFOOT     SK5           XXCOMP           15 4
     C     XXCOMP        IFNE      SV5THB
     C     SV5THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK5(1)
     C                   ENDIF
     C*** BEGIN DEL ***  01/11/08  ************************************
     C*********            ENDIF
     C*** END DEL *****  01/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
     C*
     C     XX6THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV6THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK6(U)
     C                   Z-ADD     SVAMT2        SK6(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK6           XXCOMP           15 4
     C     XXCOMP        IFNE      XX6THB
     C     XX6THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK6(1)
     C                   ENDIF
     C                   XFOOT     SK6           XXCOMP           15 4
     C     XXCOMP        IFNE      SV6THB
     C     SV6THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK6(1)
     C                   ENDIF
     C*
     C* 7TH ASSESSMENT
     C*
     C     XX7THB        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     SV7THB        DIV(H)    T             SVAMT2           11 2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  SVAMT2        XXWK10
     C                   Z-ADD     XXWK10        SVAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK7(U)
     C                   Z-ADD     SVAMT2        SK7(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK7           XXCOMP           15 4
     C     XXCOMP        IFNE      XX7THB
     C     XX7THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK7(1)
     C                   ENDIF
     C                   XFOOT     SK7           XXCOMP           15 4
     C     XXCOMP        IFNE      SV7THB
     C     SV7THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         SK7(1)
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*
     C* BILL WEEKLY INSTALLMENTS INTO A/R, CATCHING UP TO CURRENT DATE
     C* IF NECESSARY.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
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
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* CALCULATE A/R ADJUSTMENTS.
     C*
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   Z-ADD     0             SVAMT2
     C                   Z-ADD     0             S1AMT2
     C*** END ADD *****  02/28/06  ************************************
     C***BEGIN ADD*****  05/15/07  ************************************
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C                   Z-ADD     0             SV1AA2           11 2
     C                   Z-ADD     0             SV2AA2           11 2
     C                   Z-ADD     0             SV3AA2           11 2
     C                   Z-ADD     0             SV4AA2           11 2
     C                   Z-ADD     0             SV5AA2           11 2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C                   Z-ADD     0             SV6AA2           11 2
     C                   Z-ADD     0             SV7AA2           11 2
     C*** END ADD *****  12/11/08  ************************************
     C*** BEGIN ADD ***  01/11/08  ************************************
     C                   ADD       XU1STB        WK1(1)
     C                   ADD       XU2NDB        WK2(1)
     C                   ADD       XU3RDB        WK3(1)
     C                   ADD       XU4THB        WK4(1)
     C                   ADD       XU5THB        WK5(1)
     C*
     C                   ADD       SU1STB        SK1(1)
     C                   ADD       SU2NDB        SK2(1)
     C                   ADD       SU3RDB        SK3(1)
     C                   ADD       SU4THB        SK4(1)
     C                   ADD       SU5THB        SK5(1)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       XU6THB        WK6(1)
     C                   ADD       XU7THB        WK7(1)
     C*
     C                   ADD       SU6THB        SK6(1)
     C                   ADD       SU7THB        SK7(1)
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  01/11/08  ************************************
     C***END   ADD*****  05/15/07  ************************************
     C                   Z-ADD     0             Y
     C                   Z-ADD     0             Z
     C*
     C     1             DO        53            U
     C*
     C     WKA(U)        IFNE      *ZERO
     C*
     C* CATCH UP TO NEXT INSTALLMENT DUE.
     C*
     C     WKD(U)        IFLT      XXCURD
     C                   ADD       WKA(U)        XXAMT2
     C                   ADD       WKT(U)        X1AMT2
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
     C*** END ADD *****  02/28/06  ************************************
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C                   ADD       WK1(U)        X1AA2
     C                   ADD       WK2(U)        X2AA2
     C                   ADD       WK3(U)        X3AA2
     C                   ADD       WK4(U)        X4AA2
     C                   ADD       WK5(U)        X5AA2
     C*
     C                   ADD       SK1(U)        SV1AA2
     C                   ADD       SK2(U)        SV2AA2
     C                   ADD       SK3(U)        SV3AA2
     C                   ADD       SK4(U)        SV4AA2
     C                   ADD       SK5(U)        SV5AA2
     C*
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       WK6(U)        X6AA2
     C                   ADD       WK7(U)        X7AA2
     C*
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD ***  05/15/07  ************************************
     C                   ELSE
     C*
     C     Y             IFLT      2
     C*
     C                   ADD       1             Y
     C     Y             IFEQ      2
     C                   ADD       1             Z
     C                   ENDIF
     C     WKA(U)        ADD       XXAMT2        PA(Y)
     C     WKT(U)        ADD       X1AMT2        P1(Y)
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C     WK1(U)        ADD       X1AA2         A1(Y)
     C     WK2(U)        ADD       X2AA2         A2(Y)
     C     WK3(U)        ADD       X3AA2         A3(Y)
     C     WK4(U)        ADD       X4AA2         A4(Y)
     C     WK5(U)        ADD       X5AA2         A5(Y)
     C*
     C                   ADD       SK1(U)        SV1AA2
     C                   ADD       SK2(U)        SV2AA2
     C                   ADD       SK3(U)        SV3AA2
     C                   ADD       SK4(U)        SV4AA2
     C                   ADD       SK5(U)        SV5AA2
     C*
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     WK6(U)        ADD       X6AA2         A6(Y)
     C     WK7(U)        ADD       X7AA2         A7(Y)
     C*
     C                   ADD       SK6(U)        SV6AA2
     C                   ADD       SK7(U)        SV7AA2
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  02/28/06  ************************************
     C                   ADD       SKA(U)        SVAMT2
     C                   ADD       SKT(U)        S1AMT2
     C*** END ADD *****  02/28/06  ************************************
     C     XXAMT2        IFNE      *ZERO
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C                   ENDIF
     C*
     C* SET DATE RANGE FOR PRIOR BILLED A/R.
     C*
     C                   Z-ADD     WKD(U)        XXDTE1
     C     U             ADD       1             T
     C                   Z-ADD     WKD(T)        XXDTE2
     C*
     C* IF CHANGING BSCD, DO NOT INCLUDE NEXT INSTALLMENT IN THE CATCH
     C* UP PRIOR A/R SINCE IT WILL BE DELETED.
     C*
     C     ELBSCD        IFNE      XXBSCD
     C     Y             ANDEQ     1
     C                   Z-ADD     XXCURD        XXDTE2
     C                   ENDIF                                                  ELBSCD IFNE
     C*
     C                   EXSR      SRWASB
     C*
     C* BILL FULL WEEKLY AMOUNT IF CHANGING BILLING SCHEDULE CODE.
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C     Y             OREQ      1
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C                   Z-ADD     SVAMT2        XXAMT1
     C                   Z-ADD     S1AMT2        X1AMT1
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     SV1AA2        X1AA1
     C                   Z-ADD     SV2AA2        X2AA1
     C                   Z-ADD     SV3AA2        X3AA1
     C                   Z-ADD     SV4AA2        X4AA1
     C                   Z-ADD     SV5AA2        X5AA1
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     SV6AA2        X6AA1
     C                   Z-ADD     SV7AA2        X7AA1
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C                   ENDIF
     C*** END ADD *****  02/28/06  ************************************
     C                   SUB       XXAMT1        PA(Y)
     C                   SUB       X1AMT1        P1(Y)
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   SUB       X1AA1         A1(Y)
     C                   SUB       X2AA1         A2(Y)
     C                   SUB       X3AA1         A3(Y)
     C                   SUB       X4AA1         A4(Y)
     C                   SUB       X5AA1         A5(Y)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   SUB       X6AA1         A6(Y)
     C                   SUB       X7AA1         A7(Y)
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C                   ENDIF                                                  ELBSCD IFEQ
     C*
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C                   ELSE
     C                   ADD       1             Z
     C                   ENDIF                                                  Y IFLT 2
     C*
     C                   ENDIF                                                  WKD,U IFLT
     C*
     C                   ENDIF                                                  WKA,U IFNE
     C*
     C                   ENDDO                                                  1 DO 53
     C*
     C                   MOVE      *ON           *IN62
     C*** BEGIN ADD ***  01/06/04  ************************************
     C     PA(1)         IFEQ      *ZERO
     C     P1(1)         ANDEQ     *ZERO
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     A1(1)         ANDEQ     *ZERO
     C     A2(1)         ANDEQ     *ZERO
     C     A3(1)         ANDEQ     *ZERO
     C     A4(1)         ANDEQ     *ZERO
     C     A5(1)         ANDEQ     *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(1)         ANDEQ     *ZERO
     C     A7(1)         ANDEQ     *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C     PA(2)         IFNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     A1(2)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(2)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C                   Z-ADD     PA(2)         PA(1)
     C                   Z-ADD     P1(2)         P1(1)
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   Z-ADD     A1(2)         A1(1)
     C                   Z-ADD     A2(2)         A2(1)
     C                   Z-ADD     A3(2)         A3(1)
     C                   Z-ADD     A4(2)         A4(1)
     C                   Z-ADD     A5(2)         A5(1)
     C                   Z-ADD     A6(2)         A6(1)
     C                   Z-ADD     A7(2)         A7(1)
      ***** END ADD ******* 10/14/10 **********************************
     C                   Z-ADD     DA(2)         DA(1)
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     0             A1(2)
     C                   Z-ADD     0             A2(2)
     C                   Z-ADD     0             A3(2)
     C                   Z-ADD     0             A4(2)
     C                   Z-ADD     0             A5(2)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     0             A6(2)
     C                   Z-ADD     0             A7(2)
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C                   SUB       1             Z
     C*
     C     Z             IFGT      0
     C                   Z-ADD     1             U
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2DATE            7 0
     C     X2DATE        LOOKUP    WKD(U)                             60
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(2)
     C                   ELSE
     C                   Z-ADD     0             PA(2)
     C                   Z-ADD     0             P1(2)
     C                   Z-ADD     0             DA(2)
     C                   ENDIF                                                  Z IFGT
     C*
     C                   ENDIF                                                  PA,2 IFNE
     C*
     C                   ENDIF                                                  PA,1 IFEQ
     C*** END ADD *****  01/06/04  ************************************
     C*
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         ORNE      *ZERO
     C     P1(1)         ORNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     A1(1)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*********  A1,3      ORNE *ZERO
     C*********  A1,4      ORNE *ZERO
     C*********  A1,5      ORNE *ZERO
     C*********  A2,1      ORNE *ZERO
     C*********  A2,2      ORNE *ZERO
     C*********  A2,3      ORNE *ZERO
     C*********  A2,4      ORNE *ZERO
     C*********  A2,5      ORNE *ZERO
     C     A2(1)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(1)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(1)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(1)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(1)         ORNE      *ZERO
     C     A6(2)         ORNE      *ZERO
     C     A7(1)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C*** END CHG *****  12/11/08  ************************************
     C*
     C*** END   ADD ***  05/15/07  ************************************
     C                   Z-ADD     2             X
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  04/25/03  ************************************
     C                   ENDIF                                                  BSFREQ IFEQ
     C*** END ADD *****  04/25/03  ************************************
     C                   ENDIF                                                  BSDOWK IFEQ
     C*** END ADD *****  04/21/03  ************************************
     C*
     C*  TEST TO SEE IF ANY ADJUSTMENTS ARE BEING POSTED
     C*
     C                   XFOOT     PA            SCTOT
     C*
     C*  ADD THE STATE TAX ADJUSTMENTS TO TOTAL ADJUSTMENT
     C*
     C                   XFOOT     P1            XXTEST
     C                   ADD       XXTEST        SCTOT
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   XFOOT     A1            SCTOT2
     C                   XFOOT     A2            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A3            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A4            SCTOT3
     C                   XFOOT     A5            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   XFOOT     A6            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A7            SCTOT4
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  04/21/03  ************************************
     C     *IN62         IFEQ      *ON
     C*
     C                   Z-ADD     Z             SCINST
     C     PA(2)         MULT      Z             SCTOT
     C                   ADD       PA(1)         SCTOT
     C     P1(2)         MULT      Z             XXTEST
     C                   ADD       P1(1)         XXTEST
     C                   ADD       XXTEST        SCTOT
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     A1(2)         MULT      Z             SCTOT2
     C                   ADD       A1(1)         SCTOT2
     C     A2(2)         MULT      Z             XXTEST
     C                   ADD       A2(1)         XXTEST
     C                   ADD       XXTEST        SCTOT2
     C     A3(2)         MULT      Z             XXTEST
     C                   ADD       A3(1)         XXTEST
     C                   ADD       XXTEST        SCTOT2
      *** BEG CHG ***** 09/17/07 **************************************
     C********** A4,2      MULT Z         XXTEST
     C**********           ADD  A4,1      XXTEST
     C**********           ADD  XXTEST    SCTOT3
     C     A4(2)         MULT      Z             SCTOT3
     C                   ADD       A4(1)         SCTOT3
      *** END CHG ***** 09/17/07 **************************************
     C     A5(2)         MULT      Z             XXTEST
     C                   ADD       A5(1)         XXTEST
     C                   ADD       XXTEST        SCTOT3
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(2)         MULT      Z             XXTEST
     C                   ADD       A6(1)         XXTEST
     C                   ADD       XXTEST        SCTOT3
     C     A7(2)         MULT      Z             XXTEST
     C                   ADD       A7(1)         XXTEST
     C                   ADD       XXTEST        SCTOT4
     C*** END ADD *****  12/11/08  ************************************
     C*
     C     Z             IFEQ      0
     C                   MOVE      *ON           *IN63
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN62 IFEQ
     C*** END ADD *****  04/21/03  ************************************
     C                   MOVE      '0'           *IN80
     C*** BEGIN ADD ***  01/08/02  ************************************
     C*** BEGIN CHG ***  10/06/03  ************************************
     C*********  X         IFEQ 1
     C*********  PA,1      ANDEQ*ZERO
     C*********  DA,1      ANDNE*ZERO
     C*********            Z-ADD0         DA,1
     C*********            Z-ADD0         X
     C*********            ENDIF
     C                   Z-ADD     12            X
     C     X             DOWGT     0
     C     PA(X)         IFNE      *ZERO
     C     P1(X)         ORNE      *ZERO
     C*** BEG ADD *****  05/15/07  ************************************
     C     A1(X)         ORNE      *ZERO
     C     A2(X)         ORNE      *ZERO
     C     A3(X)         ORNE      *ZERO
     C     A4(X)         ORNE      *ZERO
     C     A5(X)         ORNE      *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(X)         ORNE      *ZERO
     C     A7(X)         ORNE      *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  05/15/07  ************************************
     C                   LEAVE
     C                   ELSE
     C                   Z-ADD     0             DA(X)
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
     C*** END CHG *****  10/06/03  ************************************
     C     X             IFEQ      *ZEROS
     C     F1UPAR        OREQ      'N'
     C*
     C*  DUE TO CHANGE MADE ON 01/19/92, OVERRIDE RECORDS WILL ALWAYS EXIST
     C*  AT THIS POINT IN WAELP AND WAEPP WHETHER OR NOT ANY REVISIONS WERE
     C*  MADE THEREFORE WE NEED TO SUBMIT A BILLING REVISION TO PROCESS THESE
     C*  RECORDS AND MOVE THEM TO LIVE.  IN ADDITION, IF NO ADJUSTMENT RECORDS
     C*  EXIST AND THERE WAS A BILLING SCHEDULE CHANGE WE STILL NEED TO SUBMIT
     C*  THE BILING REVISION TO DELETE ALL FUTURE BILLINGS AND ADJUSTMENTS
     C*
     C                   MOVE      '1'           *IN80
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD   ***  05/15/07  ************************************
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANK        SCDEP
     C                   MOVE      *BLANKS       SCAR
     C*
     C*** BEGIN CHG ***  02/26/98  ************************************
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTMDY8
     C*****                MOVELSRDTO     SCEFDT
     C                   Z-ADD     0             SCEFDT
     C*** END CHG *****  02/26/98  ************************************
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     XXDIVC        IFEQ      1
     C*** END ADD *****  11/15/99  ************************************
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C                   ELSE
     C*
     C     XXDIVC        IFGT      1
     C     SCTOT         IFNE      *ZERO
     C     X             ORGT      1
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DA(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA(3)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA(4)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA(5)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA(7)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA(8)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA(9)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA(10)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA(11)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA(12)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
      *** BEG ADD ***** 01/31/11 **************************
     C                   MOVEA     J1D           J2D
      *** END ADD ***** 01/31/11 **************************
     C*
     C                   Z-ADD     PA(1)         EAAM01
     C                   Z-ADD     PA(2)         EAAM02
     C                   Z-ADD     PA(3)         EAAM03
     C                   Z-ADD     PA(4)         EAAM04
     C                   Z-ADD     PA(5)         EAAM05
     C                   Z-ADD     PA(6)         EAAM06
     C                   Z-ADD     PA(7)         EAAM07
     C                   Z-ADD     PA(8)         EAAM08
     C                   Z-ADD     PA(9)         EAAM09
     C                   Z-ADD     PA(10)        EAAM10
     C                   Z-ADD     PA(11)        EAAM11
     C                   Z-ADD     PA(12)        EAAM12
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*** BEGIN ADD ***  04/21/03  ************************************
     C     BSDOWK        IFNE      *BLANKS
     C*** BEGIN ADD ***  04/25/03  ************************************
     C     BSFREQ        OREQ      'T'
     C*** END ADD *****  04/25/03  ************************************
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*** END ADD *****  04/21/03  ************************************
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C*** BEGIN ADD ***  04/25/03  ************************************
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*** END ADD *****  04/25/03  ************************************
     C*
     C                   MOVE      ELCO#         EACO#
     C                   MOVE      ELFND         EAFND
     C                   Z-ADD     ELFYR         EAFYR
     C                   MOVE      ELEMP#        EAEMP#
     C                   MOVE      ELDIV         EADIV
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'C'           EASTAT
     C                   ELSE
     C                   MOVE      'B'           EASTAT
     C                   ENDIF
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'U'           EASTAT
     C                   ELSE
     C                   MOVE      'T'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     P1(1)         EAAM01
     C                   Z-ADD     P1(2)         EAAM02
     C                   Z-ADD     P1(3)         EAAM03
     C                   Z-ADD     P1(4)         EAAM04
     C                   Z-ADD     P1(5)         EAAM05
     C                   Z-ADD     P1(6)         EAAM06
     C                   Z-ADD     P1(7)         EAAM07
     C                   Z-ADD     P1(8)         EAAM08
     C                   Z-ADD     P1(9)         EAAM09
     C                   Z-ADD     P1(10)        EAAM10
     C                   Z-ADD     P1(11)        EAAM11
     C                   Z-ADD     P1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*** BEGIN ADD ***  04/21/03  ************************************
     C     BSDOWK        IFNE      *BLANKS
     C*** BEGIN ADD ***  04/25/03  ************************************
     C     BSFREQ        OREQ      'T'
     C*** END ADD *****  04/25/03  ************************************
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*** END ADD *****  04/21/03  ************************************
     C*** BEGIN ADD ***  04/25/03  ************************************
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*** END ADD *****  04/25/03  ************************************
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** BEGIN ADD ***  05/15/07  ************************************
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '6'       EASTAT
     C                   MOVE      'D'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '1'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A1(1)         EAAM01
     C                   Z-ADD     A1(2)         EAAM02
     C                   Z-ADD     A1(3)         EAAM03
     C                   Z-ADD     A1(4)         EAAM04
     C                   Z-ADD     A1(5)         EAAM05
     C                   Z-ADD     A1(6)         EAAM06
     C                   Z-ADD     A1(7)         EAAM07
     C                   Z-ADD     A1(8)         EAAM08
     C                   Z-ADD     A1(9)         EAAM09
     C                   Z-ADD     A1(10)        EAAM10
     C                   Z-ADD     A1(11)        EAAM11
     C                   Z-ADD     A1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C*
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '7'       EASTAT
     C                   MOVE      'E'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '2'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A2(1)         EAAM01
     C                   Z-ADD     A2(2)         EAAM02
     C                   Z-ADD     A2(3)         EAAM03
     C                   Z-ADD     A2(4)         EAAM04
     C                   Z-ADD     A2(5)         EAAM05
     C                   Z-ADD     A2(6)         EAAM06
     C                   Z-ADD     A2(7)         EAAM07
     C                   Z-ADD     A2(8)         EAAM08
     C                   Z-ADD     A2(9)         EAAM09
     C                   Z-ADD     A2(10)        EAAM10
     C                   Z-ADD     A2(11)        EAAM11
     C                   Z-ADD     A2(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C*
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '8'       EASTAT
     C                   MOVE      'F'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '3'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A3(1)         EAAM01
     C                   Z-ADD     A3(2)         EAAM02
     C                   Z-ADD     A3(3)         EAAM03
     C                   Z-ADD     A3(4)         EAAM04
     C                   Z-ADD     A3(5)         EAAM05
     C                   Z-ADD     A3(6)         EAAM06
     C                   Z-ADD     A3(7)         EAAM07
     C                   Z-ADD     A3(8)         EAAM08
     C                   Z-ADD     A3(9)         EAAM09
     C                   Z-ADD     A3(10)        EAAM10
     C                   Z-ADD     A3(11)        EAAM11
     C                   Z-ADD     A3(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C*
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '9'       EASTAT
     C                   MOVE      'G'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C                   ELSE
     C                   MOVE      '4'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A4(1)         EAAM01
     C                   Z-ADD     A4(2)         EAAM02
     C                   Z-ADD     A4(3)         EAAM03
     C                   Z-ADD     A4(4)         EAAM04
     C                   Z-ADD     A4(5)         EAAM05
     C                   Z-ADD     A4(6)         EAAM06
     C                   Z-ADD     A4(7)         EAAM07
     C                   Z-ADD     A4(8)         EAAM08
     C                   Z-ADD     A4(9)         EAAM09
     C                   Z-ADD     A4(10)        EAAM10
     C                   Z-ADD     A4(11)        EAAM11
     C                   Z-ADD     A4(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C*
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
      *** BEG CHG ***** 09/17/07 **********************************
     C*****************    MOVE '10'      EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
     C************         MOVE '0'       EASTAT
     C                   MOVE      'H'           EASTAT
     C*** BEGIN CHG ***  12/11/08  ************************************
      *** END CHG ***** 09/17/07 **********************************
     C                   ELSE
     C                   MOVE      '5'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A5(1)         EAAM01
     C                   Z-ADD     A5(2)         EAAM02
     C                   Z-ADD     A5(3)         EAAM03
     C                   Z-ADD     A5(4)         EAAM04
     C                   Z-ADD     A5(5)         EAAM05
     C                   Z-ADD     A5(6)         EAAM06
     C                   Z-ADD     A5(7)         EAAM07
     C                   Z-ADD     A5(8)         EAAM08
     C                   Z-ADD     A5(9)         EAAM09
     C                   Z-ADD     A5(10)        EAAM10
     C                   Z-ADD     A5(11)        EAAM11
     C                   Z-ADD     A5(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C*
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** END   ADD ***  05/15/07  ************************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'J'           EASTAT
     C                   ELSE
     C                   MOVE      '6'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A6(1)         EAAM01
     C                   Z-ADD     A6(2)         EAAM02
     C                   Z-ADD     A6(3)         EAAM03
     C                   Z-ADD     A6(4)         EAAM04
     C                   Z-ADD     A6(5)         EAAM05
     C                   Z-ADD     A6(6)         EAAM06
     C                   Z-ADD     A6(7)         EAAM07
     C                   Z-ADD     A6(8)         EAAM08
     C                   Z-ADD     A6(9)         EAAM09
     C                   Z-ADD     A6(10)        EAAM10
     C                   Z-ADD     A6(11)        EAAM11
     C                   Z-ADD     A6(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C*
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'K'           EASTAT
     C                   ELSE
     C                   MOVE      '7'           EASTAT
     C                   ENDIF
     C*
     C                   Z-ADD     A7(1)         EAAM01
     C                   Z-ADD     A7(2)         EAAM02
     C                   Z-ADD     A7(3)         EAAM03
     C                   Z-ADD     A7(4)         EAAM04
     C                   Z-ADD     A7(5)         EAAM05
     C                   Z-ADD     A7(6)         EAAM06
     C                   Z-ADD     A7(7)         EAAM07
     C                   Z-ADD     A7(8)         EAAM08
     C                   Z-ADD     A7(9)         EAAM09
     C                   Z-ADD     A7(10)        EAAM10
     C                   Z-ADD     A7(11)        EAAM11
     C                   Z-ADD     A7(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C*
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** END ADD *****  12/11/08  ************************************
     C*
     C                   ENDIF                                                  SCTOT IFNE
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*** END ADD *****  11/15/99  ************************************
     C                   END                                                    IF X = 0 OR
     C*                                                    F1UPAR = 'N'
     C*** BEGIN ADD ***  11/11/03  ************************************
     C                   ENDIF                                                  ELARSM IFEQ
     C*** END ADD *****  11/11/03  ************************************
     C*
     C                   END                                                    BSSEFL = 'E'
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*** BEGIN DEL ***  11/11/03  ************************************
     C*********            ENDIF                           ELARSM IFEQ
     C*** END DEL *****  11/11/03  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  XXCNCL IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      *BLANK        XXWAR
     C                   MOVE      *BLANK        XXSUM
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C*** BEGIN CHG ***  02/28/06  ************************************
     C*********  EMKY1     READEPEM02                    60
     C  N67EMKY1         READE     PEM02                                  60
     C   67              MOVE      *ON           *IN60
     C*** END CHG *****  02/28/06  ************************************
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '0'           *IN80
     C*
     C     XXDIVC        IFGT      1
     C*
     C     EAKY4         CHAIN(N)  PEA04                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      '1'           *IN80
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C*** END ADD *****  12/11/08  ************************************
     C*** END  ADD ****  05/15/07  ************************************
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANK        SCDEP
     C                   MOVE      *BLANKS       SCAR
     C                   Z-ADD     0             SCEFDT
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   MOVE      ' '           Q1CALL            1
     C                   MOVE      XXCO#         Q1CO#             3
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      SCYR          Q1FYR             3
     C                   MOVE      SCEMP#        Q1EMP#            9
     C                   MOVE      XSDIV         Q1DIV             5
     C                   MOVE      *BLANK        Q1KC
     C                   MOVE      *BLANK        Q1KL
      *** BEG ADD ***** 01/03/05 **************************************
      *
      *   PROMPT TO LUMP SUM ADJUSTMENTS OR NOT
      *
     C     XXLSFL        IFEQ      'Y'
     C                   EXSR      SRCADJ
      *
     C     SCPRAJ        IFNE      *ZERO
     C                   WRITE     BLANK
      *
     C                   MOVE      *ON           *IN90
     C     *IN90         DOWEQ     *ON
     C                   EXFMT     WDLUMP
      *
     C     SCLUMP        IFNE      'Y'
     C     SCLUMP        ANDNE     *BLANK
     C                   MOVE      *ON           *IN88
     C                   ELSE
     C                   MOVE      *OFF          *IN90
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF
      *
     C     SCLUMP        IFEQ      'Y'
     C                   EXSR      SRLUMP
     C                   ENDIF
      *
     C                   ENDIF
      *** END ADD ***** 01/03/05 **************************************
     C*
     C                   CALL      'W0091R'
     C                   PARM                    Q1CALL            1
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1FYR             3
     C     *INKC         PARM                    Q1KC              1
     C     *INKL         PARM                    Q1KL              1
     C*
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *INKC IFEQ
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ELSE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANK        SCDEP
     C                   MOVE      *BLANKS       SCAR
     C                   Z-ADD     0             SCEFDT
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C*** END ADD *****  11/15/99  ************************************
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C*** BEGIN ADD ***  05/15/07  ************************************
     C                   Z-ADD     12            S                 2 0
     C     S             DOWGT     0
     C     A1(S)         IFNE      *ZERO
     C     A2(S)         ORNE      *ZERO
     C     A3(S)         ORNE      *ZERO
     C                   MOVE      'Y'           XX3A
     C                   ENDIF
     C     A4(S)         IFNE      *ZERO
     C     A5(S)         ORNE      *ZERO
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(S)         ORNE      *ZERO
     C*** END ADD *****  12/11/08  ************************************
     C                   MOVE      'Y'           XX3B
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A7(S)         IFNE      *ZERO
     C                   MOVE      'Y'           XX3C
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
      *** BEG CHG ***** 09/17/07 **************************************
     C********** 1         SUB  S         S
     C                   SUB       1             S
      *** END CHG ***** 09/17/07 **************************************
     C                   ENDDO
     C*** END ADD *****  05/15/07  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   ENDIF                                                  XXDIVC IFGT
     C*** END ADD *****  11/15/99  ************************************
     C                   GOTO      STRT
     C                   END                                                    IF SCR02
     C                   END                                                    IF *INKG
     C*
     C*  F9=PAYROLLS
     C*
     C     *INKI         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   EXSR      LDOVRF
     C*
     C                   CALL      'W0060R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C***  BEGIN CHG  ***  01/29/99  *********************************
     C**********           PARM ' '       ##CALL  1
     C                   PARM      'R'           ##CALL            1
     C***  END   CHG  ***  01/29/99  *********************************
     C                   PARM                    ##RRN             9 0
     C                   PARM      DBRRN4        ##RRN2            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C*  WRITE TRANSACTION FILE RECORD IF NOT EXISTING
     C*
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C*  CALL PROGRAM TO COMPUTE NEW BILLING TOTALS
     C*
     C                   EXSR      BILL
     C*** BEGIN CHG ***  01/20/04  ************************************
     C                   MOVE      'Y'           XXPYCG
     C*** END CHG *****  01/20/04  ************************************
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C*  DELETE OVERRIDES
     C*
     C                   EXSR      DLTOVR
     C*
     C*  RELOAD BILLING DETAIL SCREEN
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C                   ENDIF                                                  *INKI=*ON
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C     *INKS         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   EXSR      LDOVRF
     C*
     C                   CALL      'W1191R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      ' '           ##CALL            1
     C                   PARM      DBRRN4        ##RRN             9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C*  WRITE TRANSACTION FILE RECORD IF NOT EXISTING
     C*
     C     *INKC         IFEQ      *OFF
     C     *INKL         ANDEQ     *OFF
     C*
     C*  CALL PROGRAM TO COMPUTE NEW BILLING TOTALS
     C                   EXSR      BILL
     C*
     C                   ENDIF                                                  *INKL=*OFF
     C*
     C*  DELETE OVERRIDES
     C*
     C                   EXSR      DLTOVR
     C*
     C*  RELOAD BILLING DETAIL SCREEN
     C*
     C                   GOTO      SCR01T
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C                   ENDIF                                                  *INKS=*ON
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C*  F10=SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C****** BEGIN CHG ***** 07/24/98 *********************************
     C**********           SELEC
     C********** HFLD      WHEQ 'SCCOM1'
     C**********           MOVEL'?'       SCCOM1    P
     C********** HFLD      WHEQ 'SCCOM2'
     C**********           MOVEL'?'       SCCOM2    P
     C********** HFLD      WHEQ 'SCCOM3'
     C**********           MOVEL'?'       SCCOM3    P
     C********** HFLD      WHEQ 'SCAR'
     C**********           MOVEL'?'       SCAR      P
     C********** HFLD      WHEQ 'SCRMCD'
     C**********           MOVEL'?'       SCRMCD    P
     C**********           OTHER
     C**********           GOTO ENDF10
     C**********           ENDSL
     C*
     C*  F10=TABLE FILE SEARCH - COMMENT CODE 1
     C*
     C**********           MOVELSCCOM1    XX2     2
     C********** XX2       IFEQ '? '
     C********** HFLD      OREQ 'SCCOM1'
     C**********
     C**********           MOVE *BLANK    SCKEY  10
     C**********           CALL 'W2191R'
     C**********           PARM '2'       ##CALL  1
     C********** *INKC     PARM           ##KC    1
     C********** *INKL     PARM           ##KL    1
     C**********           PARM           SCKEY
     C**********
     C**********           MOVELSCKEY     SCCOM1
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C********** *INKL     COMP *ON                      40
     C**********
     C********** N40                SETON                         41
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR04'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C*
     C*  F10=TABLE FILE SEARCH - COMMENT CODE 2
     C*
     C**********           MOVELSCCOM2    XX2     2
     C********** XX2       IFEQ '? '
     C********** HFLD      OREQ 'SCCOM2'
     C**********
     C**********           MOVE *BLANK    SCKEY  10
     C**********           CALL 'W2191R'
     C**********           PARM '2'       ##CALL  1
     C********** *INKC     PARM           ##KC    1
     C********** *INKL     PARM           ##KL    1
     C**********           PARM           SCKEY
     C**********
     C**********           MOVELSCKEY     SCCOM2
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C********** *INKL     COMP *ON                      41
     C**********
     C********** N41                SETON                         42
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR04'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C**********
     C*  COMMAND 10 = TABLE FILE SEARCH - COMMENT CODE 3
     C*
     C**********           MOVELSCCOM3    XX2     2
     C********** XX2       IFEQ '? '
     C********** HFLD      OREQ 'SCCOM3'
     C**********
     C**********           MOVE *BLANK    SCKEY  10
     C**********           CALL 'W2191R'
     C**********           PARM '2'       ##CALL  1
     C********** *INKC     PARM           ##KC    1
     C********** *INKL     PARM           ##KL    1
     C**********           PARM           SCKEY
     C**********
     C**********           MOVELSCKEY     SCCOM3
     C**********
     C********** *INKC     CABEQ*ON       TERM
     C********** *INKL     COMP *ON                      42
     C**********
     C********** N42               SETON                         43
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR04'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C*
     C     HFLD          IFEQ      'SCCOM1'
     C     HFLD          OREQ      'SCCOM2'
     C     HFLD          OREQ      'SCCOM3'
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2191R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    SCKEY
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCCOM1'
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   SETON                                        40
     C                   ELSE
     C                   SETON                                        41
     C                   MOVEL     SCKEY         SCCOM1
     C                   ENDIF                                                  SCKEY
     C*
     C     HFLD          WHENEQ    'SCCOM2'
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   SETON                                        41
     C                   ELSE
     C                   MOVEL     SCKEY         SCCOM2
     C                   SETON                                        42
     C                   ENDIF                                                  SCKEY
     C*
     C     HFLD          WHENEQ    'SCCOM3'
     C*
     C     *INKL         IFEQ      *ON
     C     SCKEY         OREQ      *BLANKS
     C                   SETON                                        42
     C                   ELSE
     C                   MOVEL     SCKEY         SCCOM3
      ***** BEG CHG ******* 05/19/00 **********************************
     C*************        SETON                     43
     C                   SETON                                        52
      ***** END CHG ******* 05/19/00 **********************************
     C                   ENDIF                                                  SCKEY
     C*
     C                   ENDSL
     C*
     C                   GOTO      ENDF10
     C                   ENDIF                                                  HFLD
     C****** END   CHG ***** 07/24/98 *********************************
     C*
     C*  COMMAND 10 = TABLE FILE SEARCH - A/R CODE
     C*
     C****** BEGIN CHG ***** 07/24/98 *********************************
     C**********
     C**********           MOVELSCAR      XX2     2
     C********** XX2       IFEQ '? '
     C********** HFLD      OREQ 'SCAR'
     C     HFLD          IFEQ      'SCAR'
     C****** END   CHG ***** 07/24/98 *********************************
     C*
     C                   MOVE      *BLANK        SCKEY            10
     C*
     C                   CALL      'W2161R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      *INKC         ##KC              1
     C     *INKL         PARM      *INKL         ##KL              1
     C                   PARM                    SCKEY
     C*
      ***** BEG DEL ******* 05/19/00 **********************************
     C*******BEGIN ADD ***** 07/24/98 *********************************
     C********** SCKEY     IFNE *BLANKS
     C**********           MOVELSCKEY     SCAR
     C**********           ENDIF                           SCKEY
     C*******END   ADD ***** 07/24/98 *********************************
     C**********
     C********** *INKC     CABEQ'A'       TERM
     C********** *INKL     COMP 'B'                      44
     C**********
     C********** *IN44     IFEQ *OFF
     C**********
     C**********           SETON                         40
     C**********
     C**********           ENDIF                           *IN44
      ***** END DEL ******* 05/19/00 **********************************
      ***** BEG ADD ******* 05/19/00 **********************************
      *
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCAR
     C                   SETON                                        51
      *
     C                   ELSE
      *
     C                   SETON                                        44
     C                   ENDIF                                                  SCKEY
      *
      ***** END ADD ******* 05/19/00 **********************************
     C*
     C****** BEGIN CHG ****** 07/24/98 ********************************
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR04'   Q2RECF
     C**********           GOTO STRT
     C                   GOTO      ENDF10
     C****** END   CHG ****** 07/24/98 ********************************
     C                   ENDIF                                                  SCAR
     C*
     C****** BEGIN CHG ***** 07/24/98 *********************************
     C**********           MOVELSCRMCD    XX2     2
     C********** HFLD      IFEQ 'SCRMCD'
     C********** XX2       OREQ '? '
     C     HFLD          IFEQ      'SCRMCD'
     C****** END   CHG ***** 07/24/98 *********************************
     C*
     C***  Begin Chg  ***  10/11/06  **************************************
     C******BEGIN CHG ****** 07/24/98 *********************************
     C**********           MOVE *BLANKS   SCRMCD
     C**********           MOVE *BLANKS   SCKEY
     C******END*  CHG ****** 07/24/98 *********************************
     C**********           CALL 'SC580R'
     C******BEGIN CHG ****** 07/24/98 *********************************
     C**********           PARM           SCRMCD
     C**********           PARM           SCKEY
     C******END*  CHG ****** 07/24/98 *********************************
     C**********           PARM           QQRTC   1
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C     SCKEY         PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANK        ##RTCD            1
     C***  End   Chg  ***  10/11/06  **************************************
     C*
      ***** BEG DEL ******* 05/19/00 **********************************
     C*******BEGIN ADD ***** 07/24/98 *********************************
     C***********SCKEY     IFNE *BLANKS
     C************         MOVELSCKEY     SCRMCD
     C************         ENDIF                           SCKEY
     C*******END** ADD ***** 07/24/98 *********************************
     C************
     C************         MOVE *ON       *IN83
      ***** END DEL ******* 05/19/00 **********************************
      ***** BEG ADD ******* 05/19/00 **********************************
      *
     C     SCKEY         IFNE      *BLANKS
     C                   MOVEL     SCKEY         SCRMCD
     C                   SETON                                        44
      *
     C                   ELSE
      *
     C                   SETON                                        83
     C                   ENDIF                                                  SCKEY
      *
      ***** END ADD ******* 05/19/00 **********************************
     C*
     C******** BEGIN CHG ***** 07/24/98 *******************************
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR04'   Q2RECF
     C**********           GOTO STRT
     C                   GOTO      ENDF10
     C******** END   CHG ***** 07/24/98 *******************************
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C     ENDF10        TAG
     C******* BEGIN ADD ******** 07/24/98 *****************************
     C                   MOVE      *BLANK        Q2RECF
     C******* END   ADD ******** 07/24/98 *****************************
     C                   MOVEL(P)  'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ=*ON
     C*
     C*   PROCESS HELP REQUESTS
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
     C*   COMMAND 13 = CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         XXCO#
     C                   MOVE      *BLANK        SCCONM
     C                   MOVEL     *ZERO         SCFND
     C                   Z-ADD     *ZERO         SCYR
     C                   MOVEL     *BLANK        SCNAME
     C                   Z-ADD     *ZERO         SCEMP#
     C                   Z-ADD     *ZERO         SCDIV
     C*
     C                   ELSE
     C*
     C*  GET THE COMPANY RECORD
     C*
     C                   Z-ADD     UPCO#         XXCO#
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
      *** BEG ADD ***** 02/03/09 **************************************
     C     EX3RDB        ANDNE     'Y'
      *** END ADD ***** 02/03/09 **************************************
     C                   MOVE      EXREV         SCRMCD
     C                   ELSE
     C                   MOVE      *BLANKS       SCRMCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C****** BEGIN ADD ***** 07/24/98 *********************************
     C     SCRMCD        IFEQ      *BLANKS
     C                   MOVE      'N'           SCDART
     C                   ENDIF                                                   CRMCD
     C****** END   ADD ***** 07/24/98 *********************************
     C     FNKY          CHAIN     PFN01                              60
     C     FNKY          CHAIN     PF201                              60
     C   60              CLEAR                   PF201
     C                   Z-ADD     UPFND         XXFND
     C                   Z-ADD     UPFYR         XXFYR
     C                   Z-ADD     UPFND         SCFND
     C                   Z-ADD     UPFYR         SCYR
     C                   MOVEL     FNNAME        SCNAME
     C                   Z-ADD     UPEMP#        SCEMP#
     C                   Z-ADD     UPDIV         SCDIV
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*** BEG ADD ***** 05/15/07 ***********************************************
     C     Q2RECF        CABEQ     'SCR03A'      SCR3AT
     C     Q2RECF        CABEQ     'SCR03B'      SCR3BT
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     Q2RECF        CABEQ     'SCR03C'      SCR3CT
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD ***** 05/15/07 ***********************************************
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C     Q2RECF        CABEQ     'SCR04A'      SCR4AT
     C*** END ADD ***** 02/09/98 ***********************************************
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR TO BE VALID FOR THE CURRENT FUND YEAR
     C*
     C                   Z-ADD     SCYR          XXFYR
     C     F1KY          CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C     RRDV1L        COMP      'S'                                    85
     C*
     C*  VALIDATE THE EMPLOYER NUMBER AND DIVISION
     C*
     C     EMKY          CHAIN     PEM02                              21
     C     *IN21         CABEQ     '1'           STRT
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
     C     XXOPN2        IFEQ      '0'
     C                   OPEN      WDELL
     C                   MOVE      '1'           XXOPN2
     C                   END
     C     ELKY          CHAIN     PEL01                              60
     C                   Z-ADD     ELANPR        XXANPR
     C                   Z-ADD     ELENMP        XXENMP
     C                   Z-ADD     ELSTPR        XXSTPR
     C                   Z-ADD     ELMAPR        XXMAPR
     C                   Z-ADD     ELBSCD        XXBSCD
     C*** BEGIN ADD ***  10/07/03  ************************************
     C                   Z-ADD     ELTXDU        X1TXDU
     C*** END ADD *****  10/07/03  ************************************
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN27
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*  SUBTRACT EXPENSE CONSTANT FROM ANNUAL PREM & SAVE IT
     C*
     C     ELANPR        SUB       ELPRTD        SVANPR           11 2
     C*** BEGIN ADD ***  04/21/06  ************************************
     C*** BEGIN DEL ****** 08/09/10 ************************************
     C**********           SUB  XXRTCR    SVANPR
     C*** END   DEL ****** 08/09/10 ************************************
     C*** END ADD *****  04/21/06  ************************************
     C                   Z-ADD     ELSTPR        SVSTPR           11 2
     C                   Z-ADD     ELMAPR        SVMAPR           11 2
     C                   Z-ADD     ELENMP        SVNMPR           11 2
     C*** BEGIN ADD ***  04/07/05  ************************************
     C*** BEGIN CHG ***  01/08/07  ************************************
     C***********          Z-ADDELANPR    S2ANPR 112
     C     ELANPR        SUB       ELPRTD        S2ANPR           11 2
     C*** END CHG *****  01/08/07  ************************************
     C*** END ADD *****  04/07/05  ************************************
     C                   MOVE      '1'           *IN59
     C     ELKY          CHAIN     PEL02                              60
     C     *IN60         IFEQ      '0'
     C     ELANPR        IFNE      XXANPR
     C     ELENMP        ORNE      XXENMP
     C     ELMAPR        ORNE      XXMAPR
     C     ELSTPR        ORNE      XXSTPR
     C     ELBSCD        ORNE      XXBSCD
     C*** BEGIN ADD ***  10/07/03  ************************************
     C     ELTXDU        ORNE      X1TXDU
     C*** END ADD *****  10/07/03  ************************************
     C                   MOVE      '0'           *IN59
     C                   END
     C                   END
     C*
     C*  DETERMINE IF SPREADING DISCOUNTS
     C*
     C                   MOVE      XXCO#         Q1CO#             3
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      SCYR          Q1FYR             3
     C                   MOVE      SCEMP#        Q1EMP#            9
     C                   MOVE      ' '           Q1SPRD            1
     C*** BEGIN CHG ***  11/15/99  ************************************
     C*********            CALL 'SRDISC'
     C*********            PARM           Q1CO#
     C*********            PARM           Q1FND
     C*********            PARM           Q1FYR
     C*********            PARM           Q1EMP#
     C*********            PARM           Q1SPRD
     C*
     C*  IF NO OVERRIDE EXISTS AND DISCOUNT IS SPREAD, CALL SRDIS2
     C*  TO LOAD OVERRIDE FILES WAELP, WAEPP, WADSP FOR ALL
     C*  DIVISIONS IF DISCOUNT IS SPREAD
     C*
     C*********  Q1SPRD    IFEQ 'Y'
     C*
     C*********  *IN60     IFEQ '1'
     C*
     C*********            MOVE '3'       Q1SBRT  1
     C*********            MOVE 'E'       Q1TYPE  1
     C*********            CALL 'SRDIS2'
     C*********            PARM           Q1CO#
     C*********            PARM           Q1FND
     C*********            PARM           Q1FYR
     C*********            PARM           Q1EMP#
     C*********            PARM           Q1SBRT
     C*********            PARM           Q1TYPE
     C*********            PARM           Q1SPRD
     C*
     C*********            END
     C*
     C     *IN60         IFEQ      '1'
     C*
     C                   MOVE      SCDIV         Q1DIV
     C                   Z-ADD     0             Q1PREM
     C                   MOVE      'E'           Q1TYPE
     C                   MOVE      '3'           Q1CALL
     C                   MOVE      ' '           Q1RNB
     C*
     C                   CALL      'SRMDIV'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1PREM           11 2
     C                   PARM                    Q1TYPE            1
     C                   PARM                    Q1CALL            1
     C                   PARM                    Q1RNB             1
     C*
     C*** END CHG *****  11/15/99  ************************************
     C*
     C*  IF WE ARE SPREADING DISCOUNT, RUN THE BILLING CALC IN THE
     C*  CASE THAT ANOTHER DIVISIONS STD PREMIUM WAS CHANGED IN ORDER
     C*  SHOW THE EFFECT ON THIS DIVISION.
     C*
     C* EXECUTE OVERRIDE STATEMENTS
     C                   EXSR      BGNOVR
     C* CALL THE BILL
     C                   EXSR      BILL
     C* DELETE OVERRIDES
     C                   EXSR      DLTOVR
     C*  CHECK TOTALS AGAIN
     C*
     C     ELKY          CHAIN     PEL02                              60
     C                   MOVE      '1'           *IN59
     C     *IN60         IFEQ      '0'
     C     ELANPR        IFNE      XXANPR
     C     ELENMP        ORNE      XXENMP
     C     ELMAPR        ORNE      XXMAPR
     C     ELSTPR        ORNE      XXSTPR
     C     ELBSCD        ORNE      XXBSCD
     C*** BEGIN ADD ***  10/07/03  ************************************
     C     ELTXDU        ORNE      X1TXDU
     C*** END ADD *****  10/07/03  ************************************
     C                   MOVE      '0'           *IN59
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  THE EMPLOYER/DIVISION NUMBER MUST BE VALID.
     C*
     C     EMKY          CHAIN     PEM02                              21
     C     *IN21         CABEQ     '1'           STRT
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
     C                   Z-ADD     SCYR          XXFYR
     C     F1KY          CHAIN     PF101                              20
     C     RRDV1L        COMP      'S'                                    85
     C*
     C*  CHECK THE INCREASED LIMITS FACTORS TABLE
     C*
     C                   Z-ADD     *ZERO         LMPCT
     C     ELILCD        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     ELILCD        XXKEY
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C*** BEGIN ADD ***  06/16/98  ************************************
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** END ADD *****  06/16/98  ************************************
     C   60              Z-ADD     *ZERO         LMPCT
     C*
     C                   END
     C*
     C*  CALCULATE EARNED PREMIUM TO DATE
     C*
     C     XXOPN2        IFEQ      '1'
     C                   CLOSE     WDELL
     C                   MOVE      '0'           XXOPN2
     C                   END
     C*
     C                   MOVE      '0'           *IN87
     C     ELKY          CHAIN     PEL02                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  OVERRIDE DATABASE FILE WDELL TO WAELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           *IN87
     C                   END
     C                   TIME                    DSDTTM
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     XXFND         SRFND             3
     C                   MOVEL     ELFYR         SRFYR             3
     C                   MOVEL     ELEMP#        SREMP#            9
     C                   MOVEL     ELDIV         SRDIV             5
     C*
     C                   MOVEL     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         QQDTE             8
     C*
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRTD'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         SCPRTD
     C*
     C*  INITIALIZE SCREEN ARRAYS
     C*
     C                   CLEAR                   HDG
     C                   CLEAR                   TAM
     C     *IN87         IFEQ      '1'
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN87
     C                   END
     C*
     C     XXOPN2        IFEQ      '0'
     C                   OPEN      WDELL
     C                   MOVE      '1'           XXOPN2
     C                   END
     C*
     C                   MOVE      ELOUTS        SCOUTS
     C                   MOVE      ELTCLP        SCTCLP
     C                   MOVE      ELT#CL        SCT#CL
     C                   MOVE      ELTCLC        SCTCLC
     C*
     C                   MOVE      '0'           *IN45
     C     ELFAF         IFEQ      'A'
     C                   MOVE      '1'           *IN45
     C                   END
     C*
     C                   MOVE      ' '           XXUP
     C                   MOVE      '1'           XXDOWN
     C*
     C     XXOPN         IFEQ      '1'
     C                   CLOSE     WMEPL
     C                   MOVE      '0'           XXOPN
     C                   END
     C*
     C     *IN86         IFEQ      '0'
     C     EPKY1         CHAIN     PEP02                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  OVERRIDE DATABASE FILE WMEPL TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           *IN86
     C                   END
     C                   END
     C*
     C     XXOPN         IFEQ      '0'
     C                   OPEN      WMEPL
     C                   MOVE      '1'           XXOPN
     C                   END
     C*
     C*  LOAD SUBFILE
     C*
     C     SCR02A        TAG
     C*
     C                   Z-ADD     0             XXEFD1
     C                   Z-ADD     0             XXCLAS
     C*
     C*  INVALID ROLL REQUEST
     C*
     C   18XXUP          IFNE      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C   19XXDOWN        IFNE      *BLANK
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL UP REQUEST
     C*
     C     *IN18         IFEQ      '1'
     C                   MOVE      *BLANK        XXDOWN
     C*
     C                   Z-ADD     KEYF(2)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFD1
     C*
     C                   Z-ADD     KEYM(2)       XXCLAS
     C     EPKY2         CHAIN     PEP01                              60
     C                   EXSR      LOAD
     C                   GOTO      STRT
     C                   END
     C*
     C*  ROLL DOWN REQUEST
     C*
     C     *IN19         IFEQ      '1'
     C                   MOVE      *BLANK        XXUP
     C*
     C                   Z-ADD     KEYF(1)       SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFD1
     C*
     C                   Z-ADD     KEYM(1)       XXCLAS
     C     EPKY2         CHAIN     PEP01                              60
     C                   EXSR      LOADP
     C                   GOTO      STRT
     C                   END
     C*
     C     EPKY1         CHAIN     PEP01                              60
     C                   EXSR      LOAD
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C     XXDIVC        IFGT      1
     C     XX1ST         ANDEQ     *BLANK
     C*** BEGIN ADD ***  02/28/06  ************************************
     C     *IN67         ANDEQ     *OFF
     C*** END ADD *****  02/28/06  ************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   ELSE
     C*** END ADD *****  11/15/99  ************************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   ENDIF
     C*** END ADD *****  11/15/99  ************************************
     C                   GOTO      STRT
     C*
     C*
     C*  BILLING DETAIL SCREEN
     C*
     C     SCR02T        TAG
     C*
     C*  ENTER - RETURN TO SCREEN 1
     C*
     C     *IN86         IFEQ      '1'
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C                   END
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     Q1PRMS        CABGT     1             TERM
     C*
     C                   GOTO      STRT
     C*
     C*  BILLING REVISION SCREEN
     C*
     C     SCR03T        TAG
     C*
     C*  EDIT ENTERED DATES
     C*  DATE AND AMOUNT FIELD ARE MUTUALLY INCLUSIVE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*** BEGIN ADD  ***  08/07/98  ***********************************
     C                   Z-ADD     XXCURD        XXCURR            7 0
     C*** END ADD ******  08/07/98  ***********************************
     C*** BEGIN ADD ***  02/19/98  ************************************
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
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*** END ADD *****  02/19/98  ************************************
     C*
     C     1             DO        12            Y                 3 0
     C     DA(Y)         IFNE      *ZERO
     C*
     C                   Z-ADD     DA(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C     SRERR         IFEQ      '1'
     C     Y             ADD       19            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     DA(Y)         IFNE      0
     C     PA(Y)         ANDEQ     0
     C     P1(Y)         ANDEQ     0
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     A1(Y)         ANDEQ     0
     C     A2(Y)         ANDEQ     0
     C     A3(Y)         ANDEQ     0
     C     A4(Y)         ANDEQ     0
     C     A5(Y)         ANDEQ     0
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     A6(Y)         ANDEQ     0
     C     A7(Y)         ANDEQ     0
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  10/07/08  ************************************
     C     DA(Y)         OREQ      0
     C     PA(Y)         ANDNE     0
     C*** BEGIN ADD ***  06/12/03  ************************************
     C     DA(Y)         OREQ      0
     C*** END ADD *****  06/12/03  ************************************
     C     P1(Y)         ANDNE     0
     C*** BEGIN ADD ***  10/07/08  ************************************
     C     DA(Y)         OREQ      0
     C     A1(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A2(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A3(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A4(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A5(Y)         ANDNE     0
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     DA(Y)         OREQ      0
     C     A6(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A7(Y)         ANDNE     0
     C*** END ADD *****  12/11/08  ************************************
     C*** END ADD *****  10/07/08  ************************************
     C     Y             ADD       37            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADJUSTMENT POSTING DATES MUST BE GREATER THAN OR EQUAL TO THE
     C*  CURRENT SYSTEM DATE (I.E. NO "BACK-POSTING" OF ADJ ALLOWED)
     C*
     C     DA(Y)         IFNE      *ZEROS
     C*
     C                   Z-ADD     DA(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWK60
     C*
     C*** BEGIN CHG  ***  08/07/98  ***********************************
     C*****      XXWK60    IFLT XXCURD
     C     XXWK60        IFLT      XXCURR
     C*** END CHG ******  08/07/98  ***********************************
     C     Y             ADD       67            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C                   END
     C*
     C*  TOTAL MUST MATCH CALCULATED ADJUSTMENT
     C*
     C                   XFOOT     PA            XXTEST           11 2
     C                   XFOOT     P1            X1TEST           11 2
     C                   ADD       X1TEST        XXTEST
     C*
     C     XXTEST        IFNE      SCTOT
     C*** BEGIN ADD ***  04/21/03  ************************************
     C     *IN62         ANDEQ     *OFF
     C*** END ADD *****  04/21/03  ************************************
     C                   MOVE      '1'           *IN32
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     XX3A          IFEQ      'Y'
      *** BEG CHG ***** 09/17/07 **************************************
     C*****************    MOVE 'SCR03A'  Q2RECF    P
     C                   MOVEL(P)  'SCR03A'      Q2RECF
      *** END CHG ***** 09/17/07 **************************************
     C                   GOTO      STRT
     C                   ELSE
     C     XX3B          IFEQ      'Y'
      *** BEG CHG ***** 09/17/07 **************************************
     C*****************    MOVE 'SCR03B'  Q2RECF    P
     C                   MOVEL(P)  'SCR03B'      Q2RECF
      *** END CHG ***** 09/17/07 **************************************
     C                   GOTO      STRT
     C                   ELSE
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     XX3C          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03C'      Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C*** END ADD *****  12/11/08  ************************************
     C                   GOTO      PROMPT
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCR3AT        TAG
     C*
     C                   XFOOT     A1            XXTEST
     C                   XFOOT     A2            X1TEST
     C                   ADD       X1TEST        XXTEST
     C                   XFOOT     A3            X1TEST
     C                   ADD       X1TEST        XXTEST
     C*
     C*** BEGIN ADD ***  11/12/08  ***********************************
     C     DA(13)        IFNE      *ZERO
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DA(13)
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN(33)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     DA(13)        IFNE      0
     C     A1(13)        ANDEQ     0
     C     A2(13)        ANDEQ     0
     C     A3(13)        ANDEQ     0
     C     A4(13)        ANDEQ     0
     C     A5(13)        ANDEQ     0
     C     DA(13)        OREQ      0
     C     A1(13)        ANDNE     0
     C     DA(13)        OREQ      0
     C     A2(13)        ANDNE     0
     C     DA(13)        OREQ      0
     C     A3(13)        ANDNE     0
     C     DA(13)        OREQ      0
     C     A4(13)        ANDNE     0
     C     DA(13)        OREQ      0
     C     A5(13)        ANDNE     0
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     DA(13)        OREQ      0
     C     A6(13)        ANDNE     0
     C     DA(13)        OREQ      0
     C     A7(13)        ANDNE     0
     C*** END ADD *****  12/11/08  ************************************
     C                   MOVE      '1'           *IN(53)
     C                   GOTO      STRT
     C                   END
     C*
     C*  ADJUSTMENT POSTING DATES MUST BE GREATER THAN OR EQUAL TO THE
     C*  CURRENT SYSTEM DATE (I.E. NO "BACK-POSTING" OF ADJ ALLOWED)
     C*
     C     DA(13)        IFNE      *ZEROS
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWK60
     C*
     C     XXWK60        IFLT      XXCURR
     C                   MOVE      '1'           *IN(82)
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *****  11/12/08  ***********************************
     C*
     C     XXTEST        IFNE      SCTOT2
     C     *IN62         ANDEQ     *OFF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** BEG ADD ***** 04/15/10 **********************************
     C*
     C     1             DO        12            Y                 3 0
     C     DA(Y)         IFNE      *ZERO
     C*
     C                   Z-ADD     DA(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C     SRERR         IFEQ      '1'
     C     Y             ADD       19            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     DA(Y)         IFNE      0
     C     PA(Y)         ANDEQ     0
     C     P1(Y)         ANDEQ     0
     C     A1(Y)         ANDEQ     0
     C     A2(Y)         ANDEQ     0
     C     A3(Y)         ANDEQ     0
     C     A4(Y)         ANDEQ     0
     C     A5(Y)         ANDEQ     0
     C     A6(Y)         ANDEQ     0
     C     A7(Y)         ANDEQ     0
     C     DA(Y)         OREQ      0
     C     PA(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     P1(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A1(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A2(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A3(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A4(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A5(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A6(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A7(Y)         ANDNE     0
     C     Y             ADD       37            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** END ADD ***** 04/15/10 **********************************
     C*
     C     XX3B          IFEQ      'Y'
      *** BEG CHG ***** 09/17/07 **********************************
     C*****************    MOVE 'SCR03B'  Q2RECF    P
     C                   MOVEL(P)  'SCR03B'      Q2RECF
      *** END CHG ***** 09/17/07 **********************************
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      PROMPT
     C                   ENDIF
     C*
     C     SCR3BT        TAG
     C*
     C                   XFOOT     A4            XXTEST
     C                   XFOOT     A5            X1TEST
     C                   ADD       X1TEST        XXTEST
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   XFOOT     A6            X1TEST
     C                   ADD       X1TEST        XXTEST
     C*** END ADD *****  12/11/08  ************************************
     C*
     C     XXTEST        IFNE      SCTOT3
     C     *IN62         ANDEQ     *OFF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*** BEG ADD ***** 04/15/10 **********************************
     C*
     C     1             DO        12            Y                 3 0
     C     DA(Y)         IFNE      *ZERO
     C*
     C                   Z-ADD     DA(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C     SRERR         IFEQ      '1'
     C     Y             ADD       19            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     DA(Y)         IFNE      0
     C     PA(Y)         ANDEQ     0
     C     P1(Y)         ANDEQ     0
     C     A1(Y)         ANDEQ     0
     C     A2(Y)         ANDEQ     0
     C     A3(Y)         ANDEQ     0
     C     A4(Y)         ANDEQ     0
     C     A5(Y)         ANDEQ     0
     C     A6(Y)         ANDEQ     0
     C     A7(Y)         ANDEQ     0
     C     DA(Y)         OREQ      0
     C     PA(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     P1(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A1(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A2(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A3(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A4(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A5(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A6(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A7(Y)         ANDNE     0
     C     Y             ADD       37            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** END ADD ***** 04/15/10 **********************************
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C     XX3C          IFEQ      'Y'
     C                   MOVEL(P)  'SCR03C'      Q2RECF
     C                   GOTO      STRT
     C                   ELSE
     C                   GOTO      PROMPT
     C                   ENDIF
     C*
     C     SCR3CT        TAG
     C*
     C                   XFOOT     A7            XXTEST
     C*
     C     XXTEST        IFNE      SCTOT4
     C     *IN62         ANDEQ     *OFF
     C                   MOVE      *ON           *IN32
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** BEG ADD ***** 04/15/10 **********************************
     C*
     C     1             DO        12            Y                 3 0
     C     DA(Y)         IFNE      *ZERO
     C*
     C                   Z-ADD     DA(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C     SRERR         IFEQ      '1'
     C     Y             ADD       19            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C     DA(Y)         IFNE      0
     C     PA(Y)         ANDEQ     0
     C     P1(Y)         ANDEQ     0
     C     A1(Y)         ANDEQ     0
     C     A2(Y)         ANDEQ     0
     C     A3(Y)         ANDEQ     0
     C     A4(Y)         ANDEQ     0
     C     A5(Y)         ANDEQ     0
     C     A6(Y)         ANDEQ     0
     C     A7(Y)         ANDEQ     0
     C     DA(Y)         OREQ      0
     C     PA(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     P1(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A1(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A2(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A3(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A4(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A5(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A6(Y)         ANDNE     0
     C     DA(Y)         OREQ      0
     C     A7(Y)         ANDNE     0
     C     Y             ADD       37            X
     C                   MOVE      '1'           *IN(X)
     C                   GOTO      STRT
     C                   END
     C                   END
     C*** END ADD ***** 04/15/10 **********************************
     C*
     C     PROMPT        TAG
     C*
     C*** END   ADD ***  05/15/07  ************************************
     C*  WRITE BILLING PROMPT SCREEN
     C*
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   MOVE      *BLANKS       SCCOM3
     C                   MOVE      *BLANK        SCDEP
     C                   MOVE      *BLANKS       SCAR
     C*
     C*** BEGIN CHG ***  02/26/98  ************************************
     C*****                Z-ADDDSDATE    SRDTI
     C*****                CALL 'SRCVTDAT'DTMDY8
     C*****                MOVELSRDTO     SCEFDT
     C                   Z-ADD     0             SCEFDT
     C*** END CHG *****  02/26/98  ************************************
     C*
     C*  CHECK IF OFFICE IS INSTALLED.
     C*
     C*** BEGIN DEL  ***  03/16/99  **********************************
     C******               MOVEL'SY'      XXSYK
     C******     SYKY      CHAINSSY01                60
     C******     *IN60     IFEQ *ON
     C******               MOVE *ON       *IN46
     C******               MOVE 'N'       SCDART
     C******               ELSE
     C*  DEFAULT RESOLVE TO DARTS FLAG TO 'Y' INITIALLY
     C******               MOVE *OFF      *IN46
     C****** BEGIN CHG ***** 07/24/98 *********************************
     C*  RESOLVE TO DARTS FLAG SET TO 'N' IF REMARK CODE FIELD = *BLANK
     C******     SCRMCD    IFEQ *BLANKS
     C******               MOVE 'N'       SCDART
     C******               ELSE
     C******               MOVE 'Y'       SCDART
     C******               ENDIF
     C****** END   CHG ***** 07/24/98 *********************************
     C******               ENDIF
     C*** END DEL *******  03/16/99  **********************************
      *** BEG CHG ***** 09/17/07 **************************************
     C*****************    MOVEL'SCR04'   Q2RECF
     C                   MOVEL(P)  'SCR04'       Q2RECF
      *** END CHG ***** 09/17/07 **************************************
     C                   GOTO      STRT
     C*
     C*  ADJUSTMENT SCREEN
     C*
     C     SCR04T        TAG
     C*** BEGIN ADD ***  01/16/01  ************************************
     C*
     C     XXOPN2        IFEQ      '0'
     C                   OPEN      WDELL
     C                   MOVE      '1'           XXOPN2
     C                   END
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     XXOPN2        IFEQ      '1'
     C                   CLOSE     WDELL
     C                   MOVE      '0'           XXOPN2
     C                   END
     C*** BEGIN ADD ***  02/12/01  ************************************
     C     ELKY          CHAIN     PEL02                              60
     C*** END ADD *****  02/12/01  ************************************
     C*
     C*** END ADD *****  01/16/01  ************************************
     C*** BEGIN ADD ***  06/23/04  ************************************
     C*
     C* EDIT EFFECTIVE DATE IF THERE ARE NO A/R ADJUSTMENTS TO ENSURE
     C* PROPER DATE IS PASSED ALONG.
     C*
     C     *IN80         IFEQ      '1'
     C*
     C*   THE ENDORSEMENT EFFECTIVE DATE MUST BE VALID
     C*
     C     SCEFDT        IFEQ      *ZERO                                                         4
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  ENDORSEMENT EFF DATE MUST BE WITHIN THE POLICY PERIOD
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFD2            7 0
     C*
     C     XXEFD2        IFLT      ELPFDT
      *** BEG CHG ***** 02/15/06 **********************************
     C********** XXEFD2    ORGT ELPTDT
     C     XXEFD2        ORGE      ELPTDT
      *** END CHG ***** 02/15/06 **********************************
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCEFDT IFEQ
     C*
     C                   ENDIF                                                  *IN80 IFEQ
     C*** END ADD *****  06/23/04  ************************************
     C*
     C*  EDIT COMMENT CODES IF ENTERED
     C*
     C*  CLEAR XXKEY FOR CHAIN OPERATION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C*
     C     SCCOM1        IFNE      *BLANKS
     C                   MOVEL     SCCOM1        XXKEY
     C     TBKY          CHAIN     PBC01                              20
     C  N20BCSTAT        COMP      'I'                                    20
     C     *IN20         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM2        IFNE      *BLANKS
     C                   MOVEL     SCCOM2        XXKEY
     C     TBKY          CHAIN     PBC01                              21
     C  N21BCSTAT        COMP      'I'                                    21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C     SCCOM3        IFNE      *BLANKS
     C                   MOVEL     SCCOM3        XXKEY
     C     TBKY          CHAIN     PBC01                              22
     C  N22BCSTAT        COMP      'I'                                    22
     C     *IN22         CABEQ     '1'           STRT
     C                   END
     C*** BEGIN ADD ***  02/15/05  ************************************
     C     SCCOM1        IFNE      *BLANKS
     C     SCCOM2        ORNE      *BLANKS
     C     SCCOM3        ORNE      *BLANKS
     C*
     C     SCFCM1        IFNE      *BLANKS
     C     SCFCM2        ORNE      *BLANKS
     C     SCFCM3        ORNE      *BLANKS
     C                   MOVE      *ON           *IN46
     C                   GOTO      STRT
     C                   ENDIF                                                  SCFCM1 IFNE
     C*
     C                   ENDIF                                                  SCCOM1 IFNE
     C*** END ADD *****  02/15/05  ************************************
     C*
     C*  VALIDATE RECALCULATE DEPOSIT FLAG
     C*
     C     SCDEP         IFNE      'Y'
     C     SCDEP         ANDNE     'N'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE THE RESOLVE TO DARTS FLAG.
     C*
     C     SCDART        IFNE      'Y'
     C     SCDART        ANDNE     'N'
     C                   MOVE      *ON           *IN45
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     SCDART        IFEQ      'Y'
     C     SCRMCD        CHAIN     PCT01                              84
     C*
     C     *IN84         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     CTSTAT        IFEQ      'I'
     C                   MOVE      *ON           *IN85
     C                   GOTO      STRT
     C                   ENDIF                                                  CTSTAT IFEQ
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SCRMCD
     C*
     C                   ENDIF                                                  SCDART IFEQ
     C*
     C*  *IN80 WILL BE SET IF EITHER THE FUND DETAIL FLAG IS 'N' OR IF
     C*  NO ADJUSTMENTS ARE BEING POSTED.  IN BOTH OF THESE CASES THERE
     C*  IS NO NEED TO VALIDATE THE ADJUSTMENT A/R CODE ON THIS SCREEN
     C*  NOR DO WE NEED TO WRITE A RECORD TO THE A/R ADJUSTMENT FILE
     C*  (WAEAP) SINCE NO ADJUSTMENTS WILL BE WRITTEN.
     C*
     C     *IN80         IFEQ      '0'
     C*
     C*  GET BILLING SCHEDULE FOR STYLE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C   60              MOVE      *BLANKS       BSSEFL
     C*
     C*  VALIDATE THE PAYMENT DUE DATE FOR SELF REPORTING TYPES
     C*
     C     BSSEFL        IFEQ      'S'
     C*** BEGIN ADD ***  03/03/98  ************************************
     C     SCDEP         ANDEQ     'Y'
     C*** END ADD *****  03/03/98  ************************************
     C     SCDATE        COMP      0                                      24
     C   24              GOTO      STRT
     C*
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   THE ENDORSEMENT EFFECTIVE DATE MUST BE VALID
     C*
     C     SCEFDT        IFEQ      *ZERO                                                         4
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C*
     C                   ELSE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN71
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*** BEGIN ADD ***  10/12/98  ************************************
     C*
     C*  ENDORSEMENT EFF DATE MUST BE WITHIN THE POLICY PERIOD
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFD2            7 0
     C*
     C     XXEFD2        IFLT      ELPFDT
      *** BEG CHG ***** 02/15/06 **********************************
     C********** XXEFD2    ORGT ELPTDT
     C     XXEFD2        ORGE      ELPTDT
      *** END CHG ***** 02/15/06 **********************************
     C                   MOVE      *ON           *IN34
     C                   GOTO      STRT
     C                   ENDIF
     C*** END ADD *****  10/12/98  ************************************
     C                   ENDIF
     C*
     C*  VALIDATE A/R TRANSACTION CODE - REQUIRED
     C*  MUST BE ADJUSTMENT TYPE FOR ESTIMATING TYPES OF EMPLOYERS
     C*
     C     BSSEFL        IFEQ      'E'
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCAR          XXKEY
     C     TBKY          CHAIN     PTT01                              28
     C  N28TTSTAT        COMP      'I'                                    28
     C     *IN28         CABEQ     '1'           STRT
     C     TTTYPE        COMP      'A'                                3030
     C     *IN30         CABEQ     '1'           STRT
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCAR          XXKEY
     C     TBKY          CHAIN     PTT01                              28
     C  N28TTSTAT        COMP      'I'                                    28
     C     *IN28         CABEQ     '1'           STRT
     C     TTTYPE        COMP      'H'                                3030
     C     *IN30         CABEQ     '1'           STRT
     C                   END
     C*** BEGIN ADD ***  07/09/08  *************************************
     C                   ENDIF                                                  *IN80 IFEQ
     C*** END   ADD ***  07/09/08  *************************************
     C*** BEGIN DEL ***  07/07/06  *************************************
     C*
     C*  WRITE TRANSACTION RECORD AND SUBMIT BATCH UPDATE
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*********  XXDIVC    IFEQ 1
     C*** END ADD *****  11/15/99  ************************************
     C*********            Z-ADDDA,1      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO01
     C*
     C*********            Z-ADDDA,2      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO02
     C*
     C*********            Z-ADDDA,3      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO03
     C*
     C*********            Z-ADDDA,4      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO04
     C*
     C*********            Z-ADDDA,5      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO05
     C*
     C*********            Z-ADDDA,6      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO06
     C*
     C*********            Z-ADDDA,7      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO07
     C*
     C*********            Z-ADDDA,8      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO08
     C*
     C*********            Z-ADDDA,9      SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO09
     C*
     C*********            Z-ADDDA,10     SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO10
     C*
     C*********            Z-ADDDA,11     SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO11
     C*
     C*********            Z-ADDDA,12     SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EAPO12
     C*
     C*********            Z-ADDPA,1      EAAM01
     C*********            Z-ADDPA,2      EAAM02
     C*********            Z-ADDPA,3      EAAM03
     C*********            Z-ADDPA,4      EAAM04
     C*********            Z-ADDPA,5      EAAM05
     C*********            Z-ADDPA,6      EAAM06
     C*********            Z-ADDPA,7      EAAM07
     C*********            Z-ADDPA,8      EAAM08
     C*********            Z-ADDPA,9      EAAM09
     C*********            Z-ADDPA,10     EAAM10
     C*********            Z-ADDPA,11     EAAM11
     C*********            Z-ADDPA,12     EAAM12
     C*
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EATRDT
     C*
      *** BEG ADD ***** 01/03/05 **************************************
     C*********  SCLUMP    IFNE 'Y'
      *** END ADD ***** 01/03/05 **************************************
     C*
     C*** BEGIN ADD ***  04/21/03  ************************************
     C*********  BSDOWK    IFNE *BLANKS
     C*** BEGIN ADD ***  04/25/03  ************************************
     C*********  BSFREQ    OREQ 'T'
     C*** END ADD *****  04/25/03  ************************************
     C*********            Z-ADDZ         EATRDT
     C*********            ENDIF
     C*** END ADD *****  04/21/03  ************************************
     C*
      *** BEG ADD ***** 01/03/05 **************************************
     C*********            ENDIF
      *** END ADD ***** 01/03/05 **************************************
     C*
     C*********            MOVELSCAR      EAARCD
     C*********            MOVELTTTYPE    EAARTY
     C*********            MOVELTTDESC    EADESC
     C*** BEGIN ADD ***  04/25/03  ************************************
     C*********  BSFREQ    IFEQ 'T'
      *** BEG ADD ***** 01/03/05 **************************************
     C*********  SCLUMP    ANDNE'Y'
      *** END ADD ***** 01/03/05 **************************************
     C*********            MOVE 'T'       EAEDIT
     C*********            ELSE
     C*********            MOVE *BLANK    EAEDIT
     C*********            ENDIF
     C*** END ADD *****  04/25/03  ************************************
     C*
     C*********            MOVE ELCO#     EACO#
     C*********            MOVE ELFND     EAFND
     C*********            Z-ADDELFYR     EAFYR
     C*********            MOVE ELEMP#    EAEMP#
     C*********            MOVE ELDIV     EADIV
     C*********            MOVE 'A'       EASTAT
     C*********            MOVELQ1USER    EAUSER
     C*********            MOVELQ1PGM     EAPGM
     C*********            TIME           DSDTTM
     C*
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EADT
     C*
     C*********            Z-ADDDSTIME    EATIME
     C*********            WRITEPEA02
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*********            ELSE
     C*
     C*********  EAKY4     CHAINPEA04                60
     C*********  *IN60     DOWEQ*OFF
     C*
     C*********  EASTAT    IFEQ 'B'
     C*********  EASTAT    OREQ 'C'
     C*********            MOVE 'A'       EASTAT
     C*********            MOVELSCAR      EAARCD
     C*********            MOVELTTTYPE    EAARTY
     C*********            MOVELTTDESC    EADESC
     C*********            UPDATPEA04
     C*********            ENDIF
     C*
     C*********  EAKY4     READEPEA04                    60
     C*********            ENDDO                           *IN60 DOWEQ
     C*
     C*********            ENDIF                           XXDIVC IFGT
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*  GET THE STATE TAX A/R TYPE
     C*
     C*********            MOVE *BLANKS   EAARCD
     C*********            MOVE *BLANKS   EAARTY
     C*********            MOVE *BLANKS   EADESC
     C*********  *LOVAL    SETLLPTT01
     C*********            READ PTT01                    60
     C*********  *IN60     DOWEQ'0'
     C*********  TTCTYP    IFEQ 'T'
     C*********  TTTYPE    ANDEQ'F'
     C*** BEGIN ADD ***  04/12/99  ************************************
     C*********  BSSEFL    ANDEQ'E'
     C*********  TTCTYP    OREQ 'D'
     C*********  TTTYPE    ANDEQ'F'
     C*********  BSSEFL    ANDEQ'S'
     C*** END ADD *****  04/12/99  ************************************
     C*********            MOVELTTKEY     EAARCD
     C*********            MOVE TTTYPE    EAARTY
     C*********            MOVE TTDESC    EADESC
     C*********            MOVE *ON       *IN60
     C*********            ENDIF                           TTCTYP IFEQ
     C**N60****            READ PTT01                    60
     C*********            ENDDO                           *IN60 DOWEQ
     C*** BEGIN CHG ***  01/29/04  ************************************
     C*********  EAARCD    IFEQ *BLANKS
     C*********            MOVE *BLANKS   TTKEY
     C*********            MOVE *BLANKS   TTTYPE
     C*********            MOVE *BLANKS   TTDESC
     C*********            ENDIF
     C*** END CHG *****  01/29/04  ************************************
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*********  XXDIVC    IFEQ 1
     C*** END ADD *****  11/15/99  ************************************
     C*********            Z-ADDP1,1      EAAM01
     C*********            Z-ADDP1,2      EAAM02
     C*********            Z-ADDP1,3      EAAM03
     C*********            Z-ADDP1,4      EAAM04
     C*********            Z-ADDP1,5      EAAM05
     C*********            Z-ADDP1,6      EAAM06
     C*********            Z-ADDP1,7      EAAM07
     C*********            Z-ADDP1,8      EAAM08
     C*********            Z-ADDP1,9      EAAM09
     C*********            Z-ADDP1,10     EAAM10
     C*********            Z-ADDP1,11     EAAM11
     C*********            Z-ADDP1,12     EAAM12
     C*********            ADD  1         EATIME
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     EATRDT
      *
      *** BEG ADD ***** 01/03/05 **************************************
     C*********  SCLUMP    IFNE 'Y'
      *** END ADD ***** 01/03/05 **************************************
      *
     C*** BEGIN ADD ***  04/21/03  ************************************
     C*********  BSDOWK    IFNE *BLANKS
     C*** BEGIN ADD ***  04/25/03  ************************************
     C*********  BSFREQ    OREQ 'T'
     C*** END ADD *****  04/25/03  ************************************
     C*********            Z-ADDZ         EATRDT
     C*********            ENDIF
     C*** END ADD *****  04/21/03  ************************************
      *
     C*** BEGIN ADD ***  04/25/03  ************************************
     C*********  BSFREQ    IFEQ 'T'
     C*********            MOVE 'T'       EAEDIT
     C*********            ELSE
     C*********            MOVE *BLANK    EAEDIT
     C*********            ENDIF
     C*** END ADD *****  04/25/03  ************************************
      *
      *** BEG ADD ***** 01/03/05 **************************************
     C*********            ENDIF
      *** END ADD ***** 01/03/05 **************************************
      *
     C*********            MOVELSRDTO     EADT
     C*** BEGIN ADD ***  01/29/04  ************************************
     C*********  EAARCD    IFNE *BLANKS
     C*** END ADD *****  01/29/04  ************************************
     C*********            WRITEPEA02
     C*** BEGIN ADD ***  01/29/04  ************************************
     C*********            ENDIF
     C*** END ADD *****  01/29/04  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*********            ELSE
     C*
     C*********  EAKY4     CHAINPEA04                60
     C*********  *IN60     DOWEQ*OFF
     C*
     C*** BEGIN ADD ***  01/29/04  ************************************
     C*** BEGIN DEL ***  03/11/04  ************************************
     C********   TTKEY     IFEQ *BLANKS
     C********             DELETPEA04
     C********             ELSE
     C*** END DEL *****  03/11/04  ************************************
     C*** END ADD *****  01/29/04  ************************************
     C*********  EASTAT    IFEQ 'T'
     C*********  EASTAT    OREQ 'U'
     C*** BEGIN ADD ***  03/11/04  ************************************
     C*********  TTKEY     IFEQ *BLANKS
     C*********            DELETPEA04
     C*********            ELSE
     C*** END ADD *****  03/11/04  ************************************
     C*********            MOVE 'A'       EASTAT
     C*********            MOVELTTKEY     EAARCD
     C*********            MOVE TTTYPE    EAARTY
     C*********            MOVE TTDESC    EADESC
     C*********            UPDATPEA04
     C*** BEGIN ADD ***  03/11/04  ************************************
     C*********            ENDIF
     C*** END ADD *****  03/11/04  ************************************
     C*********            ENDIF
     C*** BEGIN ADD ***  01/29/04  ************************************
     C*** BEGIN DEL ***  03/11/04  ************************************
     C********             ENDIF
     C*** END DEL *****  03/11/04  ************************************
     C*** END ADD *****  01/29/04  ************************************
     C*
     C*********  EAKY4     READEPEA04                    60
     C*********            ENDDO                           *IN60 DOWEQ
     C*
     C*********            ENDIF                           XXDIVC IFGT
     C*** END ADD *****  11/15/99  ************************************
     C*********            END                             *IN80
     C*
     C*  CALL PROGRAM TO UPDATE A/R
     C*
     C*********            MOVE XXCO#     QQCO#   3
     C*********            MOVE XXFND     QQFND   3
     C*********            MOVE XXFYR     QQFYR
     C*********            MOVE SCEMP#    QQEMP#
     C*********            MOVE SCDIV     QQDIV
     C*********            MOVE SCDATE    QQDUE   8
     C*********            MOVE SCAR      QQAR    2
     C*** BEGIN ADD ***  07/11/05  ************************************
     C*********            Z-ADDSCDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DATCHK
     C*********            MOVELSRDTO     SCDATE
     C*** END ADD *****  07/11/05  ************************************
     C*********            MOVE SCEFDT    QQEFDT  8
     C*** BEGIN DEL ***  11/15/99  ************************************
     C********             CALL 'W8011C'
     C********             PARM           QQCO#
     C********             PARM           QQFND
     C********             PARM           QQFYR
     C********             PARM           QQEMP#
     C********             PARM           QQDIV
     C********             PARM           QQAR
     C********             PARM           QQEFDT
     C*** END DEL *****  11/15/99  ************************************
     C*
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C*** BEGIN CHG ***  02/28/06  ************************************
     C*********  EMKY1     CHAINPEM02                60
     C*****N67   EMKY1     CHAINPEM02                60
     C******67   EMKY      CHAINPEM02                60
     C*** END CHG *****  02/28/06  ************************************
     C*********  *IN60     DOWEQ*OFF
     C*
     C*********            Z-ADDEMDIV     XXDIV
     C*********  ELKY1     CHAINPEL02                60
     C*********  *IN60     IFEQ *OFF
     C*
     C*********            MOVELELBSCD    XXKEY     P
     C*********  TBKY      CHAINPBS01                60
     C*********  *IN60     IFEQ *OFF
     C*** END ADD *****  11/15/99  ************************************
     C*
     C*  IF SELF-REPORTING ACCOUNT, DELETE ANY FUTURE BILLING RECORDS
     C*  BECAUSE ACCOUNT MAY HAVE BEEN CHANGED FROM EST TO S/R.
     C*
     C*********  BSSEFL    IFEQ 'S'
     C*
     C*  ONLY DELETE FUTURE BILLINGS AND ADJUSTMENTS IF UPDATE A/R FLAG
     C*  IN FUND DETAIL IS 'Y'
     C*
     C*********  F1UPAR    IFEQ 'Y'
     C*
     C*********            MOVE 'B'       XXARTY
     C*
     C*********            Z-ADDDSDATE    SRDTI
     C*********            CALL 'SRCVTDAT'DTCYMD
     C*********            MOVELSRDTO     XXPODT
     C*
     C*********  EAKY      SETLLPEA01
     C*********  EAKY2     READEPEA01                    50
     C*********  *IN50     DOWEQ'0'
     C*
     C*********            MOVELEAARCD    XXKEY     P
     C*********  TBKY      CHAINPTT01                60
     C*********  *IN60     IFEQ *OFF
     C*
     C*********  TTCTYP    IFNE 'E'
     C*********            DELETPEA01
     C*********            ENDIF
     C*********            ENDIF
     C*
     C*********  EAKY2     READEPEA01                    50
     C**********           END
     C*** BEGIN ADD ***  01/25/05  ************************************
     C*********  EAKY      SETLLPXA01
     C*********  EAKY2     READEPXA01                    50
     C*********  *IN50     DOWEQ'0'
     C*
     C*********            MOVELXAARCD    XXKEY     P
     C*********  TBKY      CHAINPTT01                60
     C*********  *IN60     IFEQ *OFF
     C*
     C*********  TTCTYP    IFNE 'E'
     C*********            DELETPXA01
     C*********            ENDIF
     C*********            ENDIF
     C*
     C*********  EAKY2     READEPXA01                    50
     C*********            ENDDO
     C*** END ADD *****  01/25/05  ************************************
     C*
     C*********            MOVE 'A'       XXARTY
     C*********  EAKY      SETLLPEA01
     C*********  EAKY2     READEPEA01                    50
     C*********  *IN50     DOWEQ'0'
     C*
     C*********            MOVELEAARCD    XXKEY     P
     C*********  TBKY      CHAINPTT01                60
     C*********  *IN60     IFEQ *OFF
     C*
     C*********  TTCTYP    IFNE 'E'
     C*********            DELETPEA01
     C*********            ENDIF
     C*********            ENDIF
     C*
     C*********  EAKY2     READEPEA01                    50
     C*********            END
     C*** BEGIN ADD ***  01/25/05  ************************************
     C*********  EAKY      SETLLPXA01
     C*********  EAKY2     READEPXA01                    50
     C*********  *IN50     DOWEQ'0'
     C*
     C*********            MOVELXAARCD    XXKEY     P
     C*********  TBKY      CHAINPTT01                60
     C*********  *IN60     IFEQ *OFF
     C*
     C*********  TTCTYP    IFNE 'E'
     C*********            DELETPXA01
     C*********            ENDIF
     C*********            ENDIF
     C*
     C*********  EAKY2     READEPXA01                    50
     C*********            ENDDO
     C*** END ADD *****  01/25/05  ************************************
     C*
     C*** BEGIN ADD ***  04/21/05  ************************************
     C*********  RSKY      CHAINPRS01                60
     C*********            MOVE RSJRST    XXST
     C*********            Z-ADDELPFDT    XXEFDT
     C*********  RRKY      SETGTPIF01
     C*********  RRKY2     REDPEPIF01                    60
     C*********  *IN60     IFEQ *ON
     C*********            MOVE *BLANKS   IFIFBL
     C*********            ENDIF
     C*
     C*********            MOVE 'F'       XXARTY
     C*********  EAKY      SETLLPEA01
     C*********  EAKY2     READEPEA01                    50
     C*********  *IN50     DOWEQ'0'
     C*
     C*********            MOVELEAARCD    XXKEY     P
     C*********  TBKY      CHAINPTT01                60
     C*********  *IN60     IFEQ *OFF
     C*
     C*********  TTCTYP    IFEQ 'T'
     C*********  EAARCD    OREQ IFIFBL
     C*********            DELETPEA01
     C*********            ENDIF
     C*********            ENDIF
     C*
     C*********  EAKY2     READEPEA01                    50
     C*********            END
     C*
     C*********  EAKY      SETLLPXA01
     C*********  EAKY2     READEPXA01                    50
     C*********  *IN50     DOWEQ'0'
     C*
     C*********            MOVELXAARCD    XXKEY     P
     C*********  TBKY      CHAINPTT01                60
     C*********  *IN60     IFEQ *OFF
     C*
     C*********  TTCTYP    IFEQ 'T'
     C*********  EAARCD    OREQ IFIFBL
     C*********            DELETPXA01
     C*********            ENDIF
     C*********            ENDIF
     C*
     C*********  EAKY2     READEPXA01                    50
     C*********            ENDDO
     C*** END ADD *****  04/21/05  ************************************
     C*********            END
     C*********            END                             F1UPAR = 'Y'
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C*********            ENDIF                           *IN60 IFEQ
     C*
     C*********            ENDIF                           *IN60 IFEQ
     C*
      *** BEG ADD ***** 06/13/05 **************************************
     C*********  EMSTRP    IFEQ 'Y'
     C*********            MOVELEMNAME    SVNAME
     C*********            MOVELEMFDNM    SVFDNM
     C*********            MOVELEMNAM2    SVNAM2
     C*********            MOVELEMFDN2    SVFDN2
     C*********            ENDIF
      *** END ADD ***** 06/13/05 **************************************
     C*
     C*** BEGIN CHG ***  02/28/06  ************************************
     C*********  EMKY1     READEPEM02                    60
     C*****N67   EMKY1     READEPEM02                    60
     C******67             MOVE *ON       *IN60
     C*** END CHG *****  02/28/06  ************************************
     C*********            ENDDO
     C*
     C*** END ADD *****  11/15/99  ************************************
     C*** END DEL *****  07/07/06  *************************************
     C*
     C**** BEGIN ADD **** 06/01/01 ************************************
     C*
     C*  DETERMINE IF CALLED FROM EMPLOYER MID TERM ENDORSEMENTS
     C*    IF SO, WE NEED TO DISPLAY SCR04A
     C*
     C***        *NAMVAR   DEFN *LDA      SVLDA
     C     *LOCK         IN        *DTAARA
     C                   OUT       *DTAARA
     C*
     C*** BEG ADD*****  07/09/08  ************************************
     C***BEG DLT *** 07/09/08 *****************************************
     C************         Z-ADDKEYF,1    SRDTI
     C************         CALL 'SRCVTDAT'DTCYMD
     C************         MOVELSRDTO     XXEFD1
     C***END DLT *** 07/09/08 *****************************************
     C                   MOVE      *BLANKS       XXCHG
     C     EPKY3         SETLL     PEP02
     C     EPKY3         READE     PEP02                                  60
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     EPEFDT        XXEFD1
     C                   MOVE      EPCLAS        X@CLAS            5 0
     C     EPKY4         CHAIN     PEP03                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      'Y'           XXCHG             1
     C                   ENDIF
     C     EPKY3         READE     PEP02                                  60
     C                   ENDDO
     C*** END ADD*****  07/09/08  ************************************
     C     LDENDA        IFNE      *ZEROS
     C     LDENDA        ANDNE     *BLANKS
      ***** BEG DEL ******* 06/08/10 **********************************
     C****BEG ADD*****  07/09/08  ************************************
     C**IF*NON-ZERO ADJUSTMENT EXIST (*IN80 = *OFF)
     C***BEG DLT *** 07/09/08 *****************************************
     C********** XXCHG     ANDEQ'Y'
     C***END DLT *** 07/09/08 *****************************************
     C*****      *IN80     ANDEQ*OFF
     C**OR*NEW PAYROLL IS ADDED
     C*****      XXCHG     OREQ 'Y'
     C****BEG ADD*****  04/30/09  ************************************
     C*****      LDENDA    ANDNE*BLANKS
     C*****      LDENDA    ANDNE*ZEROS
     C****END ADD*****  04/30/09  ************************************
     C*****END   ADD **** 07/09/08 ************************************
      ***** END DEL ******* 06/08/10 **********************************
     C                   MOVEL(P)  'SCR04A'      Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C**** END   ADD **** 06/01/01 ************************************
     C*
     C*  SUBMIT PROGRAM TO PRINT BILL
     C*
     C     PRNT          TAG
     C*** BEGIN ADD ***  07/09/08  ************************************
     C     *IN80         IFEQ      *OFF
     C*** END   ADD ***  07/09/08  ************************************
     C*** BEGIN ADD ***  07/07/06  *************************************
     C*
     C*  WRITE TRANSACTION RECORD AND SUBMIT BATCH UPDATE
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     DA(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA(3)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA(4)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA(5)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA(7)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA(8)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA(9)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA(10)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA(11)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA(12)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
      *** BEG ADD ***** 01/31/11 **************************
     C                   MOVEA     J1D           J2D
      *** END ADD ***** 01/31/11 **************************
     C*
     C                   Z-ADD     PA(1)         EAAM01
     C                   Z-ADD     PA(2)         EAAM02
     C                   Z-ADD     PA(3)         EAAM03
     C                   Z-ADD     PA(4)         EAAM04
     C                   Z-ADD     PA(5)         EAAM05
     C                   Z-ADD     PA(6)         EAAM06
     C                   Z-ADD     PA(7)         EAAM07
     C                   Z-ADD     PA(8)         EAAM08
     C                   Z-ADD     PA(9)         EAAM09
     C                   Z-ADD     PA(10)        EAAM10
     C                   Z-ADD     PA(11)        EAAM11
     C                   Z-ADD     PA(12)        EAAM12
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     SCLUMP        IFNE      'Y'
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL     SCAR          EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C*
     C     BSFREQ        IFEQ      'T'
     C     SCLUMP        ANDNE     'Y'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVE      ELCO#         EACO#
     C                   MOVE      ELFND         EAFND
     C                   Z-ADD     ELFYR         EAFYR
     C                   MOVE      ELEMP#        EAEMP#
     C                   MOVE      ELDIV         EADIV
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      'B'
     C     EASTAT        OREQ      'C'
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     SCAR          EAARCD
     C                   MOVEL     TTTYPE        EAARTY
     C                   MOVEL     TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C*  GET THE STATE TAX A/R TYPE
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      'T'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'S'
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     P1(1)         EAAM01
     C                   Z-ADD     P1(2)         EAAM02
     C                   Z-ADD     P1(3)         EAAM03
     C                   Z-ADD     P1(4)         EAAM04
     C                   Z-ADD     P1(5)         EAAM05
     C                   Z-ADD     P1(6)         EAAM06
     C                   Z-ADD     P1(7)         EAAM07
     C                   Z-ADD     P1(8)         EAAM08
     C                   Z-ADD     P1(9)         EAAM09
     C                   Z-ADD     P1(10)        EAAM10
     C                   Z-ADD     P1(11)        EAAM11
     C                   Z-ADD     P1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      'T'
     C     EASTAT        OREQ      'U'
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C****BEG DEL ************07/09/08 *******************************
     C*********            END                             *IN80
     C****END DEL ************07/09/08 *******************************
     C****BEG ADD ************05/15/07 *******************************
     C*  GET THE STATE TAX A/R TYPE
     C*
     C*** BEGIN ADD ***  11/12/08  ************************************
     C                   Z-ADD     EAPO01        SVDT7
     C*** END ADD *****  11/12/08  ************************************
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C*** BEGIN ADD ***  11/12/08  ************************************
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C*** END ADD *****  11/12/08  ************************************
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     A1(1)         EAAM01
     C                   Z-ADD     A1(2)         EAAM02
     C                   Z-ADD     A1(3)         EAAM03
     C                   Z-ADD     A1(4)         EAAM04
     C                   Z-ADD     A1(5)         EAAM05
     C                   Z-ADD     A1(6)         EAAM06
     C                   Z-ADD     A1(7)         EAAM07
     C                   Z-ADD     A1(8)         EAAM08
     C                   Z-ADD     A1(9)         EAAM09
     C                   Z-ADD     A1(10)        EAAM10
     C                   Z-ADD     A1(11)        EAAM11
     C                   Z-ADD     A1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** BEGIN ADD ***  11/12/08  **********************************
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A1(13)        IFNE      *ZERO
     C                   Z-ADD     A1(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*** END ADD *****  11/12/08  **********************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '1'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*********  EASTAT    OREQ '6'
     C     EASTAT        OREQ      'D'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C*** BEGIN ADD ***  11/12/08  ************************************
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C*** END ADD *****  11/12/08  ************************************
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     A2(1)         EAAM01
     C                   Z-ADD     A2(2)         EAAM02
     C                   Z-ADD     A2(3)         EAAM03
     C                   Z-ADD     A2(4)         EAAM04
     C                   Z-ADD     A2(5)         EAAM05
     C                   Z-ADD     A2(6)         EAAM06
     C                   Z-ADD     A2(7)         EAAM07
     C                   Z-ADD     A2(8)         EAAM08
     C                   Z-ADD     A2(9)         EAAM09
     C                   Z-ADD     A2(10)        EAAM10
     C                   Z-ADD     A2(11)        EAAM11
     C                   Z-ADD     A2(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** BEGIN ADD ***  11/12/08  **********************************
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A2(13)        IFNE      *ZERO
     C                   Z-ADD     A2(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*** END ADD *****  11/12/08  **********************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '2'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*********  EASTAT    OREQ '7'
     C     EASTAT        OREQ      'E'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C*** BEGIN ADD ***  11/12/08  ************************************
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C*** END ADD *****  11/12/08  ************************************
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     A3(1)         EAAM01
     C                   Z-ADD     A3(2)         EAAM02
     C                   Z-ADD     A3(3)         EAAM03
     C                   Z-ADD     A3(4)         EAAM04
     C                   Z-ADD     A3(5)         EAAM05
     C                   Z-ADD     A3(6)         EAAM06
     C                   Z-ADD     A3(7)         EAAM07
     C                   Z-ADD     A3(8)         EAAM08
     C                   Z-ADD     A3(9)         EAAM09
     C                   Z-ADD     A3(10)        EAAM10
     C                   Z-ADD     A3(11)        EAAM11
     C                   Z-ADD     A3(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** BEGIN ADD ***  11/12/08  **********************************
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A3(13)        IFNE      *ZERO
     C                   Z-ADD     A3(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*** END ADD *****  11/12/08  **********************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '3'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*********  EASTAT    OREQ '8'
     C     EASTAT        OREQ      'F'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C*** BEGIN ADD ***  11/12/08  ************************************
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C*** END ADD *****  11/12/08  ************************************
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     A4(1)         EAAM01
     C                   Z-ADD     A4(2)         EAAM02
     C                   Z-ADD     A4(3)         EAAM03
     C                   Z-ADD     A4(4)         EAAM04
     C                   Z-ADD     A4(5)         EAAM05
     C                   Z-ADD     A4(6)         EAAM06
     C                   Z-ADD     A4(7)         EAAM07
     C                   Z-ADD     A4(8)         EAAM08
     C                   Z-ADD     A4(9)         EAAM09
     C                   Z-ADD     A4(10)        EAAM10
     C                   Z-ADD     A4(11)        EAAM11
     C                   Z-ADD     A4(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** BEGIN ADD ***  11/12/08  **********************************
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A4(13)        IFNE      *ZERO
     C                   Z-ADD     A4(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*** END ADD *****  11/12/08  **********************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '4'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*********  EASTAT    OREQ '9'
     C     EASTAT        OREQ      'G'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C*** BEGIN ADD ***  11/12/08  ************************************
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C*** END ADD *****  11/12/08  ************************************
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     A5(1)         EAAM01
     C                   Z-ADD     A5(2)         EAAM02
     C                   Z-ADD     A5(3)         EAAM03
     C                   Z-ADD     A5(4)         EAAM04
     C                   Z-ADD     A5(5)         EAAM05
     C                   Z-ADD     A5(6)         EAAM06
     C                   Z-ADD     A5(7)         EAAM07
     C                   Z-ADD     A5(8)         EAAM08
     C                   Z-ADD     A5(9)         EAAM09
     C                   Z-ADD     A5(10)        EAAM10
     C                   Z-ADD     A5(11)        EAAM11
     C                   Z-ADD     A5(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*** BEGIN ADD ***  11/12/08  **********************************
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A5(13)        IFNE      *ZERO
     C                   Z-ADD     A5(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*** END ADD *****  11/12/08  **********************************
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '5'
      *** BEG CHG ***** 09/17/07 **********************************
     C********** EASTAT    OREQ '10'
     C*** BEGIN CHG ***  12/11/08  ************************************
     C*********  EASTAT    OREQ '0'
     C     EASTAT        OREQ      'H'
     C*** BEGIN CHG ***  12/11/08  ************************************
      *** END CHG ***** 09/17/07 **********************************
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*** BEGIN ADD ***  12/11/08  ************************************
     C*
     C* 6TH ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE ASSESSMENT ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     A6(1)         EAAM01
     C                   Z-ADD     A6(2)         EAAM02
     C                   Z-ADD     A6(3)         EAAM03
     C                   Z-ADD     A6(4)         EAAM04
     C                   Z-ADD     A6(5)         EAAM05
     C                   Z-ADD     A6(6)         EAAM06
     C                   Z-ADD     A6(7)         EAAM07
     C                   Z-ADD     A6(8)         EAAM08
     C                   Z-ADD     A6(9)         EAAM09
     C                   Z-ADD     A6(10)        EAAM10
     C                   Z-ADD     A6(11)        EAAM11
     C                   Z-ADD     A6(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A6(13)        IFNE      *ZERO
     C                   Z-ADD     A6(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '6'
     C     EASTAT        OREQ      'J'
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C* 7TH ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C*
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C*
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAARCD        IFEQ      *BLANKS
     C                   MOVE      *BLANKS       TTKEY
     C                   MOVE      *BLANKS       TTTYPE
     C                   MOVE      *BLANKS       TTDESC
     C                   ENDIF
     C*
     C*  WRITE A RECORD FOR THE ASSESSMENT ADJUSTMENT
     C*
     C     XXDIVC        IFEQ      1
     C*
     C                   Z-ADD     A7(1)         EAAM01
     C                   Z-ADD     A7(2)         EAAM02
     C                   Z-ADD     A7(3)         EAAM03
     C                   Z-ADD     A7(4)         EAAM04
     C                   Z-ADD     A7(5)         EAAM05
     C                   Z-ADD     A7(6)         EAAM06
     C                   Z-ADD     A7(7)         EAAM07
     C                   Z-ADD     A7(8)         EAAM08
     C                   Z-ADD     A7(9)         EAAM09
     C                   Z-ADD     A7(10)        EAAM10
     C                   Z-ADD     A7(11)        EAAM11
     C                   Z-ADD     A7(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     SCLUMP        IFNE      'Y'
      *
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
      *
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   MOVEL     SRDTO         EADT
     C     EAARCD        IFNE      *BLANKS
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A7(13)        IFNE      *ZERO
     C                   Z-ADD     A7(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
      ***** BEG ADD ******* 10/14/10 **********************************
     C                   EXSR      SREAP
      ***** END ADD ******* 10/14/10 **********************************
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EASTAT        IFEQ      '7'
     C     EASTAT        OREQ      'K'
     C*
     C     TTKEY         IFEQ      *BLANKS
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C*
     C                   ELSE
     C*
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   UPDATE    PEA04
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXDIVC IFGT
     C*** END ADD *****  12/11/08  ************************************
     C*
     C****BEG ADD ************07/09/08 *******************************
     C                   END                                                    *IN80
     C****END ADD ************07/09/08 *******************************
     C*  CALL PROGRAM TO UPDATE A/R
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C                   MOVE      SCDATE        QQDUE             8
     C                   MOVE      SCAR          QQAR              2
     C                   Z-ADD     SCDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDATE
     C                   MOVE      SCEFDT        QQEFDT            8
     C*
     C*  CHAIN THROUGH ALL EMPLOYERS FOR S/R A/R DELETION.
     C*
     C  N67EMKY1         CHAIN     PEM02                              60
     C   67EMKY          CHAIN     PEM02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMDIV         XXDIV
     C     ELKY1         CHAIN     PEL02                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*  IF SELF-REPORTING ACCOUNT, DELETE ANY FUTURE BILLING RECORDS
     C*  BECAUSE ACCOUNT MAY HAVE BEEN CHANGED FROM EST TO S/R.
     C*
     C     BSSEFL        IFEQ      'S'
     C*
     C*  ONLY DELETE FUTURE BILLINGS AND ADJUSTMENTS IF UPDATE A/R FLAG
     C*  IN FUND DETAIL IS 'Y'
     C*
     C     F1UPAR        IFEQ      'Y'
     C*
     C                   MOVE      'B'           XXARTY
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C*
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKY2         READE     PEA01                                  50
     C                   END
     C*
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
     C                   DELETE    PXA01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKY2         READE     PXA01                                  50
     C                   ENDDO
     C*
     C                   MOVE      'A'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKY2         READE     PEA01                                  50
     C                   END
     C*
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
     C                   DELETE    PXA01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKY2         READE     PXA01                                  50
     C                   ENDDO
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   ENDIF
     C*
     C                   MOVE      'F'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C****BEGIN ADD ******05/15/07*************************************
     C*** BEGIN CHG ***  11/12/08  ************************************
     C*********  TTCTYP    OREQ '1'
     C*********  TTCTYP    OREQ '2'
     C*********  TTCTYP    OREQ '3'
     C*********  TTCTYP    OREQ '4'
     C*********  TTCTYP    OREQ '5'
     C     TTCTYP        OREQ      '1'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '2'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '3'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '4'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '5'
     C     CLTID         ANDNE     '139'
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     TTCTYP        OREQ      '6'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '7'
     C     CLTID         ANDNE     '139'
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  11/12/08  ************************************
     C****END   ADD ******05/15/07*************************************
     C     EAARCD        OREQ      IFIFBL
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKY2         READE     PEA01                                  50
     C                   END
     C*
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C****BEGIN ADD ******05/15/07*************************************
     C*** BEGIN CHG ***  11/12/08  ************************************
     C*********  TTCTYP    OREQ '1'
     C*********  TTCTYP    OREQ '2'
     C*********  TTCTYP    OREQ '3'
     C*********  TTCTYP    OREQ '4'
     C*********  TTCTYP    OREQ '5'
     C     TTCTYP        OREQ      '1'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '2'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '3'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '4'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '5'
     C     CLTID         ANDNE     '139'
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     TTCTYP        OREQ      '6'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '7'
     C     CLTID         ANDNE     '139'
     C*** END ADD *****  12/11/08  ************************************
     C*** END CHG *****  11/12/08  ************************************
     C****END   ADD ******05/15/07*************************************
     C     EAARCD        OREQ      IFIFBL
     C                   DELETE    PXA01
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKY2         READE     PXA01                                  50
     C                   ENDDO
     C*
     C                   END
     C*
     C                   END                                                    F1UPAR = 'Y'
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C  N67EMKY1         READE     PEM02                                  60
     C   67              MOVE      *ON           *IN60
     C                   ENDDO
     C*
     C*** END ADD *****  07/07/06  *************************************
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C*
     C*** BEGIN CHG ***  05/01/98  ************************************
     C*****                Z-ADDUPPRDT    SRDTI
     C*****                CALL 'SRCVTDAT'DTMDYY
     C*****                MOVELSRDTO     WKDATE  80
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         WKDATE            8 0
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* REVISED INTERIM DAYS PLUS THE PRESENT DATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          WKDATE
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        WKDATE
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END CHG *****  05/01/98  ************************************
     C*
     C*** BEGIN CHG ***  09/24/98  ************************************
     C*****                MOVE WKDATE    QQDATE  8
     C     DA(1)         IFNE      *ZERO
     C                   MOVE      DA(1)         QQDATE
     C                   ELSE
     C                   MOVE      WKDATE        QQDATE            8
     C                   ENDIF                                                  DA,1 IFNE
     C*** END CHG *****  09/24/98  ************************************
     C                   MOVE      *BLANKS       QQAR              2
     C                   MOVE      SCDATE        QQDUE             8
     C*
     C*  GET BILLING SCHEDULE FOR STYLE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C   60              MOVE      *BLANKS       BSSEFL
     C                   MOVE      BSSEFL        QQSTL             1
     C                   MOVE      ' '           QQPRT             1
     C                   MOVE      'Y'           QQOVR             1
     C                   MOVE      *BLANKS       QQSRCD            2
     C                   MOVE      *BLANKS       QQAJCD
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   MOVE      ' '           QQRNB             1
     C                   MOVE      SCDART        QQDART            1
     C*** BEGIN ADD ***  02/15/05  ************************************
     C                   MOVEL     SCFCM1        QQFCM1
     C                   MOVEL     SCFCM2        QQFCM2
     C                   MOVEL     SCFCM3        QQFCM3
     C*** END ADD *****  02/15/05  ************************************
     C*** BEGIN CHG ***  02/28/06  ************************************
     C     *IN67         IFEQ      *ON
     C                   MOVE      'Z'           QQFQT
     C                   ELSE
     C                   MOVE      *BLANK        QQFQT
     C                   ENDIF
     C*** END CHG *****  02/28/06  ************************************
     C                   CALL      'W8001C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQDATE
     C                   PARM                    QQAR
     C                   PARM                    SCCOM1
     C                   PARM                    SCCOM2
     C                   PARM                    SCCOM3
     C                   PARM                    QQDUE
     C                   PARM                    SCDEP
     C                   PARM                    QQSTL
     C                   PARM                    QQPRT
     C                   PARM                    QQOVR
     C                   PARM                    QQSRCD
     C                   PARM                    QQAJCD
     C                   PARM      'W'           QQQTE
     C                   PARM                    QQOUTQ
     C                   PARM                    QQRNB
     C                   PARM                    QQDART
     C                   PARM      'Y'           QQHARD            1
     C                   PARM      'N'           QQAGTO            1
     C*** BEGIN CHG ***  02/28/06  ************************************
     C*********            PARM ' '       QQFQT   1
     C                   PARM                    QQFQT             1
     C*** END CHG *****  02/28/06  ************************************
     C                   PARM                    SCRMCD
     C*** BEGIN ADD ***  11/15/99  ************************************
     C                   PARM                    SCAR
     C                   PARM                    QQEFDT
     C*** END ADD *****  11/15/99  ************************************
     C*** BEG ADD *****  09/30/04  ************************************
     C*** BEGIN CHG ***  02/15/05  ************************************
     C************         PARM *BLANKS   QQFCM1 30
     C************         PARM *BLANKS   QQFCM2 30
     C************         PARM *BLANKS   QQFCM3 30
     C                   PARM                    QQFCM1           30
     C                   PARM                    QQFCM2           30
     C                   PARM                    QQFCM3           30
     C*** END CHG *****  02/15/05  ************************************
     C*** END ADD *****  09/30/04  ************************************
     C*
     C*** BEGIN ADD ******* 08/23/11 **********************************
     C     Q1PRMS        IFGT      4
     C*** END ADD ********* 08/23/11 *******************************************
     C*** BEG ADD ***** 11/29/10 ***********************************************
     C                   MOVE      *ON           @@RTCD
     C*** END ADD ***** 11/29/10 ***********************************************
     C*** BEGIN ADD ******* 08/23/11 **********************************
     C                   ENDIF
     C*** END ADD ********* 08/23/11 *******************************************
     C*** BEG ADD ***** 04/19/10 ***********************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C     @@RRN         CHAIN     WDELP                              60
     C*
     C                   MOVE      UPOUTQ        QQQUE            10
     C                   MOVE      'N'           SCREIS            1
     C*
     C*   CHECK SUBMISSION HISTORY
     C*
     C     SHKY          CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C     SHSTAT        IFEQ      '4'
     C     SHCODE        OREQ      'I'
     C     SHCODE        OREQ      'T'
     C                   MOVE      'Y'           SCREIS
     C                   ENDIF                                                  SHSTAT
     C     ELPFDT        IFNE      SHPFDT
     C     SHPFDT        ANDNE     0
     C     SHSTAT        ANDEQ     '4'
     C                   Z-ADD     SHPFDT        SHSBDT
     C                   Z-ADD     ELPFDT        SHPFDT
     C                   MOVE      'K'           SHCODE
     C                   ELSE
     C                   MOVE      *BLANKS       SHCODE
     C                   Z-ADD     0             SHSBDT
     C                   ENDIF                                                  ELPFDT IFNE SHPFDT
     C                   MOVE      '2'           SHSTAT
     C                   UPDATE    PSH01
     C                   ENDIF                                                  SHKY
     C*
     C                   MOVE      SCREIS        QQREIS            1
     C*
     C                   OPEN      WDELL1
     C     RRKY3         SETLL     WDELL1
     C     RRKY3         READE     WDELL1                                 60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      ELRTCD        XX1RTC
     C     RSKY1         CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXSTAT
     C                   Z-ADD     ELPFDT        XX1EFD
     C                   Z-ADD     ELPTDT        XXTPFD
     C                   MOVE      ELDIV         QQDIV
     C                   MOVE      XX1EFD        QQFPFD            8
     C                   MOVE      XXTPFD        QQTPFD            8
     C     RRKY4         SETGT     PRR01
     C     RRKY5         READPE    PRR01                                  60
     C     RRPPFL        IFEQ      'Y'
     C                   CALL      'W7412C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQREIS
     C                   PARM                    QQFPFD
     C                   PARM                    QQTPFD
     C                   PARM                    QQQUE
     C                   PARM                    SCDART
     C                   PARM      'Y'           QQHARD            1
     C                   PARM      'Y'           QQAGTO            1
     C                   LEAVE                                                  LEAVE DO LOOP
     C                   ENDIF
     C*
     C     RRKY3         READE     WDELL1                                 60
     C                   ENDDO
     C                   CLOSE     WDELL1
     C*
     C*** END ADD ***** 04/19/10 ***********************************************
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C**** BEGIN DEL **** 06/01/01 ************************************
     C*
     C*  DETERMINE IF CALLED FROM EMPLOYER MID TERM ENDORSEMENTS
     C*    IF SO, WE NEED TO DISPLAY SCR04A
     C*
     C********** *NAMVAR   DEFN *LDA      SVLDA
     C********** *LOCK     IN   *NAMVAR
     C**********           OUT  *NAMVAR
     C*
     C********** LDENDA    IFNE *ZEROS
     C********** LDENDA    ANDNE*BLANKS
     C**********           MOVEL'SCR04A'  Q2RECF    P
     C**********           GOTO STRT
     C**********           ELSE
     C**** END   DEL **** 06/01/01 ************************************
     C*** END ADD ***** 02/09/98 ***********************************************
     C*
     C*  IF DISCOUNT IS BEING SPREAD, DISPLAY SCR05, WHICH SHOWS A
     C*  MESSAGE THAT ALL DIVISIONS SHOULD BE SUBMITTED IF STANDARD
     C*  PREM WAS MODIFIED
     C*
     C*** BEGIN CHG ***  01/21/98  ************************************
     C*********  Q1SPRD    IFEQ 'Y'
     C*** BEGIN DEL ***  11/15/99  ************************************
     C******               MOVE XXCO#     QQCO#   3
     C******               MOVE XXFND     QQFND   3
     C******               MOVE SCYR      QQFYR   3
     C******               MOVE SCEMP#    QQEMP#  9
     C******               MOVE ' '       QQMCHK  1
     C******               CALL 'SRMCHK'
     C******               PARM           QQCO#
     C******               PARM           QQFND
     C******               PARM           QQFYR
     C******               PARM           QQEMP#
     C******               PARM           QQMCHK
     C******     QQMCHK    IFEQ 'Y'
     C*** END CHG *****  01/21/98  ************************************
     C*
     C******               MOVE *BLANKS   Q2RECF
     C******               MOVEL'SCR05'   Q2RECF
     C******               GOTO STRT
     C******
     C******               ELSE
     C*** END DEL *****  11/15/99  ************************************
     C*
     C*  RETURN TO SCREEN 1
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     Q1PRMS        IFGT      1
     C                   GOTO      TERM
     C                   ENDIF                                                  Q1PRMS>1
     C*
     C                   GOTO      STRT
     C*** BEGIN DEL ***  11/15/99  ************************************
     C********             END
     C*** END DEL *****  11/15/99  ************************************
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C**** BEGIN DEL **** 06/01/01 ************************************
     C******************** ENDIF                           LDENDA<>' '/0
     C**** END   DEL **** 06/01/01 ************************************
     C*** END ADD ***** 02/09/98 ***********************************************
     C*
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C     SCR4AT        TAG
     C*
     C*  THE ENDORSEMENT DESCRIPTION CANNOT BE BLANK
     C*
     C**** BEGIN CHG **** 06/01/01 ************************************
     C********** SCMTDS    IFEQ *BLANK
     C     SCDSC1        IFEQ      *BLANK
     C**** END   CHG **** 06/01/01 ************************************
     C                   MOVE      *ON           *IN20
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C*  WRITE AN INTERIM ADJUSTMENT OF PREMIUM MID TERM ENDORSEMENT
     C*
     C                   EXSR      WRTPEN
     C*
     C*  IF DISCOUNT IS BEING SPREAD, DISPLAY SCR05, WHICH SHOWS A
     C*  MESSAGE THAT ALL DIVISIONS SHOULD BE SUBMITTED IF STANDARD
     C*  PREM WAS MODIFIED
     C*
     C*** BEGIN CHG ***  01/21/98  ************************************
     C*********  Q1SPRD    IFEQ 'Y'
     C*** BEGIN DEL ***  11/15/99  ************************************
     C******               MOVE XXCO#     QQCO#   3
     C******               MOVE XXFND     QQFND   3
     C******               MOVE SCYR      QQFYR   3
     C******               MOVE SCEMP#    QQEMP#  9
     C******               MOVE ' '       QQMCHK  1
     C******               CALL 'SRMCHK'
     C******               PARM           QQCO#
     C******               PARM           QQFND
     C******               PARM           QQFYR
     C******               PARM           QQEMP#
     C******               PARM           QQMCHK
     C******     QQMCHK    IFEQ 'Y'
     C*** END CHG *****  01/21/98  ************************************
     C******
     C******               MOVE *BLANKS   Q2RECF
     C******               MOVEL'SCR05'   Q2RECF
     C******               GOTO STRT
     C******
     C******               ELSE
     C*** END DEL *****  11/15/99  ************************************
      **** BEGIN ADD **** 06/01/01 ************************************
     C                   GOTO      PRNT
      **** END   ADD **** 06/01/01 ************************************
     C*
     C*  RETURN TO SCREEN 1
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C*
     C     Q1PRMS        IFGT      1
     C                   GOTO      TERM
     C                   ENDIF                                                  Q1PRMS>1
     C*
     C                   GOTO      STRT
     C*** BEGIN DEL ***  11/15/99  ************************************
     C********             END
     C*** END DEL *****  11/15/99  ************************************
     C*** END ADD ***** 02/09/98 ***********************************************
     C*
     C     SCR05T        TAG
     C*
     C*  RETURN TO SCREEN 1
     C*
     C*** BEGIN CHG ***  11/15/99  ************************************
     C********             MOVEL'SCR01'   Q2RECF    P
     C*
     C********   Q1PRMS    CABGT1         TERM
     C                   MOVE      'Y'           XX1ST
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C*** END CHG *****  11/15/99  ************************************
     C*
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C* Delete override for WDHCLR
     C                   CALL      'QCMDEXC'                            99
     C                   PARM                    QCA(43)
     C                   PARM      55            LENGTH           15 5
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 10/14/10 **********************************
      *
      *================================================================
      *  PROCESSING BEFORE WRITE WAEAP RECORD
      *================================================================
     C     SREAP         BEGSR
      *** BEG ADD ***** 01/31/11 *******************************
      *
      * RESTORE ORIGINAL POST DATES IN CASE PREVIOUS INSTALLMENT
      * BILLING CLEARED THEM
      *
     C                   MOVEA     J2D           J1D
      *** END ADD ***** 01/31/11 *******************************
      *
      *  ASSURE ZERO DATE WITH ZERO AMOUNT
      *
     C     1             DO        12            J1                3 0
     C     AR(J1)        IFEQ      *ZERO
     C     J1D(J1)       ANDNE     *ZERO
     C                   Z-ADD     *ZERO         J1D(J1)
     C                   ENDIF
     C                   ENDDO
      *
      *  ASSURE DATE WITH NON-ZERO AMOUNT - SLOT 1
      *
     C     AR(1)         IFNE      *ZERO
     C     J1D(1)        ANDEQ     *ZERO
     C                   TIME                    DSDTTM
      *
      *      IF AMOUNT < 0 THEN SET TO CURRENT DATE
      *
     C     AR(1)         IFLT      *ZERO
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         J1D(1)
     C                   ENDIF                                                  AMT < 0
      *
      *       IF THE AMOUNT IS GREATER THAN ZERO, SET THE DATE TO THE
      *       CURRENT DATE + INTERIM DAYS, OR THE ACCOUNT CURRENT DATE
      *       (XXACDT) IF ACCOUNT CURRENT DATE IS NOT EQUAL ZERO
      *
     C     AR(1)         IFGT      *ZERO
     C     FNKY          CHAIN     PEX01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        #1DT              8 0
     C                   PARM      EXRVID        #1DAYS            5 0
     C                   PARM                    #1ERR             1
     C                   PARM                    #1TERM            1
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        #1DT
     C                   ENDIF
      *
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        #1DT
     C                   ENDIF                                                  XXACDT
      *
     C                   Z-ADD     #1DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         J1D(1)
      *
     C                   ENDIF                                                  AMT > 0
      *
     C                   ENDIF                                                  DATE SLOT 1
      *
     C                   ENDSR                                                  SREAP
      *
      ***** END ADD ******* 10/14/10 **********************************
     C*
     C*================================================================
     C*  CLEAR SCREEN ARRAYS FOR PAYROLL SEARCH
     C*================================================================
     C*
     C     CLR           BEGSR
     C                   MOVE      *ZEROS        DT
     C                   MOVE      *BLANKS       CL
     C                   MOVE      *BLANKS       SS
     C                   MOVE      *BLANKS       TP
     C                   MOVE      *ZEROS        AM
     C                   Z-ADD     *ZERO         I1                2 0
     C                   Z-ADD     *ZERO         I                 2 0
     C                   ENDSR
     C*
     C*================================================================
     C*  FILL SCREEN ARRAYS
     C*================================================================
     C*
     C     FILL          BEGSR
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DT(I1)
     C*
     C                   MOVE      EPCLAS        XXCLS
     C                   MOVEA     XXCLS         X0
     C                   SETOFF                                       60
     C*
     C*  PROCESS EACH ELEMENT IN THE ARRAY X0.
     C*
     C     1             DO        5             I
     C*
     C*  IF THE ELEMENT IS ZERO MOVE IN A BLANK
     C*
     C  N60X0(I)         IFEQ      '0'
     C                   MOVE      ' '           XB(I)
     C                   END
     C*
     C*  IF THE ELEMENT IS NOT A ZERO STOP THE ZERO CHECK(60)
     C*
     C     X0(I)         IFNE      '0'
     C     X0(I)         OREQ      '0'
     C     *IN60         ANDEQ     '1'
     C                   MOVE      X0(I)         XB(I)
     C                   SETON                                        60
     C                   END
     C*
     C                   END
     C                   MOVEA     XB            XXCLS             5
     C                   MOVEL     XXCLS         XXCLSP            6
     C                   MOVE      EPSPEC        XXCLSP
     C                   MOVEL     XXCLSP        CL(I1)
     C                   MOVEL     EPSTAT        SS(I1)
     C                   MOVEL     EPSAFL        TP(I1)
     C     EPSAFL        IFEQ      'A'
     C                   Z-ADD     EPAAPR        AM(I1)
     C                   ELSE
     C                   Z-ADD     EPEAPR        AM(I1)
     C                   END
     C                   ENDSR
     C*
     C***  Begin Add  ***  11/19/09  **************************************
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR
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
     C***  End   Add  ***  11/19/09  **************************************
     C*================================================================
     C*  LOAD FORWARD SUBROUTINE
     C*================================================================
     C*
     C     LOAD          BEGSR
     C                   EXSR      CLR
     C     OVER          TAG
     C*
     C*  END OF FILE
     C*
     C   60              MOVE      '1'           XXUP              1
     C   60              GOTO      DONE
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     EPKY1         READE     PEP01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONE
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVER
     C     DONE          TAG
     C                   Z-ADD     DT(1)         KEYF(1)
     C                   Z-ADD     DT(10)        KEYF(2)
     C                   MOVEL     CL(1)         KEYM(1)
     C                   MOVEL     CL(10)        KEYM(2)
     C                   ENDSR
     C*
     C*================================================================
     C*  LOAD BACKWARD SUBROUTINE
     C*================================================================
     C*
     C     LOADP         BEGSR
     C                   EXSR      CLR
     C                   Z-ADD     11            I1
     C     OVERP         TAG
     C*
     C*  BEGINNING OF FILE
     C*
     C     *IN60         IFEQ      '1'
     C                   EXSR      LOADB
     C                   GOTO      DONEP
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C                   SUB       1             I1
     C                   EXSR      FILL
     C                   READP     PEP01                                  60
     C  N60              DO
     C     EPCO#         IFNE      XXCO#
     C     EPFND         ORNE      XXFND
     C     EPEMP#        ORNE      SCEMP#
     C     EPDIV         ORNE      SCDIV
     C     EPFYR         ORNE      XXFYR
     C                   MOVE      '1'           *IN60
     C                   END
     C                   END
     C   60              DO
     C                   MOVE      '1'           XXDOWN
     C                   GOTO      OVERP
     C                   END
     C     I1            COMP      1                                  61
     C   61              GOTO      OVERP
     C     DONEP         TAG
     C                   Z-ADD     DT(1)         KEYF(1)
     C                   Z-ADD     DT(10)        KEYF(2)
     C                   MOVEL     CL(1)         KEYM(1)
     C                   MOVEL     CL(10)        KEYM(2)
     C                   ENDSR
     C*
     C*================================================================
     C*  LOAD FROM BEGINNING OF FILE SUBROUTINE
     C*================================================================
     C*
     C     LOADB         BEGSR
     C                   EXSR      CLR
     C*
     C*  POSITION AT BEGINNING OF FILE
     C*
     C                   MOVE      '1'           XXDOWN            1
     C                   MOVE      *BLANK        XXUP
     C                   Z-ADD     *ZERO         XXEFD1
     C                   Z-ADD     *ZERO         XXCLAS
     C     EPKY2         SETLL     PEP01
     C     EPKY1         READE     PEP01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP              1
     C                   GOTO      DONEB
     C                   END
     C*
     C*  FILL SCREEN AND GET NEXT RECORD
     C*
     C     OVERB         TAG
     C                   ADD       1             I1
     C                   EXSR      FILL
     C     EPKY1         READE     PEP01                                  60
     C   60              DO
     C                   MOVE      '1'           XXUP
     C                   GOTO      DONEB
     C                   END
     C     I1            COMP      10                                   61
     C   61              GOTO      OVERB
     C     DONEB         ENDSR
     C*
     C***  Begin Add  11/10/10  *******************************************
     C*================================================================
     C*   Load HCN Data to overrride files
     C*================================================================
     C     LDHCN         BEGSR
     C*
     C     XXOPNH        IFEQ      *OFF
     C                   OPEN      WDHCL
     C                   MOVE      *ON           XXOPNH
     C                   ENDIF
     C*
     C     HCKY          CHAIN     PHC0R                              61
     C     *IN61         DOWEQ     *OFF
     C     HCKY          CHAIN     PHC00                              60
     C     *IN60         IFEQ      *ON
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        HCUSER
     C                   MOVEL     Q1PGM         HCPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HCDT
     C*
     C                   Z-ADD     DSTIME        HCTIME
     C                   WRITE     PHC00
     C                   ENDIF
     C     HCKY          READE     PHC0R                                  61
     C                   ENDDO
     C*
     C     XXOPNH        IFEQ      *ON
     C                   CLOSE     WDHCL
     C                   MOVE      *OFF          XXOPNH
     C                   ENDIF
     C*
     C                   ENDSR
     C***  End   Add  11/10/10  *******************************************
     C*
     C*================================================================
     C*   SUBROUTINE LDOVRF
     C*   LOAD DATA TO OVERRIDE FILES
     C*================================================================
     C*
     C     LDOVRF        BEGSR
     C*
     C     EPKY1         CHAIN     PEP02                              60
     C     *IN60         IFEQ      '1'
     C     XXOPN         IFEQ      '0'
     C                   OPEN      WMEPL
     C                   MOVE      '1'           XXOPN
     C                   END
     C     EPKY1         CHAIN     PEP01                              61
     C     *IN61         DOWEQ     '0'
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C***  BEGIN DEL  ***  11/19/98  *********************************
     C********             ADD  1         DSTIME
     C***  END   DEL  ***  11/19/98  *********************************
     C                   Z-ADD     DSTIME        EPTIME
     C  N61              WRITE     PEP02
     C     EPKY1         READE     PEP01                                  61
     C                   END
     C     XXOPN         IFEQ      '1'
     C                   CLOSE     WMEPL
     C                   MOVE      '0'           XXOPN
     C                   END
     C                   END
     C*
     C     ELKY          CHAIN     PEL02                              60
     C     *IN60         IFEQ      '1'
     C     ELKY          CHAIN     PEL01                              61
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C***  BEGIN DEL  ***  11/19/98  *********************************
     C********             ADD  1         DSTIME
     C***  END   DEL  ***  11/19/98  *********************************
     C                   Z-ADD     DSTIME        ELTIME
     C  N61              WRITE     PEL02
     C                   END
     C*
     C*  EXECUTE OVERRIDE STATEMENTS
     C*
     C                   EXSR      BGNOVR
     C*
     C***  Begin Add  11/10/10  *******************************************
     C                   EXSR      LDHCN
     C***  End   Add  11/10/10  *******************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   SUBROUTINE BGNOVR
     C*   EXECUTES THE OVERRIDE STATEMENTS
     C*================================================================
     C*
     C     BGNOVR        BEGSR
     C*
     C*  OVERRIDE DATABASE FILE WDELL TO WAELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDELP TO WAELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*** BEGIN ADD ***  07/22/99  ************************************
     C*
     C*  OVERRIDE DATABASE FILE WDELL1 TO WAELL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(15)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  07/22/99  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C*  OVERRIDE DATABASE FILE WDILL TO WAILL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(17)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDMPL TO WAMPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(19)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  02/27/06  ************************************
     C*
     C*  OVERRIDE DATABASE FILE WDPPL TO WAPPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(21)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  02/27/06  ************************************
     C*
     C*  OVERRIDE DATABASE FILE WMEPL1 TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL6 TO WAEPL6
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(13)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPP TO WAEPP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDDSL TO WADSL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*** BEGIN ADD ***  06/15/09  *************************************
     C*
     C*  OVERRIDE DATABASE FILE WMEPL1E TO WAEPL1E
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(23)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  06/15/09  *************************************
     C*** BEGIN ADD ***  10/27/09  ************************************
     C*
     C*  OVERRIDE DATABASE FILE WMEPL6E TO WAEPL6E
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(25)
     C                   PARM                    LENGTH           15 5
     C*** END   ADD ***  10/27/09  ************************************
     C*
     C*** BEG ADD ***** 08/30/12 ***************************************
      *  OVERRIDE DATABASE FILE WMLDL1 TO WALDL1
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(44)
     C                   PARM                    LENGTH           15 5
      *  OVERRIDE DATABASE FILE WMLDL1 TO WALDL2
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(48)
     C                   PARM                    LENGTH           15 5
      *  OVERRIDE DATABASE FILE WMLDP  TO WALDP
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(46)
     C                   PARM                    LENGTH           15 5
     C*** END ADD ***** 08/30/12 ***************************************
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C                   EXSR      OVRHC
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C                   ENDSR
     C*
     C***  Begin Add  ***  11/10/10  **************************************
     C*================================================================
     C* Execute overrrides for WDHC*
     C*================================================================
     C     OVRHC         BEGSR
     C*
     C*  Check for REVISED member in WDHCP
     C                   CALL      'SRCHKMBR'                           99
     C                   PARM      'WDHCP'       ##OBJ            10
     C                   PARM      '*FILE'       ##TYPE            7
     C                   PARM      'REVISED'     ##MBR            10
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFEQ      *ON
     C*  Create members for WDHCP/L
     C                   CALL      'QCMDEXC'                            99
     C                   PARM                    QCA(35)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'                            99
     C                   PARM                    QCA(36)
     C                   PARM      55            LENGTH           15 5
     C                   ENDIF
     C*
     C*  Override HCD Program File (WDHCL*)
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(27)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(29)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(31)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(33)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(37)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(39)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(41)
     C                   PARM      55            LENGTH           15 5
     C*
     C                   ENDSR
     C***  End   Add  ***  11/10/10  **************************************
     C*
     C*** BEG ADD ***** 08/30/12 ***************************************
      *================================================================
      * EXECUTE OVER-RIDES FOR LARGE DED FILE
      *================================================================
     C     OVRLDF        BEGSR
      *  OVERRIDE DATABASE FILE WMLDL1 TO WALDL1
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(44)
     C                   PARM                    LENGTH           15 5
      *  OVERRIDE DATABASE FILE WMLDL1 TO WALDL2
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(48)
     C                   PARM                    LENGTH           15 5
      *  OVERRIDE DATABASE FILE WMLDP  TO WALDP
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(46)
     C                   PARM                    LENGTH           15 5
     C                   ENDSR
     C*** END ADD ***** 08/30/12 ***************************************
     C*================================================================
     C*   SUBROUTINE BILL
     C*   CALLS THE BILL WITH THE OVERRIDES AND THEN DELETES THE
     C*   OVERRIDES
     C*================================================================
     C*
     C     BILL          BEGSR
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCEMP#        QQEMP#
     C                   MOVE      SCDIV         QQDIV
     C*
     C*** BEGIN CHG ***  05/01/98  ************************************
     C*****                Z-ADDUPPRDT    SRDTI
     C*****                CALL 'SRCVTDAT'DTMDYY
     C*****                MOVELSRDTO     WKDATE  80
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         WKDATE            8 0
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* REVISED INTERIM DAYS PLUS THE PRESENT DATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          WKDATE
     C                   ENDIF
     C*** BEGIN ADD ***  11/30/04  ************************************
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        WKDATE
     C                   ENDIF                                                  XXACDT IFNE
     C*** END ADD *****  11/30/04  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END CHG *****  05/01/98  ************************************
     C*
     C                   MOVE      WKDATE        QQDATE            8
     C                   MOVE      *BLANKS       QQESCD            2
     C                   MOVE      'Y'           QQOVR             1
     C                   MOVE      *BLANKS       QQSRCD            2
     C                   MOVE      *BLANKS       QQPRT             1
     C                   MOVE      *BLANKS       QQAJCD            2
     C                   MOVE      'W'           QQQTE             1
     C                   MOVE      ' '           QQRNB1            1
     C*** BEGIN ADD ***  09/18/07  ****************************************
     C     *IN67         IFEQ      *ON
     C                   MOVE      '#'           QQRNB1
     C                   ENDIF
     C*** END ADD *****  09/18/07  ****************************************
     C                   CALL      'W8010R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQDATE
     C                   PARM                    QQESCD
     C                   PARM                    QQOVR
     C                   PARM                    QQSRCD
     C                   PARM                    QQPRT
     C                   PARM                    QQAJCD
     C                   PARM                    QQQTE
     C                   PARM                    QQRNB1
     C*** BEGIN ADD ***  10/01/01  ************************************
     C     XXLOSS        IFEQ      'Y'
     C                   CALL      'W8010R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQDATE
     C                   PARM                    QQESCD
     C                   PARM                    QQOVR
     C                   PARM                    QQSRCD
     C                   PARM                    QQPRT
     C                   PARM                    QQAJCD
     C                   PARM                    QQQTE
     C                   PARM                    QQRNB1
     C                   ENDIF                                                  XXLOSS IFEQ
     C*** END ADD *****  10/01/01  ************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   SUBROUTINE DLTOVR
     C*   DELETES ALL OVERRIDES
     C*================================================================
     C*
     C     DLTOVR        BEGSR
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(11)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(12)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(14)
     C                   PARM                    LENGTH           15 5
     C*** BEGIN ADD ***  07/22/99  ************************************
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(16)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  07/22/99  ************************************
     C*** BEGIN ADD ***  11/15/99  ************************************
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(18)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(20)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  11/15/99  ************************************
     C*** BEGIN ADD ***  02/27/06  ************************************
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(22)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  02/27/06  ************************************
     C*** BEGIN ADD ***  06/15/09  *************************************
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(24)
     C                   PARM                    LENGTH           15 5
     C*** END ADD *****  06/15/09  *************************************
     C*** BEGIN ADD ***  10/27/09  ************************************
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(26)
     C                   PARM                    LENGTH           15 5
     C*** END   ADD ***  10/27/09  ************************************
     C***  Begin Add  ***  11/10/10  **************************************
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(28)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(30)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(32)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(34)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(38)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(40)
     C                   PARM      55            LENGTH           15 5
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(42)
     C                   PARM      55            LENGTH           15 5
     C***  End   Add  ***  11/10/10  **************************************
      *
     C*** BEG ADD ***** 08/30/12 ***************************************
      *  DELETE OVERRIDE DATABASE FILE TO WALDL1
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(45)
     C                   PARM      55            LENGTH           15 5
      *  DELETE OVERRIDE DATABASE FILE TO WALDL2
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(49)
     C                   PARM      55            LENGTH           15 5
      *  DELETE OVERRIDE DATABASE FILE TO WALDP
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(47)
     C                   PARM      55            LENGTH           15 5
     C*** END ADD ***** 08/30/12 ***************************************
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   SUBROUTINE SRWASB
     C*   THIS SUBROUTINE ACCUMLATES ALL 'B' AND 'A' A/R RECORDS
     C*   WITH POSTING DATES BETWEEN XXDTE1 AND XXDTE2
     C*================================================================
     C*
     C     SRWASB        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXAMT1
     C***BEGIN ADD ****  05/15/07 *************************************
     C                   Z-ADD     *ZEROS        X1AA1
     C                   Z-ADD     *ZEROS        X2AA1
     C                   Z-ADD     *ZEROS        X3AA1
     C                   Z-ADD     *ZEROS        X4AA1
     C                   Z-ADD     *ZEROS        X5AA1
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     *ZEROS        X6AA1
     C                   Z-ADD     *ZEROS        X7AA1
     C*** END ADD *****  12/11/08  ************************************
     C****END ADD ****  05/15/07 *************************************
     C     Y             IFEQ      1
     C     XXFUT         ANDEQ     '0'
     C                   MOVE      '1'           XXFUT
     C                   MOVE      '0010101'     XXPODT
     C                   ELSE
     C                   Z-ADD     XXDTE1        XXPODT
     C                   ENDIF                                                  Y EQ 1
     C                   MOVE      'B'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C*** BEGIN ADD ***  10/02/03  ************************************
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C*** BEGIN ADD ***  10/15/03  ************************************
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** END ADD *****  10/15/03  ************************************
     C*** END ADD *****  10/02/03  ************************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
      *** beg add ***** 07/02/13 **************************************
     C     EAARCD        ANDNE     F1DVBL
      *** end add ***** 07/02/13 **************************************
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C*** BEGIN ADD ***  01/25/05  ************************************
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C*
     C     *IN50         DOWEQ     '0'
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
      *** beg add ***** 07/02/13 **************************************
     c     EAarcd        Andne     F1dvbl
      *** end add ***** 07/02/13 **************************************
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C                   END
     C*
     C*** END ADD *****  01/25/05  ************************************
     C*
     C                   MOVE      'A'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C*** BEGIN ADD ***  10/02/03  ************************************
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C*** BEGIN ADD ***  10/15/03  ************************************
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** END ADD *****  10/15/03  ************************************
     C*** END ADD *****  10/02/03  ************************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C*** BEGIN ADD ***  04/21/06  ************************************
     C*** BEGIN DEL ****** 08/09/10 ************************************
     C********** EAARCD    ANDNEF1RDCD
     C*** END   DEL ****** 08/09/10 ************************************
     C*** END ADD *****  04/21/06  ************************************
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C*** BEGIN ADD ***  01/25/05  ************************************
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C*
     C     *IN50         DOWEQ     '0'
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C*** BEGIN ADD ***  04/21/06  ************************************
     C*** BEGIN DEL ****** 08/09/10 ************************************
     C********** XAARCD    ANDNEF1RDCD
     C*** END   DEL ****** 08/09/10 ************************************
     C*** END ADD *****  04/21/06  ************************************
      *** BEG CHG 01/11/07 ********************************************
     C     TTCTYP        ANDNE     'C'
      *** END CHG 01/11/07 ********************************************
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C                   END
     C*
     C*** END ADD *****  01/25/05  ************************************
     C*
     C*  THIS SECTION ACCUMULATES STATE TAX BILLING RECORDS.
     C*
     C                   Z-ADD     *ZEROS        X1AMT1
     C                   MOVE      'F'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C*** BEGIN ADD ***  10/02/03  ************************************
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C*** BEGIN ADD ***  10/15/03  ************************************
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*** END ADD *****  10/15/03  ************************************
     C*** END ADD *****  10/02/03  ************************************
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C*** BEGIN ADD ***  04/12/99  ************************************
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'S'
     C*** END ADD *****  04/12/99  ************************************
     C                   ADD       EAAMT         X1AMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     TTCTYP        IFEQ      '1'
     C                   ADD       EAAMT         X1AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '2'
     C                   ADD       EAAMT         X2AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '3'
     C                   ADD       EAAMT         X3AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '4'
     C                   ADD       EAAMT         X4AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '5'
     C                   ADD       EAAMT         X5AA1
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     TTCTYP        IFEQ      '6'
     C                   ADD       EAAMT         X6AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '7'
     C                   ADD       EAAMT         X7AA1
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C*** BEGIN ADD ***  01/25/05  ************************************
     C     EAKY          SETLL     PXA01
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C*
     C     *IN50         DOWEQ     '0'
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     XAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
     C*
     C                   MOVEL(P)  XAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'S'
     C                   ADD       EAAMT         X1AMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*** BEGIN ADD ***  05/15/07  ************************************
     C     TTCTYP        IFEQ      '1'
     C                   ADD       EAAMT         X1AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '2'
     C                   ADD       EAAMT         X2AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '3'
     C                   ADD       EAAMT         X3AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '4'
     C                   ADD       EAAMT         X4AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '5'
     C                   ADD       EAAMT         X5AA1
     C                   ENDIF
     C*** BEGIN ADD ***  12/11/08  ************************************
     C     TTCTYP        IFEQ      '6'
     C                   ADD       EAAMT         X6AA1
     C                   ENDIF
     C     TTCTYP        IFEQ      '7'
     C                   ADD       EAAMT         X7AA1
     C                   ENDIF
     C*** END ADD *****  12/11/08  ************************************
     C*** END   ADD ***  05/15/07  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PXA01                                  50
     C  N50              EXCEPT    PXA01E
     C                   END
     C*
     C*** END ADD *****  01/25/05  ************************************
     C*
     C                   ENDSR
     C*** BEG ADD ***** 02/09/98 ***********************************************
     C*================================================================
     C*  WRITE AN INTERIM ADJUSTMENT OF PREMIUM MID TERM ENDORSEMENT
     C*================================================================
     C     WRTPEN        BEGSR
     C*
     C*  GET THE PREVIOUS EMPLOYER MID TERM ENDORSEMENT RECORD
     C*
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
     C*
     C*  GET ENDORS/DARTS INFORMATION
     C*
     C     DXKY          CHAIN     PDX01                              60
     C   60              CLEAR                   PDX01
     C*
     C*  GET AGENT/PRODUCER INFORMATION
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVE      XXCO#         ENCO#
     C                   MOVE      XXFND         ENFND
     C                   MOVE      SCEMP#        ENEMP#
     C                   MOVE      SCDIV         ENDIV
     C                   MOVE      XXFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
     C*
     C                   MOVE      LDEEDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENEEDT
     C******************
      *** BEG CHG ***** 06/13/05 **************************************
     C******************   MOVELEMNAME    ENNAM1
     C******************   MOVELEMFDNM    ENFDN1
     C****BEGIN*ADD***** 03/07/05  ************************************
     C******************
     C******************   MOVELEMNAME    ENENM1
     C******************   MOVELEMNAM2    XXNM10 10 P
     C******************   MOVE XXNM10    ENENM1
     C******************   MOVE EMNAM2    ENENM2
     C******************
     C******************   MOVELEMFDNM    ENEFN1
     C******************   MOVELEMFDN2    XXNM10 10 P
     C******************   MOVE XXNM10    ENEFN1
     C******************   MOVE EMFDN2    ENEFN2
     C*
     C                   MOVEL     SVNAME        ENNAM1
     C                   MOVEL     SVFDNM        ENFDN1
     C*** BEGIN ADD ***  03/07/05  ************************************
     C*
     C                   MOVEL     SVNAME        ENENM1
     C                   MOVEL(P)  SVNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      SVNAM2        ENENM2
     C*
     C                   MOVEL     SVFDNM        ENEFN1
     C                   MOVEL(P)  SVFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      SVFDN2        ENEFN2
     C*
      *** END CHG ***** 06/13/05 **************************************
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
     C*
      **** BEGIN ADD **** 04/01/99 *********************************************
     C                   MOVE      *BLANKS       ENMSFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENILFL
     C                   MOVE      *BLANKS       ENERFL
     C*** BEGIN CHG ***  01/20/04  ************************************
     C     XXPYCG        IFEQ      'Y'
     C                   MOVE      'Y'           ENCRFL
     C                   ELSE
     C                   MOVE      *BLANKS       ENCRFL
     C                   ENDIF
     C*** END CHG *****  01/20/04  ************************************
     C                   MOVE      *BLANKS       ENNOBF
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENCIFL
     C                   MOVE      *BLANKS       ENCMFL
      **** END ADD ****** 04/01/99 *********************************************
     C                   MOVE      'Y'           ENIAPF
     C                   MOVEL     DXPIAP        ENRMCD
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
     C**** BEGIN CHG **** 06/01/01 ************************************
      **** BEGIN ADD **** 07/30/98 *********************************************
     C********** SCMTDS    IFNE *BLANKS
     C**********           MOVELSCMTDS    ENDSC3
     C**********           ENDIF                           SCMTDS IFNE *BLANKS
      **** END   ADD **** 07/30/98 *********************************************
     C     SCDSC1        IFNE      *BLANKS
     C                   MOVEL     SCDSC1        ENDSC1
     C                   MOVEL     SCDSC2        ENDSC2
     C                   MOVEL     SCDSC3        ENDSC3
     C                   ENDIF                                                  SCDSC1 IFNE *BLANKS
     C**** END   CHG **** 06/01/01 ************************************
     C*
     C                   MOVEL     *BLANKS       ENBLIN
     C                   MOVEL     *BLANKS       ENALIN
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
     C*
      **** BEGIN ADD **** 10/14/98 *********************************************
     C                   MOVE      F2CAR#        ENCAR1
      **** END   ADD **** 10/14/98 *********************************************
      **** BEGIN ADD **** 11/23/98 *********************************************
     C                   Z-ADD     ELPFDT        ENPFD1
      **** END   ADD **** 11/23/98 *********************************************
     C                   WRITE     PEN01
     C*
     C                   ENDSR
     C*================================================================
     C*** END ADD ***** 02/09/98 ***********************************************
     C*** BEGIN ADD ***  10/01/01  ************************************
     C*****************************************************************
     C*
     C* CHECK IF POLICY NEEDS TO RUN CALCULATIONS TWICE.
     C*
     C*****************************************************************
     C*
     C     CHKLC         BEGSR
     C*
     C     EAKY4         CHAIN     PEL03                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     SCKY          SETGT     PSC01
     C     SCKY1         READPE    PSC01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     S2PRBS        ANDGT     *ZERO
     C*
     C                   MOVE      'Y'           XXLOSS
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY4         READE     PEL03                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR                                                  CHKLC SUBR
     C*
     C*****************************************************************
     C*** END ADD *****  10/01/01  ************************************
     C*** BEGIN ADD ***  04/21/03  ************************************
     C*****************************************************************
     C*
     C* SUBROUTINE TO DETERMINE IF A DATE RANGE CONTAINS FEBRUARY 29TH.
     C*
     C     SRLEAP        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM            7 0
     C*
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO              7 0
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
     C*****************************************************************
     C*** END ADD *****  04/21/03  ************************************
     C*** BEG ADD ***** 01/03/05 **************************************
      *================================================================
      *  SRCADJ - DISPLAY TOTAL OF ADJUSTMENTS IN LUMP SUM WINDOW
      *================================================================
     C     SRCADJ        BEGSR
      *
     C     XXDIVC        IFEQ      1
      *
      *  CROSS FOOT PROGRAM ARRAY FOR SINGLE DIVISION POLICIES
      *
     C     BSFREQ        IFEQ      'T'
     C     BSDOWK        ORNE      *BLANKS
     C                   XFOOT     WKA           XXPA             15 2
     C                   XFOOT     WKT           XXP1             15 2
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   XFOOT     WK1           XXA1             15 2
     C                   XFOOT     WK2           XXA2             15 2
     C                   XFOOT     WK3           XXA3             15 2
     C                   XFOOT     WK4           XXA4             15 2
     C                   XFOOT     WK5           XXA5             15 2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   XFOOT     WK6           XXA6             15 2
     C                   XFOOT     WK7           XXA7             15 2
     C*** END ADD *****  12/11/08  ************************************
     C*** END  ADD ****  05/15/07  ************************************
      *** BEG CHG ***** 10/04/07 *************************************
     C*****************    Z-ADDDA,1      XXDA    70
     C                   Z-ADD     DA(1)         XXDA              8 0
      *** END CHG ***** 10/04/07 *************************************
     C                   ELSE
     C                   XFOOT     PA            XXPA
     C                   XFOOT     P1            XXP1
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   XFOOT     A1            XXA1
     C                   XFOOT     A2            XXA2
     C                   XFOOT     A3            XXA3
     C                   XFOOT     A4            XXA4
     C                   XFOOT     A5            XXA5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   XFOOT     A6            XXA6
     C                   XFOOT     A7            XXA7
     C*** END ADD *****  12/11/08  ************************************
     C*** END  ADD ****  05/15/07  ************************************
      *** BEG CHG ***** 10/04/07 *************************************
     C*****************    Z-ADDDA,1      XXDA    70
     C                   Z-ADD     DA(1)         XXDA              8 0
      *** END CHG ***** 10/04/07 *************************************
     C                   ENDIF                                                  BSFREQ = 'T'
      *
     C                   Z-ADD     XXPA          SCPRAJ
     C                   ADD       XXP1          SCPRAJ
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   ADD       XXA1          SCPRAJ
     C                   ADD       XXA2          SCPRAJ
     C                   ADD       XXA3          SCPRAJ
     C                   ADD       XXA4          SCPRAJ
     C                   ADD       XXA5          SCPRAJ
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   ADD       XXA6          SCPRAJ
     C                   ADD       XXA7          SCPRAJ
     C*** END ADD *****  12/11/08  ************************************
     C*** END  ADD ****  05/15/07  ************************************
     C                   ELSE
      *
      *  TOTAL AMOUNTS FROM WAEAP IF MULTI-DIVISION
      *
     C                   CLEAR                   XXPATT
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
      *
      *  EATRDT WILL BE LESS THAN 55 IF WEEKLY OR TWICE MONTHLY AND INDICATES
      *  THE NUMBER OF INSTALLMENTS.
      *
     C     EATRDT        IFLT      55
     C     EAAM02        MULT      EATRDT        XXPA
     C                   ADD       EAAM01        XXPA
     C                   ELSE
     C                   XFOOT     AR            XXPA
     C                   ENDIF
      *
     C                   ADD       XXPA          XXPATT           15 2
      *
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   Z-ADD     XXPATT        SCPRAJ
      *
     C                   ENDIF                                                  XXDIVC = 1
      *
     C                   ENDSR
      *================================================================
      *  SRLUMP - LUMP SUM ADJUSTMENTS
      *================================================================
     C     SRLUMP        BEGSR
      *
     C     XXDIVC        IFEQ      1
      *
     C     BSFREQ        IFEQ      'T'
     C     BSDOWK        ORNE      *BLANKS
     C                   XFOOT     WKA           XXPA             15 2
     C                   XFOOT     WKT           XXP1             15 2
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   XFOOT     WK1           XXA1             15 2
     C                   XFOOT     WK2           XXA2             15 2
     C                   XFOOT     WK3           XXA3             15 2
     C                   XFOOT     WK4           XXA4             15 2
     C                   XFOOT     WK5           XXA5             15 2
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   XFOOT     WK6           XXA6             15 2
     C                   XFOOT     WK7           XXA7             15 2
     C*** END ADD *****  12/11/08  ************************************
     C*** END  ADD ****  05/15/07  ************************************
      *** BEG CHG ***** 10/04/07 *************************************
     C*****************    Z-ADDDA,1      XXDA    70
     C                   Z-ADD     DA(1)         XXDA              8 0
      *** END CHG ***** 10/04/07 *************************************
     C                   MOVE      *OFF          *IN62
     C                   ELSE
     C                   XFOOT     PA            XXPA
     C                   XFOOT     P1            XXP1
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   XFOOT     A1            XXA1
     C                   XFOOT     A2            XXA2
     C                   XFOOT     A3            XXA3
     C                   XFOOT     A4            XXA4
     C                   XFOOT     A5            XXA5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   XFOOT     A6            XXA6
     C                   XFOOT     A7            XXA7
     C*** END ADD *****  12/11/08  ************************************
     C*** END  ADD ****  05/15/07  ************************************
     C                   Z-ADD     DA(1)         XXDA
     C                   ENDIF
      *
     C                   CLEAR                   PA
     C                   CLEAR                   P1
     C***BEGIN ADD ****  05/15/07  ************************************
     C                   CLEAR                   A1
     C                   CLEAR                   A2
     C                   CLEAR                   A3
     C                   CLEAR                   A4
     C                   CLEAR                   A5
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   CLEAR                   A6
     C                   CLEAR                   A7
     C*** END ADD *****  12/11/08  ************************************
     C*** END  ADD ****  05/15/07  ************************************
     C                   CLEAR                   DA
     C                   Z-ADD     XXPA          PA(1)
     C                   Z-ADD     XXP1          P1(1)
     C***BEGIN ADD ****  05/15/07  *********************************** *
     C                   Z-ADD     XXA1          A1(1)
     C                   Z-ADD     XXA2          A2(1)
     C                   Z-ADD     XXA3          A3(1)
     C                   Z-ADD     XXA4          A4(1)
     C                   Z-ADD     XXA5          A5(1)
     C*** BEGIN ADD ***  12/11/08  ************************************
     C                   Z-ADD     XXA6          A6(1)
     C                   Z-ADD     XXA7          A7(1)
     C*** END ADD *****  12/11/08  ************************************
     C***BEGIN ADD ****  05/15/07  *********************************** *
     C                   Z-ADD     XXDA          DA(1)
      *
      *  A/R WILL BE WRITTEN AT END OF PROGRAM FOR SINGLE DIVISION POLICIES
      *
     C                   ELSE
      *
      *  A/R WILL HAVE TO BE ADJUSTED FOR MULTI-DIVISION POLICIES
      *
     C     EAKY4         CHAIN     PEA04                              60
     C     *IN60         DOWEQ     *OFF
      *
      *  FOR WEEKLY OR TWICE MONTHLY BILLING PLANS
      *
     C     EATRDT        IFLT      55
      *
     C     EAAM02        MULT      EATRDT        XXPA
     C                   ADD       EAAM01        XXPA
     C                   Z-ADD     XXPA          EAAM01
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
      *
     C     EAAM01        IFNE      *ZERO
     C                   CLEAR                   EAAM02
     C                   CLEAR                   EAPO02
     C                   CLEAR                   EAEDIT
     C                   UPDATE    PEA04
     C                   ELSE
     C***  Begin Add  ***  11/19/09  **************************************
     C                   EXSR      HOLDAR
     C***  End   Add  ***  11/19/09  **************************************
     C                   DELETE    PEA04
     C                   ENDIF
      *
      *  FOR MONTHLY PLANS
      *
     C                   ELSE
     C                   XFOOT     AR            XXPA
     C                   CLEAR                   AR
     C                   Z-ADD     XXPA          EAAM01
     C*** BEG ADD ***** 04/20/11 ******************************************
     C*** BEG CHG ***** 06/27/11 ******************************************
     C***********          Z-ADDSCDAT1    SVDAT1  80
     C***********          Z-ADDSCDAT1    SRDTI
     C***********          CALL 'SRCVTDAT'DTCYMD
     C***********          MOVELSRDTO     SVPO01  70
     C***********          CLEARDA
     C***********          CLEARJ1D
     C***********          Z-ADDSVDAT1    SCDAT1
     C***********          Z-ADDSVPO01    EAPO01
     C     1             DO        12            X4                3 0
     C     J1D(X4)       IFNE      *ZERO
     C                   Z-ADD     J1D(X4)       SVPO01            7 0
     C                   CLEAR                   J1D
     C                   Z-ADD     SVPO01        EAPO01
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*** END CHG ***** 06/27/11 ******************************************
     C*** END ADD ***** 04/20/11 ******************************************
     C                   UPDATE    PEA04
     C                   ENDIF
      *
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ENDIF                                                  XXDIVC = 1
      *
     C                   ENDSR                                                  SRLIMP
     C***** BEGIN ADD ****** 11/29/10 *********************************
     C*---------------------------------------------------------------
     C* CHECK RECORD IN USE
     C*---------------------------------------------------------------
     C     CHKRIU        BEGSR
     C                   MOVE      *OFF          @@RTCD
     C     RIUKY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RIJOBN        JOB6A             6
     C                   MOVEL(P)  RIJOB         SCJOB
     C     SCJOB         CAT       '/':0         SCJOB
     C     SCJOB         CAT       JOB6A:0       SCJOB
     C     SCJOB         CAT       '/':0         SCJOB
     C     SCJOB         CAT       RIUSER:0      SCJOB
     C                   EXFMT     WDWMSG4
     C                   MOVE      *ON           @@RTCD
     C                   GOTO      TERM
     C                   ELSE
     C                   TIME                    DSDTTM
     C                   MOVEL     'WMEMP'       RINAM
     C                   MOVE      @@RRN2        RIRRN
     C                   MOVE      XXCO#         RICO#
     C                   MOVE      XXFND         RIFND
     C                   MOVE      SCEMP#        RIEMP#
     C                   MOVE      Q1JOBN        RIJOBN
     C                   MOVE      Q1JOB         RIJOB
     C                   MOVE      Q1USER        RIUSER
     C                   MOVE      DSTIME        RITIME
     C                   MOVE      DSDATE        RIDATE
     C                   WRITE     PRI01
     C                   ENDIF                                                  *IN60
     C                   ENDSR
     C***** END ADD   ****** 11/29/10 *********************************
     C*---------------------------------------------------------------
     C* INITIALIZATION
     C*---------------------------------------------------------------
     C     *INZSR        BEGSR
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C     SVLDA         IFEQ      *BLANKS
     C                   MOVE      *ZEROS        LDENDA
     C                   MOVE      *ZEROS        LDMYRR
     C                   MOVE      *ZEROS        LDEEDT
     C                   MOVE      *ZEROS        LDFYR
     C                   ENDIF
     C                   OUT       *DTAARA
     C                   ENDSR
     C*** END ADD ***** 01/03/05 **************************************
     OPEA01     E            PEA01E
     O*** BEGIN ADD ***  01/25/05  ************************************
     OPXA01     E            PXA01E
     O*** END ADD *****  01/25/05  ************************************
**  QCAEXEC TABLE
OVRDBF FILE(WDELL) TOFILE(WAELL)
OVRDBF FILE(WMEPL1) TOFILE(WAEPL1)
OVRDBF FILE(WMEPL) TOFILE(WAEPL)
DLTOVR FILE(WDELL)
DLTOVR FILE(WMEPL1)
DLTOVR FILE(WMEPL)
OVRDBF FILE(WDDSL) TOFILE(WADSL)
DLTOVR FILE(WDDSL)
OVRDBF FILE(WDELP) TOFILE(WAELP)
OVRDBF FILE(WMEPP) TOFILE(WAEPP)
DLTOVR FILE(WDELP)
DLTOVR FILE(WMEPP)
OVRDBF FILE(WMEPL6) TOFILE(WAEPL6)
DLTOVR FILE(WMEPL6)
OVRDBF FILE(WDELL1) TOFILE(WAELL1)
DLTOVR FILE(WDELL1)
OVRDBF FILE(WDILL) TOFILE(WAILL)
DLTOVR FILE(WDILL)
OVRDBF FILE(WDMPL) TOFILE(WAMPL)
DLTOVR FILE(WDMPL)
OVRDBF FILE(WDPPL) TOFILE(WAPPL)
DLTOVR FILE(WDPPL)
OVRDBF FILE(WMEPL1E) TOFILE(WAEPL1E)
DLTOVR FILE(WMEPL1E)
OVRDBF FILE(WMEPL6E) TOFILE(WAEPL6E)
DLTOVR FILE(WMEPL6E)
OVRDBF FILE(WDHCP)  MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCP)  LVL(*JOB)                              11/10/10
OVRDBF FILE(WDHCL)  MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCL)  LVL(*JOB)                              11/10/10
OVRDBF FILE(WDHCL1) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCL1) LVL(*JOB)                              11/10/10
OVRDBF FILE(WDHCL3) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCL3) LVL(*JOB)                              11/10/10
ADDPFM FILE(WDHCP) MBR(REVISED)                            11/10/10
ADDLFMBRS PHYFILE(WDHCP)                                   11/10/10
OVRDBF FILE(WDHCL4) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCL4) LVL(*JOB)                              11/10/10
OVRDBF FILE(WDHCL5) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCL5) LVL(*JOB)                              11/10/10
OVRDBF FILE(WDHCL6) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCL6) LVL(*JOB)                              11/10/10
DLTOVR FILE(WDHCLR) LVL(*JOB)                              11/10/10
OVRDBF FILE(WMLDL1) TOFILE(WALDL1)                         08/30/12
DLTOVR FILE(WMLDL1)                                        08/30/12
OVRDBF FILE(WMLDP) TOFILE(WALDP)                           08/30/12
DLTOVR FILE(WMLDP)                                         08/30/12
OVRDBF FILE(WMLDL2) TOFILE(WALDL2)                         08/30/12
DLTOVR FILE(WMLDL2)                                        08/30/12
** DAYS OF WEEK FOR ARRAY                                              ***
MOTUWETHFRSASUMOTUWETHFRSA
** DEFAULT ASSESSMENT HEADINGS
1st Assessment
2nd Assessment
3rd Assessment
4th Assessment
5th Assessment
6th Assessment
7th Assessment
**
312831303130313130313031
