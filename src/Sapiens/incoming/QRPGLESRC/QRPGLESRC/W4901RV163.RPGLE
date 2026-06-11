     Hcopyright('(c) 2024 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W4901R - CALENDAR EARNED/UNEARNED PREMIUM REPORT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM PRINTS THE CALENDAR EARNED/UNEARNED PREMIUM    *
     H*   REPORT FOR THE REQUESTED DATE RANGE.                        *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W4901C                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: C:57                                              *
     H* CREATED: 12/12/97               PROJECT: CM036                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/17/24 C:344 Project: WC2952                                *
     H*                Increase rate field to zoned 8.3.              *
     H*                                                               *
     H* 06/21/16 C:136 Project: SY371                                 *
     H*                Implement Scriptura Phase 2 processing for     *
     H*                Demo. Still need to create the Cancellation    *
     H*                Report and add the parameters recorded on      *
     H*                the routing page.                              *
     H*                                                               *
     H* 12/15/14 C:136 Support: WA146 87516                           *
     H*               -Corrections in S/R Man Prem Calc               *
     H*               -Suppress CX Report if File Only                *
     H*                                                               *
     H* 05/02/12 C:279 SUPPORT: WA133  79634                          *
     H*                Created new version.                           *
     H*                Correct page break error in Summary Report.    *
     H*                                                               *
     H* 08/11/11 C:136 SUPPORT: WA146 73783                           *
     H*                CREATE V111 VERSION.                           *
     H*                CORRECT PYEA CALC SO THAT IT PROCESSES         *
     H*                TRANSACTIONS THAT HAVE A TRANS DATE EQUAL      *
     H*                TO PRIOR YEAR END VALUE CORRECTLY              *
     H*                                                               *
     H* 09/03/10 C:190 SUPPORT: 74046                                 *
     H*                Created new version.                           *
     H*                Corrected Commission Rate.                     *
     H*                                                               *
     H* 06/21/10 C:136 SUPPORT: WC1776                                *
     H*                REMOVE PROJECT, IT IS UNDERGOING RE-DESIGN     *
     H*                                                               *
     H* 06/09/10 C:136 SUPPORT: WA132 72959                           *
     H*                FIXED DISCREPANCY BETWEEN MDEA AND YDEA        *
     H*                WHEN S/R ADJUSTMENT NEGATES REVISED BILL'S     *
     H*                MDWR CALC                                      *
     H*                                                               *
     H* 06/03/10 C:136 SUPPORT: WA099 71736                           *
     H*                CORRECT CX ERROR REPORT                        *
     H*                                                               *
     H* 05/21/10 C:136 SUPPORT: 72217                                 *
     H*                CORRECT ROUTING FOR SUMMARY AND ERROR REPORT   *
     H*                                                               *
     H* 02/15/10 C:136 SUPPORT                                        *
     H*               -CORRECT MANUAL PREMIUM CALCULATION             *
     H*               -CORRECT ROUTING                                *
     H*               -CORRECT CANCELLATION ROUTINE, SVCNDT (USED TO  *
     H*                ADJUST CX FOR S/R POLICIES) WAS CARRYING       *
     H*                OVER TO NEXT YEAR INAPPROPRIATELY              *
     H*                                                               *
     H* 01/04/10 C:136 PROJECT: 25WRE                                 *
     H*                ADD BILLING SCHEDULE CODE AND BILLING TYPE     *
     H*                (ESTIMATED OR S/R) TO OUTFILE                  *
     H*                                                               *
     H* 12/02/09 C:136 PROJECT: 25WPZ                                 *
     H*                ADD FISCAL YEAR PROCESSING                     *
     H*                                                               *
     H* 09/07/09 C:189 PROJECT: WC1776                                *
     H*                CHANGE PER CAPITA CLASS CODES TO STORE PAYROLL *
     H*                IN MULTIPLES OF 10                             *
     H*                                                               *
     H* 05/12/09 C:136 PROJECT: 135UMB                                *
     H*                ADD CARRIER # TO OUTFILE                       *
     H*                                                               *
     H* 03/09/09 C:062 Project: WC1611                                *
     H*                Add new/renewal flag logic                     *
     H*                                                               *
     H* 02/12/09 C:220 Project: 93WHY                                 *
     H*                Additional field - info only out file WFEUP    *
     H*                                                               *
     H* 01/20/09 C:136 SUPPORT: WA139 65431                           *
     H*                IF THE POST DATE IS LESS THAN THE POLICY EFF   *
     H*                DATE, USE THE POLICY EFFECTIVE DATE TO RATE    *
     H*                EARNINGS FOR THE TRANSACTION.  (A)             *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 02/26/08 C:136 SUPPORT: WA129 62179                           *
     H*                CORRECT PRIOR YR EARNED FOR POLICIES AUDITED   *
     H*                ON YEAR END                                    *
     H*                                                               *
     H* 05/25/07 C:136 SUPPORT: WA139 58496                           *
     H*               *TOTAL NEGATIVE PRIOR YEAR UNEARNED PREMIUM     *
     H*               -ADJUST REINSTATEMENT ROUTINE TO ACCOUNT FOR    *
     H*                CX POST DATE BEING ADJUSTED TO FIRST DAY       *
     H*                OF SUBSEQUENT MONTH IF S/R ADJ EXISTS IN       *
     H*                THE CX MONTH.                                  *
     H*               -THE SAVE CX DATE WAS BEING CARRIED OVER IN     *
     H*                SUBSEQUENT TRANSACTIONS. THIS WAS CLEARED TO   *
     H*                PREVENT THIS.                                  *
     H*               -CORRECTED SRREIN SO THAT THE S/R ADJ ARE       *
     H*                NOT INCLUDED IN THE WAPR ARRAY SO THAT         *
     H*                SUBSEQUENT CX TRANSACTIONS DO NO PRORATE       *
     H*                THE S/R ADJ FOR THE REINSTATEMENT.             *
     H*                                                               *
     H* 02/26/07 C:136 SUPPORT: WA129 55471                           *
     H*               -REVERSE CODE THAT BASED REVISED BILL EARNINGS  *
     H*                ON THE POLICY EFFECTIVE DATE INSTEAD OF THE    *
     H*                POST DATE                                      *
     H*               -CHANGE THE EXPECTED PREMIUM ADJUSTMENT TO USE  *
     H*                THE POST DATE OF THE REVISED BILL TO INCLUDE   *
     H*                OR EXCLUDE PRIOR ADJUSMENTS.                   *
     H*                                                               *
     H* 02/09/07 C:136 SUPPORT: WA136 57076                           *
     H*               -NEGATIVE UNEARNED REPORTED. CORRECTED HOW      *
     H*                REPORT HANDLES PRIOR SELF/REPORTED ADJUSTMENTS *
     H*                FOR FLAT CX POLICIES                           *
     H*               -PRIOR AND FUTURE S/R ADJUSTMENTS WERE LOST     *
     H*                IF TRDT IN PRIOR YEAR AND PODT IN CURRENT YEAR *
     H*                SEPERATED PRIOR YEAR AND CURRENT YEAR ADJUST-  *
     H*                MENTS                                          *
     H*                                                               *
     H* 02/05/07 C:136 SUPPORT: WA132 56879                           *
     H*                ACCOUNT FOR S/R BILLINGS THAT OCCUR SUBSEQUENT *
     H*                TO REVISED BILLS THAT POST TO FUTURE DATES.    *
     H*                CALL TO W4901RA IS USED TO DETERMINE IF A      *
     H*                'FUTURE' ADJUSTMENT IS NEEDED.                 *
     H*                                                               *
     H*   !!!!   NOTE: ALL INSTALLS OF W4901R WILL ALSO NEED W4901RA   !!!!!
     H*                                                               *
     H* 01/18/07 C:136 SUPPORT: 139WGP                                *
     H*                USE EHPFDT INSTEAD OF ELPFDT FOR DATE CALC'S   *
     H*                IF EHPFDT EXISTS SO THAT REPORT WILL PROVIDE   *
     H*                RESULTS THAT ARE MORE DUPLICATIBLE             *
     H*                                                               *
     H* 11/28/06 C:136 SUPPORT: WA129 55471   WA139 55951 WA114 56392 *
     H*               -CORRECT EARNED PREMIUM CALC FOR S/R ADJUSTMENTS*
     H*                BY NOT LETTING THE SR ADJ GET PRORATED.        *
     H*               -ADJUST FOR ALL 'EA' TRANSACTIONS REGARDLESS    *
     H*                OF THE REVISED BILL'S POSTING DATE             *
     H*               -STORE PREVIOUS S/R ADJ'S TO 'EH' RECORDS.      *
     H*                THEY WILL BE USED IN THE SRCANX ROUTINE TO     *
     H*                NEGATE S/R ADJUSMENTS IN 'EH' RECORDS.         *
     H*               -CORRECT P1DAYS IN EXPECTED PREMIUM CALCULATION *
     H*                TO USE XXPTDT INSTEAD OF ELPTDT SO THAT EP     *
     H*                ARRAY GETS CALCULATED WITH PROPER XXPROR       *
     H*               -ADD CODE TO ADJUST FOR EXP PREMIUMS IF 'EA'    *
     H*                TRANSACTION RECEIVED BEFORE FIRST BILL.        *
     H*               -ADD CODE TO CX ROUTINE THAT WILL ADJUST THE    *
     H*                WORKING POST DATE TO THE FIRST DAY OF THE      *
     H*                SUBSEQUENT MONTH IF A SELF-REPORT IS RECIEVED  *
     H*                DURING THE SAME MONTH AS THE CANCELLATION      *
     H*               -ADD CODE TO CX AND RE ROUTINES THAT WILL       *
     H*                NOT ALLOW S/R ADJUSTMENTS TO BE PRORATED IN    *
     H*                THE EARNINGS CALC                              *
     H*               -REFINE S/R ADJ TO AFFECT WRTN AND EARNED FOR   *
     H*                PRIOR YEAR OR FOR CURRENT YEAR ONLY            *
     H*                                                               *
     H* 09/20/06 C:136 Project: 93WFW                                 *
     H*              - Assign value of New/Renew flag based on        *
     H*                previous carrier policy information.           *
     H*              - Add RTVCLTID for 93WFW project                 *
     H*                                                               *
     H* 03/23/06 C:136 SUPPORT: WA135 52009                           *
     H*                ENLARGE THE COUNTERS TO ACCOMDATE THE          *
     H*                INCREASED SIZE OF THE ARRAYS. REFERENCE 49698. *
     H*                                                               *
     H* 02/27/06 C:136 SUPPORT: WA093 51465                           *
     H*                CHECK EMPLOYER CROSS-REFERENCE FILE TO         *
     H*                SEE IF POLICY IS NEW OR RENEWAL.               *
     H*                                                               *
     H* 01/30/06 C:136 SUPPORT: WA129 51113                           *
     H*                CORRECT EXPECTED PREM CALCULATION SO THAT      *
     H*                THE NUMBER OF DAYS IN THE POLICY ARE USED      *
     H*                TO PRORATE THE EXPENSE CONSTANT.               *
     H*                                                               *
     H* 01/26/06 C:136 PROJECT: WC1293                                *
     H*                BREAK OUT DOMESTIC TERRORISM CHARGE.  THIS     *
     H*                DOESN'T GET PRINTED.                           *
     H*                                                               *
     H* 12/01/05 C:136 WA135 49698                                    *
     H*                INCREASE NUMBER OF ARRAY ELEMENTS TO PREVENT   *
     H*                QSYSOPR MESSAGE. PPC RAN OVER THE 500 LIMIT.   *
     H*                                                               *
     H* 10/10/05 C:136 WA129 48995                                    *
     H*                CORRECTED CALCULATION FOR XXPTDT (POLICY TO    *
     H*                DATE) FOR INSTANCE WHERE THERE IS ONLY ONE     *
     H*                AUDIT BILLING RECORD FOR PREMIUM HISTORY.  IT  *
     H*                WAS PREVIOUSLY DEFAULTING THIS VALUE TO        *
     H*                ELPFDT + 10000.  THIS WILL CORRECT CALCULATION *
     H*                OF EARNED PREMIUM.                             *
     H*                                                               *
     H* 08/30/05 C:136 WA091 48772                                    *
     H*                FIX EXPECTED PREMIUM ARRAY, INCREASE TO        *
     H*                13 MONTHLY BUCKETS TO HANDLE POLICIES THAT     *
     H*                BEGIN AND END MID-MONTH.                       *
     H*                                                               *
     H* 05/26/05 C:136 WA033 42546                                    *
     H*                VARIOUS TWEAKS TO CX AND EXP CONSTANT.         *
     H*                                                               *
     H* 05/19/05 C:136 SUPPORT: WA109 45715                           *
     H*                ADD CODE SO THAT PROGRAM WILL CREATE AN        *
     H*                ARTIFICIAL REINSTATEMENT AND CANCELLATION      *
     H*                IF AN S/R ENTRY IS REPORTED WHILE THE POLICY   *
     H*                IS IN CX STATUS.                               *
     H*                                                               *
     H* 03/22/05 C:136 SUPPORT: WC1228                                *
     H*                REMOVE TERR CALCULATIONS FROM SR ENTRIES       *
     H*                                                               *
     H* 03/07/05 C:136 SUPPORT: WC1228                                *
     H*                CORRECT WRTN/EARNED TERR CHG IN SRCXAU         *
     H*                                                               *
     H* 01/28/05 C:33  Poject: WC1228                                 *
     H*                Add Written/Earned Terrorism Charge            *
     H*                                                               *
     H* 11/23/04 C:136 SUPPORT: WA129 44496                           *
     H*                Populate seq# field for Quest product.         *
     H*                                                               *
     H* 10/06/04 C:136 SUPPORT: WA109 49367                           *
     H*                If premium level is changed, recalculate       *
     H*                the A/R adjustment to premium. See             *
     H*                I:\WCMS\CEU REPORT\CEU_Expected_Premium_       *
     H*                Adjustment_Algorithm for more information      *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                Add voluntary labor charge calculations.       *
     H*                                                               *
     H* 07/26/04 C:136 PROJECT: 93WEE                                 *
     H*                Add manual premium calculations.               *
     H*                                                               *
     H* 07/21/04 C:136 Support: WA093 42407                           *
     H*               -Additional corrections to expense constant.    *
     H*                The canx's and reinstatements expense          *
     H*                constant level is saved to SVPRTD which        *
     H*                is compared to for expense constant change     *
     H*                for each transaction in WXEUP. In SRCXAU,      *
     H*                S2PRTD which saves the previous EHPRTD,        *
     H*                is used to calculate the exp constant change   *
     H*                which is subtracted from EUXAPR when exp       *
     H*                constant is excluded.
     H*               -Added code to print out the program arrays     *
     H*                for debugging if the CEUPRT *DTAATA is set     *
     H*                to 'Y'.                                        *
     H*                                                               *
     H* 06/09/04 C:33  Seq#: WA109/42821                              *
     H*                If a CX Audit occurs prior to the reinstatement*
     H*                 do NOT reverse the audit, only the CX.        *
     H*                                                               *
     H* 05/24/04 C:136 Support: WC970                                 *
     H*                Modify WC1094 to load the correct state value  *
     H*                from WDELP.ELRTCD into WFEUP.EUST even if      *
     H*                the state sort isn't requested.                *
     H*                WFEUP.EUST was being loaded with 'XX' from     *
     H*                WXEUP.EUST unless the state sort was requested *
     H*                This change is needed for the Clarendon        *
     H*                preprocessor to generate deferred amounts      *
     H*                for self-reporting accounts in W8126R5.        *
     H*                                                               *
     H* 04/27/04 C:136 Support: WC970 42066                           *
     H*                Correct expense constant processing in SRARBL  *
     H*                (S/R Payroll Records). Do not annualize        *
     H*                expense constant if expense constant is set    *
     H*                to charge full expense constant on short term  *
     H*                policies.                                      *
     H*                                                               *
     H* 03/10/04  C:33   Project: WC970   Seq#: WA033/41518           *
     H*                  Correct expense constant processing.         *
     H*                                                               *
     H* 01/08/04  C:33   Project: WC970                               *
     H*                  Correct expense constant processing.         *
     H*                                                               *
     H* 12/09/03 C:136 PROJECT: 109DL                                 *
     H*                ADD PROCESSING FOR 4TH AND 5TH ASSESSMENTS.    *
     H*                                                               *
     H* 12/01/03  C:33   Project: WC970                               *
     H*                  Add processing for Terrorism Risk.           *
     H*                                                               *
     H* 10/22/02  C:142  PROJECT: WC1094                              *
     H*                  ADD PARMS AND CHANGE CODE FOR SORT           *
     H*                                                               *
     H* 06/18/02  C:35   PROJECT: WC970/SUPPORT WA121                 *
     H*                  SINCE CANCELLATION TRANSACTION DOES NOT      *
     H*                  ADJUST UNEARNED EXPENSE CONSTANT, NEED TO    *
     H*                  ADJUST AT TIME OF AUDIT.                     *
     H*                                                               *
     H* 04/05/02  C:65   PROJECT: SUPPORT                             *
     H*                  PULL ASSESSMENT AMOUNTS FROM WDELP FIELDS.   *
     H*                                                               *
     H* 02/04/02  C:35   PROJECT: SUPPORT WA093 #31097                *
     H*                  IF AN AUDIT IS PROCESSED BEFORE THE POLICY   *
     H*                  IS EFFECTIVE, DO NOT SHOW AMOUNTS IN PRIOR   *
     H*                  YEAR AND MONTH IF POLICY IS NOT EFFECTIVE    *
     H*                  IN PRIOR YEAR OR MONTH.                      *
     H*                                                               *
     H* 01/30/02  C:35   PROJECT: SUPPORT WA100                       *
     H*                  NEED TO ACCOUNT FOR POSSIBILITY THAT THERE   *
     H*                  IS NO FIRST "E" OR "R" RECORD WHEN           *
     H*                  EXCLUDING EXPENSE CONSTANT.                  *
     H*                                                               *
     H* 01/21/02  C:35   PROJECT: SUPPORT WA033 #30591 / WA096 30158  *
     H*                  IF EXCLUDE EXPENSE CONSTANT = 'Y', DO NOT    *
     H*                  RE-SUBTRACT IT WHEN CALCULATING EXPECTED     *
     H*                  PREMIUM.  CALCULATE WRITTEN/INFORCE EXPENSE  *
     H*                  CONSTANT EVEN WHEN NOT EXCLUDED.             *
     H*                                                               *
     H* 01/04/02  C:35   PROJECT: SUPPORT WA033 #30591                *
     H*                  CODE TO EXCLUDE EXPENSE CONSTANT WAS NOT     *
     H*                  IMPLEMENTED INTO CANCELLATION AUDIT PROCESS. *
     H*                                                               *
     H* 12/06/01  C:35   PROJECT: SUPPORT WA096 #30158                *
     H*                  NEED TO USE ANNUAL PREMIUM WITHOUT EXPENSE   *
     H*                  CONSTANT WHEN DETERMINING EXPECTED PREMIUM.  *
     H*                                                               *
     H* 10/30/01 C:65 PROJECT: WC970                                  *
     H*               RESET THE FIRST TIME THROUGH FLAGS FOR EACH     *
     H*               GROUP PROCESSED TO PREVENT ERRORS.              *
     H*                                                               *
     H* 10/17/01 C:65 PROJECT: WC970                                  *
     H*               ADD EXTRA PARM FOR CLARENDON REPORTING, TO      *
     H*               ALLOW SPECIFICATION OF PRINT, FILE, OR BOTH.    *
     H*               BREAK OUT THE EXPENSE CONSTANT AS WELL FOR      *
     H*               THE REPORTING FILE WFEUP.                       *
     H*                                                               *
     H* 08/21/01 C:65 PROJECT: SUPP#28643                             *
     H*               PULL ASSESSMENTS PER THE ANNIV DATE IF APPLIC.  *
     H*                                                               *
     H* 07/19/01  C:35   PROJECT: SUPPORT WA112 #27296                *
     H*                  INCORRECT PREMIUM AMOUNT WAS BEING USED WHEN *
     H*                  DETERMINING EXPECTED PREMIUMS.  THIS CAUSED  *
     H*                  A DISCREPANCY ON SHORT-TERM S/R POLICIES.    *
     H*                                                               *
     H* 05/29/01  C:35   PROJECT: WC577                               *
     H*                  LOAD NEW/RENEWAL FLAG IN WFEUP.              *
     H*                                                               *
     H* 02/28/01  C:35   PROJECT: WC577                               *
     H*                  ADD CODE FOR INCLUDE/EXCLUDE EXP CONSTANT.   *
     H*                                                               *
     H* ************************************************************* *
     H* * REMOVED COMMENTS IN SOURCE FOR ALL CHANGES PRIOR TO THIS  * *
     H* * POINT TO MAKE CODE MORE READABLE. C:35 02/05/02           * *
     H* ************************************************************* *
     H*                                                               *
     H* 02/27/01  C:09   PROJECT: 112AS                               *
     H*                  ADDED REGION CODE AND DESCRIPTION TO         *
     H*                  REPORT WORKFILE.                             *
     H*                                                               *
     H* 02/06/01  C:35   PROJECT: SUPPORT WA096 #26222                *
     H*                  INDICATOR 43 WAS BEING USED FOR TWO          *
     H*                  DIFFERENT PURPOSES.  THIS WAS CAUSING AN     *
     H*                  ERROR IN CALCULATION WHEN PROCESSING A       *
     H*                  REPORT-FABRICATED REINSTATEMENT DUE TO A     *
     H*                  REVISED CANCELLATION.  ALSO, TRANSACTION     *
     H*                  DATE WAS NOT BEING LOADED BEFORE PRINT.      *
     H*                                                               *
     H* 01/02/01 C:65  PROJECT: WC1024                                *
     H*                INCLUDE 3RD ASSESSMENTS IN WORKFILE.           *
     H*                                                               *
     H* 01/16/01  C:35   PROJECT: WC981/SUPPORT WA096 #25884          *
     H*                  IF NO PRIOR WDEHP RECORD FOUND, THEN SET     *
     H*                  *IN43 OFF TO INDICATE THAT NO CHANGE IN      *
     H*                  POLICY-TO-DATE HAS OCCURRED.                 *
     H*                                                               *
     H* 09/18/00  C:35   PROJECT: WC981                               *
     H*                  ADJUST CURRENT MONTH-TO-DATE EARNED IN THE   *
     H*                  CASE OF A CHANGE IN POLICY-TO-DATE.          *
     H*                                                               *
     H* 08/22/00  C:35   PROJECT: WC577                               *
     H*                  LOAD RATE SET CODE IN OUTPUT FILE.           *
     H*                                                               *
     H*  08/10/00 C:33 SEQ#: WA109/23913                              *
     H*                CHECK FOR INACTIVE RATE CODES.                 *
     H*                                                               **
     H* 07/24/00  C:65   PROJECT: WC609                               **
     H*                  DO NOT CONSIDER AUDIT REVERSAL RECORDS AS    **
     H*                  AUDITED.                                     **
     H*                                                               *
     H* 06/22/00  C:35   PROJECT: SUPPORT WA033 #23270                *
     H*                  REPORT-FABRICATED REINSTATEMENTS FOR REVISED *
     H*                  CANCELLATIONS NEED TO USE THE GREATER OF THE *
     H*                  POSTING AND TRANSACTION DATE OF THE          *
     H*                  REVISION TRANSACTION AS THE TRANSACTION DATE *
     H*                  FOR THE REINSTATEMENT.                       *
     H*                                                               *
     H* 06/13/00  C:35   PROJECT: SUPPORT WA033                       *
     H*                  REPORT WAS LOOPING.                          *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*                                                               *
     H* 05/11/00  C:65   PROJECT: WC609                               **
     H*                  ADD FLAG FOR AUDIT REVERSAL DESIGNATION.     **
     H*                                                               *
     H* 03/01/00  C:35   PROJECT: SUPPORT                             *
     H*                  FABRICATE A REINSTATEMENT PRIOR TO           *
     H*                  PROCESSING A REVISED CANCELLATION.           *
     H*                                                               *
     H* 02/17/00  C:35   PROJECT: SUPPORT WA093 #21898                *
     H*                  USE POLICY TO DATE AS OF THE VALUATION DATE  *
     H*                  OF THE REPORT.                               *
     H*                                                               *
     H* 01/27/00  C:35   PROJECT: SUPPORT                             *
     H*                  ADDED CONDITION TO PRINT A LINE ON THE ERROR *
     H*                  REPORT IF THE POLICY IS CANCELLED PRIOR TO   *
     H*                  THE POLICY EFFECTIVE DATE FOR THE YEAR IN    *
     H*                  QUESTION AND THERE IS NO WDCXP RECORD.       *
     H*                                                               *
     H* 01/26/00  C:35   PROJECT: SUPPORT                             *
     H*                  ADD A CONDITION FOR THE MISSING CX REPORT SO *
     H*                  THAT FLAT CANCELLATIONS WILL BE PICKED UP.   *
     H*                                                               *
     H* 11/22/99  C:87   PROJECT: 96NCQ                               *
     H*                  Load new fields.  Chang process to run for   *
     H*                  all groups (except 999) when the group       *
     H*                  passed in equals 0 (QQFND=0).                *
     H*                  Do not print output if QQFND=0.              *
     H*                                                               *
     H* 10/28/99  C:35   PROJECT: SUPPORT WA051 #20233                *
     H*                  CORRECTED PAGE NUMBERING.                    *
     H*                                                               *
     H* 09/30/99  C:35   PROJECT: SUPPORT WA100                       *
     H*                  LOOK AT EXPIRATION DATE AS WELL AS CANCEL    *
     H*                  DATE WHEN DETERMINING POLICY STATUS.         *
     H*                                                               *
     H* 09/02/99  C:35   PROJECT: SUPPORT WA096                       *
     H*                  CHANGED TO DETERMINE POLICY STATUS AS OF THE *
     H*                  VALUATION DATE.                              *
     H*                                                               *
     H* 07/27/99  C:35   PROJECT: SUPPORT WA096 #19020                *
     H*                  1. ONLY ACCUMULATE THE AMOUNT OF THE         *
     H*                     FABRICATED REINSTATEMENT AND THE REVISION *
     H*                     WHEN DETERMINING THE AMOUNT OF THE        *
     H*                     FABRICATED CANCELLATION.                  *
     H*                  2. WHEN CREATING THE FABRICATED              *
     H*                     REINSTATEMENT, PROGRAM WAS NOT USING THE  *
     H*                     POSTING DATE OF THE MOST RECENT           *
     H*                     CANCELLATION.                             *
     H*                  3. WHEN CREATING THE FABRICATED              *
     H*                     CANCELLATION, PROGRAM WAS NOT USING THE   *
     H*                     POSTING DATE OF THE FABRICATED            *
     H*                     REINSTATEMENT.                            *
     H*                  4. THE POSTING DATE PRINTING FOR FABRICATED  *
     H*                     TRANSACTIONS WAS THE POSTING DATE OF THE  *
     H*                     REVISION.  THIS HAS BEEN CHANGED TO BE    *
     H*                     THE POSTING DATE OF THE MOST RECENT       *
     H*                     CANCELLATION.                             *
     H*                                                               *
     H* 07/20/99  C:35   PROJECT: SUPPORT WA093 #18830                *
     H*                  READ PREMIUM HISTORY FILE TO DETERMINE       *
     H*                  PRE-CANCELLATION POLICY EXP DATE, SO THAT    *
     H*                  SHORT-TERM POLICIES ARE HANDLED CORRECTLY.   *
     H*                                                               *
     H* 04/21/99  C:35   PROJECT: SUPPORT WA096 #16926                *
     H*                  IF AUDIT TRANSACTION DATE IS PRIOR TO THE    *
     H*                  POLICY EFFECTIVE DATE, EARN/INCLUDE THE      *
     H*                  AUDIT AS OF THE POLICY EFFECTIVE DATE.       *
     H*                                                               *
     H* 02/23/99  C:35   PROJECT: 96WBZ                               *
     H*                  LOAD NEW FIELDS IN OUTPUT FILE: RATING       *
     H*                  STATE, CHAPTER, DECRIPTION, ASSESSMENT 1,    *
     H*                  ASSESSMENT 2, TAX AMOUNT, GROUP NAME,        *
     H*                  COMPANY NAME.                                *
     H*                                                               *
     H* 12/04/98  C:35   PROJECT: SUPPORT                             *
     H*                  PRINT ALL 3 DIGITS OF FUND YEAR.  ALSO, ADD  *
     H*                  CODE TO PRODUCE AN ERROR REPORT LISTING ANY  *
     H*                  EMPLOYERS WHO HAVE A CANCELLATION DATE IN    *
     H*                  THE EMPLOYER MASTER RECORD, BUT ARE NOT      *
     H*                  CANCELLED ACCORDING TO THE CANCELLATION      *
     H*                  HISTORY FILE.                                *
     H*                                                               *
     H* 11/09/98  C:35   PROJECT: SUPPORT                             *
     H*                  CORRECT THE CONDITION FOR YTD EARNED.        *
     H*                                                               *
     H* 11/05/98  C:35   PROJECT: SUPPORT                             *
     H*                  FABRICATED CANCELLATIONS (USED TO PROCESS    *
     H*                  REVISIONS AFTER CANCELLATIONS) WERE NOT      *
     H*                  BACKING OUT FABRICATED REVISIONS.            *
     H*                                                               *
     H* 10/27/98  C:35   PROJECT: CM002                               *
     H*                  AUDITS SHOULD BE INCLUDED/EARNED BASED ON    *
     H*                  THE TRANSACTION DATE, REGARDLESS OF THE      *
     H*                  POSTING DATE.                                *
     H*                                                               *
     H*  04/29/98  C:78  PROJ:WC745                                   *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL. WDF2L      *
     H*                 DELETED CAUSE ITS NOT BEING USED ANYMORE.     *
     H*                                                               *
     H*  04/16/98 C:57 PROJECT CM003                                  *
     H*                ACCUMULATE FULL AMOUNT OF BACKPOSTED ITEMS     *
     H*                TO CURRENT PERIOD.                             *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*****************************************************************
     F*
     FWXEUL     IF   E           K DISK
      **** BEGIN ADD **** 10/22/02 ************************************
     FWXEUL2    IF   E           K DISK
     F                                     RENAME(PEU01:PEUL2)
     FSTSTL3    IF   E           K DISK
      **** END   ADD **** 10/22/02 ************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
      ***** BEG ADD ***** 09/20/06 ************************************
     FWDF2L     IF   E           K DISK
      ***** END ADD ***** 09/20/06 ************************************
     FWMUPL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWFEUP     UF A E             DISK
     F                                     RENAME(PEU01:PEUOUT)
     FWMRSL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWDEHL12   IF   E           K DISK
     FWDXHL15   IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWMARL     IF   E           K DISK
     FWMREL     IF   E           K DISK
      *** BEG ADD ***** 07/26/04 *************************************
     FWASRL6    IF   E           K DISK
     F                                     RENAME(PSR01:PSR06)
     FWASRL10   IF   E           K DISK
     F                                     RENAME(PSR01:PSR10)
     FWMPRL1    IF   E           K DISK
     FWMEPL9    IF   E           K DISK
     F                                     RENAME(PEP01:PEP09)
     FWMEPLQ    IF   E           K DISK
     F                                     RENAME(PEP01:PEP0Q)
      *** END ADD ***** 07/26/04 *************************************
      *** BEG ADD ***** 04/27/04 *************************************
     FWDECL     IF   E           K DISK
      *** END ADD ***** 04/27/04 *************************************
     F*** BEGIN ADD ***  02/27/06  ************************************
     FWZEXL     IF   E           K DISK
     F*** END ADD *****  02/27/06  ************************************
      *** beg add ***** 06/21/16 ***************************
     fXmlPrel1  if   e           k disk    usropn
      *** end add ***** 06/21/16 ***************************
     F*** BEG ADD ***** 01/04/10 ******************************************
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     F*** END ADD ***** 01/04/10 ******************************************
      **** BEG   ADD **** 09/07/09 *********************************************
      *** BEG DEL ***** 06/21/10 ***********************************************
     F*********** SWCPF   IF  E                    DISK
      *** END DEL ***** 06/21/10 ***********************************************
      **** END   ADD **** 09/07/09 *********************************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOB)
      *** BEG ADD ***** 07/21/04 *************************************
     FQSYSPRT7  O    F  198        PRINTER OFLIND(*INOC)
     F                                     USROPN
      *** END ADD ***** 07/21/04 *************************************

     D*
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)            # DAYS IN MONTH
      *** BEG ADD ***** 06/03/10 ******************************************
     D NE              S             33    DIM(1) CTDATA PERRCD(1)              NO ERROR MSG
      *** END ADD ***** 06/03/10 ******************************************
      *** BEG CHG ***** 08/30/05 ******************************************
     D***********         EP         12 11 2             EXPECTED PAYMENT
     D EP              S             11  2 DIM(13)                              EXPECTED PAYMENT
      *** END CHG ***** 08/30/05 ******************************************
      *** BEG CHG ***** 12/01/05 ******************************************
      **********
     D**********          XACT      500  2               ACTION
     D**********          FRDT      500  7 0             FROM DATE
     D**********          TODT      500  7 0             TO DATE
     D**********          INPR      500 11 2             INFORCE PREM
     D**********          WAPR      500 11 2             WRTN ANNL PREM
     D**********          WNMP      500 11 2             WRTN NORN PREM
     D*****Begin Add  ***  12/01/03  **************************************
     D**********          ITER      500 11 2             INFORCE TERRORISM
     D**********          WTER      500 11 2             WRTN TERRORISM RISK
     D*****End** Add  ***  12/01/03  **************************************
     D*****Begin Add  ***  01/08/04  **************************************
     D**********          PRTD      500 11 2             EXPENSE CONSTANT
     D**********          INEX      500 11 2             INFORCE EXP CONST
     D*****End** Add  ***  01/08/04  **************************************
     D****BEG*ADD ***** 07/26/04 ******************************************
     D**********          IMPR      500 11 2             INFORCE MAN PREMIUM
     D**********          WMPR      500 11 2             WRITTEN MAN PREMIUM
     D XACT            S              2    DIM(9999)                            ACTION
     D FRDT            S              7  0 DIM(9999)                            FROM DATE
     D TODT            S              7  0 DIM(9999)                            TO DATE
     D INPR            S             11  2 DIM(9999)                            INFORCE PREM
     D WAPR            S             11  2 DIM(9999)                            WRTN ANNL PREM
     D WNMP            S             11  2 DIM(9999)                            WRTN NORN PREM
     D ITER            S             11  2 DIM(9999)                            INFORCE TERRORISM
     D WTER            S             11  2 DIM(9999)                            WRTN TERRORISM RISK
     D PRTD            S             11  2 DIM(9999)                            EXPENSE CONSTANT
     D INEX            S             11  2 DIM(9999)                            INFORCE EXP CONST
     D IMPR            S             11  2 DIM(9999)                            INFORCE MAN PREMIUM
     D WMPR            S             11  2 DIM(9999)                            WRITTEN MAN PREMIUM
      *** END CHG ***** 12/01/05 ******************************************
      *** BEG ADD ***** 11/28/06 ******************************************
     D SRAD            S             11  2 DIM(9999)                            S/R ADJ TO REVISED
     D XAD             S              1    DIM(13)                              TRACK ADJ BEFORE FRS
     D XEP             S             11  2 DIM(13)                              'AR' AMTS BEFORE FRS
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     D FADM            S             11  2 DIM(13)                              FUTURE ADJ AMOUNT AR
     D CFAM            S             11  2 DIM(13)                              CURR FUT AMT ARRAY
     D FADJ            S              1    DIM(13)                              FUTURE ADJ MONTH ARR
     D CFAD            S              1    DIM(13)                              CURR FUT ADJ ARRAY
      *** END ADD ***** 02/05/07 ******************************************
      *** BEG CHG ***** 08/30/05 ******************************************
     D***********         EMP        12 11 2             EXPECTED MAN PREM
     D EMP             S             11  2 DIM(13)                              EXPECTED MAN PREM
      *** END CHG ***** 08/30/05 ******************************************
     D*** END ADD ***** 07/26/04 ******************************************
     D*** BEG ADD ***** 09/01/04 ******************************************
      *** BEG CHG ***** 12/01/05 ******************************************
     D***********         WVLC      500 11 2             WRITTEN MAN PREMIUM
     D***********         IVLC      500 11 2             INFORCE MAN PREMIUM
     D****END*ADD ***** 09/01/04 ******************************************
     D*****Begin*Add  ***  01/28/05  **************************************
     D***********         WATR      500 11 2             WRTN ANNL TERROR
     D*****End***Add  ***  01/28/05  **************************************
     D***********         DAYS      500  3 0             TODT - FRDT
     D WVLC            S             11  2 DIM(9999)                            WRITTEN MAN PREMIUM
     D IVLC            S             11  2 DIM(9999)                            INFORCE MAN PREMIUM
     D WATR            S             11  2 DIM(9999)                            WRTN ANNL TERROR
     D DAYS            S              3  0 DIM(9999)                            TODT - FRDT
      *** END CHG ***** 12/01/05 ******************************************
      *** BEG ADD ***** 01/26/06 ******************************************
     D WDTR            S             11  2 DIM(9999)                            WRITTEN DTEC
     D IDTR            S             11  2 DIM(9999)                            INFORCE DTEC
      *** END ADD ***** 01/26/06 ******************************************
      *** BEG ADD ***** 10/06/04 ******************************************
      *** BEG CHG ***** 08/30/05 ******************************************
     D***********         SEP        12 11 2             SAVE EP ARRAY
     D***********         NEP        12 11 2             NEW  EP ARRAY
     D***********         SEMP       12 11 2             SAVE EMP ARRAY
     D***********         NEMP       12 11 2             NEW  EMP ARRAY
     D***********         ADJ        12  1               A/R ADJ TRACK (Y/ )
     D SEP             S             11  2 DIM(13)                              SAVE EP ARRAY
     D NEP             S             11  2 DIM(13)                              NEW  EP ARRAY
     D SEMP            S             11  2 DIM(13)                              SAVE EMP ARRAY
     D NEMP            S             11  2 DIM(13)                              NEW  EMP ARRAY
     D ADJ             S              1    DIM(13)                              A/R ADJ TRACK (Y/ )
     D*** END ADD ***** 01/04/10 ******************************************
     D*** Beg Add ***** 06/17/24 ******************************************
     D xxArAt          S              8S 3 INZ
     D*** End Add ***** 06/17/24 ******************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
      *** BEG CHG ***** 01/18/07 ***********************************************
     D***********                             1   70ELPFDT
     D  SVPFDT                 1      7  0
      *** END CHG ***** 01/18/07 ***********************************************
     D  XXPCY                  1      3  0
     D  XXPYR                  2      3  0
     D  XXPMO                  4      5  0
     D  XXPDY                  6      7  0
     D*
     D                 DS
     D  EUPODT                 1      7  0
      *** BEG CHG ***** 08/30/05 ***********************************************
     D***********                             1   10XEUPOC
     D  XEUPOC                 1      3  0
      *** END CHG ***** 08/30/05 ***********************************************
     D  XEUPOY                 2      3  0
     D  XEUPOM                 4      5  0
     D  XEUPOD                 6      7  0
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
      *** BEG ADD **** 07/26/04 *************************************
     D                 DS
     D  SRRPMY                 1      5  0
     D  SRY                    1      3  0
     D  SRM                    4      5  0
     D                 DS
     D  XXRPDT                 1      7  0
     D  XXRPYR                 1      3  0
     D  XXRPMD                 4      7  0
      *** END ADD **** 07/26/04 *************************************
      *** BEG ADD **** 12/02/09 *************************************
     D                 DS
     D  XXVDAT                 1      7  0
     D  XXVYR                  1      3  0
     D  XXVMTH                 4      5  0
     D  XXVDAY                 6      7  0
      *
     D POLVBG          C                   CONST('POLICY DATE RANGE:')
     D CALVBG          C                   CONST('CALENDAR DATE RANGE:')
     D FISVBG          C                   CONST('FISCAL DATE RANGE:')
      *** END ADD ***** 12/02/09 ************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** END CHG ***** 08/30/05 ******************************************
      *** END ADD ***** 10/06/04 ******************************************
      *** beg add ***** 12/15/14 ******************************************
     d flgManCalc      s               n
      *** end add ***** 12/15/14 ******************************************
      *** beg add ***** 06/21/16 ******************************************
     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outFldrPrefix   s              2    inz
     d outPreString    s             10    inz
     d outReturn       s            250    inz
     d outXMLFile      s             61    inz
     d processType     s            100    inz
     d strCommand      s            255
     d xmlFile         s             60    inz
     d/copy sPrModule
     d/copy sPrXML
     d WrtRptCriteria  pr
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const
     d S4401C          pr                  extpgm('S4401C')
     d  xmlFile                      61
      *** end add ***** 06/21/16 ******************************************

     I*
     IPEUOUT
     I              EUCO#                       EOCO#
     I              EUFND                       EOFND
     I              EUEMP#                      EOEMP#
     I              EUDIV                       EODIV
     I              EUFYR                       EOFYR
     I              EUPFDT                      EOPFDT
     I              EUNAME                      EONAME
     I              EUPRDT                      EOPRDT
     I              EUTRDT                      EOTRDT
     I              EUPODT                      EOPODT
     I              EUINPR                      EOINPR
     I              EUYDWR                      EOYDWR
     I              EUYDEA                      EOYDEA
     I              EUUNEA                      EOUNEA
     I              EUMDWR                      EOMDWR
     I              EUMDEA                      EOMDEA
     I              EUPMUN                      EOPMUN
     I              EUPYWR                      EOPYWR
     I              EUPYEA                      EOPYEA
     I              EUPYUN                      EOPYUN
     I              EUST                        EOST
     I              EUITER                      EOITER
     I              EUUNTR                      EOUNTR
     I              EUMDTR                      EOMDTR
     I              EUIVLC                      EOIVLC
     I              EUUVLC                      EOUVLC
     I              EUMVLC                      EOMVLC
     I              EUINEX                      EOINEX
     I              EUUNEX                      EOUNEX
     I              EUMWEX                      EOMWEX
     I              EUIMPR                      EOIMPR
     I              EUUNMN                      EOUNMN
     I              EUYDWM                      EOYDWM
     I              EUYDEM                      EOYDEM
     I              EUMDWM                      EOMDWM
     I              EUMDEM                      EOMDEM
     I              EUPMUM                      EOPMUM
     I              EUPYWM                      EOPYWM
     I              EUPYEM                      EOPYEM
     I              EUPYUM                      EOPYUM
     I              EUMDET                      EOMDET
     I              EUPMUT                      EOPMUT
     I              EUPYUT                      EOPYUT
     I              EUPYET                      EOPYET
     I              EUPYWT                      EOPYWT
     I              EUYDWT                      EOYDWT
     I              EUYDET                      EOYDET
     I              EUYDWD                      EOYDWD
     I              EUYDED                      EOYDED
     I              EUMDWD                      EOMDWD
     I              EUMDED                      EOMDED
     I              EUPMUD                      EOPMUD
     I              EUPYWD                      EOPYWD
     I              EUPYED                      EOPYED
     I              EUPYUD                      EOPYUD
     I              EUIDTR                      EOIDTR
     I              EUUDTR                      EOUDTR
     I              EUCAR#                      EOCAR#
     I              EUBSCD                      EOBSCD
     I              EUSEFL                      EOSEFL
      *** beg add ***** 06/21/16 ************************
     I              EUXMLFILE                   EOXMLFILE
     I              EURFRDT                     EORFRDT
     I              EURTODT                     EORTODT
     I              EURVLDT                     EORVLDT
     I              EUEXC                       EOEXC
     I              EUSTATNM                    EOSTATNM
      *** end add ***** 06/21/16 ************************
     I*** begin add ***  08/05/16  *******************************************
     I              EUEMPNAM                    EOEMPNAM
     I              EUEMPFED                    EOEMPFED
     I*** end add *****  08/05/16  *******************************************

     C*
     C     *LIKE         DEFINE    ORTITL        XXTTL
     C     *LIKE         DEFINE    ORTITL        XXTTL1
     C     *LIKE         DEFINE    ORTITL        XXTTL2
     C     *LIKE         DEFINE    EUCO#         SVCO#
     C     *LIKE         DEFINE    EUFND         SVFND
     C     *LIKE         DEFINE    EUEMP#        SVEMP#
      **** BEGIN ADD **** 10/22/02 ************************************
     C     *LIKE         DEFINE    EUST          SVST
      **** END   ADD **** 10/22/02 ************************************
     C     *LIKE         DEFINE    EUDIV         SVDIV
     C     *LIKE         DEFINE    EUFYR         SVFYR
     C     *LIKE         DEFINE    ELPFDT        XXPTDT
     C     *LIKE         DEFINE    ELPFDT        XXLPDT
     C     *LIKE         DEFINE    EURWDT        SVRWDT
     C     *LIKE         DEFINE    EUTRDT        SVTRDT
     C     *LIKE         DEFINE    EUPODT        SVPODT
     C     *LIKE         DEFINE    EUSORT        SVSORT
     C     *LIKE         DEFINE    ELPFDT        XQFRM7
     C     *LIKE         DEFINE    ELPFDT        XQTO7
     C     *LIKE         DEFINE    EUSORT        @@SORT
     C     *LIKE         DEFINE    EUARCD        @@ARCD
     C     *LIKE         DEFINE    EUAMT         @@AMT
     C     *LIKE         DEFINE    EUTRDT        @@TRDT
     C     *LIKE         DEFINE    EUPODT        @@PODT
     C     *LIKE         DEFINE    EUFAF         @@FAF
     C     *LIKE         DEFINE    EUIAPR        @@IAPR
     C     *LIKE         DEFINE    EUWAPR        @@WAPR
     C     *LIKE         DEFINE    EUWNMP        @@WNMP
     C     *LIKE         DEFINE    EUJAPR        @@JAPR
     C     *LIKE         DEFINE    EUXAPR        @@XAPR
     C     *LIKE         DEFINE    EUXNMP        @@XNMP
     C     *LIKE         DEFINE    EURWDT        @@RWDT
     C     *LIKE         DEFINE    EURWFL        @@RWFL
     C     *LIKE         DEFINE    EUFAF2        @@FAF2
     C***  Begin Add  ***  12/01/03  **************************************
     C     *LIKE         DEFINE    EUITER        @@ITER
     C     *LIKE         DEFINE    EUWTER        @@WTER
     C     *LIKE         DEFINE    EUXTER        @@XTER
     C     *LIKE         DEFINE    EUJTER        @@JTER
     C     *LIKE         DEFINE    EUITER        XXITER
     C     *LIKE         DEFINE    EUWTER        XXWTER
     C     *LIKE         DEFINE    EUXTER        XXXTER
     C     *LIKE         DEFINE    EUJTER        XXJTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C     *LIKE         DEFINE    EUIDTR        @@IDTR
     C     *LIKE         DEFINE    EUWDTR        @@WDTR
     C     *LIKE         DEFINE    EUXDTR        @@XDTR
     C     *LIKE         DEFINE    EUJDTR        @@JDTR
     C     *LIKE         DEFINE    EUIDTR        XXIDTR
     C     *LIKE         DEFINE    EUWDTR        XXWDTR
     C     *LIKE         DEFINE    EUXDTR        XXXDTR
     C     *LIKE         DEFINE    EUJDTR        XXJDTR
     C***  END ADD ******  01/26/06  **************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C     *LIKE         DEFINE    EUPRTD        XXINEX
     C     *LIKE         DEFINE    EUPRTD        XXUNEX
     C     *LIKE         DEFINE    EUPRTD        XXPRTD
     C     *LIKE         DEFINE    EUPRTD        @@FEXC
     C     *LIKE         DEFINE    EUPRTD        @@INXC
     C     *LIKE         DEFINE    EUPRTD        @@PRTD
     C***  End   Add  ***  01/08/04  **************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     *LIKE         DEFINE    EUIMPR        @@IMPR
     C     *LIKE         DEFINE    EUWMPR        @@WMPR
     C     *LIKE         DEFINE    EUJMPR        @@JMPR
     C     *LIKE         DEFINE    EUXMPR        @@XMPR
     C     *LIKE         DEFINE    EUIMPR        XXIMPR
     C     *LIKE         DEFINE    EUWMPR        XXWMPR
     C     *LIKE         DEFINE    EUJMPR        XXJMPR
     C     *LIKE         DEFINE    EUXMPR        XXXMPR
     C     *LIKE         DEFINE    SRCLAS        SVCLAS
     C*** BEG ADD ***** 04/01/08 ******************************************
     C     *LIKE         DEFINE    SREFDT        SVEFDT
     C*** END ADD ***** 04/01/08 ******************************************
     C     *LIKE         DEFINE    SRRPMY        KKRPMY
     C     *LIKE         DEFINE    SRTODT        KKEFDT
     C*** END ADD ***** 07/26/04 ******************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     *LIKE         DEFINE    EUIVLC        @@IVLC
     C     *LIKE         DEFINE    EUWVLC        @@WVLC
     C     *LIKE         DEFINE    EUXVLC        @@XVLC
     C     *LIKE         DEFINE    EUJVLC        @@JVLC
     C     *LIKE         DEFINE    EUIVLC        XXIVLC
     C     *LIKE         DEFINE    EUWVLC        XXWVLC
     C     *LIKE         DEFINE    EUXVLC        XXXVLC
     C     *LIKE         DEFINE    EUJVLC        XXJVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C     *LIKE         DEFINE    EUEMP#        XXEMPH
     C     *LIKE         DEFINE    EUDIV         XXDIVH
     C     *LIKE         DEFINE    EUFYR         XXFYRH
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    AHAGT#        ZZAGT#
     C     *LIKE         DEFINE    ELDIV         ZZDIV
     C     *LIKE         DEFINE    AHEFDT        ZZEFDT
     C     *LIKE         DEFINE    ELEMP#        ZZEMP#
     C     *LIKE         DEFINE    AAFEIN        ZZFEIN
     C     *LIKE         DEFINE    ELFYR         ZZFYR
     C     *LIKE         DEFINE    EUFYR         ZZFYRP
     C     *LIKE         DEFINE    AAMOD         ZZMOD
     C     *LIKE         DEFINE    AGNAME        ZZNAME
     C     *LIKE         DEFINE    EUPRNM        ZZPNAM
     C     *LIKE         DEFINE    AHPROD        ZZPROD
     C     *LIKE         DEFINE    EURATE        ZZRATE
     C     *LIKE         DEFINE    AHRT#         ZZRT
      **** BEGIN ADD **** 02/28/01 ************************************
     C     *LIKE         DEFINE    EUPRTD        SVPRTD
     C     *LIKE         DEFINE    EUPRTD        SVFEXC
      **** END   ADD **** 02/28/01 ************************************
     C***  Begin Add  ***  03/10/04  **************************************
     C     *LIKE         DEFINE    EUPRTD        S2PRTD
     C***  End   Add  ***  03/10/04  **************************************
     C*** BEG ADD *******  11/23/04  **************************************
     C     *LIKE         DEFINE    EUSEQ#        XXSEQ#
     C*** END ADD *******  11/23/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C     *LIKE         DEFINE    EOYDWT        XXWATR
     C     *LIKE         DEFINE    EOYDWT        XXYDWT
     C     *LIKE         DEFINE    EOPYWT        XXPYWT
     C     *LIKE         DEFINE    XXWATR        SVWATR
     C***  End   Add  ***  01/28/05  **************************************
      ***** BEG ADD ***** 09/20/06 ************************************
     C     *LIKE         DEFINE    F2CAR#        X2CAR#
      ***** END ADD ***** 09/20/06 ************************************
      ***** BEG ADD ***** 02/12/09 ************************************
     C     *LIKE         DEFINE    AGMKRG        ZZMKRG
      ***** END ADD ***** 02/12/09 ************************************
      *** BEG ADD ***** 05/12/09 **************************************
     C     *LIKE         DEFINE    F2CAR#        SVCAR#
      *** BEG ADD ***** 05/12/09 **************************************
      *** BEG ADD ***** 07/21/04 **************************************
     C     *DTAARA       DEFINE    CEUARY        PRTARY            1
      *** END ADD ***** 07/21/04 **************************************
      *
     C                   Z-ADD     0             XXAPR            13 2
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE EMPLOYER WORKFILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C*
      ***** BEG ADD ***** 09/20/06 ************************************
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
      ***** END ADD ***** 09/20/06 ************************************
     C*
     C     ELKYP         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZEMP#
     C                   KFLD                    ZZDIV
     C                   KFLD                    ZZFYRP
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C*
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZEMP#
     C                   KFLD                    ZZDIV
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVFYR
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
      *** BEG CHG ***** 02/15/10 *************************************
     C***********          KFLD           Q1PGM
     C                   KFLD                    XXPGM            10
      *** END CHG ***** 02/15/10 *************************************
     C                   KFLD                    XXPRT             1
     C*
      ***** BEG ADD ***** 09/20/06 ************************************
     C*
     C*   Key List for the Fund Auxiliary using X-Ref fields.
     C*
     C     F2OKY         KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXOFND
      ***** END ADD ***** 09/20/06 ************************************
      *
      * KEY LIST FOR THE RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    ELRTCD
      *
      * KEY LIST FOR CHAPTER FILE
      *
     C     CPKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    EOFND
     C                   KFLD                    EMCHAP
      *
      * KEY LIST FOR THE STAT RPT/RATING DEFINITION FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
      *  EMPLOYER HISTORY FILE
      *
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     EHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    XXKDAT
      *
      * KEY LIST FOR THE CANCELLATION HISTORY FILE
      *
     C     XHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C                   KFLD                    XXVDAT
      *
     C     XHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C*  Agent history
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZEMP#
     C                   KFLD                    ZZDIV
     C                   KFLD                    ZZFYR
     C*
     C     AHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZEMP#
     C                   KFLD                    ZZDIV
     C                   KFLD                    ZZFYR
     C                   KFLD                    ZZEFDT
     C*
     C*  Agent assignment file
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZAGT#
     C*
     C*  Agent master file
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ZZFEIN
     C                   KFLD                    ZZMOD
     C*
     C*  Agent producer file
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ZZFEIN
     C                   KFLD                    ZZMOD
     C                   KFLD                    ZZPROD
     C*
     C*  Agent rate file
     C     ARKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZRT
     C*
     C     ARKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ZZRT
     C                   KFLD                    ZZEFDT
      *** BEG ADD ***** 04/27/04 **************************************
      *
      *  KEY LIST FOR THE EXP CONSTANT FILE
      *
     C     ECKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
      *
     C     ECKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
      *** BEG CHG ***** 01/18/07 **************************************
     C***********          KFLD           ELPFDT
     C                   KFLD                    SVPFDT
      *** END CHG ***** 01/18/07 **************************************
      *** END ADD ***** 04/27/04 **************************************
      *** BEG ADD ***** 07/26/04 **************************************
      *
      *  KEY LIST FOR THE EXP CONSTANT FILE
      *
     C     SRKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    KKRPMY
      *
     C     SRKY1         KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    XXRPDT
      *
      *  KEY LIST FOR PAYROLL TABLE
      *
     C     PRKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SVCLAS
     C*** BEG CHG ***** 04/01/08 **************************************
     C**************       KFLD           KKEFDT
     C                   KFLD                    KKEFD1            7 0
     C*** END CHG ***** 04/01/08 **************************************
     C*
     C*  KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVCLAS
     C*
     C     EPKY1         KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVFYR
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVCLAS
     C                   KFLD                    XXRPDT
      *
     C     EPKY2         KLIST
     C                   KFLD                    SVCO#
     C                   KFLD                    SVFND
     C                   KFLD                    SVEMP#
     C                   KFLD                    SVDIV
     C                   KFLD                    SVFYR
     C                   KFLD                    EPEFDT
     C                   KFLD                    SVCLAS
      *** END ADD ***** 07/26/04 **************************************
      *
      *  KEY LIST FOR THE REGION
      *
     C     REKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMRGN#
     C*
     C*** BEG ADD ***** 01/04/10 ******************************************
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*** END ADD ***** 01/04/10 ******************************************
      *** BEG CHG ***** 03/23/06 **************************************
     C*****************    Z-ADD0         Y       30
     C                   Z-ADD     0             Y                 6 0
      *** END CHG ***** 03/23/06 **************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQVLDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQPRNT            1
      **** BEGIN ADD **** 02/28/01 ************************************
     C                   PARM                    QQPRTD            1
      **** END   ADD **** 02/28/01 ************************************
     C                   PARM                    QQSORT            1
      **** BEGIN ADD **** 10/17/01 ************************************
     C                   PARM                    QQPFB1            1
      **** END   ADD **** 10/17/01 ************************************
     C*
      *** BEG ADD ***** 09/07/09 *************************************
      *** BEG DEL ***** 06/21/10 ***********************************************
     C***********          READ SWCPF                    60
      *** BEG DEL ***** 06/21/10 ***********************************************
      *** END ADD ***** 09/07/09 *************************************
      **** BEGIN ADD **** 10/22/02 ************************************
     C     Q1PRMS        IFLT      11
     C                   MOVE      ' '           QQSRST            1
     C                   MOVE      'B'           QQPFB             1
     C                   ELSE
     C                   MOVE      QQSORT        QQSRST
     C                   MOVE      QQPFB1        QQPFB
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
      ***** BEG ADD ***** 09/20/06 ************************************
      *
      * RTVCLTID - Retrieve Client Number from Data Area
     C                   MOVE      *BLANKS       XXCLT#            3
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVEL     CLTID         XXCLT#            3
      *
      ***** END ADD ***** 09/20/06 ************************************
      **** BEG ADD ***** 11/23/04 *************************************
     C                   Z-ADD     0             XXSEQ#
      **** END ADD ***** 11/23/04 *************************************
     C*
     C*
     C*  PARM LIST FOR SRDDIF8 (DAYS DIFFERENCE SUB-PROGRAM)
     C*
     C     PLDDIF        PLIST
     C                   PARM                    QQFROM            8 0
     C                   PARM                    QQTO              8 0
     C                   PARM                    QQDAYS            5 0
     C                   PARM                    QQERR             1
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
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           FIRST             1
      **** BEGIN ADD **** 02/28/01 ************************************
     C                   MOVE      *ON           FRSTE             1
      *** BEG ADD ***** 10/06/04 **************************************
     C                   MOVE      *ON           FRSTEP            1
      *** END ADD ***** 10/06/04 **************************************
     C*
     C     QQPRTD        IFEQ      ' '
     C                   MOVE      'N'           QQPRTD
     C                   ENDIF
      **** END   ADD **** 02/28/01 ************************************
     C**** BEGIN ADD **** 10/17/01 ************************************
     C*
     C* *IN91 = PRINT FLAG    *IN92 = FILE FLAG
     C*
     C                   MOVEA     '11'          *IN(91)
     C*
     C     Q1PRMS        IFGT      9
     C*
     C* IF FLAG IS SET TO FILE ONLY, DO NOT PRINT REPORT
     C*
     C     QQPFB         IFEQ      'F'
     C                   MOVE      '0'           *IN91
     C                   ENDIF
     C*
     C* IF FLAG IS SET TO PRINT ONLY, DO NOT WRITE TO FILE.
     C*
     C     QQPFB         IFEQ      'P'
     C                   MOVE      '0'           *IN92
     C                   ENDIF
     C*
     C                   ENDIF                                                  Q1PRMS IFEQ
     C**** END   ADD **** 10/17/01 ************************************

     C*** beg add ***** 06/21/16 **************************************
      /free
       processType = 'CEU Detail Report';
       outReturn = processType;
       outFldrPrefix = *blanks;
       isUseSpoolOut  = UseSpoolOut(processType);
       isUseScriptura = UseScriptura(processType);
       isUse3rdParty  = Use3rdXml(processType);
       isUseXML = isUseScriptura or isUse3rdParty;
       // Outfile is required to produce Scriptura doc.
       If isUseXML and *in91;
          Open XmlPrel1;
          *in92 = *on;
          outPreString = 'CEUDetail';
          xmlFile = GetXMLFileName(' ':*zeros:outCo#:outFnd:
           outFldrPrefix:outPreString:outReturn);
          Chain xmlFile xp100;
          xmlFile = XPXmlT;
          Close XmlPrel1;
       Endif;
       // Turn off printed ouput if spooled output is no longer required.
       If not(isUseSpoolOut);
          *in91 = *off;
       Endif;
      /End-free
     C*** end add ***** 06/21/16 **************************************

     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0
     C*
     C                   MOVE      QQTODT        XXTODT            8 0
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT            7 0
     C*
     C                   MOVE      QQVLDT        XXVLDT            8 0
     C                   Z-ADD     XXVLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXVDAT            7 0
     C*
     C*  SET POLICY DATE RANGE INDICATOR *IN77
     C*
      *** BEG CHG ***** 12/02/09 *************************************
     C*********  QQTYPE    IFEQ 'P'
     C*********            MOVE *ON       *IN77
     C*********            ENDIF
     C                   SELECT
     C     QQTYPE        WHENEQ    'P'
     C                   MOVE      POLVBG        XXDRNV           20
     C     QQTYPE        WHENEQ    'F'
     C                   MOVE      FISVBG        XXDRNV
     C     QQTYPE        WHENEQ    'C'
     C                   MOVE      CALVBG        XXDRNV
     C                   ENDSL
      *** END CHG ***** 12/02/09 *************************************
     C*
     C*  CALC FIRST DAY OF YEAR, BASED ON "VALUE" DATE.
     C*
     C                   MOVE      XXVDAT        XXJAN1            7 0
     C                   MOVE      0101          XXJAN1
     C*
     C*  CALC LAST DAY OF PRIOR YEAR, BASED ON "TO" DATE.
     C*
     C                   MOVEL     XXVDAT        XXWK30            3 0
     C                   SUB       1             XXWK30
     C                   MOVEL     XXWK30        XXYEND            7 0
     C                   MOVE      1231          XXYEND
     C*
     C*  CALC FIRST DAY OF MONTH, BASED ON "VALUE" DATE.
     C*
     C                   MOVE      XXVDAT        XXMTH1            7 0
     C                   MOVE      01            XXMTH1
      *
      * CALC LAST DAY OF PRIOR MONTH
      *
     C                   Z-ADD     XXMTH1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFRM8
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    XQFRM8            8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM      *BLANK        QQERR             1
      *
     C                   Z-ADD     XQFRM8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPMTH            7 0
     C*
     C*  CALC NEXT DAY AFTER "TO" DATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      XXVLDT        XQFRM8            8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM      *BLANK        QQERR             1
     C*
     C                   Z-ADD     XQFRM8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXNEXT            7 0
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      *BLANK
     C                   MOVE      *ON           *IN09
     C                   ELSE
     C                   MOVE      *ON           *IN08
     C                   ENDIF
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     UPUSNM        X1USNM
     C                   ELSE
     C                   MOVE      *BLANKS       X1USNM           50
     C                   ENDIF
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM           40
     C*
     C*  CHAIN TO THE FUND FILE
     C*
     C*
     C                   MOVE      QQFND         Q1FND             3 0
     C     Q1FND         IFNE      0
     C     FNKY          CHAIN     PFN01                              60
     C                   ELSE
     C     FNKY1         CHAIN     PFN01                              60
     C                   ENDIF                                                  Q1FND<>0
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     FNFND         XXFND
     C*
      *** BEG ADD ***** 12/02/09 **************************************
     C                   EXSR      GETFYR
      *** END ADD ***** 12/02/09 **************************************
     C*
     C     FNTEST        IFNE      'Y'
     C*
     C     Q1FND         ORNE      0
     C*
     C*  LOAD THE FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS
     C*  SET UP FOR NON-COMMON EXPIRATION DATE POLICIES
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        FNCGFL
     C                   ENDIF
     C*
     C*  GET THE DISTRIBUTION INFORMATION & PRINT DETAIL ROUTE PAGE
     C*
     C     QQPRNT        IFEQ      'D'
     C     QQPRNT        OREQ      'B'
     C                   OPEN      QSYSPRT
      *** BEG ADD ***** 02/15/10 **********************************
     C                   MOVEL     'W4901R'      XXPGM
      *** END ADD ***** 02/15/10 **********************************
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ORTITL        XXTTL
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTROUTE
     C   91              EXCEPT    ROUTE
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTHEAD
     C   91              EXCEPT    HEAD
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C*  GET THE DISTRIBUTION INFORMATION & PRINT SUMMARY ROUTE PAGE
     C*
     C     QQPRNT        IFEQ      'S'
     C     QQPRNT        OREQ      'B'
     C     Q1FND         IFNE      0
     C                   OPEN      QSYSPRT1
     C                   ENDIF
      *** BEG ADD ***** 05/21/10 **********************************
     C                   MOVEL     'W4901R'      XXPGM
      *** END ADD ***** 05/21/10 **********************************
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ORTITL        XXTTL1
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTROUTE1
     C   91              EXCEPT    ROUTE1
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTHEAD1
     C   91              EXCEPT    HEAD1
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
      *
      * GET THE DISTRIBUTION INFORMATION & PRINT ERROR ROUTE PAGE
      *
      *** BEG ADD ***** 05/21/10 **********************************
     C                   If        *in91 = *on
      *** END ADD ***** 05/21/10 **********************************
      *** BEG ADD ***** 05/21/10 **********************************
     C                   MOVEL     'W4901R'      XXPGM
      *** END ADD ***** 05/21/10 **********************************
     C                   MOVE      '2'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ORTITL        XXTTL2
     C     Q1FND         IFNE      0
     C                   EXCEPT    ROUTE2
     C                   ENDIF
     C                   ENDIF
      *
     C     Q1FND         IFNE      0
     C                   EXCEPT    ERRHD
     C                   ENDIF
     C                   MOVE      *OFF          *INOB
      *** BEG ADD ***** 06/03/10 **************************************
     C                   Z-ADD     0             ERRCT             1 0
      *** END ADD ***** 06/03/10 **************************************
      *** BEG ADD ***** 12/15/14 **********************************
     C                   Endif                                                  *in91 = *on
      *** END ADD ***** 12/15/14 **********************************
      /EJECT
     C*
     C*  LOOP THROUGH EMPLOYER WORKFILE.
     C*
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQSRST        IFEQ      'Y'
     C     EUKY          SETLL     PEUL2
     C     EUKY          READE     PEUL2                                  50
     C                   MOVE      EUST          SVST
     C                   MOVEL(P)  EUST          H10              10
     C     H10           CHAIN     STSTL3                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  STATNM        STAT15           15
     C                   MOVEL(P)  STNAME        STATNM           40
      *
     C                   ELSE
     C                   MOVEL(P)  '(NONE)'      STATNM
     C                   ENDIF
      *
     C     QQSRST        IFEQ      'Y'
     C     *IN91         ANDEQ     *ON
     C     QQPRNT        IFEQ      'D'
     C     QQPRNT        OREQ      'B'
     C                   EXCEPT    SHEAD
     C                   ENDIF
     C     Q1FND         IFNE      0
     C     QQPRNT        IFEQ      'S'
     C     QQPRNT        OREQ      'B'
     C                   EXCEPT    SHEAD1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
      **** END   ADD **** 10/22/02 ************************************
     C     EUKY          SETLL     PEU01
     C     EUKY          READE     PEU01                                  50
      **** BEGIN ADD **** 10/22/02 ************************************
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
      *
     C     *IN50         DOWEQ     *OFF
     C*
     C*  SKIP DUPLICATE/BACK TO BACK CANCELLATIONS
     C*
     C     EUSORT        IFEQ      '3'                                          CANCELLATIONS
     C     EUSORT        ANDEQ     SVSORT                                       CANCELLATIONS
     C     EUTRDT        ANDEQ     SVTRDT
     C     EUPODT        ANDEQ     SVPODT
     C                   GOTO      NEXTEU
     C                   ENDIF
      *
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQSRST        IFEQ      'Y'
     C*
     C*  PROCESS EMPLOYER/DIV/FYR/STATE  BREAK
     C*
     C     EUCO#         IFNE      SVCO#
     C     EUFND         ORNE      SVFND
     C     EUEMP#        ORNE      SVEMP#
     C     EUDIV         ORNE      SVDIV
     C     EUFYR         ORNE      SVFYR
     C     EUST          ORNE      SVST
     C*
     C                   EXSR      BRKEMP
      *
     C     EMOKAY        IFEQ      *OFF
     C     ELOKAY        OREQ      *OFF
     C                   GOTO      NEXTEU
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
      **** END   ADD **** 10/22/02 ************************************
     C*
     C*  PROCESS EMPLOYER/DIV/FYR BREAK
     C*
     C     EUCO#         IFNE      SVCO#
     C     EUFND         ORNE      SVFND
     C     EUEMP#        ORNE      SVEMP#
     C     EUDIV         ORNE      SVDIV
     C     EUFYR         ORNE      SVFYR
     C*
     C                   EXSR      BRKEMP
      *
     C     EMOKAY        IFEQ      *OFF
     C     ELOKAY        OREQ      *OFF
     C                   GOTO      NEXTEU
     C                   ENDIF
     C                   ENDIF
     C*
      **** BEGIN ADD **** 10/22/02 ************************************
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C*
      *** BEG DEL ***** 06/03/10 **************************************
     C*******    EUSORT    IFEQ '3'
     C*******              MOVE *ON       *IN42
     C*******              ENDIF                           EUSORT IFEQ '3'
      *
     C*******    EUSORT    IFEQ '4'
     C*******              MOVE *OFF      *IN42
     C*******              ENDIF                           EUSORT IFEQ '4'
      *** END DEL ***** 06/03/10 **************************************
     C*
     C                   Z-ADD     EUTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   Z-ADD     EUPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*
     C                   MOVE      EUTRDT        SVTRDT
     C                   MOVE      EUPODT        SVPODT
     C                   MOVE      EUSORT        SVSORT
     C*
     C                   ADD       1             Q
     C                   MOVE      *BLANK        XXSORT            2
     C                   MOVE      *OFF          *IN89
     C     EURWFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C                   SELECT
      *** BEG ADD ***** 05/19/05 **********************************
     C     EUSORT        WHENEQ    '1'                                          A/R
     C     XXCXST        ANDEQ     *ON                                          CANCELLED ACCT
     C                   EXSR      CRTRXC                                       CRT REIN FOR RPT
     C                   ADD       1             Q
     C                   EXSR      SRARBL
     C                   ADD       1             Q
     C                   EXSR      CRTCXC                                       CRT CX FOR RPT
      *** END ADD ***** 05/19/05 **********************************
     C     EUSORT        WHENEQ    '1'                                          A/R
     C                   EXSR      SRARBL
     C     EUSORT        WHENEQ    '2'                                          REVISION ON
     C     XXCXST        ANDEQ     *ON                                          CANCELLED ACCT
     C                   EXSR      CRTRXC                                       CRT REIN FOR RPT
     C                   ADD       1             Q
     C                   EXSR      SREMPH                                       NORMAL STUFF
     C                   ADD       1             Q
     C                   EXSR      CRTCXC                                       CRT CX FOR RPT
     C     EUSORT        WHENEQ    '5'                                          CANCEL AUDIT
     C     XXCXST        ANDEQ     *ON
     C                   EXSR      SRCXAU
     C     EUSORT        WHENEQ    '2'                                          EMP HISTORY
     C     EUSORT        OREQ      '5'                                          EMP HISTORY
     C                   EXSR      SREMPH
     C     EUSORT        WHENEQ    '3'                                          CANCELLATIONS
     C     XXCXST        IFEQ      *ON                                          CANCELLED ACCT
      *
     C     EUTRDT        IFNE      XXSVTD
     C     EUPODT        ORNE      XXSVPD
      *
     C                   MOVE      *ON           *IN43
     C                   EXSR      CRTRXC                                       CRT REIN FOR RPT
     C                   MOVE      *OFF          *IN43
     C                   ADD       1             Q
      *
     C                   ENDIF                                                  EUTRDT IFEQ XXSVTD
      *
     C                   ENDIF                                                  XXCXST IFEQ *ON
     C                   EXSR      SRCANX
     C     EUSORT        WHENEQ    '4'                                          REINSTATES
     C                   EXSR      SRREIN
     C                   ENDSL
     C*
     C* READ THE NEXT EMPLOYER DETAIL RECORD
     C*
     C     NEXTEU        TAG
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQSRST        IFEQ      'Y'
     C     EUKY          READE     PEUL2                                  50
     C                   MOVEL(P)  EUST          H10              10
     C     H10           CHAIN     STSTL3                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  STATNM        STAT15
     C                   MOVEL(P)  STNAME        STATNM
     C                   ELSE
     C                   MOVEL(P)  '(NONE)'      STATNM
     C                   ENDIF
      *
     C                   ELSE
      **** END   ADD **** 10/22/02 ************************************
     C     EUKY          READE     PEU01                                  50
      **** BEGIN ADD **** 10/22/02 ************************************
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C                   ENDDO                                                  *IN50 DOWEQ *OFF
      /EJECT
     C*
     C*  PROCESS LAST EMPLOYER BREAK
     C*
     C                   EXSR      BRKEMP
      *** BEG ADD ***** 06/03/10 **********************************
     C     ERRCT         IFEQ      0
      *** BEG ADD ***** 06/03/10 **********************************
     C     *IN91         ANDEQ     *ON
      *** END ADD ***** 06/03/10 **********************************
     C     Q1FND         ANDNE     *ZERO
     C                   EXCEPT    NOERR
     C                   ENDIF
      *** BEG ADD ***** 06/03/10 **********************************
     C*
     C*  PRINT THE REPORT DETAIL REPORT TOTALS
     C*
     C     QQPRNT        IFEQ      'D'
     C     QQPRNT        OREQ      'B'
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQSRST        IFEQ      'Y'
     C     *IN91         ANDEQ     *ON
     C                   EXCEPT    STTOT
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTTOT
     C   91              EXCEPT    TOT
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C*  PRINT THE REPORT SUMMARY REPORT TOTALS
     C*
     C     QQPRNT        IFEQ      'S'
     C     QQPRNT        OREQ      'B'
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQSRST        IFEQ      'Y'
     C     *IN91         ANDEQ     *ON
      ****** Beg Add ***** 05/02/12 ***********************************
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
      ****** End Add ***** 05/02/12 ***********************************
     C                   EXCEPT    STTOT1
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTTOT1
      ****** Beg Add ***** 05/02/12 ***********************************
     C     *INOA         IFEQ      *ON
     C     *IN91         ANDEQ     *ON
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
      ****** End Add ***** 05/02/12 ***********************************
     C   91              EXCEPT    TOT1
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXFND<>999
     C*
     C     Q1FND         IFEQ      0
     C     FNKY1         READE     PFN01                                  60
      **** BEGIN ADD **** 10/30/01 ************************************
     C                   MOVE      *ON           FIRST             1
     C                   MOVE      *ON           FRSTE             1
      **** END ADD ****** 10/30/01 ************************************
      *** BEG ADD ***** 10/06/04 **************************************
     C                   MOVE      *ON           FRSTEP            1
      *** END ADD ***** 10/06/04 **************************************
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  Q1FND=0
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*

      *** beg add ***** 06/21/16 **********************
      /free
       // Print Scriptura if printing a detail report...
       If isUseXML and *in91 and QQprnt <> 'S';
           strCommand = 'CPYF FROMFILE(WFEUP) TOFILE(WXEUP1) MBROPT(*ADD)';
           Callp QCMDEXC(strCommand:%len(strCommand));
           outXMLFile = xmlFile + '$';
           Callp S4401C(outXMLFile);
       Endif;
      /end-free
      *** end add ***** 06/21/16 **********************

     C                   MOVE      *ON           *INLR
     C*
     C*  THE FOLLOWING LINE WILL NEVER BE EXECUTED!!!!!  IT IS REQUIRED
     C*  TO ALLOW FOR I-SPECS FIELD RENAMING.  DO NOT REMOVE!!!!!
     C   LR
     CANNLR              READ      PEUOUT                                 LR
      /EJECT
     C****************************************************************
     C*  SRARBL SUBROUTINE - SEARCHES A/R FOR SELF-REPORTED BILLS AND
     C*                      CLAIMS REPLENISHMENT BILLS.
     C****************************************************************
     C     SRARBL        BEGSR
     C*
     C                   MOVE      'EA'          XXSORT
     C*
     C*  IF SELF-REPORTED, FIND THE EXPECTED PREMIUM FOR THE POSTED MONTH
     C*
     C     EUARCD        IFEQ      F1SRBB
     C     XEUPOM        SUB       XXPMO         X
     C                   ADD       1             X
     C*
     C     X             IFLE      *ZERO
     C                   ADD       12            X
     C                   ENDIF
      *** BEG ADD ***** 08/30/05 *************************************
      *
      * IF POSTING DATE CENT YEAR <> POLICY FROM DT YEAR, IT MUST BE THE
      * 13TH BUCKET OF THE EP ARRAY IF THE MONTH VALUES ARE EQUAL
      *
     C     XEUPOC        IFNE      XXPCY
     C     XEUPOM        ANDEQ     XXPMO
     C                   Z-ADD     13            X
     C                   ENDIF
      *** END ADD ***** 08/30/05 *************************************
      *** BEG ADD ***** 12/15/14 *************************************
      *
      * Set flag so that man prem calculates for the first
      * S/R transaction for a particular month
      *
      /free
       If xad(x) = *blank and adj(x) = *blank;
          flgManCalc = *on;
       Else;
          flgManCalc = *off;
       Endif;
      /end-free
      *** END ADD ***** 12/15/14 *************************************
      *
     C     EUAMT         SUB       EP(X)         XXWAPR
      *** BEG ADD ***** 02/05/07 **********************************
     C     ADJ(X)        IFEQ      ' '
     C     FADJ(X)       ANDEQ     'Y'
     C                   ADD       FADM(X)       XXWAPR
     C                   SUB       FADM(X)       XXNETR           11 2
     C                   CLEAR                   FADJ(X)
     C                   CLEAR                   FADM(X)
     C                   ENDIF
      *** END ADD ***** 02/05/07 **********************************
      *** BEG ADD ***** 10/06/04 **********************************
      *** BEG CHG ***** 11/28/06 **********************************
     C***********          MOVE 'Y'       ADJ,X
     C     FRSTEP        IFEQ      *OFF
     C                   MOVE      'Y'           ADJ(X)
     C                   ELSE
     C                   MOVE      'Y'           XAD(X)
     C                   Z-ADD     EUAMT         XEP(X)
     C                   ENDIF
      *** END CHG ***** 11/28/06 **********************************
      *** END ADD ***** 10/06/04 **********************************
     C                   Z-ADD     *ZERO         EP(X)
     C                   ADD       XXWAPR        XXNETR           11 2
     C*
      *** BEG ADD ***** 07/26/04 **********************************
      *
      *  CALCULATE MANUAL PREMIUM FOR MONTHLY ACCOUNTS
      *
      *** BEG ADD ***** 12/15/14 **********************************
     C                   If        flgManCalc = *on
      *** END ADD ***** 12/15/14 **********************************
     C                   CLEAR                   XXCPTT                         CLASS PAYROLL TOTAL
     C                   CLEAR                   XXMPTT                         MAN PREM TOTAL
     C                   CLEAR                   XXRPDT                         S/R REPORT DATE
     C                   MOVEL     EUPODT        KKRPMY
     C                   Z-ADD     EUPODT        XXRPDT
     C     SRKY          CHAIN     PSR06                              60
      *
      *  USE EOM REPORT DATE FOR GETTING PAYROLL RATE. ALL PAYROLL CLASSES
      *  SHOULD HAVE THE SAME REPORT DATE.
      *
     C     *IN60         IFEQ      *OFF
      *** beg del ***** 12/15/14 **********************************
     C*****SRRPMY        ANDNE     *ZERO
     C*****SRTODT        ANDEQ     *ZERO
      *** end del ***** 12/15/14 **********************************
     C                   Z-ADD     SRCLAS        SVCLAS
     C*** BEG ADD ***** 04/01/08 **********************************
     C                   Z-ADD     SREFDT        SVEFDT
     C*** END ADD ***** 04/01/08 **********************************
     C                   ENDIF
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     SRRPMY        IFNE      *ZERO
     C     SRTODT        ANDEQ     *ZERO
      *
     C     SRCLAS        IFNE      SVCLAS
     C                   EXSR      CALMAN
     C                   Z-ADD     SRCLAS        SVCLAS
     C*** BEG ADD ***** 02/15/10 **********************************
     C                   Z-ADD     SRAMTP        XXCPTT
     C*** END ADD ***** 02/15/10 **********************************
     C*** BEG ADD ***** 04/01/08 **********************************
     C                   Z-ADD     SREFDT        SVEFDT
     C*** END ADD ***** 04/01/08 **********************************
     C                   ELSE
     C                   ADD       SRAMTP        XXCPTT           15 2
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     SRKY          READE     PSR06                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   EXSR      CALMAN
     C                   MOVE      *ON           *IN60
     C                   ENDIF
      *
     C                   ENDDO
      *
      *  CALCULATE MANUAL PREMIUM FOR WEEKLY REPORTING ACCOUNTS
      *
     C                   CLEAR                   XXCPTT
     C     SRKY1         CHAIN     PSR10                              60
      *
     C     *IN60         IFEQ      *OFF
     C     SRTODT        ANDNE     *ZERO
     C     SRRPMY        ANDEQ     *ZERO
     C                   Z-ADD     SRCLAS        SVCLAS
     C*** BEG ADD ***** 02/15/10 **********************************
     C                   Z-ADD     SRAMTP        XXCPTT
     C*** END ADD ***** 02/15/10 **********************************
     C*** BEG ADD ***** 04/01/08 **********************************
     C                   Z-ADD     SREFDT        SVEFDT
     C*** END ADD ***** 04/01/08 **********************************
     C                   ENDIF
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     SRTODT        IFNE      *ZERO
     C     SRRPMY        ANDEQ     *ZERO
      *
     C     SRCLAS        IFNE      SVCLAS
     C                   EXSR      CALMAN
     C                   Z-ADD     SRCLAS        SVCLAS
     C*** BEG ADD ***** 04/01/08 **********************************
     C                   Z-ADD     SREFDT        SVEFDT
     C*** END ADD ***** 04/01/08 **********************************
     C                   ELSE
     C                   ADD       SRAMTP        XXCPTT
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     SRKY1         READE     PSR10                                  60
      *
     C     *IN60         IFEQ      *ON
     C                   EXSR      CALMAN
     C                   MOVE      *ON           *IN60
     C                   ENDIF
      *
     C                   ENDDO
     C*
     C     XXMPTT        SUB       EMP(X)        XXWMPR
     C                   Z-ADD     *ZERO         EMP(X)
     C                   ADD       XXWMPR        XXNETM           15 2
      *** END ADD ***** 07/26/04 **********************************
     C*
      *** beg add ***** 12/15/14 **********************************
     C                   Else
     C                   Clear                   xxWmpr
     C                   Endif                                                  flgManCalc
      *** end add ***** 12/15/14 **********************************
     C*
     C                   ELSE
     C*  FOR CLAIMS REPLENISHMENT, JUST USE THE TRANSACTION AMOUNT.
     C                   Z-ADD     EUAMT         XXWAPR
     C                   ENDIF
     C*
     C*  INFORCE PREMIUM
     C*
     C     CANCLD        IFEQ      *ON
     C     XXPTDT        ORLE      XXVDAT
     C                   Z-ADD     *ZERO         XXINPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  04/27/04  **************************************
     C*******************  Z-ADD*ZERO     XXINEX
     C***  End   Del  ***  04/27/04  **************************************
     C***  End   Add  ***  01/08/04  **************************************
     C                   ELSE
     C                   Z-ADD     XXWAPR        XXINPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXIMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  04/27/04  **************************************
     C*******************  Z-ADDXXPRTD    XXINEX
     C***  End   Del  ***  01/08/04  **************************************
     C***  End   Add  ***  01/08/04  **************************************
     C                   ENDIF
     C*
     C*  PRIOR YEAR & YTD WRITTEN/EARNED
     C*
     C     EUPRDT        IFLT      XXJAN1
     C                   Z-ADD     XXWAPR        XXPYWR
     C                   Z-ADD     XXWAPR        XXPYEA
      *** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXPYWM
     C                   Z-ADD     XXWMPR        XXPYEM
      *** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C*** BEG DEL ***** 03/22/05 ******************************************
     C*****************    Z-ADDXXWATR    XXPYTR
     C*****************    Z-ADDXXWATR    XXPYET
     C*** END DEL ***** 03/22/05 ******************************************
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
     C                   Z-ADD     XXWAPR        XXYDWR
     C                   Z-ADD     XXWAPR        XXYDEA
      *** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXYDWM
     C                   Z-ADD     XXWMPR        XXYDEM
      *** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C*** BEG DEL ***** 03/22/05 ******************************************
     C*****************    Z-ADDXXWATR    XXYDWT
     C*****************    Z-ADDXXWATR    XXYDET
     C*** END DEL ***** 03/22/05 ******************************************
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C*  MTD WRITTEN/EARNED
     C*
     C     EUPRDT        IFGE      XXMTH1
     C                   Z-ADD     XXWAPR        XXMDWR
     C                   Z-ADD     XXWAPR        XXMDEA
      *** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXMDWM
     C                   Z-ADD     XXWMPR        XXMDEM
      *** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  04/27/04  **************************************
     C*******************  Z-ADDXXPRTD    XXMWEX
     C***  End   Del  ***  01/08/04  **************************************
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C*** BEG DEL ***** 03/22/05 ******************************************
     C*****************    Z-ADDXXWATR    XXMDWT
     C*****************    Z-ADDXXWATR    XXMDET
     C*** END DEL ***** 03/22/05 ******************************************
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C*  SAVE DATA TO ARRAYS
     C*
     C                   MOVE      'AR'          XACT(Q)
     C                   MOVE      EUPODT        FRDT(Q)
     C                   MOVE      EUPODT        TODT(Q)
     C                   MOVE      XXINPR        INPR(Q)
     C                   MOVE      XXWAPR        WAPR(Q)
      *** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      XXIMPR        IMPR(Q)
     C                   MOVE      XXWMPR        WMPR(Q)
      *** END ADD ***** 07/26/04 ******************************************
     C                   MOVE      *ZERO         WNMP(Q)
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      *ZERO         ITER(Q)
     C                   MOVE      *ZERO         WTER(Q)
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      *ZERO         IDTR(Q)
     C                   MOVE      *ZERO         WDTR(Q)
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MOVE      *ZERO         IVLC(Q)
     C                   MOVE      *ZERO         WVLC(Q)
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  04/27/04  **************************************
     C*******************  MOVE *ZERO     INEX,Q
     C*******************  MOVE *ZERO     PRTD,Q
     C***  End   Del  ***  01/08/04  **************************************
     C***  End   Add  ***  01/08/04  **************************************
     C                   MOVE      1             DAYS(Q)
     C***  Begin Add  ***  01/28/05  **************************************
     C*** BEG DEL ***** 03/22/05 ******************************************
     C*****************    MOVE XXWATR    WATR,Q
     C*** END DEL ***** 03/22/05 ******************************************
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C*  PRINT DETAIL OUTPUT
     C*
      *** BEG ADD ***** 05/19/05 **********************************
     C                   Z-ADD     EUTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C                   Z-ADD     EUPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
      *** END ADD ***** 05/19/05 **********************************
     C*
     C                   EXSR      PRTDET
     C*
     C                   ENDSR
      *
      /EJECT
     C**************************************************************
     C*  PROCESS EMPLOYER HISTORY
     C**************************************************************
      *
     C     SREMPH        BEGSR
      *
      **** BEGIN ADD **** 02/28/01 ************************************
      **** BEGIN DEL **** 01/21/02 *********************************************
     C********** QQPRTD    IFEQ 'Y'
      **** END   DEL **** 01/21/02 *********************************************
     C*  Calculate the full expense constant
     C     EUPROR        IFNE      *ZERO
      *** BEG ADD ***** 04/27/04 **************************************
     C     XXSFUL        ANDEQ     *BLANK
      *** END ADD ***** 04/27/04 **************************************
     C     EUPRTD        DIV(H)    EUPROR        XXFEXC            9 0
     C                   ELSE
     C                   Z-ADD     EUPRTD        XXFEXC
     C                   ENDIF                                                  EUPROR<>0
     C*
     C*  If first 'E' rec then store expense consts in save fields
     C     FRSTE         IFEQ      *ON
     C                   Z-ADD     EUPRTD        SVPRTD
     C                   Z-ADD     XXFEXC        SVFEXC
     C                   ENDIF                                                  FRSTE=*ON
     C*
      **** BEGIN ADD **** 01/21/02 *********************************************
     C     QQPRTD        IFEQ      'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C                   SUB       XXFEXC        EUIAPR
     C                   SUB       EUPRTD        EUWAPR
      **** BEGIN ADD **** 01/21/02 *********************************************
      *** BEG ADD ***** 07/21/04 ***********************************************
      *
      * SAVE LAST EXP CONSTANT VALUE FOR CX AUDIT.
      *
     C                   Z-ADD     EUPRTD        S2PRTD
      *** END ADD ***** 07/21/04 ***********************************************
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C*
     C     FRSTE         IFEQ      *ON
      **** BEGIN ADD **** 01/21/02 *********************************************
     C     QQPRTD        IFEQ      'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C                   SUB       XXFEXC        EUJAPR
     C                   SUB       EUPRTD        EUXAPR
      **** BEGIN ADD **** 01/21/02 *********************************************
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C*** BEGIN ADD ***  10/17/01  ************************************
     C                   Z-ADD     EUPRTD        XXWRXC            5 2
     C                   Z-ADD     XXFEXC        XXINXC            5 2
     C*** END ADD *****  10/17/01  ************************************
     C                   ELSE
     C*
     C     SVFEXC        IFNE      XXFEXC
     C     XXFEXC        SUB       SVFEXC        XXDIF             9 0
      **** BEGIN ADD **** 01/21/02 *********************************************
     C     QQPRTD        IFEQ      'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C                   SUB       XXDIF         EUJAPR
      **** BEGIN ADD **** 01/21/02 *********************************************
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C                   Z-ADD     XXFEXC        SVFEXC
     C*** BEGIN ADD ***  10/17/01  ************************************
     C                   Z-ADD     XXDIF         XXINXC            5 2
     C*** END ADD *****  10/17/01  ************************************
     C                   ENDIF                                                  SVFEXC<>XXFEXC
     C*
     C     SVPRTD        IFNE      EUPRTD
     C     EUPRTD        SUB       SVPRTD        XXDIF
      **** BEGIN ADD **** 01/21/02 *********************************************
     C     QQPRTD        IFEQ      'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C                   SUB       XXDIF         EUXAPR
      **** BEGIN ADD **** 01/21/02 *********************************************
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C                   Z-ADD     EUPRTD        SVPRTD
     C*** BEGIN ADD ***  10/17/01  ************************************
     C                   Z-ADD     XXDIF         XXWRXC            5 2
     C*** END ADD *****  10/17/01  ************************************
     C                   ENDIF                                                  SVPRTD<>EUPRTD
     C*
     C                   ENDIF                                                  FRSTE=*ON
     C*
      **** BEGIN DEL **** 01/21/02 *********************************************
     C******************** ENDIF                           QQPRTD='Y'
      **** END   DEL **** 01/21/02 *********************************************
     C*
     C                   MOVE      *OFF          FRSTE
      **** END   ADD **** 02/28/01 ************************************
     C*
     C                   MOVE      'EH'          XXSORT
     C     EUREVF        IFEQ      'Y'
     C                   MOVE      'RV'          XXSORT
     C                   ENDIF
     C*
     C*  FOR ORIGINAL BILL, PRORATE FROM POLICY START DATE
     C*
     C     EUFAF         IFEQ      'E'
     C                   MOVE      ELPFDT        EUPRDT
     C                   ENDIF
     C*
     C*  CALCULATE PRINTED FIELDS
     C*       INFORCE PREMIUM
     C*
     C     EUFAF         IFEQ      'E'
     C                   Z-ADD     EUIAPR        XXINPR           11 2
     C                   Z-ADD     EUWAPR        XXWAPR           11 2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUIMPR        XXIMPR
     C                   Z-ADD     EUWMPR        XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUWNMP        XXWNMP           11 2
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     EUITER        XXITER
     C                   Z-ADD     EUWTER        XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     EUIDTR        XXIDTR
     C                   Z-ADD     EUWDTR        XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     EUIVLC        XXIVLC
     C                   Z-ADD     EUWVLC        XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXFEXC        XXINEX
     C                   Z-ADD     EUPRTD        XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     EUWTER        XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
     C     EUFAF         IFEQ      'R'
     C*
     C     EURWDT        IFNE      SVRWDT
     C                   Z-ADD     EUIAPR        XXINPR
     C                   Z-ADD     EUWAPR        XXWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUIMPR        XXIMPR
     C                   Z-ADD     EUWMPR        XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUWNMP        XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     EUITER        XXITER
     C                   Z-ADD     EUWTER        XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     EUIDTR        XXIDTR
     C                   Z-ADD     EUWDTR        XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     EUIVLC        XXIVLC
     C                   Z-ADD     EUWVLC        XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXFEXC        XXINEX
     C                   Z-ADD     EUPRTD        XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     EUWTER        XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
     C                   Z-ADD     EUJAPR        XXINPR
     C                   Z-ADD     EUXAPR        XXWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUJMPR        XXIMPR
     C                   Z-ADD     EUXMPR        XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUXNMP        XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     EUJTER        XXITER
     C                   Z-ADD     EUXTER        XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     EUJDTR        XXIDTR
     C                   Z-ADD     EUXDTR        XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     EUJVLC        XXIVLC
     C                   Z-ADD     EUXVLC        XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXINXC        XXINEX
     C                   Z-ADD     XXWRXC        XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     EUXTER        XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C                   ELSE
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     EUJTER        XXITER
     C                   Z-ADD     EUXTER        XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     EUJDTR        XXIDTR
     C                   Z-ADD     EUXDTR        XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C***  BEG ADD ******  03/07/05  **************************************
     C                   Z-ADD     EUXTER        XXWATR
     C***  END ADD ******  03/07/05  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     EUJVLC        XXIVLC
     C                   Z-ADD     EUXVLC        XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXWRXC        XXPRTD
     C                   Z-ADD     XXINXC        XXINEX
     C***  End   Add  ***  01/08/04  **************************************
     C                   Z-ADD     EUJAPR        XXINPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUJMPR        XXIMPR
     C                   Z-ADD     EUXMPR        XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C*  FOR AUDIT TRANSACTIONS, SUBTRACT NET CHANGES MADE BY SELF
     C*  REPORTING AND CLAIMS REPLENISHMENT BILLS.
     C     EUXAPR        SUB       XXNETR        XXWAPR
     C     EUXNMP        SUB       XXNETR        XXWNMP
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     EUXMPR        SUB       XXNETM        XXWMPR
     C                   CLEAR                   XXNETM
     C*** END ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   XXNETR
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      EURWDT        SVRWDT
     C*
     C*       INFORCE PREMIUM
     C*
     C     XXPTDT        IFLE      XXVDAT
     C                   Z-ADD     *ZERO         XXINPR
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXINEX
     C                   Z-ADD     *ZERO         XXITER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   ENDIF
     C*
     C*  SAVE DATA TO ARRAYS
     C*
     C                   MOVE      'EH'          XACT(Q)
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          MOVE EUPODT    FRDT,Q
     C     EUPODT        IFLT      SVPFDT
     C                   MOVE      SVPODT        FRDT(Q)
     C                   ELSE
     C                   MOVE      EUPODT        FRDT(Q)
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
     C                   MOVE      XXPTDT        TODT(Q)
     C                   MOVE      XXINPR        INPR(Q)
     C                   MOVE      XXWAPR        WAPR(Q)
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      XXIMPR        IMPR(Q)
     C                   MOVE      XXWMPR        WMPR(Q)
     C*** END ADD ***** 07/26/04 ******************************************
     C                   MOVE      XXWNMP        WNMP(Q)
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      XXITER        ITER(Q)
     C                   MOVE      XXWTER        WTER(Q)
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      XXIDTR        IDTR(Q)
     C                   MOVE      XXWDTR        WDTR(Q)
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MOVE      XXIVLC        IVLC(Q)
     C                   MOVE      XXWVLC        WVLC(Q)
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   MOVE      XXINEX        INEX(Q)
     C                   MOVE      XXPRTD        PRTD(Q)
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   MOVE      XXWATR        WATR(Q)
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C*  GET DAYS
     C*
      *** BEG CHG ***** 11/28/06 ******************************************
     C***********          Z-ADDEUPODT    XQFRM7
      *** BEG CHG ***** 01/18/07 ******************************************
     C***********          Z-ADDELPFDT    XQFRM7
      *** BEG CHG ***** 02/26/07 ******************************************
     C***********          Z-ADDSVPFDT    XQFRM7
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *** END CHG ***** 02/26/07 ******************************************
      *** END CHG ***** 01/18/07 ******************************************
      *** END CHG ***** 11/28/06 ******************************************
     C                   Z-ADD     XXPTDT        XQTO7
     C                   EXSR      GETDYS
     C                   MOVE      QQDAYS        DAYS(Q)
     C*
     C*  CALCULATE COLUMN AMOUNTS
     C*
     C                   EXSR      SRCALC
     C*
     C                   MOVE      *OFF          XXCAST
     C*
     C                   ENDSR                                                  SREMPH
      /EJECT
     C**************************************************************
     C*  PROCESS CANCELLATION AUDITS SUB-ROUTINE
     C**************************************************************
      *
     C     SRCXAU        BEGSR
     C*
      **** BEGIN ADD **** 01/30/02 *********************************************
     C     FRSTE         IFEQ      *ON
     C                   Z-ADD     EUPRTD        SVPRTD
     C     QQPRTD        IFEQ      'Y'
     C                   SUB       EUPRTD        EUXAPR
     C                   SUB       EUPRTD        EUWAPR
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
     C                   Z-ADD     EUPRTD        XXWRXC
     C                   MOVE      *OFF          FRSTE
     C                   ELSE                                                   FRSTE=*ON
      **** END   ADD **** 01/30/02 *********************************************
      **** BEGIN ADD **** 01/04/02 *********************************************
      **** BEGIN DEL **** 01/21/02 *********************************************
     C********** QQPRTD    IFEQ 'Y'
      **** END   DEL **** 01/21/02 *********************************************
      *
      **** BEGIN ADD **** 01/30/02 *********************************************
     C     QQPRTD        IFEQ      'Y'
     C                   SUB       EUPRTD        EUWAPR
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
      **** END   ADD **** 01/30/02 *********************************************
     C     SVPRTD        IFNE      EUPRTD
      *** BEG ADD ***** 07/21/04 ***********************************************
     C     S2PRTD        ORNE      EUPRTD
     C     EUPRTD        SUB       S2PRTD        XXDIF2            9 0
      *** END ADD ***** 07/21/04 ***********************************************
     C     EUPRTD        SUB       SVPRTD        XXDIF
      **** BEGIN ADD **** 01/21/02 *********************************************
     C     QQPRTD        IFEQ      'Y'
      **** END   ADD **** 01/21/02 *********************************************
      *** BEG CHG ***** 07/21/04 ***********************************************
     C*****************    SUB  XXDIF     EUXAPR
      *** BEG CHG ***** 05/26/05 ***********************************************
     C*****************    SUB  XXDIF2    EUXAPR
     C     XXCXST        IFEQ      *ON
     C     XXCAST        ANDEQ     *ON
     C                   SUB       XXDIF         EUXAPR
     C                   ELSE
     C                   SUB       XXDIF2        EUXAPR
     C                   ENDIF
      *** END CHG ***** 05/26/05 ***********************************************
      *** END CHG ***** 07/21/04 ***********************************************
      **** BEGIN ADD **** 01/21/02 *********************************************
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
      **** END   ADD **** 01/21/02 *********************************************
     C                   Z-ADD     EUPRTD        SVPRTD
     C                   Z-ADD     XXDIF         XXWRXC            5 2
     C                   ENDIF                                                  SVPRTD IFNE EUPRTD
      **** BEGIN ADD **** 01/30/02 *********************************************
     C                   ENDIF                                                  FRSTE IFEQ *ON
      **** END   ADD **** 01/30/02 *********************************************
      *
      **** BEGIN DEL **** 01/21/02 *********************************************
     C******************** ENDIF                           QQPRTD IFEQ 'Y'
      **** END   DEL **** 01/21/02 *********************************************
      **** END   ADD **** 01/04/02 *********************************************
     C                   MOVE      'CA'          XXSORT
     C     EUREVF        IFEQ      'Y'
     C                   MOVE      'RV'          XXSORT
     C                   ENDIF
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     EUJTER        XXITER
     C                   Z-ADD     EUXTER        XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     EUJDTR        XXIDTR
     C                   Z-ADD     EUXDTR        XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C***  BEG ADD ******  03/07/05  **************************************
     C                   Z-ADD     EUXTER        XXWATR
     C***  END ADD ******  03/07/05  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     EUJVLC        XXIVLC
     C                   Z-ADD     EUXVLC        XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXWRXC        XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     EUXMPR        XXWMPR                         Q #1
     C*** END ADD ***** 07/26/04 ******************************************
     C     EUXAPR        SUB       XXNETR        XXWAPR
     C     EUXNMP        SUB       XXNETR        XXWNMP
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     EUXMPR        SUB       XXNETM        XXWMPR
     C                   CLEAR                   XXNETM
     C*** END ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   XXNETR
     C*** BEG CHG ***** 03/23/06 ******************************************
     C*********  Q         SUB  1         Z       30
     C     Q             SUB       1             Z                 6 0
     C*** END CHG ***** 03/23/06 ******************************************
     C*
     C     Z             DOWGE     1
     C*  STOP LOOKING IF A REINSTATE OR CX AUDIT ENCOUNTERED
     C     XACT(Z)       IFEQ      'CA'
     C     XACT(Z)       OREQ      'RE'
     C     XXCAST        ANDEQ     *OFF
     C     XACT(Z)       OREQ      'XR'
     C     XXCAST        ANDEQ     *OFF
     C                   LEAVE
     C                   ENDIF
     C*
     C     XACT(Z)       IFEQ      'RE'
     C     XXCAST        ANDEQ     *ON
     C     XACT(Z)       OREQ      'XR'
     C     XXCAST        ANDEQ     *ON
     C                   Z-ADD     *ZERO         XXINPR
     C                   SUB       WAPR(Z)       XXWAPR
     C                   SUB       WNMP(Z)       XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXITER
     C                   SUB       WTER(Z)       XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C                   SUB       WDTR(Z)       XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C                   SUB       WVLC(Z)       XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
      *** BEG DEL ***** 07/21/04 ******************************************
     C********** 07/21/04  Z-ADD*ZERO     XXINEX
     C********** 07/21/04  SUB  PRTD,Z    XXPRTD
      *** END DEL ***** 07/21/04 ******************************************
     C***  End   Add  ***  01/08/04  **************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C                   SUB       WMPR(Z)       XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   SUB       WATR(Z)       XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C                   LEAVE
     C                   ENDIF
     C*
     C     XACT(Z)       IFEQ      'CX'
     C     XACT(Z)       OREQ      'XC'
     C                   MOVE      *OFF          XXCXST
     C*
     C                   Z-ADD     *ZERO         XXINPR
     C                   SUB       WAPR(Z)       XXWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C                   SUB       WMPR(Z)       XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   SUB       WNMP(Z)       XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXITER
     C                   SUB       WTER(Z)       XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C                   SUB       WDTR(Z)       XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C                   SUB       WVLC(Z)       XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C*** BEG DEL ***** 07/21/04 ******************************************
     C********** 07/21/04  SUB  PRTD,Z    XXPRTD
     C*** END DEL ***** 07/21/04 ******************************************
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   SUB       WATR(Z)       XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C                   SUB       1             Z
     C                   ENDDO
     C*
     C*  SAVE DATA TO ARRAYS
     C*
     C                   MOVE      'CA'          XACT(Q)
     C                   MOVE      EUPODT        FRDT(Q)
     C                   MOVE      XXPTDT        TODT(Q)
     C                   MOVE      XXINPR        INPR(Q)
     C                   MOVE      XXWAPR        WAPR(Q)
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      XXIMPR        IMPR(Q)
     C                   MOVE      XXWMPR        WMPR(Q)
     C*** END ADD ***** 07/26/04 ******************************************
     C                   MOVE      XXWNMP        WNMP(Q)
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      XXITER        ITER(Q)
     C                   MOVE      XXWTER        WTER(Q)
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      XXIDTR        IDTR(Q)
     C                   MOVE      XXWDTR        WDTR(Q)
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MOVE      XXIVLC        IVLC(Q)
     C                   MOVE      XXWVLC        WVLC(Q)
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   MOVE      XXINEX        INEX(Q)
     C                   MOVE      XXPRTD        PRTD(Q)
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   MOVE      XXWATR        WATR(Q)
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C                   Z-ADD     EUPODT        XQFRM7
     C                   Z-ADD     XXPTDT        XQTO7
     C                   EXSR      GETDYS
     C                   MOVE      QQDAYS        DAYS(Q)
     C*
     C*  CALCULATE COLUMN AMOUNTS
     C*
     C                   EXSR      SRCALC
     C*
     C                   MOVE      *ON           XXCAST
     C*
     C                   ENDSR
      /EJECT
     C**************************************************************
     C*  PROCESS CANCELLATIONS SUB-ROUTINE
     C**************************************************************
     C     SRCANX        BEGSR
     C*
     C                   MOVE      'CX'          XXSORT
     C     EUSORT        IFEQ      '6'
     C                   MOVE      'XC'          XXSORT
     C                   ENDIF
     C*
      *** BEG ADD ***** 11/28/06 ******************************************
     C                   MOVE      *OFF          SRCAFL
     C     XXSORT        IFEQ      'CX'
     C                   MOVE      EUPODT        SVCXDT            7 0
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
     C                   Z-ADD     XXWAPR        SVWAPR           11 2
     C                   Z-ADD     *ZERO         XXWAPR
     C                   Z-ADD     XXWNMP        SVWNMP           11 2
     C                   Z-ADD     *ZERO         XXWNMP
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        SVWMPR           11 2
     C                   Z-ADD     *ZERO         XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXWTER        SVWTER           11 2
     C                   Z-ADD     *ZERO         XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     XXWDTR        SVWDTR           11 2
     C                   Z-ADD     *ZERO         XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     XXWVLC        SVWVLC           11 2
     C                   Z-ADD     *ZERO         XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C***  Begin Chg  ***  03/10/04  **************************************
     C**********           Z-ADDXXPRTD    SVPRTD
     C*** BEG DEL ***** 07/21/04 ******************************************
     C*****************    Z-ADDXXPRTD    S2PRTD
     C*** END DEL ***** 07/21/04 ******************************************
     C***  End   Chg  ***  03/10/04  **************************************
     C                   Z-ADD     *ZERO         XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     XXWATR        SVWATR
     C                   Z-ADD     *ZERO         XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C     XXCAST        IFEQ      *OFF
     C*
      *** BEG ADD ***** 11/28/06 ******************************************
      *
      *  IF POLICY IS NOT FLAT CANCELLED,
      *  FIND OUT IF ANY SELF REPORTS EXIST WITH A POST DATE <= CX POST DATE.
      *  USE FIRST DAY OF SUBSEQUENT MTH TO CALCULATE CX PERIOD IF ONE EXISTS.
      *
     C     SVCXDT        IFNE      ELPFDT
      *
     C     Q             SUB       1             Z
     C     XXSORT        IFEQ      'XC'
     C     Z             SUB       1             Y
     C                   ELSE
     C                   Z-ADD     1             Y
     C                   ENDIF                                                  XXSORT IFEQ 'XC'
     C*
      *** BEG ADD ***** 05/25/07 ******************************************
     C                   MOVE      *OFF          ADJDTF            1            ADJUST DATE FLAG
     C                   Z-ADD     0             SVACXD            7 0          SAVE ADJ CX DATE
      *** END ADD ***** 05/25/07 ******************************************
     C                   Z-ADD     0             SVCNDT            7 0
     C     Z             DOWGE     Y
     C     XACT(Z)       IFEQ      'AR'
     C                   MOVEL     EUPODT        X5DAT1            5 0          FROM 'CX' TRANS
     C                   MOVEL     FRDT(Z)       X5DAT2            5 0          FROM 'AR' TRANS
     C     X5DAT2        SUB       X5DAT1        X5DAT2
     C     X5DAT2        IFEQ      *ZERO                                        SAME MTH/YR
     C                   Z-ADD     EUPODT        SVCNDT                         SV DATE FOR CX
     C                   Z-ADD     1             XEUPOD
     C                   ADD       1             XEUPOM
     C     XEUPOM        IFEQ      13
     C                   Z-ADD     1             XEUPOM
     C                   ADD       1             XEUPOC
     C                   ENDIF
      *** BEG ADD ***** 05/25/07 ******************************************
     C                   MOVE      *ON           ADJDTF                         ADJUST DATE FLAG
     C                   Z-ADD     EUPODT        SVACXD            7 0          SAVE ADJ CX DATE
      *** END ADD ***** 05/25/07 ******************************************
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   SUB       1             Z
     C                   ENDDO
      *
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *
     C     Q             SUB       1             Z
     C     XXSORT        IFEQ      'XC'
     C     Z             SUB       1             Y
     C                   ELSE
     C                   Z-ADD     1             Y
     C                   ENDIF                                                  XXSORT IFEQ 'XC'
     C*
     C*  ACCUMULATE PROCESSED TRANSACTIONS
     C*
      *** BEG ADD ***** 11/28/06 ******************************************
     C                   Z-ADD     0             XXCXCY
     C                   Z-ADD     0             XXCXPY
      *** END ADD ***** 11/28/06 ******************************************
     C     Z             DOWGE     Y
     C     INPR(Z)       MULT      -1            XXWORK
     C                   ADD       XXWORK        XXINPR
     C***  Begin Add  ***  12/01/03  **************************************
     C     ITER(Z)       MULT      -1            XXWORK
     C                   ADD       XXWORK        XXITER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C     IDTR(Z)       MULT      -1            XXWORK
     C                   ADD       XXWORK        XXIDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     IVLC(Z)       MULT      -1            XXWORK
     C                   ADD       XXWORK        XXIVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C     INEX(Z)       MULT      -1            XXWORK
     C                   ADD       XXWORK        XXINEX
     C***  End   Add  ***  01/08/04  **************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     IMPR(Z)       MULT      -1            XXWORK
     C                   ADD       XXWORK        XXIMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C*
     C*  IF TRANSACTION DATES ARE BOTH GREATER THAN THE CANCEL POST DT
     C*  OR ALREADY CANCELLED AND A CANCEL TRANSACTION,
     C*  REVERSE THE FULL AMOUNT.
     C*  SEPERATE 'EA' ADJUSTMENTS FROM PREMIUM SO THAT THEY DON'T GET PRORATED
     C*
     C     FRDT(Z)       IFGE      EUPODT
     C     TODT(Z)       ANDGE     EUPODT
     C     XXCXST        OREQ      *ON
     C     XACT(Z)       ANDEQ     'CX'
     C     XXCXST        OREQ      *ON
     C     XACT(Z)       ANDEQ     'XC'
      *** BEG ADD ***** 11/28/06 ******************************************
     C     SVCNDT        ORNE      *ZERO
     C     SVCNDT        ANDEQ     ELPFDT                                       FLAT CX ON S/R POLIC
      *** END ADD ***** 11/28/06 ******************************************
     C                   MOVE      *OFF          XXCXST
      *
     C     WAPR(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWAPR                         ADD IT UP
     C*
      *** BEG ADD ***** 11/28/06 ******************************************
     C     XACT(Z)       IFEQ      'EH'
      *** BEG CHG ***** 02/09/07 ******************************************
     C*****************    SUB  SRAD,Z    XXWAPR
     C     SRAD(Z)       IFNE      *ZERO
     C                   MOVE      *ON           SRCAFL            1            SR CX ADJ FLG
     C                   ADD       SRAD(Z)       XXWAPR
     C     EUTRDT        IFLT      XXJAN1
     C     EUPODT        ANDLT     XXJAN1
     C                   SUB       SRAD(Z)       XXCXPY           11 2          CX IN PRIOR YEAR
     C                   ELSE
     C                   SUB       SRAD(Z)       XXCXCY           11 2          CX IN CURR YEAR
     C                   ENDIF                                                  EUTRDT <= XXYEND
     C                   ENDIF                                                  SRAD,Z
      *** END CHG ***** 02/09/07 ******************************************
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
     C*
      *** BEG ADD ***** 11/28/06 ******************************************
     C     XACT(Z)       IFEQ      'AR'
     C                   MOVE      *ON           SRCAFL            1            SR CX ADJ FLG
     C                   SUB       XXWORK        XXWAPR
     C                   MOVEL     SVCNDT        X5DAT1            5 0          FROM 'CX' TRANS
     C                   MOVEL     FRDT(Z)       X5DAT2            5 0          FROM 'AR' TRANS
     C     X5DAT2        SUB       X5DAT1        X5DAT2
     C     X5DAT2        IFGT      *ZERO                                        'AR' IN FUTURE
     C     SVCNDT        OREQ      ELPFDT                                       FLAT CX
      *** BEG CHG ***** 02/09/07 ******************************************
     C********** EUTRDT    IFLE XXYEND
     C     EUTRDT        IFLT      XXJAN1
     C     EUPODT        ANDLT     XXJAN1
      *** END CHG ***** 02/09/07 ******************************************
     C                   ADD       XXWORK        XXCXPY           11 2          CX IN PRIOR YEAR
     C                   ELSE
     C                   ADD       XXWORK        XXCXCY           11 2          CX IN CURR YEAR
     C                   ENDIF                                                  FRDT,Z LE XXYEND
     C                   ADD       XXWORK        SRAD(Q)                        SAVE FOR REINSTATE
     C                   ENDIF                                                  XXDAT2 = 0
     C                   ENDIF                                                  XACT,Z = 'AR'
      *
     C     XACT(Z)       IFEQ      'XR'
     C     XACT(Z)       OREQ      'RE'
     C     SRAD(Z)       IFNE      *ZERO
     C                   MOVE      *ON           SRCAFL
     C                   ADD       SRAD(Z)       SRAD(Q)                        SAVE FOR REINSTATE
      *** BEG CHG ***** 02/09/07 ******************************************
     C********** EUTRDT    IFLE XXYEND
     C     EUTRDT        IFLT      XXJAN1
     C     EUPODT        ANDLT     XXJAN1
      *** END CHG ***** 02/09/07 ******************************************
     C                   ADD       SRAD(Z)       XXCXPY
     C                   ELSE
     C                   ADD       SRAD(Z)       XXCXCY
     C                   ENDIF
     C                   ENDIF                                                  SRAD,Z <> 0
     C                   ENDIF                                                  XACT,Z = 'XR'
      *** END ADD ***** 11/28/06 ******************************************
     C*
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     WMPR(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWMPR                         ADD IT UP
     C*** END ADD ***** 07/26/04 ******************************************
     C*
     C     WNMP(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWNMP                         ADD IT UP
     C***  Begin Add  ***  12/01/03  **************************************
     C*
     C     WTER(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWTER                         ADD IT UP
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C*
     C     WDTR(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWDTR                         ADD IT UP
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C*
     C     WVLC(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWVLC                         ADD IT UP
     C*** END ADD ***** 09/01/04 ******************************************
     C*
     C***  Begin Add  ***  01/08/04  **************************************
     C     PRTD(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXPRTD                         ADD IT UP
     C***  End   Add  ***  01/08/04  **************************************
     C*
     C***  Begin Add  ***  01/28/05  **************************************
     C     WATR(Z)       MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWATR                         ADD IT UP
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C                   ELSE
     C*
     C*  DETERMINE CANCELLATION DAYS FOR TRANSACTION PRORATION
     C*
     C     FRDT(Z)       IFLT      EUPODT
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ELSE
     C                   Z-ADD     FRDT(Z)       XQFRM7
     C                   ENDIF
     C*
     C     TODT(Z)       IFLT      EUPODT
     C                   Z-ADD     TODT(Z)       XQTO7
     C                   ELSE
     C                   Z-ADD     XXPTDT        XQTO7
     C                   ENDIF
     C*
     C                   EXSR      GETDYS
     C*
     C*  IF CANCELLATION HAS AFFECTED THIS TRANSACTION, DERIVE AFFECTED
     C*  WRITTEN ANNUAL PREMIUM AND WRITTEN NORMAL PREMIUM.
     C*   FIRST DERIVE THE AVERAGE DAILY AMOUNT, MULTIPLY IT BY THE NUMBER
     C*   OF DAYS AFFECTED BY THE CANCELLATION, THEN NEGATE THE AMOUNT.
     C*
     C     QQDAYS        IFGE      1
     C     DAYS(Z)       ANDGE     1
     C*
      *** BEG CHG ***** 11/28/06 ****************************************
     C*********  WAPR,Z    DIV  DAYS,Z    XXDAMT 155H      GET DAILY AMT
     C*********  XXDAMT    MULT QQDAYS    XXWORK 112       * CANCEL DAYS
     C*********            MULT -1        XXWORK           NEGATE
     C*********            ADD  XXWORK    XXWAPR           ADD IT UP
     C     QQDAYS        DIV(H)    DAYS(Z)       XXPRO
     C*
      *** BEG ADD ***** 11/28/06 ******************************************
     C     XACT(Z)       IFEQ      'EH'
     C                   SUB       SRAD(Z)       WAPR(Z)
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
     C*
     C     WAPR(Z)       MULT(H)   XXPRO         XXWORK
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWAPR                         ADD IT UP
     C*
      *** BEG ADD ***** 11/28/06 ******************************************
     C     XACT(Z)       IFEQ      'EH'
     C                   ADD       SRAD(Z)       WAPR(Z)
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
     C*
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     WMPR(Z)       DIV(H)    DAYS(Z)       XXDAMT           15 5          GET DAILY AMT
     C     XXDAMT        MULT      QQDAYS        XXWORK           11 2          * CANCEL DAYS
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWMPR                         ADD IT UP
     C*** END ADD ***** 07/26/04 ******************************************
     C*
     C     WNMP(Z)       DIV(H)    DAYS(Z)       XXDAMT                         GET DAILY AMT
     C     XXDAMT        MULT      QQDAYS        XXWORK                         * CANCEL DAYS
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWNMP                         ADD IT UP
     C***  Begin Add  ***  12/01/03  **************************************
     C*
     C     WTER(Z)       DIV(H)    DAYS(Z)       XXDAMT                         GET DAILY AMT
     C     XXDAMT        MULT      QQDAYS        XXWORK                         * CANCEL DAYS
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWTER                         ADD IT UP
     C***  End   Add  ***  12/01/03  **************************************
     C*
     C***  BEG ADD ******  01/26/06  **************************************
     C     WDTR(Z)       DIV(H)    DAYS(Z)       XXDAMT                         GET DAILY AMT
     C     XXDAMT        MULT      QQDAYS        XXWORK                         * CANCEL DAYS
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWDTR                         ADD IT UP
     C***  END ADD ******  01/26/06  **************************************
     C*
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     WVLC(Z)       DIV(H)    DAYS(Z)       XXDAMT                         GET DAILY AMT
     C     XXDAMT        MULT      QQDAYS        XXWORK                         * CANCEL DAYS
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWVLC                         ADD IT UP
     C*** END ADD ***** 09/01/04 ******************************************
     C*
     C***  Begin Add  ***  01/08/04  **************************************
     C     PRTD(Z)       DIV(H)    DAYS(Z)       XXDAMT                         GET DAILY AMT
     C     XXDAMT        MULT      QQDAYS        XXWORK                         * CANCEL DAYS
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXPRTD                         ADD IT UP
     C***  End   Add  ***  01/08/04  **************************************
     C*
     C***  Begin Add  ***  01/28/05  **************************************
     C     WATR(Z)       DIV(H)    DAYS(Z)       XXDAMT           15 5          GET DAILY AMT
     C     XXDAMT        MULT      QQDAYS        XXWORK                         * CANCEL DAYS
     C                   MULT      -1            XXWORK                         NEGATE
     C                   ADD       XXWORK        XXWATR                         ADD IT UP
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  FRDT,Z>=EUPODT
     C*
     C                   SUB       1             Z
     C                   ENDDO
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         XXINPR
     C                   Z-ADD     *ZERO         XXWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C                   Z-ADD     *ZERO         XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXITER
     C                   Z-ADD     *ZERO         XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C                   Z-ADD     *ZERO         XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C                   Z-ADD     *ZERO         XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXINEX
     C                   Z-ADD     *ZERO         XXPRTD
     C***  Begin Add  ***  03/10/04  **************************************
     C                   Z-ADD     SVPRTD        EUPRTD
     C***  End   Add  ***  03/10/04  **************************************
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C***  Begin Add  ***  03/10/04  **************************************
     C***  BEG CHG ******  07/21/04  **************************************
     C*******************  Z-ADDEUPRTD    SVPRTD
      *** BEG ADD ***** 05/26/05 ******************************************
     C     XXPRTD        MULT(H)   1             X1PRTD            5 0
     C                   Z-ADD     X1PRTD        XXPRTD
      *** END ADD ***** 05/26/05 ******************************************
     C     SVPRTD        ADD       XXPRTD        SVPRTD
     C***  END CHG ******  07/21/04  **************************************
     C***  End   Add  ***  03/10/04  **************************************
     C*
     C                   MOVE      *ON           XXCXST
     C*
     C*  SAVE DATA TO ARRAYS
     C*
     C                   MOVE      XXSORT        XACT(Q)
     C                   MOVE      EUPODT        FRDT(Q)
      *** BEG ADD ***** 11/28/06 **********************************************
     C********   SVCNDT    IFNE *ZERO
     C********             MOVE SVCNDT    FRDT,Q
     C********             ENDIF
      *** END ADD ***** 11/28/06 **********************************************
     C                   MOVE      XXPTDT        TODT(Q)
     C                   MOVE      XXINPR        INPR(Q)
     C                   MOVE      XXWAPR        WAPR(Q)
      *** BEG ADD ***** 11/28/06 ******************************************
     C     SRCAFL        IFEQ      *ON
     C                   ADD       XXCXPY        WAPR(Q)
     C                   ADD       XXCXCY        WAPR(Q)
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
     C                   MOVE      XXWNMP        WNMP(Q)
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      XXIMPR        IMPR(Q)
     C                   MOVE      XXWMPR        WMPR(Q)
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      XXITER        ITER(Q)
     C                   MOVE      XXWTER        WTER(Q)
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      XXIDTR        IDTR(Q)
     C                   MOVE      XXWDTR        WDTR(Q)
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MOVE      XXIVLC        IVLC(Q)
     C                   MOVE      XXWVLC        WVLC(Q)
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   MOVE      XXINEX        INEX(Q)
     C                   MOVE      XXPRTD        PRTD(Q)
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   MOVE      XXWATR        WATR(Q)
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C                   Z-ADD     EUPODT        XQFRM7
     C                   Z-ADD     XXPTDT        XQTO7
     C                   EXSR      GETDYS
     C                   MOVE      QQDAYS        DAYS(Q)
     C*
     C*  CALCULATE COLUMN AMOUNTS
     C*
     C                   EXSR      SRCALC
     C*
     C                   Z-ADD     SVWAPR        XXWAPR
     C                   Z-ADD     *ZERO         SVWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     SVWMPR        XXWMPR
     C                   Z-ADD     *ZERO         SVWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     SVWNMP        XXWNMP
     C                   Z-ADD     *ZERO         SVWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     SVWTER        XXWTER
     C                   Z-ADD     *ZERO         SVWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     SVWDTR        XXWDTR
     C                   Z-ADD     *ZERO         SVWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     SVWVLC        XXWVLC
     C                   Z-ADD     *ZERO         SVWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C***  Begin Chg  ***  03/10/04  **************************************
     C**********           Z-ADDSVPRTD    XXPRTD
     C**********           Z-ADD*ZERO     SVPRTD
     C*** BEG DEL ***** 07/21/04 ******************************************
     C*****************    Z-ADDS2PRTD    XXPRTD
     C*****************    Z-ADD*ZERO     S2PRTD
     C*** END DEL ***** 07/21/04 ******************************************
     C***  End   Chg  ***  03/10/04  **************************************
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     SVWATR        XXWATR
     C                   Z-ADD     *ZERO         SVWATR
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C*  SAVE THE CANCELLATION DATE
     C*
     C                   MOVE      EUPODT        XXCNDT
     C                   Z-ADD     EUTRDT        XXSVTD            7 0
     C                   Z-ADD     EUPODT        XXSVPD            7 0
     C*
     C                   ENDSR
      /EJECT
     C**************************************************************
     C*  PROCESS REINSTATEMENTS SUB-ROUTINE
     C**************************************************************
     C*
     C     SRREIN        BEGSR
     C*
     C                   MOVE      'RE'          XXSORT
     C     EUSORT        IFEQ      '7'
     C                   MOVE      'XR'          XXSORT
     C                   ENDIF
     C                   Z-ADD     *ZERO         XXINPR
     C                   Z-ADD     *ZERO         XXWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C                   Z-ADD     *ZERO         XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXITER
     C                   Z-ADD     *ZERO         XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C                   Z-ADD     *ZERO         XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C                   Z-ADD     *ZERO         XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXINEX
     C                   Z-ADD     *ZERO         XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C     XXCAST        IFEQ      *OFF
     C*
     C     Q             SUB       1             Z
     C*
     C     Z             DOWGE     1
     C     XACT(Z)       ANDNE     'RE'
     C     XACT(Z)       ANDNE     'XR'
     C*
     C     XACT(Z)       IFEQ      'CX'
     C***  Begin Del  ***  06/09/04  **************************************
     C     XACT(Z)       OREQ      'CA'
     C***  End   Del  ***  06/09/04  **************************************
     C     XACT(Z)       OREQ      'XC'
     C                   MOVE      *OFF          XXCXST
     C                   ADD       INPR(Z)       XXINPR
     C                   ADD       WAPR(Z)       XXWAPR
      *** BEG ADD ***** 11/28/06 ******************************************
      *
      *  DON'T LET S/R ADJUSTMENTS CONTAINED IN CX WRTN PREM GET PRO RATED
      *  IN SRCALC
      *
     C     SRAD(Z)       IFNE      *ZERO
      *
     C                   MOVE      *ON           SRCAFL
     C                   SUB       SRAD(Z)       XXWAPR
     C                   ADD       SRAD(Z)       SRAD(Q)
      *** BEG CHG ***** 02/09/07 ******************************************
     C********** EUTRDT    IFLE XXYEND
     C     EUTRDT        IFLT      XXJAN1
     C     EUPODT        ANDLT     XXJAN1
      *** END CHG ***** 02/09/07 ******************************************
     C                   SUB       SRAD(Z)       XXCXPY
     C                   ELSE
     C                   SUB       SRAD(Z)       XXCXCY
     C                   ENDIF
      *
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   ADD       IMPR(Z)       XXIMPR
     C                   ADD       WMPR(Z)       XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   ADD       WNMP(Z)       XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   ADD       ITER(Z)       XXITER
     C                   ADD       WTER(Z)       XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   ADD       IDTR(Z)       XXIDTR
     C                   ADD       WDTR(Z)       XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   ADD       IVLC(Z)       XXIVLC
     C                   ADD       WVLC(Z)       XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   ADD       INEX(Z)       XXINEX
     C                   ADD       PRTD(Z)       XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   ADD       WATR(Z)       XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C*  RESET THE CANCELLATION DATE
     C*
     C                   MOVE      9999999       XXCNDT
     C                   ENDIF
     C*
     C                   SUB       1             Z
     C                   ENDDO
     C*
     C                   ELSE
     C                   Z-ADD     *ZERO         XXINPR
     C                   Z-ADD     *ZERO         XXWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C                   Z-ADD     *ZERO         XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXITER
     C                   Z-ADD     *ZERO         XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C                   Z-ADD     *ZERO         XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C                   Z-ADD     *ZERO         XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXINEX
     C                   Z-ADD     *ZERO         XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C                   MULT      -1            XXINPR
     C                   MULT      -1            XXWAPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MULT      -1            XXIMPR
     C                   MULT      -1            XXWMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   MULT      -1            XXWNMP
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MULT      -1            XXITER
     C                   MULT      -1            XXWTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MULT      -1            XXIDTR
     C                   MULT      -1            XXWDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MULT      -1            XXIVLC
     C                   MULT      -1            XXWVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   MULT      -1            XXINEX
     C                   MULT      -1            XXPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   MULT      -1            XXWATR
     C***  End   Add  ***  01/28/05  **************************************
     C*
     C*  SAVE DATA TO ARRAYS
     C*
     C                   MOVE      XXSORT        XACT(Q)
     C                   MOVE      EUPODT        FRDT(Q)
     C                   MOVE      XXPTDT        TODT(Q)
     C                   MOVE      XXINPR        INPR(Q)
     C                   MOVE      XXWAPR        WAPR(Q)
      *** BEG ADD ***** 05/25/07 ******************************************
     C     SRCAFL        IFEQ      *ON
     C                   ADD       XXCXPY        WAPR(Q)
     C                   ADD       XXCXCY        WAPR(Q)
     C                   ENDIF
      *** END ADD ***** 05/25/07 ******************************************
     C                   MOVE      XXWNMP        WNMP(Q)
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      XXITER        ITER(Q)
     C                   MOVE      XXWTER        WTER(Q)
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      XXIDTR        IDTR(Q)
     C                   MOVE      XXWDTR        WDTR(Q)
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MOVE      XXIVLC        IVLC(Q)
     C                   MOVE      XXWVLC        WVLC(Q)
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   MOVE      XXINEX        INEX(Q)
     C                   MOVE      XXPRTD        PRTD(Q)
     C***  End   Add  ***  01/08/04  **************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      XXIMPR        IMPR(Q)
     C                   MOVE      XXWMPR        WMPR(Q)
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   MOVE      XXWATR        WATR(Q)
     C***  End   Add  ***  01/28/05  **************************************
     C*** BEG ADD ***** 07/21/04 ******************************************
     C     SVPRTD        ADD       XXPRTD        SVPRTD
     C*** END ADD ***** 07/21/04 ******************************************
     C*
      *** BEG ADD ***** 05/25/07 ******************************************
      *
      * IF PREVIOUS CX HAD ITS DATE ADJUSTED, USE THE ADJUSTED DATE TO
      * CALCULATE REINSTATEMENT EARNINGS
      *
     C     ADJDTF        IFEQ      *ON
     C     SVACXD        ANDNE     *ZERO
     C                   MOVE      *OFF          ADJDTF                         ADJUST DATE FLAG
     C                   Z-ADD     EUPODT        SVREDT            7 0
     C                   Z-ADD     SVACXD        EUPODT                         SAVE ADJ CX DATE
     C                   Z-ADD     EUPODT        FRDT(Q)
     C                   ENDIF
      *** END ADD ***** 05/25/07 ******************************************
     C*
     C                   Z-ADD     EUPODT        XQFRM7
     C                   Z-ADD     XXPTDT        XQTO7
     C                   EXSR      GETDYS
     C                   MOVE      QQDAYS        DAYS(Q)
     C*
     C*  CALCULATE COLUMN AMOUNTS
     C*
     C                   EXSR      SRCALC
     C*
     C                   ENDSR
      /EJECT
     C**************************************************************
     C*  PROCESS CURRENT, MTD, YTD, & PRIOR CALCULATIONS
     C**************************************************************
     C*
     C     SRCALC        BEGSR
     C*
     C                   Z-ADD     EUPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C                   Z-ADD     EUTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*  DETERMINE THE LESSER OF THE CALCULATE POLICY "TO" DATE
     C*  AND THE CANCELLATION DATE
     C     XXCNDT        IFLT      XXPTDT
     C                   MOVE      XXCNDT        XXWKDT            7 0
     C                   ELSE
     C                   MOVE      XXPTDT        XXWKDT
     C                   ENDIF
     C*
     C*  DETERMINE THE NUMBER OF DAYS FROM THE TRANSACTION DATE
     C*  TO THE END OF THE PRIOR YEAR (PYDAYS)
     C*
     C     EUPODT        IFGT      XXYEND
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGT      XXYEND
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGT      XXYEND
      **** BEGIN ADD **** 02/04/02 *********************************************
     C     EUTRDT        ORLT      XXYEND
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C********   ELPFDT    ANDGTXXYEND
     C     SVPFDT        ANDGT     XXYEND
      *** END CHG ***** 01/18/07 ***********************************************
      **** END   ADD **** 02/04/02 *********************************************
     C                   Z-ADD     *ZERO         PYDAYS            5 0
     C                   ELSE
     C*
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *** BEG ADD ***** 11/28/06 ***********************************************
      *** BEG DEL ***** 02/26/07 ***********************************************
     C***********XACT,Q    IFEQ 'EH'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C***********          Z-ADDSVPFDT    XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C***********          ENDIF
      *** END DEL ***** 02/26/07 ***********************************************
      *** END ADD ***** 11/28/06 ***********************************************
     C     EUFAF         IFEQ      'A'
     C     EUREVF        ANDNE     'Y'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C*******    EUTRDT    IFGE ELPFDT
     C     EUTRDT        IFGE      SVPFDT
      *** END CHG ***** 01/18/07 ***********************************************
     C                   Z-ADD     EUTRDT        XQFRM7
     C                   ELSE
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C                   Z-ADD     SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C                   ENDIF                                                  EUTRDT IFGE ELPFDT
     C                   ENDIF                                                  EAFAF IFEQ 'A'
     C     XXWKDT        IFLT      XXYEND
     C                   Z-ADD     XXWKDT        XQTO7
     C                   ELSE
     C                   Z-ADD     XXJAN1        XQTO7
     C                   ENDIF
     C*  GET NUMBER OF DAYS
     C                   EXSR      GETDYS
     C                   Z-ADD     QQDAYS        PYDAYS            5 0
     C                   ENDIF                                                  EUPRDT GE XXYEND
     C*
     C*  DETERMINE THE NUMBER OF DAYS FROM THE TRANSACTION DATE
     C*  TO THE END OF THE POLICY (PDAYS).
     C*
     C     EUPODT        IFGE      XXWKDT
     C     EUFAF         ANDNE     'A'
     C     EUTRDT        ORGE      XXWKDT
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
     C     EUPODT        ORGE      XXWKDT
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C                   Z-ADD     *ZERO         PDAYS             5 0
     C                   ELSE
     C*
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *** BEG ADD ***** 11/28/06 ***********************************************
      *** BEG DEL ***** 02/26/07 ***********************************************
     C***********XACT,Q    IFEQ 'EH'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C***********          Z-ADDSVPFDT    XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C***********          ENDIF
      *** END DEL ***** 02/26/07 ***********************************************
      *** END ADD ***** 11/28/06 ***********************************************
     C     EUFAF         IFEQ      'A'
     C     EUREVF        ANDNE     'Y'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C*******    EUTRDT    IFGE ELPFDT
     C     EUTRDT        IFGE      SVPFDT
      *** END CHG ***** 01/18/07 ***********************************************
     C                   Z-ADD     EUTRDT        XQFRM7
     C                   ELSE
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C                   Z-ADD     SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C                   ENDIF                                                  EUTRDT IFGE ELPFDT
     C                   ENDIF                                                  EUFAF IFEQ 'A'
     C                   Z-ADD     XXWKDT        XQTO7
     C*  GET NUMBER OF DAYS
     C                   EXSR      GETDYS
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C                   ENDIF                                                  EUPRDT GE XXWKDT
     C*** BEG ADD ***** 10/06/04 *****************************************
     C*** BEG ADD ***** 01/30/06 *****************************************
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
      *** BEG CHG ***** 02/26/07 ***********************************************
     C***********          Z-ADDSVPFDT    XQFRM7
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *** END CHG ***** 02/26/07 ***********************************************
      *** END CHG ***** 01/18/07 ***********************************************
     C*** BEG CHG ***** 11/28/06 *****************************************
     C***********          Z-ADDELPTDT    XQTO7
     C                   Z-ADD     XXPTDT        XQTO7
     C*** END CHG ***** 11/28/06 *****************************************
     C                   EXSR      GETDYS
     C                   Z-ADD     QQDAYS        P1DAYS            5 0
     C*** END ADD ***** 01/30/06 *****************************************
     C*
     C*  UPDATE THE EXPECTED MONTHLY PREMIUMS
     C*
     C*** BEG CHG ***** 01/30/06 *****************************************
     C     P1DAYS        IFGT      *ZERO
      *** BEG ADD ***** 11/28/06 ***********************************************
     C     XACT(Q)       ANDEQ     'EH'
      *** END ADD ***** 11/28/06 ***********************************************
     C*** END CHG ***** 01/30/06 *****************************************
     C*
     C*  CALCULATE THE START AND END DATES FOR THE MONTH
     C*
      *** BEG CHG ***** 08/30/05 ***********************************************
     C********** 1         DO   12        X       30
     C     1             DO        13            X                 3 0
      *** END CHG ***** 08/30/05 ***********************************************
     C                   Z-ADD     XXPCY         XXWK30
     C     XXPMO         ADD       X             M                 2 0
     C                   SUB       1             M
     C*
     C     M             IFGT      12
     C                   SUB       12            M
     C                   ADD       1             XXWK30
     C                   ENDIF
     C*
     C     XXWK30        MULT      100           XXWK50            5 0
     C                   ADD       M             XXWK50
     C     XXWK50        MULT      100           XXMNST            7 0
     C                   ADD       1             XXMNST
     C     XXWK50        MULT      100           XXMNEN            7 0
     C                   ADD       MD(M)         XXMNEN
     C*
     C*  IF THE POLICY ENDS PRIOR TO THE MONTH, THERE ARE NO DAYS
     C*  ACTIVE IN THE MONTH
     C*
     C*** BEG ADD ***** 02/26/07 *****************************************
     C*
     C*  DO NOT ADJUST EXP PREM ARRAY IF THE POST DATE OF THE REVISED BILL
     C*  IS SUBSEQUENT TO THE MONTH BEING PROCESSED.
     C*
     C     EUPODT        IFLE      XXMNEN
     C*** END ADD ***** 02/26/07 *****************************************
     C*
     C     XXWKDT        IFLE      XXMNST
      *** BEG DEL ***** 11/28/06 *****************************************
     C*******    EUPODT    ORGE XXMNEN
      *** BEG ADD ***** 08/30/05 *****************************************
     C*******    EUPODT    ANDNEELPFDT
      *** END ADD ***** 08/30/05 *****************************************
      *** END DEL ***** 11/28/06 *****************************************
     C                   Z-ADD     *ZERO         QQDAYS
     C                   ELSE
     C*
     C*  IF THE POLICY IS ACTIVE THROUGHOUT THE MONTH, PRORATE BASED
     C*  ON THE NUMBER OF DAYS IN THE MONTH
     C*
      *** BEG CHG ***** 11/28/06 *****************************************
     C*******    EUPODT    IFLE XXMNST
      *** BEG CHG ***** 01/18/07 *****************************************
     C*******    ELPFDT    IFLE XXMNST
      *** BEG CHG ***** 02/26/07 *****************************************
     C*******    SVPFDT    IFLE XXMNST
     C     EUPODT        IFLE      XXMNST
      *** END CHG ***** 02/26/07 *****************************************
      *** END CHG ***** 01/18/07 *****************************************
      *** END CHG ***** 11/28/06 *****************************************
     C     XXWKDT        ANDGE     XXMNEN
     C                   Z-ADD     MD(M)         QQDAYS
     C                   ELSE
     C*
     C*  IF THE POLICY STARTS OR ENDS DURING THE MONTH, PRORATE BASED
     C*  ON THE NUMBER OF DAYS THE POLICY WAS ACTIVE IN THE MONTH
     C*
      *** BEG CHG ***** 11/28/06 *****************************************
     C*******    EUPODT    IFGT XXMNST
     C*******              Z-ADDEUPODT    XQFRM7
      *** BEG CHG ***** 01/18/07 *****************************************
     C*******    ELPFDT    IFGT XXMNST
      *** BEG CHG ***** 02/26/07 *****************************************
     C*******    SVPFDT    IFGT XXMNST
     C     EUPODT        IFGT      XXMNST
     C                   Z-ADD     EUPODT        XQFRM7
      *** END CHG ***** 02/26/07 *****************************************
      *** END CHG ***** 01/18/07 *****************************************
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
      *** BEG DEL ***** 02/26/07 *****************************************
     C***********          Z-ADDSVPODT    XQFRM7
      *** END DEL ***** 02/26/07 *****************************************
      *** END CHG ***** 01/18/07 ***********************************************
      *** END CHG ***** 11/28/06 *****************************************
     C                   ELSE
     C                   Z-ADD     XXMNST        XQFRM7
     C                   ENDIF
     C*
     C     XXWKDT        IFLT      XXMNEN
     C                   Z-ADD     XXWKDT        XQTO7
     C                   ELSE
     C                   Z-ADD     XXMNEN        XQTO7
     C                   ENDIF
     C*  GET NUMBER OF DAYS
     C                   EXSR      GETDYS
     C*
     C*  IF THE POLICY STARTED DURING THE MONTH, INCLUDE THE POLICY
     C*  START DATE IN THE PERIOD.
     C*
      *** BEG CHG ***** 11/28/06 *****************************************
     C*******    EUPODT    IFGT XXMNST
      *** BEG CHG ***** 01/18/07 ***********************************************
     C*******    ELPFDT    IFGE XXMNST
      *** BEG CHG ***** 02/26/07 *****************************************
     C*******    SVPFDT    IFGE XXMNST
     C     EUPODT        IFGE      XXMNST
      *** END CHG ***** 02/26/07 *****************************************
      *** END CHG ***** 01/18/07 ***********************************************
      *** END CHG ***** 11/28/06 *****************************************
     C                   ADD       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELPFDT LE XXMNST
     C                   ENDIF                                                  XXWKDT LE XXMNST
     C*
      *** BEG CHG ***** 01/30/06 ***********************************************
     C*********  QQDAYS    DIV  PDAYS     XXPRO     H
     C     QQDAYS        DIV(H)    P1DAYS        XXPRO
      *** END CHG ***** 01/30/06 ***********************************************
      **** BEGIN CHG **** 07/19/01 *********************************************
     C********** XXWNMP    MULT XXPRO     XXWK11 112H
      **** BEGIN CHG **** 12/06/01 *********************************************
     C********** XXWAPR    MULT XXPRO     XXWK11 112H
      **** BEGIN CHG **** 01/21/02 *********************************************
     C********** XXWAPR    SUB  XXWRXC    XXAWXC 112
     C*
     C     QQPRTD        IFEQ      'Y'
     C                   Z-ADD     XXWAPR        XXAWXC
     C                   ELSE
     C     XXWAPR        SUB       XXWRXC        XXAWXC           11 2
     C                   ENDIF                                                  QQPRTD IFEQ 'Y'
     C*
      **** END   CHG **** 01/21/02 *********************************************
     C     XXAWXC        MULT(H)   XXPRO         XXWK11           11 2
      **** END   CHG **** 12/06/01 *********************************************
      **** END   CHG **** 07/19/01 *********************************************
      *** BEG CHG ***** 10/06/04 ******************************************
     C**********           ADD  XXWK11    EP,X
     C     ADJ(X)        IFEQ      ' '
     C                   ADD       XXWK11        EP(X)
     C                   ENDIF
      *
     C     FRSTEP        IFEQ      *OFF
     C     SEP(X)        ADD       XXWK11        NEP(X)
     C                   ENDIF
      *** END CHG ***** 10/06/04 ******************************************
      *** BEG ADD ***** 07/26/04 ***********************************************
     C     XXWMPR        MULT(H)   XXPRO         XXWK11
      *** BEG CHG ***** 10/06/04 ******************************************
     C**********           ADD  XXWK11    EMP,X
     C     ADJ(X)        IFEQ      ' '
     C                   ADD       XXWK11        EMP(X)
     C                   ENDIF
      *
     C     FRSTEP        IFEQ      *OFF
     C     SEMP(X)       ADD       XXWK11        NEMP(X)
     C                   ENDIF
      *** END CHG ***** 10/06/04 ******************************************
      *
      *** BEG ADD ***** 02/26/07 ******************************************
     C                   ELSE
      *
      *  THERE WILL NO CHANGE IN THE EXP PREM ARRAY BUCKET IF THE POST DATE
      *  OF THE REVISED DOESN'T FALL BEFORE MONTH END
      *
     C                   Z-ADD     SEP(X)        NEP(X)
      *** BEG ADD ***** 02/15/10 ******************************************
     C                   Z-ADD     SEMP(X)       NEMP(X)
      *** END ADD ***** 02/15/10 ******************************************
      *
     C                   ENDIF                                                  EUPODT <= XXMNEN
      *** END ADD ***** 02/26/07 ******************************************
      *
      *** END ADD ***** 07/26/04 ***********************************************
     C                   ENDDO                                                  1 DO 12, X
     C                   ENDIF                                                  PDAYS <> 0
      *** BEG ADD ***** 10/06/04 ******************************************
      *
      *   RECALCULATE PAST A/R ADJUSTMENTS, THE ADJUSTMENTS ARE ACCUMULATED AND
      *   WRITTEN TO THE CURRENT RECORD
      *
      *** BEG ADD ***** 11/28/06 ******************************************
     C                   MOVE      *OFF          ADJFLG            1
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C                   MOVE      *OFF          FADJFL            1
      *** END ADD ***** 02/05/07 ******************************************
      *
     C     XACT(Q)       IFEQ      'EH'
     C     FRSTE         ANDEQ     *OFF
     C     EUFAF         ANDNE     'A'
      *
     C     FRSTEP        IFEQ      *ON
     C                   MOVE      *OFF          FRSTEP
     C                   XFOOT     EP            XXSVEP           15 2          SAVE EP ARR TOT
     C                   MOVEA     EP            SEP                            SAVE EP ARRAY
     C                   MOVEA     EMP           SEMP                           SAVE EMP ARRY
      *
      *** BEG ADD ***** 11/28/06 ****************************************
      *
      *   CALCULATE PAST 'AR' TRANSACTIONS INTO ORIGINAL BILL. FIRST NEGATE
      *   PREVIOUS ADJ TO PREMIUM AND THEN APPLY DIFFERENCE BETWEEN THE ADJ
      *   AND THE EP TO XXWADJ
      *
     C                   CLEAR                   XXWADJ
     C     1             DO        13            X
     C     XAD(X)        IFEQ      'Y'
     C                   SUB       EP(X)         WAPR(Q)
     C                   SUB       EP(X)         XXWADJ
     C                   Z-ADD     XXWADJ        SRAD(Q)
     C                   SUB       EP(X)         XXNETR
     C                   SUB       XEP(X)        XEP(X)
     C                   MOVE      ' '           XAD(X)
     C                   MOVE      'Y'           ADJ(X)
     C                   Z-ADD     0             EP(X)
     C                   MOVE      *ON           ADJFLG
     C                   ENDIF
     C                   ENDDO
      *
      *** BEG CHG ***** 02/09/07 ******************************************
     C********** EUTRDT    IFLE XXYEND
     C     EUTRDT        IFLT      XXJAN1
     C     EUPODT        ANDLT     XXJAN1
      *** END CHG ***** 02/09/07 ******************************************
     C                   Z-ADD     XXWADJ        XXADPY           11 2          ADJ IN PRIOR YEAR
     C                   ELSE
     C                   Z-ADD     XXWADJ        XXADCY           11 2          ADJ IN CURR YEAR
     C                   ENDIF
      *** END ADD ***** 11/28/06 ****************************************
      *
     C                   ELSE
     C                   XFOOT     NEP           XXCREP           15 2          CURR EP ARR TOT
     C     XXCREP        IFNE      XXSVEP
     C                   CLEAR                   XXWADJ
     C                   CLEAR                   XXMADJ
      *
      *** BEG CHG ***** 08/30/05 ****************************************
     C********** 1         DO   12        X
     C     1             DO        13            X
      *** END CHG ***** 08/30/05 ****************************************
      *
      *  IF AN A/R ADJ HAS BEEN WRITTEN FOR THIS MONTH, THE WRITTEN PREM AND MAN
      *  PREM ADJUSTMENTS = OLD EXPECTED PAYMENT - NEW EXPECTED PAYMENT.
      *
     C     ADJ(X)        IFEQ      'Y'
     C                   SUB       NEP(X)        XXWADJ           11 2
     C                   SUB       NEP(X)        XXNETR
     C                   ADD       SEP(X)        XXWADJ
     C                   ADD       SEP(X)        XXNETR
     C                   SUB       NEMP(X)       XXMADJ           11 2
     C                   SUB       NEMP(X)       XXNETM
     C                   ADD       SEMP(X)       XXMADJ
     C                   ADD       SEMP(X)       XXNETM
     C                   ENDIF                                                  ADJ,X
      *
     C                   ENDDO
      *
      *** BEG CHG ***** 11/28/06 ******************************************
     C***********          ADD  XXWADJ    XXWAPR
      *
      *** BEG CHG ***** 02/09/07 ******************************************
     C********** EUTRDT    IFLE XXYEND
     C     EUTRDT        IFLT      XXJAN1
     C     EUPODT        ANDLT     XXJAN1
      *** END CHG ***** 02/09/07 ******************************************
     C                   Z-ADD     XXWADJ        XXADPY           11 2          ADJ IN PRIOR YEAR
     C                   ELSE
     C                   Z-ADD     XXWADJ        XXADCY           11 2          ADJ IN CURR YEAR
     C                   ENDIF
      *
     C                   MOVE      *ON           ADJFLG            1
     C                   Z-ADD     XXWADJ        SRAD(Q)
      *** END CHG ***** 11/28/06 ******************************************
     C                   ADD       XXWADJ        WAPR(Q)
     C                   ADD       XXWADJ        XXINPR
     C                   ADD       XXWADJ        INPR(Q)
     C                   ADD       XXWADJ        WNMP(Q)
     C                   ADD       XXMADJ        XXWMPR
     C                   ADD       XXMADJ        WMPR(Q)
     C                   ADD       XXMADJ        XXIMPR
     C                   ADD       XXMADJ        IMPR(Q)
     C                   ENDIF                                                  XXCREP
      *** BEG ADD ***** 02/05/07 ******************************************
      *
      *   ADD FUTURE ADJUSTMENTS IF NEEDED, EXPLANATION PROVIDED IN W4901RA
      *
     C     EUPODT        IFGT      EUTRDT
     C                   MOVE      EUCO#         ##CO#             3
     C                   MOVE      EUFND         ##FND             3
     C                   MOVE      EUEMP#        ##EMP#            9
     C                   MOVE      EUDIV         ##DIV             5
     C                   MOVE      EUFYR         ##FYR             3
     C                   MOVE      QQSRST        ##SRST            1
     C                   MOVE      XXST          ##ST              2
     C                   MOVE      SVPFDT        ##PFDT            7
     C                   MOVE      Q             ##Q               6
     C                   MOVE      *BLANKS       ##MTH            13
     C                   CALL      'W4901RA'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##FYR
     C                   PARM                    ##SRST
     C                   PARM                    ##ST
     C                   PARM                    ##PFDT
     C                   PARM                    ##Q
     C                   PARM                    ##MTH
      *
      *   APPLY ANY FUTURE ADJUSTMENTS TO THE CURRENT TRANSACTION
      *
     C                   CLEAR                   CFAM
     C                   MOVEA     ##MTH         CFAD
     C                   DO        13            X
     C     ADJ(X)        IFEQ      *BLANK
     C     CFAD(X)       ANDEQ     'Y'
     C                   ADD       NEP(X)        CFAM(X)
     C                   SUB       SEP(X)        CFAM(X)
     C                   ENDIF
     C                   ENDDO
      *
     C                   CLEAR                   XXFAPY
     C                   CLEAR                   XXFACY
     C                   XFOOT     CFAM          XXFADM           11 2
     C     XXFADM        IFNE      *ZERO
     C                   MOVE      *ON           FADJFL            1
     C     EUTRDT        IFLT      XXJAN1
     C     EUPODT        ANDLT     XXJAN1
     C                   Z-SUB     XXFADM        XXFAPY           11 2          ADJ IN PRIOR YEAR
     C                   ELSE
     C                   Z-SUB     XXFADM        XXFACY           11 2          ADJ IN CURR YEAR
     C                   ENDIF
     C                   ENDIF
      *
     C                   SUB       XXFADM        SRAD(Q)
     C                   SUB       XXFADM        WAPR(Q)
     C                   SUB       XXFADM        XXINPR
     C                   SUB       XXFADM        INPR(Q)
     C                   SUB       XXFADM        WNMP(Q)
      *
      *   ACCUMLATE CURRENT ADJUSTMENTS FOR SRARBL
      *
     C                   DO        13            X
     C     ADJ(X)        IFEQ      *BLANK
     C     CFAD(X)       ANDEQ     'Y'
     C                   MOVE      'Y'           FADJ(X)
     C                   ADD       CFAM(X)       FADM(X)
     C                   ENDIF
     C                   ENDDO
      *
     C                   ENDIF                                                  EUPODT > EUTRDT
      *** END ADD ***** 02/05/07 ******************************************
      *
     C                   XFOOT     NEP           XXSVEP           15 2
     C                   MOVEA     NEP           SEP
     C                   CLEAR                   NEP
     C                   MOVEA     NEMP          SEMP
     C                   CLEAR                   NEMP
      *
     C                   ENDIF                                                  FRSTEP
      *
     C                   ENDIF                                                  XACT,Q
     C*** END ADD ***** 10/06/04 ******************************************
     C*
     C* CALCULATE PRINTED FIELDS
     C*
     C*  INFORCE PREMIUM
     C*
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXINXC        EOINEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  01/08/04  **************************************
     C****BEGIN*ADD ***  10/17/01  ************************************
     C**********           Z-ADDXXINXC    EUINEX
     C****END*ADD *****  10/17/01  ************************************
     C***  End   Del  ***  01/08/04  **************************************
     C     CANCLD        IFEQ      *ON
     C     XXPTDT        ORLE      XXVDAT
     C                   Z-ADD     *ZERO         XXINPR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXIMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXITER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXINEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  01/08/04  **************************************
     C****BEGIN*ADD ***  10/17/01  ************************************
     C**********           Z-ADD0         EUINEX
     C****END*ADD *****  10/17/01  ************************************
     C***  End   Del  ***  01/08/04  **************************************
     C                   ENDIF
     C*
     C*  PRIOR YEAR & CURRENT YEAR TO DATE WRITTEN
     C*
     C     EUPODT        IFLT      XXJAN1
     C     EUTRDT        ANDLT     XXJAN1
     C     EUTRDT        ORLT      XXJAN1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
     C*** BEG CHG ***** 01/18/07 **************************************
     C*******    EUTRDT    ANDGEELPFDT
     C     EUTRDT        ANDGE     SVPFDT
     C*** END CHG ***** 01/18/07 **************************************
     C     EUFAF         OREQ      'A'
     C     EUREVF        ANDNE     'Y'
     C*** BEG CHG ***** 01/18/07 **************************************
     C*********  EUTRDT    ANDLTELPFDT
     C*********  ELPFDT    ANDLTXXJAN1
     C     EUTRDT        ANDLT     SVPFDT
     C     SVPFDT        ANDLT     XXJAN1
     C*** END CHG ***** 01/18/07 **************************************
     C                   Z-ADD     XXWAPR        XXPYWR           11 2
      *** BEG ADD ***** 11/28/06 ******************************************
     C     SRCAFL        IFEQ      *ON
     C                   ADD       XXCXPY        XXPYWR
     C                   ENDIF
      *
     C     ADJFLG        IFEQ      *ON
     C                   ADD       XXADPY        XXPYWR
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C                   ADD       XXFAPY        XXPYWR
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C                   Z-ADD     *ZERO         XXYDWR           11 2
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXWTER        XXPYTR           11 2
     C                   Z-ADD     *ZERO         XXYDTR           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     XXWDTR        XXPYWD           11 2
     C                   Z-ADD     *ZERO         XXYDWD           11 2
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     XXWVLC        XXPYVL           11 2
     C                   Z-ADD     *ZERO         XXYDVL           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXPYWM           11 2
     C                   Z-ADD     *ZERO         XXYDWM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     XXWATR        XXPYTR
     C                   Z-ADD     *ZERO         XXYDWT
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPYWR
     C                   Z-ADD     XXWAPR        XXYDWR
      *** BEG ADD ***** 11/28/06 ******************************************
     C     SRCAFL        IFEQ      *ON
     C                   ADD       XXCXCY        XXYDWR
     C                   ENDIF
      *
     C     ADJFLG        IFEQ      *ON
     C                   ADD       XXADCY        XXYDWR
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C                   ADD       XXFACY        XXYDWR
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXPYTR
     C                   Z-ADD     XXWTER        XXYDTR
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXPYWD
     C                   Z-ADD     XXWDTR        XXYDWD
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYVL
     C                   Z-ADD     XXWVLC        XXYDVL
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYWM           11 2
     C                   Z-ADD     XXWMPR        XXYDWM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXPYTR
     C                   Z-ADD     XXWATR        XXYDWT
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C*  CURRENT YEAR TO DATE EARNED
     C*
     C     PDAYS         IFLT      1
     C     EUPODT        IFGE      XXJAN1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXJAN1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXJAN1
     C                   Z-ADD     XXWAPR        XXYDEA           11 2
      *** BEG ADD ***** 11/28/06 ******************************************
     C     SRCAFL        IFEQ      *ON
     C                   ADD       XXCXCY        XXYDEA
     C                   ENDIF
      *
     C     ADJFLG        IFEQ      *ON
     C                   ADD       XXADCY        XXYDEA
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C                   ADD       XXFACY        XXYDEA
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXWTER        XXYDET           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     XXWDTR        XXYDED           11 2
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     XXWVLC        XXYDEV           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXYDEM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     XXWATR        XXYDET
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
     C                   Z-ADD     *ZERO         XXYDEA
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXYDET
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXYDED
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXYDEV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXYDEM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXYDET
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C                   ELSE
     C*
     C     EUPODT        IFGE      XXJAN1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXJAN1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXJAN1
      **** BEGIN ADD **** 02/04/02 *********************************************
     C     EUTRDT        ORLT      XXJAN1
      *** BEG CHG ***** 01/18/07 ***********************************************
     C*********  ELPFDT    ANDGEXXJAN1
     C     SVPFDT        ANDGE     XXJAN1
      *** END CHG ***** 01/18/07 ***********************************************
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
      **** END   ADD **** 02/04/02 *********************************************
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *** BEG ADD ***** 11/28/06 ***********************************************
      *** BEG DEL ***** 02/26/07 ***********************************************
     C***********XACT,Q    IFEQ 'EH'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C***********          Z-ADDSVPFDT    XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C***********          ENDIF
      *** END DEL ***** 02/26/07 ***********************************************
      *** END ADD ***** 11/28/06 ***********************************************
     C*
     C     EUFAF         IFEQ      'A'
     C     EUREVF        ANDNE     'Y'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C********** EUTRDT    IFGE ELPFDT
     C     EUTRDT        IFGE      SVPFDT
      *** END CHG ***** 01/18/07 ***********************************************
     C                   Z-ADD     EUTRDT        XQFRM7
     C                   ELSE
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C                   Z-ADD     SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C                   ENDIF                                                  EUTRDT IFGE ELPFDT
     C                   ENDIF                                                  EUFAF IFEQ 'A'
     C*
     C                   ELSE
     C                   Z-ADD     XXJAN1        XQFRM7
     C                   ENDIF
     C*
     C     XXWKDT        IFLE      XXVDAT
     C                   Z-ADD     XXWKDT        XQTO7
     C                   ELSE
     C                   Z-ADD     XXNEXT        XQTO7
     C                   ENDIF
     C*  GET NUMBER OF DAYS
     C                   EXSR      GETDYS
     C     QQDAYS        DIV(H)    PDAYS         XXPRO            11 9
     C     XXWAPR        MULT(H)   XXPRO         XXYDEA
      *** BEG ADD ***** 11/28/06 ******************************************
     C     ADJFLG        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
     C                   ADD       XXADCY        XXYDEA
     C                   ENDIF
      *
     C     SRCAFL        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
     C                   ADD       XXCXCY        XXYDEA
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
     C                   ADD       XXFACY        XXYDEA
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXWTER        MULT(H)   XXPRO         XXYDET
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C     XXWDTR        MULT(H)   XXPRO         XXYDED
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXWVLC        MULT(H)   XXPRO         XXYDEV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXWMPR        MULT(H)   XXPRO         XXYDEM
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C     XXWATR        MULT(H)   XXPRO         XXYDET
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF                                                  PDAYS = 0
     C*
     C*  PRIOR YEAR EARNED
     C*
      *** BEG CHG ***** 08/11/11 ******************************************
     C*********  PYDAYS    IFLT 1
     C*********  EUPODT    IFLT XXYEND
     C*********  EUTRDT    ANDLTXXYEND
     C****BEG*CHG ***** 02/26/08 ******************************************
     C********** EUTRDT    ORLT XXYEND
     C*********  EUTRDT    ORLE XXYEND
     C****END*CHG ***** 02/26/08 ******************************************
     C*********  EUFAF     ANDEQ'A'
     C*********  EUREVF    ANDNE'Y'
     C****BEG*CHG ***** 01/18/07 ******************************************
     C********** EUTRDT    ANDGEELPFDT
     C*********  EUTRDT    ANDGESVPFDT
     C****END*CHG ***** 01/18/07 ******************************************
     C*********  EUFAF     OREQ 'A'
     C*********  EUREVF    ANDNE'Y'
     C****BEG*CHG ***** 01/18/07 ******************************************
     C*********  EUTRDT    ANDLTELPFDT
     C*********  ELPFDT    ANDLTXXYEND
     C*********  EUTRDT    ANDLTSVPFDT
     C*********  SVPFDT    ANDLTXXYEND
      *
     C     PYDAYS        IFLT      1
     C     EUPODT        IFLT      XXJAN1
     C     EUTRDT        ANDLT     XXJAN1
     C     EUTRDT        ORLT      XXJAN1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
     C     EUTRDT        ANDGE     SVPFDT
     C     EUFAF         OREQ      'A'
     C     EUREVF        ANDNE     'Y'
     C     EUTRDT        ANDLT     SVPFDT
     C     SVPFDT        ANDLT     XXJAN1
     C                   Z-ADD     XXWAPR        XXPYEA           11 2
      *** BEG ADD ***** 11/28/06 ******************************************
     C     ADJFLG        IFEQ      *ON
     C                   ADD       XXADPY        XXPYEA
     C                   ENDIF
      *
     C     SRCAFL        IFEQ      *ON
     C                   ADD       XXCXPY        XXPYEA
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C                   ADD       XXFAPY        XXPYEA
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXWTER        XXPYET           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     XXWDTR        XXPYED           11 2
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     XXWVLC        XXPYEV           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXPYEM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     XXWATR        XXPYET
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
     C*
     C                   Z-ADD     *ZERO         XXPYEA
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXPYET
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXPYED
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYEV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYEM
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXPYET
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C                   ELSE
     C*
     C     EUPODT        IFGT      XXYEND
     C     EUFAF         ANDNE     'A'
     C     EUTRDT        ORGT      XXYEND
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
     C     EUPODT        ORGT      XXYEND
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C                   Z-ADD     *ZERO         XXPYEA
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXPYET
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXPYED
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYEV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYEM
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXPYET
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *** BEG ADD ***** 11/28/06 ***********************************************
      *** BEG DEL ***** 02/26/07 ***********************************************
     C***********XACT,Q    IFEQ 'EH'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C***********          Z-ADDSVPFDT    XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C***********          ENDIF
      *** END DEL ***** 02/26/07 ***********************************************
      *** END ADD ***** 11/28/06 ***********************************************
     C*
     C     EUFAF         IFEQ      'A'
     C     EUREVF        ANDNE     'Y'
     C*
      *** BEG CHG ***** 01/18/07 ***********************************************
     C********** EUTRDT    IFGE ELPFDT
     C     EUTRDT        IFGE      SVPFDT
      *** END CHG ***** 01/18/07 ***********************************************
     C                   Z-ADD     EUTRDT        XQFRM7
     C                   ELSE
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C                   Z-ADD     SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C                   ENDIF                                                  EUTRDT IFGE ELPFDT
     C                   ENDIF                                                  EUFAF IFEQ 'A'
     C*
     C     XXWKDT        IFLE      XXYEND
     C                   Z-ADD     XXWKDT        XQTO7
     C                   ELSE
     C                   Z-ADD     XXJAN1        XQTO7
     C                   ENDIF
     C*  GET NUMBER OF DAYS
     C                   EXSR      GETDYS
     C*
     C     PDAYS         IFEQ      *ZERO
     C                   Z-ADD     *ZERO         XXPRO
     C                   ELSE
     C     QQDAYS        DIV(H)    PDAYS         XXPRO            11 9
     C                   ENDIF
     C*
     C     XXWAPR        MULT(H)   XXPRO         XXPYEA
      *** BEG ADD ***** 11/28/06 ******************************************
     C     ADJFLG        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
     C                   ADD       XXADPY        XXPYEA
     C                   ENDIF
      *
     C     SRCAFL        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
     C                   ADD       XXCXPY        XXPYEA
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
     C                   ADD       XXFAPY        XXPYEA
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXWTER        MULT(H)   XXPRO         XXPYET
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C     XXWDTR        MULT(H)   XXPRO         XXPYED
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXWVLC        MULT(H)   XXPRO         XXPYEV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXWMPR        MULT(H)   XXPRO         XXPYEM
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C     XXWATR        MULT(H)   XXPRO         XXPYET
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF                                                  EUPRDT > XXYEND
     C                   ENDIF                                                  PYDAYS = 0
     C*
     C*  CURRENT UNEARNED
     C*
     C     PDAYS         IFEQ      *ZERO
     C     XXWKDT        ORLE      XXVDAT
     C                   Z-ADD     *ZERO         XXUNEA           11 2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXUNMN           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXUNTR           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXUDTR           11 2
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXUNVL           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXUNEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  01/08/04  **************************************
     C****BEGIN*ADD ***  10/17/01  ************************************
     C**********           Z-ADD0         EUUNEX
     C****END*ADD *****  10/17/01  ************************************
     C***  End   Del  ***  01/08/04  **************************************
      **** BEGIN ADD **** 06/18/02 *********************************************
     C     XXPTDT        IFGT      XXVDAT
      *
      *  GET NUMBER OF DAYS ORIGINALLY IN POLICY
      *
      *** BEG CHG ***** 01/18/07 ******************************************
     C***********          MOVE ELPFDT    XQFRM7
     C                   MOVE      SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 ******************************************
     C                   Z-ADD     XXPTDT        XQTO7
     C                   EXSR      GETDYS
     C                   Z-ADD     QQDAYS        TPDAYS            3 0
      *
      *  GET NUMBER OF DAYS LEFT IN POLICY PRIOR TO CANCELLATION
      *
     C                   MOVE      XXNEXT        XQFRM7
     C                   Z-ADD     XXPTDT        XQTO7
     C                   EXSR      GETDYS
     C     QQDAYS        IFLT      *ZERO
     C***  Begin Chg  ***  01/08/04  **************************************
     C**********           Z-ADD0         EUUNEX
     C                   Z-ADD     0             EOUNEX
     C***  End   Chg  ***  01/08/04  **************************************
      *
     C                   ELSE
      *
     C     QQDAYS        DIV(H)    TPDAYS        XXPRO
     C***  Begin Chg  ***  01/08/04  **************************************
     C********** XXWRXC    MULT XXPRO     EUUNEX
     C     XXWRXC        MULT      XXPRO         EOUNEX
     C***  End   Chg  ***  01/08/04  **************************************
      *
     C                   ENDIF                                                  QQDAYS IFLT *ZERO
     C                   ENDIF                                                  XXPTDT IFGT XXVDAT
      **** END   ADD **** 06/18/02 *********************************************
     C                   ELSE
     C*
     C                   MOVE      XXNEXT        XQFRM7
     C                   Z-ADD     XXWKDT        XQTO7
     C*  GET NUMBER OF DAYS
     C                   EXSR      GETDYS
     C     QQDAYS        IFLT      *ZERO
     C                   Z-ADD     *ZERO         XXUNEA
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXUNMN
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXUNTR
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXUDTR
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXUNVL
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXUNEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  01/08/04  **************************************
     C****BEGIN*ADD ***  10/17/01  ************************************
     C**********           Z-ADD0         EUUNEX
     C****END*ADD *****  10/17/01  ************************************
     C***  End   Del  ***  01/08/04  **************************************
     C                   ELSE
     C     QQDAYS        DIV(H)    PDAYS         XXPRO
     C     XXWAPR        MULT(H)   XXPRO         XXUNEA
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXWMPR        MULT(H)   XXPRO         XXUNMN
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXWTER        MULT(H)   XXPRO         XXUNTR
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C     XXWDTR        MULT(H)   XXPRO         XXUDTR
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXWVLC        MULT(H)   XXPRO         XXUNVL
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C     XXPRTD        MULT(H)   XXPRO         XXUNEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  01/08/04  **************************************
     C****BEGIN*ADD ***  10/17/01  ************************************
     C********** XXWRXC    MULT XXPRO     EUUNEX
     C****END*ADD *****  10/17/01  ************************************
     C***  End   Del  ***  01/08/04  **************************************
     C                   ENDIF                                                  QQDAYS < 0
     C                   ENDIF                                                  PDAYS = 0
     C*
     C*  MONTH TO DATE WRITTEN
     C*
      *** BEG ADD ***** 06/09/10 ******************************************
     C                   MOVE      *OFF          MDWRFL            1
      *** END ADD ***** 06/09/10 ******************************************
     C     EUPODT        IFLT      XXMTH1
     C     EUTRDT        ANDLT     XXMTH1
     C     EUTRDT        ORLT      XXMTH1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
     C*** BEG CHG ***** 01/18/07 ******************************************
     C********** EUTRDT    ANDGEELPFDT
     C     EUTRDT        ANDGE     SVPFDT
     C*** END CHG ***** 01/18/07 ******************************************
     C     EUFAF         OREQ      'A'
     C     EUREVF        ANDNE     'Y'
     C*** BEG CHG ***** 01/18/07 ******************************************
     C********** EUTRDT    ANDLTELPFDT
     C********** ELPFDT    ANDLTXXMTH1
     C     EUTRDT        ANDLT     SVPFDT
     C     SVPFDT        ANDLT     XXMTH1
     C*** END CHG ***** 01/18/07 ******************************************
     C                   Z-ADD     *ZERO         XXMDWR           11 2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXMDWM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXMDTR           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXMDWD           11 2
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXMDVL           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Del  ***  12/01/03  **************************************
     C****BEGIN*ADD ***  10/17/01  ************************************
     C**********           Z-ADD0         EUMWEX
     C****END*ADD *****  10/17/01  ************************************
     C***  End   Del  ***  12/01/03  **************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXMWEX           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     *ZERO         XXMDWT           11 2
     C***  End   Add  ***  01/28/05  **************************************
     C                   ELSE
     C                   Z-ADD     XXWAPR        XXMDWR
      *** BEG ADD ***** 06/09/10 ******************************************
     C                   MOVE      *ON           MDWRFL            1
      *** END ADD ***** 06/09/10 ******************************************
      *** BEG ADD ***** 11/28/06 ******************************************
     C     ADJFLG        IFEQ      *ON
     C                   ADD       XXADCY        XXMDWR
     C                   ENDIF
      *
     C     SRCAFL        IFEQ      *ON
     C                   ADD       XXCXCY        XXMDWR
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C                   ADD       XXFACY        XXMDWR
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXMDWM
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXWTER        XXMDTR
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     XXWDTR        XXMDWD
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ****** 09/01/04 *****************************************
     C                   Z-ADD     XXWVLC        XXMDVL
     C*** END ADD ****** 09/01/04 *****************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXPRTD        XXMWEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Del  ***  01/08/04  **************************************
     C****BEGIN*ADD ***  10/17/01  ************************************
     C**********           Z-ADDXXWRXC    EUMWEX
     C****END*ADD *****  10/17/01  ************************************
     C***  End   Del  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   Z-ADD     XXWATR        XXMDWT
     C***  End   Add  ***  01/28/05  **************************************
     C                   ENDIF
     C*
     C*  MONTH TO DATE EARNED
     C*
     C     PDAYS         IFEQ      *ZERO
     C*
     C     EUPODT        IFGE      XXMTH1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXMTH1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXMTH1
     C                   Z-ADD     XXWAPR        XXMDEA           11 2
      *** BEG ADD ***** 11/28/06 ******************************************
     C     ADJFLG        IFEQ      *ON
      *** BEG CHG ***** 06/09/10 ******************************************
     C********** XXMDWR    ANDNE*ZERO
     C     MDWRFL        ANDEQ     *ON
      *** END CHG ***** 06/09/10 ******************************************
     C                   ADD       XXADCY        XXMDEA
     C                   ENDIF
      *
     C     SRCAFL        IFEQ      *ON
      *** BEG CHG ***** 06/09/10 ******************************************
     C********** XXMDWR    ANDNE*ZERO
     C     MDWRFL        ANDEQ     *ON
      *** END CHG ***** 06/09/10 ******************************************
     C                   ADD       XXCXCY        XXMDEA
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
      *** BEG CHG ***** 06/09/10 ******************************************
     C********** XXMDWR    ANDNE*ZERO
     C     MDWRFL        ANDEQ     *ON
      *** END CHG ***** 06/09/10 ******************************************
     C                   ADD       XXFACY        XXMDEA
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXMDEM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXWTER        XXMDET           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     XXWDTR        XXMDED           11 2
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     XXWVLC        XXMDEV           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C                   ELSE
     C                   Z-ADD     *ZERO         XXMDEA
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXMDEM
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXMDET
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXMDED           11 2
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXMDEV
     C*** END ADD ***** 09/01/04 ******************************************
     C                   ENDIF
     C*
     C                   ELSE
     C     EUPODT        IFGE      XXMTH1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXMTH1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXMTH1
      **** BEGIN ADD **** 02/04/02 *********************************************
     C     EUTRDT        ORLT      XXMTH1
      *** BEG CHG ***** 01/18/07 ***********************************************
     C********** ELPFDT    ANDGEXXMTH1
     C     SVPFDT        ANDGE     XXMTH1
      *** END CHG ***** 01/18/07 ***********************************************
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
      **** END   ADD **** 02/04/02 *********************************************
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *** BEG ADD ***** 11/28/06 ***********************************************
      *** BEG DEL ***** 02/26/07 ***********************************************
     C********** XACT,Q    IFEQ 'EH'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C**********           Z-ADDSVPFDT    XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C**********           ENDIF
      *** END DEL ***** 02/26/07 ***********************************************
      *** END ADD ***** 11/28/06 ***********************************************
     C     EUFAF         IFEQ      'A'
     C     EUREVF        ANDNE     'Y'
      *** BEG CHG ***** 01/18/07 ***********************************************
     C********** EUTRDT    IFGE ELPFDT
     C     EUTRDT        IFGE      SVPFDT
      *** END CHG ***** 01/18/07 ***********************************************
     C                   Z-ADD     EUTRDT        XQFRM7
     C                   ELSE
      *** BEG CHG ***** 01/18/07 ***********************************************
     C**********           Z-ADDELPFDT    XQFRM7
     C                   Z-ADD     SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 ***********************************************
     C                   ENDIF                                                  EUTRDT IFGE ELPFDT
     C                   ENDIF                                                  EUFAF IFEQ 'A'
     C                   ELSE
     C                   Z-ADD     XXMTH1        XQFRM7
     C                   ENDIF
     C*
     C     XXWKDT        IFLE      XXVDAT
     C                   Z-ADD     XXWKDT        XQTO7
     C                   ELSE
     C                   Z-ADD     XXNEXT        XQTO7
     C                   ENDIF
     C*  GET NUMBER OF DAYS
     C                   EXSR      GETDYS
     C     QQDAYS        DIV(H)    PDAYS         XXPRO
     C     XXWAPR        MULT(H)   XXPRO         XXMDEA
      *** BEG ADD ***** 11/28/06 ******************************************
     C     ADJFLG        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
      *** BEG CHG ***** 06/09/10 ******************************************
     C********** XXMDWR    ANDNE*ZERO
     C     MDWRFL        ANDEQ     *ON
      *** END CHG ***** 06/09/10 ******************************************
     C                   ADD       XXADCY        XXMDEA
     C                   ENDIF
      *
     C     SRCAFL        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
      *** BEG CHG ***** 06/09/10 ******************************************
     C********** XXMDWR    ANDNE*ZERO
     C     MDWRFL        ANDEQ     *ON
      *** END CHG ***** 06/09/10 ******************************************
     C                   ADD       XXCXCY        XXMDEA
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C     FADJFL        IFEQ      *ON
     C     XXPRO         ANDNE     *ZERO
      *** BEG CHG ***** 06/09/10 ******************************************
     C********** XXMDWR    ANDNE*ZERO
     C     MDWRFL        ANDEQ     *ON
      *** END CHG ***** 06/09/10 ******************************************
     C                   ADD       XXFACY        XXMDEA
     C                   ENDIF
      *** END ADD ***** 02/05/07 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXWTER        MULT(H)   XXPRO         XXMDET
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C     XXWDTR        MULT(H)   XXPRO         XXMDED
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXWVLC        MULT(H)   XXPRO         XXMDEV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXWMPR        MULT(H)   XXPRO         XXMDEM
     C*** END ADD ***** 07/26/04 ******************************************
     C                   ENDIF                                                  PDAYS = 0
      *
      * CALCULATE PRIOR MONTH YTD EARNED IF POLICY EXP DATE CHANGE
      *
     C     *IN44         IFEQ      *ON
     C                   Z-ADD     XXWKDT        XXHWDT            7 0
      *
      * PRIOR MONTH TO DATE EARNED
      *
      *** XXPMTH = LAST DAY OF PRIOR MONTH
      *** XXPPXD = EXP DATE AS OF LAST MONTH
      *** XXPCXD = CANCEL DATE AS OF PRIOR MONTH
      *** XXPMYE = PRIOR MONTH YTD EARNED
      *
      **************************************************
      *
      *  DETERMINE THE LESSER OF THE CALCULATE POLICY "TO" DATE
      *  AND THE CANCELLATION DATE
     C     XXPCXD        IFLT      XXPPXD
     C     XXPCXD        ANDNE     0
     C                   MOVE      XXPCXD        XXWKDT            7 0
     C                   ELSE
     C                   MOVE      XXPPXD        XXWKDT
     C                   ENDIF                                                  XXPCXD IFLT XXPPXD
      *
      *  DETERMINE THE NUMBER OF DAYS FROM THE TRANSACTION DATE
      *  TO THE END OF THE POLICY (PPDAYS).
      *
     C     EUPODT        IFGE      XXWKDT
     C     EUFAF         ANDNE     'A'
     C     EUTRDT        ORGE      XXWKDT
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
     C     EUPODT        ORGE      XXWKDT
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C                   Z-ADD     *ZERO         PPDAYS            5 0
      *
     C                   ELSE
      *
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *
     C     EUFAF         IFEQ      'A'
     C     EUREVF        ANDNE     'Y'
      *** BEG CHG ***** 01/18/07 *****************************************
     C********** EUTRDT    IFGE ELPFDT
     C     EUTRDT        IFGE      SVPFDT
      *** END CHG ***** 01/18/07 *****************************************
     C                   Z-ADD     EUTRDT        XQFRM7
     C                   ELSE
      *** BEG CHG ***** 01/18/07 *****************************************
     C***********          Z-ADDELPFDT    XQFRM7
     C                   Z-ADD     SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 *****************************************
     C                   ENDIF                                                  EUTRDT IFGE ELPFDT
      *
     C                   ENDIF                                                  EUFAF IFEQ 'A'
      *
     C                   Z-ADD     XXWKDT        XQTO7
      *
      *  GET NUMBER OF DAYS
      *
     C                   EXSR      GETDYS
     C                   Z-ADD     QQDAYS        PPDAYS            5 0
      *
     C                   ENDIF                                                  EUPRDT GE XXWKDT
      *
     C     PPDAYS        IFEQ      *ZERO
      *
     C     EUPODT        IFGE      XXJAN1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXJAN1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXJAN1
     C                   Z-ADD     XXWAPR        XXPMYE           11 2
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXWTER        XXPMYT           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     XXWDTR        XXPMYD           11 2
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     XXWVLC        XXPMYV           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     XXWMPR        XXPMME           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPMYE
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXPMYT
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXPMYD
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXPMYV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/2604 *******************************************
     C                   Z-ADD     *ZERO         XXPMME           11 2
     C*** END ADD ***** 07/2604 *******************************************
     C                   ENDIF                                                  EUPODT IFGE XXJAN1
      *
     C                   ELSE
      *
     C     EUPODT        IFGE      XXJAN1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXJAN1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXJAN1
      **** BEGIN ADD **** 02/04/02 *********************************************
     C     EUTRDT        ORLT      XXJAN1
      *** BEG CHG ***** 01/18/07 **********************************
     C********** ELPFDT    ANDGEXXJAN1
     C     SVPFDT        ANDGE     XXJAN1
      *** END CHG ***** 01/18/07 **********************************
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDNE     'Y'
      **** END   ADD **** 02/04/02 *********************************************
      *
      *** BEG CHG ***** 01/20/09 ************************************** (A)
     C***********          Z-ADDEUPODT    XQFRM7
     C     EUPODT        IFLT      SVPFDT
     C                   Z-ADD     SVPFDT        XQFRM7
     C                   ELSE
     C                   Z-ADD     EUPODT        XQFRM7
     C                   ENDIF
      *** END CHG ***** 01/20/09 ************************************** (A)
      *
     C     EUFAF         IFEQ      'A'
     C     EUREVF        ANDNE     'Y'
      *
      *** BEG CHG ***** 01/18/07 **********************************
     C********** EUTRDT    IFGE ELPFDT
     C     EUTRDT        IFGE      SVPFDT
      *** END CHG ***** 01/18/07 **********************************
     C                   Z-ADD     EUTRDT        XQFRM7
     C                   ELSE
      *** BEG CHG ***** 01/18/07 **********************************
     C***********          Z-ADDELPFDT    XQFRM7
     C                   Z-ADD     SVPFDT        XQFRM7
      *** END CHG ***** 01/18/07 **********************************
     C                   ENDIF                                                  EUTRDT IFGE ELPFDT
      *
     C                   ENDIF                                                  EUFAF IFEQ 'A'
      *
     C                   ELSE
     C                   Z-ADD     XXJAN1        XQFRM7
     C                   ENDIF                                                  EUPODT IFGE XXJAN1
     C*
     C     XXWKDT        IFLE      XXPMTH
     C                   Z-ADD     XXWKDT        XQTO7
     C                   ELSE
     C                   Z-ADD     XXMTH1        XQTO7
     C                   ENDIF                                                  XXWKDT IFLE XXPMTH
      *
      *  GET NUMBER OF DAYS
      *
     C                   EXSR      GETDYS
     C     QQDAYS        DIV(H)    PPDAYS        XXPRO
     C     XXWAPR        MULT(H)   XXPRO         XXPMYE
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXWTER        MULT(H)   XXPRO         XXPMYT
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C     XXWDTR        MULT(H)   XXPRO         XXPMYD
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXWVLC        MULT(H)   XXPRO         XXPMYV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXWMPR        MULT(H)   XXPRO         XXPMME
     C*** END ADD ***** 07/26/04 ******************************************
     C                   ENDIF                                                  PDAYS = 0
      *
      * ADJUST CURRENT MONTH TO DATE EARNED
      *
     C     XXYDEA        SUB       XXPMYE        XXDIFF           11 2
     C     XXDIFF        IFNE      XXMDEA
     C                   Z-ADD     XXDIFF        XXMDEA
     C                   ENDIF                                                  XXDIFF IFNE XXMDEA
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXYDET        SUB       XXPMYT        XXDIFF
     C     XXDIFF        IFNE      XXMDET
     C                   Z-ADD     XXDIFF        XXMDET
     C                   ENDIF
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C     XXYDED        SUB       XXPMYD        XXDIFF
     C     XXDIFF        IFNE      XXMDED
     C                   Z-ADD     XXDIFF        XXMDED
     C                   ENDIF
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXYDEV        SUB       XXPMYV        XXDIFF
     C     XXDIFF        IFNE      XXMDEV
     C                   Z-ADD     XXDIFF        XXMDEV
     C                   ENDIF
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXYDEM        SUB       XXPMME        XXDIFF
     C     XXDIFF        IFNE      XXPMME
     C                   Z-ADD     XXDIFF        XXMDEM
     C                   ENDIF
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C     XXYDET        SUB       XXPMYT        XXDIFF
     C     XXDIFF        IFNE      XXMDET
     C                   Z-ADD     XXDIFF        XXMDET
     C                   ENDIF                                                  XXDIFF IFNE XXMDEA
     C***  End   Add  ***  01/28/05  **************************************
      *
     C                   Z-ADD     XXHWDT        XXWKDT            7 0
     C                   ENDIF                                                  *IN44 IFEQ *ON
     C*
     C*  PRIOR MONTH UNEARNED
     C*
     C     EUPODT        IFGE      XXMTH1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXMTH1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXMTH1
      **** BEGIN ADD **** 02/04/02 *********************************************
      *** BEG CHG ***** 01/18/07 ***********************************************
     C********** ELPFDT    ORGE XXMTH1
     C     SVPFDT        ORGE      XXMTH1
      *** END CHG ***** 01/18/07 ***********************************************
     C     EUFAF         ANDEQ     'A'
      **** END   ADD **** 02/04/02 *********************************************
     C                   Z-ADD     *ZERO         XXPMUN           11 2
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXPMUT           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXPMUD           11 2
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXPMUV           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXPMUM           11 2
     C*** EDG ADD ***** 07/26/04 ******************************************
     C                   ELSE
     C     XXUNEA        ADD       XXMDEA        XXPMUN
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXUNTR        ADD       XXMDET        XXPMUT
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C     XXUDTR        ADD       XXMDED        XXPMUD
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXUNVL        ADD       XXMDEV        XXPMUV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXUNMN        ADD       XXMDEM        XXPMUM
     C*** EDG ADD ***** 07/26/04 ******************************************
     C                   ENDIF
     C*
     C*  PRIOR YEAR UNEARNED
     C*
     C     EUPODT        IFGE      XXJAN1
     C     EUFAF         ANDNE     'A'
     C     EUPODT        ORGE      XXJAN1
     C     EUFAF         ANDEQ     'A'
     C     EUREVF        ANDEQ     'Y'
     C     EUTRDT        ORGE      XXJAN1
      **** BEGIN ADD **** 02/04/02 *********************************************
      *** BEG CHG ***** 01/18/07 ***********************************************
     C********** ELPFDT    ORGE XXJAN1
     C     SVPFDT        ORGE      XXJAN1
      *** END CHG ***** 01/18/07 ***********************************************
     C     EUFAF         ANDEQ     'A'
      **** END   ADD **** 02/04/02 *********************************************
     C                   Z-ADD     *ZERO         XXPYUN           11 2
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXPYUT           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXPYUD           11 2
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYUV           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         XXPYUM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C                   ELSE
     C     XXUNEA        ADD       XXYDEA        XXPYUN
     C***  Begin Add  ***  12/01/03  **************************************
     C     XXUNTR        ADD       XXYDET        XXPYUT
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C     XXUDTR        ADD       XXYDED        XXPYUD
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C     XXUNVL        ADD       XXYDEV        XXPYUV
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C     XXUNMN        ADD       XXYDEM        XXPYUM
     C*** END ADD ***** 07/26/04 ******************************************
     C                   ENDIF
      *** BEG ADD ***** 11/28/06 ******************************************
     C*********  SVCNDT    IFNE *ZERO
     C*********            Z-ADDSVCNDT    EUPODT
     C*********            Z-ADDEUPODT    SRDTI
     C*********            CALL 'SRCVTDAT'DTMDYY
     C*********            MOVELSRDTO     XXPODT
     C*********            Z-ADD0         SVCNDT
     C*********            ENDIF
     C     SVCXDT        IFNE      *ZERO
     C                   Z-ADD     SVCXDT        EUPODT
     C                   Z-ADD     EUPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT
      *** BEG ADD ***** 05/25/07 ******************************************
      *
      *   SVCXDT WAS CARRYING OVER INTO SUBSEQUENT TRANSACTIONS AND NEEDS TO
      *   BE CLEARED
      *
     C                   CLEAR                   SVCXDT
      *** END ADD ***** 05/25/07 ******************************************
     C                   ENDIF
      *** END ADD ***** 11/28/06 ******************************************
      *
      *** BEG ADD ***** 05/25/07 ******************************************
     C     SVREDT        IFNE      *ZERO
     C                   Z-ADD     SVREDT        EUPODT
     C                   Z-ADD     EUPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT
     C                   CLEAR                   SVREDT
     C                   ENDIF
      *** END ADD ***** 05/25/07 ******************************************
     C*
     C*  PRINT DETAIL OUTPUT
     C*
     C                   EXSR      PRTDET
     C*** BEGIN ADD ***  10/17/01  ************************************
      **** BEGIN DEL **** 12/06/01 *********************************************
     C******************** Z-ADD0         XXINXC
     C******************** Z-ADD0         XXWRXC
      **** END   DEL **** 12/06/01 *********************************************
     C***  Begin Chg  ***  01/08/04  **************************************
     C**********           Z-ADD0         EUINEX
     C**********           Z-ADD0         EUMWEX
     C**********           Z-ADD0         EUUNEX
     C                   Z-ADD     0             EOINEX
     C                   Z-ADD     0             EOMWEX
     C                   Z-ADD     0             EOUNEX
     C***  End   Chg  ***  01/08/04  **************************************
     C*** END ADD *****  10/17/01  ************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         XXITER
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     *ZERO         XXIDTR
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         XXIVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   Z-ADD     *ZERO         EOIMPR
     C                   Z-ADD     *ZERO         EOUNMN
     C                   Z-ADD     *ZERO         EOYDWM
     C                   Z-ADD     *ZERO         EOYDEM
     C                   Z-ADD     *ZERO         EOMDWM
     C                   Z-ADD     *ZERO         EOMDEM
     C                   Z-ADD     *ZERO         EOPMUM
     C                   Z-ADD     *ZERO         EOPYWM
     C                   Z-ADD     *ZERO         EOPYEM
     C                   Z-ADD     *ZERO         EOPYUM
     C*** END ADD ***** 07/26/04 ******************************************
     C*** BEG DEL ***** 10/06/04 ******************************************
     C*********
     C***UPDATE THE EXPECTED MONTHLY PREMIUMS
     C*********
     C*********  PDAYS     IFGT *ZERO
     C*********
     C***CALCULATE THE START AND END DATES FOR THE MONTH
     C*********
     C*********  1         DO   12        X       30
     C*********            Z-ADDXXPCY     XXWK30
     C*********  XXPMO     ADD  X         M       20
     C*********            SUB  1         M
     C*********
     C*********  M         IFGT 12
     C*********            SUB  12        M
     C*********            ADD  1         XXWK30
     C*********            ENDIF
     C*********
     C*********  XXWK30    MULT 100       XXWK50  50
     C*********            ADD  M         XXWK50
     C*********  XXWK50    MULT 100       XXMNST  70
     C*********            ADD  1         XXMNST
     C*********  XXWK50    MULT 100       XXMNEN  70
     C*********            ADD  MD,M      XXMNEN
     C*********
     C***IF*THE POLICY ENDS PRIOR TO THE MONTH, THERE ARE NO DAYS
     C***ACTIVE IN THE MONTH
     C*********
     C*********  XXWKDT    IFLE XXMNST
     C*********  EUPODT    ORGE XXMNEN
     C*********            Z-ADD*ZERO     QQDAYS
     C*********            ELSE
     C*********
     C***IF*THE POLICY IS ACTIVE THROUGHOUT THE MONTH, PRORATE BASED
     C***ON*THE NUMBER OF DAYS IN THE MONTH
     C*********
     C*********  EUPODT    IFLE XXMNST
     C*********  XXWKDT    ANDGEXXMNEN
     C*********            Z-ADDMD,M      QQDAYS
     C*********            ELSE
     C*********
     C***IF*THE POLICY STARTS OR ENDS DURING THE MONTH, PRORATE BASED
     C***ON*THE NUMBER OF DAYS THE POLICY WAS ACTIVE IN THE MONTH
     C*********
     C*********  EUPODT    IFGT XXMNST
     C*********            Z-ADDEUPODT    XQFRM7
     C*********            ELSE
     C*********            Z-ADDXXMNST    XQFRM7
     C*********            ENDIF
     C*********
     C*********  XXWKDT    IFLT XXMNEN
     C*********            Z-ADDXXWKDT    XQTO7
     C*********            ELSE
     C*********            Z-ADDXXMNEN    XQTO7
     C*********            ENDIF
     C***GET*NUMBER OF DAYS
     C*********            EXSR GETDYS
     C*********
     C***IF*THE POLICY STARTED DURING THE MONTH, INCLUDE THE POLICY
     C***START*DATE IN THE PERIOD.
     C*********
     C*********  EUPODT    IFGT XXMNST
     C*********            ADD  1         QQDAYS
     C*********            ENDIF
     C*********
     C*********            ENDIF                           ELPFDT LE XXMNST
     C*********            ENDIF                           XXWKDT LE XXMNST
     C*********
     C*********  QQDAYS    DIV  PDAYS     XXPRO     H
     C*****BEGIN CHG **** 07/19/01 *********************************************
     C********** XXWNMP    MULT XXPRO     XXWK11 112H
     C*****BEGIN CHG **** 12/06/01 *********************************************
     C********** XXWAPR    MULT XXPRO     XXWK11 112H
     C*****BEGIN CHG **** 01/21/02 *********************************************
     C********** XXWAPR    SUB  XXWRXC    XXAWXC 112
     C*********
     C*********  QQPRTD    IFEQ 'Y'
     C*********            Z-ADDXXWAPR    XXAWXC
     C*********            ELSE
     C*********  XXWAPR    SUB  XXWRXC    XXAWXC 112
     C*********            ENDIF                           QQPRTD IFEQ 'Y'
     C*********
     C*****END*  CHG **** 01/21/02 *********************************************
     C*********  XXAWXC    MULT XXPRO     XXWK11 112H
     C*****END*  CHG **** 12/06/01 *********************************************
     C*****END*  CHG **** 07/19/01 *********************************************
     C*********            ADD  XXWK11    EP,X
     C****BEG*ADD ***** 07/26/04 ***********************************************
     C*********  XXWMPR    MULT XXPRO     XXWK11
     C*********            ADD  XXWK11    EMP,X
     C****END*ADD ***** 07/26/04 ***********************************************
     C*********            ENDDO                           1 DO 12, X
     C*********            ENDIF                           PDAYS <> 0
     C*** END DEL ***** 10/06/04 ******************************************
     C*** BEGIN ADD ***  12/06/01  ************************************
     C                   Z-ADD     0             XXINXC
     C                   Z-ADD     0             XXWRXC
     C*** END ADD *****  12/06/01  ************************************
     C*** BEG ADD *****  11/28/06  ************************************
     C                   MOVE      *OFF          SRCAFL
     C                   Z-ADD     0             XXCXPY
     C                   Z-ADD     0             XXCXCY
     C                   Z-ADD     0             XXADPY
     C                   Z-ADD     0             XXADCY
     C*** END ADD *****  11/28/06  ************************************
      *** BEG ADD ***** 02/09/07 *************************************
     C                   Z-ADD     0             XXFAPY
     C                   Z-ADD     0             XXFACY
      *** END ADD ***** 02/09/07 *************************************
     C*
     C                   ENDSR                                                  SRCALC
      /EJECT
     C**************************************************************
     C*  EMPLOYER/DIV/FYR BREAK SUB-ROUTINE
     C**************************************************************
     C*
     C     BRKEMP        BEGSR
     C*
     C     FIRST         IFEQ      *OFF
      **** BEGIN ADD **** 02/28/01 ************************************
     C                   MOVE      *ON           FRSTE
      **** END   ADD **** 02/28/01 ************************************
      *** BEG ADD ***** 10/06/04 **************************************
     C                   MOVE      *ON           FRSTEP
      *** END ADD ***** 10/06/04 **************************************
      *** BEG DEL ***** 06/03/10 **************************************
     C************IN42     IFEQ *OFF
      *** END DEL ***** 06/03/10 **************************************
     C                   Z-ADD     1             I1                1 0
     C     *ZERO         LOOKUP    EF(I1)                                 61
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *** BEG CHG ***** 06/03/10 **************************************
     C*********  I1        IFNE 0
     C*********  CX,I1     IFNE *ZERO
     C*********  CX,I1     ANDLTXXTDAT
     C*********  CX,I1     ANDGEELPFDT
     C*********  CX,I1     ANDLTELPTDT
     C*********  CX,I1     ORNE *ZERO
     C*********  CX,I1     ANDLTXXTDAT
     C*********  CX,I1     ANDEQELPFDT
     C*********  CX,I1     ANDEQELPTDT
     C*********  CX,I1     ORNE *ZERO
     C*********  CX,I1     ANDLTXXTDAT
     C*********  CX,I1     ANDLTELPFDT
     C*********  *INOB     IFEQ *ON
     C*********  Q1FND     IFNE 0
     C*********            EXCPTERRHD
     C*********            ENDIF
     C*********            MOVE *OFF      *INOB
     C*********            ENDIF
     C*********            Z-ADDCX,I1     SRDTI
     C*********            CALL 'SRCVTDAT'DTMDYY
     C*********            MOVELSRDTO     XXCXDT  80
     C*********  Q1FND     IFNE 0
     C*********            EXCPTERR
     C*********            ENDIF
     C*********            ENDIF                           CX,I1 IFNE *ZERO
     C*********            ENDIF                           I1 IFNE 0
     C*********            ELSE
     C*********            MOVE *OFF      *IN42
     C*********            ENDIF                           *IN42 IFEQ *OFF
      *
     C     I1            IFNE      0
      *** beg add ***** 12/15/14 **************************************
     C     *IN91         ANDEQ     *ON
      *** end add ***** 12/15/14 **************************************
     C                   MOVE      *OFF          CXERR             1
     C                   SELECT
     C     CX(I1)        WHENNE    *ZERO
     C     CX(I1)        ANDLT     XXTDAT
     C     CX(I1)        ANDLT     ELPTDT
     C                   MOVE      *ON           CKCXFL            1            CHK CX HIST
     C     CX(I1)        WHENNE    *ZERO
     C     CX(I1)        ANDLT     XXTDAT
     C     ELPFDT        ANDEQ     ELPTDT
     C                   MOVE      *ON           CKCXFL            1
     C                   OTHER
     C                   MOVE      *OFF          CKCXFL            1
     C                   ENDSL
     C     CKCXFL        IFEQ      *ON
     C     ELKY          SETGT     PXH01
     C     ELKY          READPE    PXH01                                  60
     C     *IN60         IFEQ      *OFF
     C                   SELECT
     C     ELPFDT        WHENNE    ELPTDT
     C     CX(I1)        ANDEQ     XHCXDT                                       NO ERROR
     C     XHREDT        ANDEQ     *ZERO
     C     CX(I1)        WHENLE    ELPFDT                                       NO ERROR
     C     XHCXDT        ANDEQ     ELPFDT
     C     XHREDT        ANDEQ     *ZERO
     C                   OTHER
     C                   MOVE      *ON           CXERR                          BAD CX HISTORY
     C                   ENDSL
     C                   ELSE
     C                   MOVE      *ON           CXERR                          NO CX HISTORY
     C                   Z-ADD     0             XHCXDT
     C                   ENDIF
     C     CXERR         IFEQ      *ON
     C                   Z-ADD     1             ERRCT
     C     *INOB         IFEQ      *ON
     C     Q1FND         IFNE      0
     C                   EXCEPT    ERRHD
     C                   ENDIF                                                  Q1FND <> 0
     C                   MOVE      *OFF          *INOB
     C                   ENDIF                                                  *IN0B = *ON
     C     XHCXDT        IFEQ      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI                          NO CX HISTORY
     C                   ELSE
     C                   Z-ADD     XHCXDT        SRDTI                          USE CX HIST DT
     C                   ENDIF                                                  XHCXDT = 0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
     C     Q1FND         IFNE      0
     C                   EXCEPT    ERR
     C                   ENDIF                                                  Q1FND <> 0
     C                   ENDIF                                                  CXERR  = *ON
     C                   ENDIF                                                  CKCXFL = *ON
     C                   ENDIF                                                  I1 IFNE 0
      *** END CHG ***** 06/03/10 **************************************
     C*
     C*  PRINT THE POLICY TOTALS
     C*
     C                   ADD       XPINPR        XTINPR           11 2
     C                   ADD       XPYDWR        XTYDWR           11 2
     C                   ADD       XPYDEA        XTYDEA           11 2
     C                   ADD       XPUNEA        XTUNEA           11 2
     C                   ADD       XPMDWR        XTMDWR           11 2
     C                   ADD       XPMDEA        XTMDEA           11 2
     C                   ADD       XPPMUN        XTPMUN           11 2
     C                   ADD       XPPYUN        XTPYUN           11 2
     C                   ADD       XPPYWR        XTPYWR           11 2
     C                   ADD       XPPYEA        XTPYEA           11 2
     C***  Begin Add  ***  12/01/03  **************************************
     C                   ADD       XPITER        XTITER           11 2
     C                   ADD       XPUNTR        XTUNTR           11 2
     C                   ADD       XPPRTD        XTPRTD           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   ADD       XPIVLC        XTIVLC           11 2
     C                   ADD       XPUNVL        XTUNVL           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   ADD       XPIMPR        XTIMPR           11 2
     C                   ADD       XPUNMN        XTUNMN           11 2
     C                   ADD       XPYDWM        XTYDWM           11 2
     C                   ADD       XPYDEM        XTYDEM           11 2
     C                   ADD       XPMDWM        XTMDWM           11 2
     C                   ADD       XPMDEM        XTMDEM           11 2
     C                   ADD       XPPMUM        XTPMUM           11 2
     C                   ADD       XPPYWM        XTPYWM           11 2
     C                   ADD       XPPYEM        XTPYEM           11 2
     C                   ADD       XPPYUM        XTPYUM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C*
      **** BEGIN ADD **** 10/22/02 ************************************
     C                   ADD       XPINPR        XSINPR           11 2
     C                   ADD       XPYDWR        XSYDWR           11 2
     C                   ADD       XPYDEA        XSYDEA           11 2
     C                   ADD       XPUNEA        XSUNEA           11 2
     C                   ADD       XPMDWR        XSMDWR           11 2
     C                   ADD       XPMDEA        XSMDEA           11 2
     C                   ADD       XPPMUN        XSPMUN           11 2
     C                   ADD       XPPYUN        XSPYUN           11 2
     C                   ADD       XPPYWR        XSPYWR           11 2
     C                   ADD       XPPYEA        XSPYEA           11 2
      **** END   ADD **** 10/22/02 ************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   ADD       XPITER        XSITER           11 2
     C                   ADD       XPUNTR        XSUNTR           11 2
     C                   ADD       XPPRTD        XSPRTD           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   ADD       XPIVLC        XSIVLC           11 2
     C                   ADD       XPUNVL        XSUNVL           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   ADD       XPIMPR        XSIMPR           11 2
     C                   ADD       XPUNMN        XSUNMN           11 2
     C                   ADD       XPYDWM        XSYDWM           11 2
     C                   ADD       XPYDEM        XSYDEM           11 2
     C                   ADD       XPMDWM        XSMDWM           11 2
     C                   ADD       XPMDEM        XSMDEM           11 2
     C                   ADD       XPPMUM        XSPMUM           11 2
     C                   ADD       XPPYWM        XSPYWM           11 2
     C                   ADD       XPPYEM        XSPYEM           11 2
     C                   ADD       XPPYUM        XSPYUM           11 2
     C*** END ADD ***** 07/26/04 ******************************************
     C*
     C     QQPRNT        IFEQ      'S'
     C     QQPRNT        OREQ      'B'
     C     EMOKAY        ANDEQ     *ON
     C     ELOKAY        ANDEQ     *ON
     C*
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTPOLTO1
     C   91              EXCEPT    POLTO1
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQPRNT        IFEQ      'S'
     C     QQPRNT        OREQ      'B'
     C     QQSRST        IFEQ      'Y'
     C     EUST          ANDNE     SVST
     C   91              EXCEPT    STTOT1
     C                   MOVE      *ON           *INOA
     C                   ENDIF
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C*
     C     QQPRNT        IFEQ      'D'
     C     QQPRNT        OREQ      'B'
     C     EMOKAY        ANDEQ     *ON
     C     ELOKAY        ANDEQ     *ON
     C*
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTPOLTOT
     C   91              EXCEPT    POLTOT
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQPRNT        IFEQ      'D'
     C     QQPRNT        OREQ      'B'
     C     QQSRST        IFEQ      'Y'
     C     EUST          ANDNE     SVST
     C   91              EXCEPT    STTOT
     C                   MOVE      *ON           *INOF
     C                   ENDIF
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C*
     C                   ELSE
     C                   MOVE      *OFF          FIRST
     C                   ENDIF                                                  FIRST = *OFF
     C*
      *** BEG ADD ***** 07/21/04 ***************************************
     C                   IN        PRTARY
     C     PRTARY        IFEQ      'Y'
      *
     C     F1ARR         IFEQ      ' '
     C                   OPEN      QSYSPRT7
     C                   MOVE      *ON           *INOC
     C                   MOVE      '1'           F1ARR             1
     C                   ENDIF
      *
     C                   Z-ADD     1             I                 3 0
     C     XACT(I)       DOWNE     *BLANK
      *
     C     *INOC         IFEQ      *ON
     C                   EXCEPT    ARRHD
     C                   MOVE      *OFF          *INOC
     C                   ENDIF
      *
     C                   EXCEPT    ARRDT
     C                   ADD       1             I
     C                   ENDDO
      *
     C                   EXCEPT    SPACE
     C                   ENDIF                                                  PRTARY = 'Y'
      *** END ADD ***** 07/21/04 ***************************************
      *
      **** BEGIN ADD **** 10/22/02 ************************************
     C     EUST          IFNE      SVST
     C*
     C*  CLEAR STATE LEVEL TOTALS
     C*
     C                   CLEAR                   XSINPR
     C                   CLEAR                   XSYDWR
     C                   CLEAR                   XSYDEA
     C                   CLEAR                   XSUNEA
     C                   CLEAR                   XSMDWR
     C                   CLEAR                   XSMDEA
     C                   CLEAR                   XSPMUN
     C                   CLEAR                   XSPYUN
     C                   CLEAR                   XSPYWR
     C                   CLEAR                   XSPYEA
     C***  Begin Add  ***  12/01/03  **************************************
     C                   CLEAR                   XSITER
     C                   CLEAR                   XSUNTR
     C                   CLEAR                   XSPRTD
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   CLEAR                   XSIVLC
     C                   CLEAR                   XSUNVL
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   XSIMPR
     C                   CLEAR                   XSUNMN
     C                   CLEAR                   XSYDWM
     C                   CLEAR                   XSYDEM
     C                   CLEAR                   XSMDWM
     C                   CLEAR                   XSMDEM
     C                   CLEAR                   XSPMUM
     C                   CLEAR                   XSPYWM
     C                   CLEAR                   XSPYEM
     C                   CLEAR                   XSPYUM
     C*** END ADD ***** 07/26/04 ******************************************
     C*
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C*
     C*  IF NOT END OF FILE
     C*
     C     *IN50         IFEQ      *OFF
     C*
     C*  CLEAR ARRAYS
     C*
     C                   CLEAR                   XACT                           ACTION
     C                   CLEAR                   FRDT                           FROM DATE
     C                   CLEAR                   TODT                           TO DATE
     C                   CLEAR                   INPR                           INFORCE PREM
     C                   CLEAR                   WAPR                           WRTN ANNL PREM
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   IMPR
     C                   CLEAR                   WMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   WNMP                           WRTN NORN PREM
     C***  Begin Add  ***  12/01/03  **************************************
     C                   CLEAR                   ITER
     C                   CLEAR                   WTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   CLEAR                   IDTR
     C                   CLEAR                   WDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   CLEAR                   IVLC
     C                   CLEAR                   WVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   CLEAR                   INEX
     C                   CLEAR                   PRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   CLEAR                   WATR                           WRTN ANNL PREM
     C***  End   Add  ***  01/28/05  **************************************
     C                   CLEAR                   DAYS                           TODT - FRDT
     C                   CLEAR                   EP                             EXPECTED PREM
      *** BEG ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   EMP                            EXPECTED PREM
      *** END ADD ***** 07/26/04 ******************************************
      *** BEG ADD ***** 10/06/04 ******************************************
     C*********            CLEARNWAD                       OLD A/R ADJUSTMENT
     C*********            CLEAROWAD                       OLD A/R ADJUSTMENT
     C                   CLEAR                   SEP
     C                   CLEAR                   SEMP
     C                   CLEAR                   NEP
     C                   CLEAR                   NEMP
     C                   CLEAR                   ADJ
     C                   CLEAR                   SVPRTD
     C                   CLEAR                   S2PRTD
     C                   CLEAR                   XXFEXC
     C                   CLEAR                   SVFEXC
     C                   CLEAR                   XXINXC
      *** END ADD ***** 10/06/04 ******************************************
      *** BEG ADD ***** 11/28/06 ******************************************
     C                   CLEAR                   XAD
     C                   CLEAR                   XEP
     C                   CLEAR                   SVCXDT
     C                   CLEAR                   SRAD
      *** END ADD ***** 11/28/06 ******************************************
      *** BEG ADD ***** 01/18/07 ******************************************
     C                   CLEAR                   SVPFDT
      *** END ADD ***** 01/18/07 ******************************************
      *** BEG ADD ***** 02/05/07 ******************************************
     C                   CLEAR                   FADM
      *** END ADD ***** 02/05/07 ******************************************
      *** BEG ADD ***** 02/15/10 ******************************************
     C                   CLEAR                   SVCNDT
     C                   MOVE      *OFF          ADJDTF            1            ADJUST DATE FLAG
     C                   Z-ADD     0             SVACXD            7 0          SAVE ADJ CX DATE
      *** END ADD ***** 02/15/10 ******************************************
     C*
     C*  CLEAR POLICY LEVEL TOTALS
     C*
     C                   CLEAR                   XPINPR
     C                   CLEAR                   XPYDWR
     C                   CLEAR                   XPYDEA
     C                   CLEAR                   XPUNEA
     C                   CLEAR                   XPMDWR
     C                   CLEAR                   XPMDEA
     C                   CLEAR                   XPPMUN
     C                   CLEAR                   XPPYUN
     C                   CLEAR                   XPPYWR
     C                   CLEAR                   XPPYEA
     C***  Begin Add  ***  12/01/03  **************************************
     C                   CLEAR                   XPITER
     C                   CLEAR                   XPUNTR
     C                   CLEAR                   XPPRTD
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   CLEAR                   XPIVLC
     C                   CLEAR                   XPUNVL
     C*** END ADD ***** 09/01/04 ******************************************
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   XPIMPR
     C                   CLEAR                   XPUNMN
     C                   CLEAR                   XPYDWM
     C                   CLEAR                   XPYDEM
     C                   CLEAR                   XPMDWM
     C                   CLEAR                   XPMDEM
     C                   CLEAR                   XPPMUM
     C                   CLEAR                   XPPYWM
     C                   CLEAR                   XPPYEM
     C                   CLEAR                   XPPYUM
     C*** END ADD ***** 07/26/04 ******************************************
     C*
     C*  RESET OTHER WORK FIELDS
     C*
     C                   MOVE      *OFF          *IN75
     C                   MOVE      *OFF          XXCXST            1
     C                   MOVE      *OFF          XXCAST            1
     C                   MOVE      9999999       XXCNDT            7 0
     C                   CLEAR                   XXNETR
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   XXNETM
     C*** END ADD ***** 07/26/04 ******************************************
     C*** BEG CHG ***** 03/23/06 ******************************************
     C***********          Z-ADD*ZERO     Q       30
     C                   Z-ADD     *ZERO         Q                 6 0
     C*** END CHG ***** 03/23/06 ******************************************
     C                   Z-ADD     *ZERO         SVRWDT
     C*
     C*  SAVE KEY VALUES
     C*
     C                   MOVE      EUCO#         SVCO#
     C                   MOVE      EUFND         SVFND
     C                   MOVE      EUEMP#        SVEMP#
      **** BEGIN ADD **** 10/22/02 ************************************
     C                   MOVE      EUST          SVST
      **** END   ADD **** 10/22/02 ************************************
     C                   MOVE      EUDIV         SVDIV
     C                   MOVE      EUFYR         SVFYR
      *** BEG ADD ***** 05/12/09 *****************************************
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C     F2CAR#        ANDNE     0
     C                   Z-ADD     F2CAR#        SVCAR#
     C                   ELSE
     C                   Z-ADD     0             SVCAR#
     C                   ENDIF
      *** END ADD ***** 05/12/09 *****************************************
     C*
     C*  GET THE EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          EMOKAY            1
     C                   MOVE      *ZERO         SVCO#
     C                   MOVE      *ZERO         SVFND
     C                   MOVE      *ZERO         SVEMP#
     C                   MOVE      *ZERO         SVDIV
     C                   MOVE      *ZERO         SVFYR
     C                   GOTO      ENDBRK
     C                   ENDIF
     C*
     C                   MOVE      *ON           EMOKAY
     C*
     C*  GET THE EMPLOYER DETAIL RECORD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *OFF          ELOKAY            1
     C                   MOVE      *ZERO         SVCO#
     C                   MOVE      *ZERO         SVFND
     C                   MOVE      *ZERO         SVEMP#
     C                   MOVE      *ZERO         SVDIV
     C                   MOVE      *ZERO         SVFYR
     C                   GOTO      ENDBRK
     C                   ENDIF
     C*
     C                   MOVE      *ON           ELOKAY
      *** BEG ADD ***** 09/20/06 **************************
     C     ELOKAY        IFEQ      *ON
     C*
     C*** begin add *** 03/09/09 **************************************
     C     ELNWRW        IFEQ      *BLANKS
     C*** end   add *** 03/09/09 **************************************
     C                   SELECT
     C     XXCLT#        WHENNE    '093'
     C*
     C*  Check if employer has prior year policy (then reset to curr year)
     C*
     C                   Z-ADD     EUEMP#        ZZEMP#
     C                   Z-ADD     EUDIV         ZZDIV
     C     SVFYR         SUB       1             ZZFYRP
     C     ELKYP         CHAIN     PEL01                              65
     C     ELKY          CHAIN     PEL01                              60
     C     *IN65         IFEQ      *OFF
     C                   MOVE      'R'           EUNWRW
     C                   ELSE
     C     EXKY          CHAIN     PEX01                              65
     C     *IN65         IFEQ      *OFF
     C                   MOVE      'R'           EUNWRW
     C                   ELSE
     C                   MOVE      'N'           EUNWRW
     C                   ENDIF                                                  *IN65 IFEQ *OFF
     C                   ENDIF                                                  *IN65 IFEQ *OFF
     C*
     C     XXCLT#        WHENEQ    '093'
      *
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  F2CAR#        X2CAR#
     C                   ELSE
     C                   MOVE      *BLANKS       X2CAR#
     C                   ENDIF
      *
     C                   MOVE      'N'           EUNWRW
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C     ELFYR         IFLT      SVFYR
     C                   MOVE      'R'           EUNWRW
     C                   LEAVE
     C                   ENDIF
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO
     C*
     C* RESET WDELP
     C*
     C     ELKY          SETLL     PEL01
     C     ELKY          READE     PEL01                                  60
     C*
     C     EUNWRW        IFEQ      'N'
     C     ELKY1         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C     F2OKY         CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C     F2CAR#        ANDEQ     X2CAR#
     C                   MOVE      'R'           EUNWRW
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C*** begin add *** 03/09/09 **************************************
     C                   ELSE
     C                   MOVEL(P)  ELNWRW        EUNWRW
     C                   ENDIF
     C*** end   add *** 03/09/09 **************************************
     C*
     C                   ENDIF
      *** END ADD ***** 09/20/06 **************************
      *** BEG ADD ***** 04/27/04 **************************
      *
      * FIND OUT IF FULL EXP CONSTANT IS CHARGED FOR SHORT TERM POLICYS
      *
     C     ELPRTD        IFGT      0
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     ECKY1         SETGT     PEC01
     C     ECKY          READPE    PEC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ECSFUL        XXSFUL            1
     C                   ELSE
     C                   MOVE      *BLANK        XXSFUL
     C                   ENDIF                                                  NOT *IN60
     C                   ELSE
     C                   MOVE      *BLANK        XXSFUL
     C                   ENDIF                                                  ELPRTD > 0
      *** END ADD ***** 04/27/04 **************************
      *** BEG ADD ***** 07/26/04 **************************
      *
      * GET ANNIVERSARY RATING DATE AND RATING STATE TO CALCULATE MAN PREM
      * IN CALMAN
      *
     C                   Z-ADD     ELANDT        XXANDT
     C                   ADD       10000         XXANDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
      *** END ADD ***** 07/26/04 **************************
      *
      * DETERMINE THE STATUS OF THE EMPLOYER AS OF THE VALUATION DATE
      *
     C                   MOVE      *OFF          CANCLD            1
      *
     C     XHKY          SETGT     PXH01
     C     XHKY1         READPE    PXH01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     XHCXDT        IFLE      XXVDAT
     C     XHREDT        ANDEQ     0
     C                   MOVE      *ON           CANCLD
     C                   ENDIF                                                  XHREDT IFEQ O
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C*  GET THE FUND DETAIL RECORD
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C*  CONVERT THE POLICY FROM DATE TO MMDDCCYY
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C*  CALCULATE THE POLICY "TO" DATE, IN CASE OF A CANCELLATION
     C*
      *
      *  CHECK EMPLOYER HISTORY TO FIND THE PRE-CANCEL ELPTDT
      *
     C*** BEG ADD ***** 10/10/05 *****************************************
     C                   CLEAR                   EHFAF
     C*** END ADD ***** 10/10/05 *****************************************
     C                   Z-ADD     XXVDAT        XXKDAT            7 0
     C     EHKY1         SETGT     PEH01
     C     EHKY          READPE    PEH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     EHFAF         ANDNE     'R'
     C     EHFAF         ANDNE     'E'
     C     EHFAF         OREQ      'E'
     C     EHEFDT        ANDGT     XXVDAT
     C     *IN60         ANDEQ     *OFF
     C     EHFAF         OREQ      'R'
     C     EHEFDT        ANDGT     XXVDAT
     C     *IN60         ANDEQ     *OFF
     C     EHKY          READPE    PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*** BEG ADD ***** 10/10/05 *****************************************
      *
      * IN THE EVENT THAT AN AUDIT BILL IS THE ONLY BILL RUN, USE EHPTDT FOR
      * XXPTDT
     C     *IN60         IFEQ      *ON
     C     EHFAF         ANDEQ     'A'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
     C*** END ADD ***** 10/10/05 *****************************************
      *
     C     *IN60         IFEQ      *OFF
      *** BEG ADD ***** 01/18/07 *****************************************
     C                   Z-ADD     EHPFDT        SVPFDT            7 0
      *** END ADD ***** 01/18/07 *****************************************
     C                   Z-ADD     EHPTDT        XXPTDT
     C                   ELSE
     C     ELPFDT        ADD       10000         XXPTDT
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      *** BEG ADD ***** 01/18/07 *****************************************
     C     SVPFDT        IFEQ      *ZERO
     C                   Z-ADD     ELPFDT        SVPFDT
     C                   ENDIF
      *** END ADD ***** 01/18/07 *****************************************
      *
      * DETERMINE POLICY EXPIRATION DATE AS OF THE END OF THE PRIOR MONTH
      *
     C                   Z-ADD     XXPMTH        XXKDAT            7 0
     C     EHKY1         SETGT     PEH01
     C     EHKY          READPE    PEH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
     C     EHFAF         ANDNE     'R'
     C     EHFAF         ANDNE     'E'
      *
     C     EHFAF         OREQ      'E'
     C     EHEFDT        ANDGT     XXPMTH
     C     *IN60         ANDEQ     *OFF
      *
     C     EHFAF         OREQ      'R'
     C     EHEFDT        ANDGT     XXPMTH
     C     *IN60         ANDEQ     *OFF
      *
     C     EHKY          READPE    PEH01                                  60
      *
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EHPTDT        XXPPXD            7 0
      *
      * SET INDICATOR OF POLICY EXPIRATION DATE HAS CHANGED
      *
     C     XXPPXD        IFNE      XXPTDT
     C                   SETON                                        44
      *
      *
      * DETERMINE THE CANCELLATION DATE OF THE EMP AS OF END OF PRIOR MONTH
      *
     C                   Z-ADD     XXVDAT        XXHVDT            7 0
     C                   Z-ADD     XXPMTH        XXVDAT
      *
     C     XHKY          SETGT     PXH01
     C     XHKY1         READPE    PXH01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     XHCXDT        IFLE      XXVDAT
     C     XHREDT        ANDEQ     0
     C                   Z-ADD     XHCXDT        XXPCXD            7 0
     C                   ENDIF                                                  XHCXDT IFLE O
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   Z-ADD     XXHVDT        XXVDAT            7 0
      *
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  XXPPXD IFNE XXPTDT
     C                   ELSE
     C                   SETOFF                                       44
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     C*
     C*  PRINT THE DETAIL REPORT POLICY HEADING
     C*
     C     QQPRNT        IFEQ      'D'
     C     QQPRNT        OREQ      'B'
     C*
     C     *INOF         IFEQ      *ON
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTHEAD
     C   91              EXCEPT    HEAD
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQSRST        IFEQ      'Y'
     C     *IN91         ANDEQ     *ON
     C                   EXCEPT    SHEAD
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTPOLHD
     C   91              EXCEPT    POLHD
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C*  PRINT THE SUMMARY REPORT POLICY HEADING
     C*
     C     QQPRNT        IFEQ      'S'
     C     QQPRNT        OREQ      'B'
     C*
     C     *INOA         IFEQ      *ON
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTHEAD1
     C   91              EXCEPT    HEAD1
      **** BEGIN ADD **** 10/22/02 ************************************
     C     Q1FND         IFNE      0
     C     QQSRST        IFEQ      'Y'
     C     *IN91         ANDEQ     *ON
     C                   EXCEPT    SHEAD1
     C                   ENDIF
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C*
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTPOLHD1
     C   91              EXCEPT    POLHD1
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN50 = *OFF
     C*
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         EOITER
     C***  End   Add  ***  12/01/03  **************************************
     C*
     C     ENDBRK        ENDSR                                                  BRKEMP
      /EJECT
     C**************************************************************
     C*  GET NUMBER OF DAYS
     C**************************************************************
     C*
     C     GETDYS        BEGSR
     C*
     C     XQFRM7        IFGT      XQTO7
     C                   Z-ADD     *ZERO         QQDAYS
     C*
     C                   ELSE                                                   XQFRM7 > XQTO7
     C*
     C                   Z-ADD     XQFRM7        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQFROM
     C*
     C                   Z-ADD     XQTO7         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQTO
     C*
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C*
     C                   CALL      'SRDDIF8'     PLDDIF
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XQFRM7        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFRM7
     C                   Z-ADD     XQTO7         XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   ENDIF
     C*
     C     XXLPDT        IFLT      XQFRM7
     C     XXLPDT        ORGT      XQTO7
     C                   Z-ADD     *ZERO         XXLPDT
     C                   ENDIF
     C*
     C     XXLPDT        IFNE      *ZERO
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YYMD>MDCY
     C*
     C     SRERR         IFNE      *ON
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     QQDAYS        IFLT      *ZERO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   ENDIF                                                  QQDAYS < 0
     C*
     C                   ENDIF                                                  XQFRM7 > XQTO7
     C*
     C                   ENDSR
      /EJECT
     C**************************************************************
     C*  PRINT DETAIL OUTPUT
     C**************************************************************
     C     PRTDET        BEGSR
     C*
     C                   ADD       XXINPR        XPINPR           11 2
     C                   ADD       XXYDWR        XPYDWR           11 2
     C                   ADD       XXYDEA        XPYDEA           11 2
     C                   ADD       XXUNEA        XPUNEA           11 2
     C                   ADD       XXMDWR        XPMDWR           11 2
     C                   ADD       XXMDEA        XPMDEA           11 2
     C                   ADD       XXPMUN        XPPMUN           11 2
     C                   ADD       XXPYUN        XPPYUN           11 2
     C                   ADD       XXPYWR        XPPYWR           11 2
     C                   ADD       XXPYEA        XPPYEA           11 2
     C***  BEG ADD ***** 07/26/04 *****************************************
     C                   ADD       XXIMPR        XPIMPR           11 2
     C                   ADD       XXUNMN        XPUNMN           11 2
     C                   ADD       XXYDWM        XPYDWM           11 2
     C                   ADD       XXYDEM        XPYDEM           11 2
     C                   ADD       XXMDWM        XPMDWM           11 2
     C                   ADD       XXMDEM        XPMDEM           11 2
     C                   ADD       XXPMUM        XPPMUM           11 2
     C                   ADD       XXPYWM        XPPYWM           11 2
     C                   ADD       XXPYEM        XPPYEM           11 2
     C                   ADD       XXPYUM        XPPYUM           11 2
     C***  END ADD ***** 07/26/04 *****************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   ADD       XXITER        XPITER           11 2
     C                   ADD       XXUNTR        XPUNTR           11 2
     C                   ADD       XXPRTD        XPPRTD           11 2
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   ADD       XXIVLC        XPIVLC           11 2
     C                   ADD       XXUNVL        XPUNVL           11 2
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   ADD       XXINEX        XPINEX           11 2
     C                   ADD       XXUNEX        XPUNEX           11 2
     C                   ADD       XXMWEX        XPMWEX           11 2
     C***  End   Add  ***  01/08/04  **************************************
     C*
     C     QQPRNT        IFEQ      'D'
     C     QQPRNT        OREQ      'B'
     C*
     C     *INOF         IFEQ      *ON
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTHEAD
     C*********            EXCPTPOLHD
     C   91              EXCEPT    HEAD
      **** BEGIN ADD **** 10/22/02 ************************************
     C     QQSRST        IFEQ      'Y'
     C     *IN91         ANDEQ     *ON
     C                   EXCEPT    SHEAD
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
     C   91              EXCEPT    POLHD
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C     Q1FND         IFNE      0
     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            EXCPTDET
     C   91              EXCEPT    DET
     C*** END CHG *****  10/17/01  ************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C*  WRITE CLIENT OUTPUT WORKFILE
     C*
     C                   MOVE      XXCO#         EOCO#
     C                   MOVE      XXFND         EOFND
     C                   MOVE      ELEMP#        EOEMP#
     C                   MOVE      ELDIV         EODIV
     C                   MOVE      ELFYR         EOFYR
      *** BEG CHG ***** 01/18/07 **************************************
     C***********          MOVE ELPFDT    SRDTI     P
     C                   MOVE(P)   SVPFDT        SRDTI
      *** END CHG ***** 01/18/07 **************************************
      **** BEG CHG ****** 05/24/04 ************************************
      **** BEGIN ADD **** 10/22/02 ************************************
     C*******************  MOVE EUST      EOST
      **** END   ADD **** 10/22/02 ************************************
      *** BEG ADD ***** 01/04/10 **************************************
     C                   MOVEL     ELBSCD        EOBSCD
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BSSEFL        EOSEFL
     C                   ENDIF
      *** END ADD ***** 01/04/10 **************************************
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        EOST
     C                   ELSE
     C                   MOVE      EUST          EOST
     C                   ENDIF
      **** END CHG ****** 05/24/04 ************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EOPFDT
     C                   MOVEL     EMNAME        EONAME
     C*
     C                   MOVE(P)   EUPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EOPRDT
     C*
     C                   MOVE(P)   EUTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EOTRDT
     C*
     C                   MOVE(P)   EUPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EOPODT
     C                   MOVE      XXINPR        EOINPR
     C                   MOVE      XXYDWR        EOYDWR
     C                   MOVE      XXYDEA        EOYDEA
     C                   MOVE      XXUNEA        EOUNEA
     C                   MOVE      XXMDWR        EOMDWR
     C                   MOVE      XXMDEA        EOMDEA
     C                   MOVE      XXPMUN        EOPMUN
     C                   MOVE      XXPYWR        EOPYWR
     C                   MOVE      XXPYEA        EOPYEA
     C                   MOVE      XXPYUN        EOPYUN
     C***  BEG ADD ***** 07/26/04 *****************************************
     C                   MOVE      XXIMPR        EOIMPR
     C                   MOVE      XXUNMN        EOUNMN
     C                   MOVE      XXYDWM        EOYDWM
     C                   MOVE      XXYDEM        EOYDEM
     C                   MOVE      XXMDWM        EOMDWM
     C                   MOVE      XXMDEM        EOMDEM
     C                   MOVE      XXPMUM        EOPMUM
     C                   MOVE      XXPYWM        EOPYWM
     C                   MOVE      XXPYEM        EOPYEM
     C                   MOVE      XXPYUM        EOPYUM
     C***  END ADD ***** 07/26/04 *****************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     XXITER        EOITER
     C                   Z-ADD     XXUNTR        EOUNTR
     C                   Z-ADD     XXMDTR        EOMDTR
     C***  End   Add  ***  12/01/03  **************************************
     C***  Begin Add  ***  01/26/06  **************************************
     C                   Z-ADD     XXYDWD        EOYDWD
     C                   Z-ADD     XXYDED        EOYDED
     C                   Z-ADD     XXMDWD        EOMDWD
     C                   Z-ADD     XXMDED        EOMDED
     C                   Z-ADD     XXPMUD        EOPMUD
     C                   Z-ADD     XXPYWD        EOPYWD
     C                   Z-ADD     XXPYED        EOPYED
     C                   Z-ADD     XXPYUD        EOPYUD
     C                   Z-ADD     XXIDTR        EOIDTR
     C                   Z-ADD     XXUDTR        EOUDTR
     C***  End   Add  ***  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     XXIVLC        EOIVLC
     C                   Z-ADD     XXUNVL        EOUVLC
     C                   Z-ADD     XXMDVL        EOMVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   MOVE      XXMWEX        EOMWEX
     C                   MOVE      XXINEX        EOINEX
     C                   MOVE      XXUNEX        EOUNEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   MOVE      XXYDWT        EOYDWT
     C                   MOVE      XXYDET        EOYDET
     C                   MOVE      XXMDET        EOMDET
     C                   MOVE      XXPMUT        EOPMUT
     C                   MOVE      XXPYTR        EOPYWT
     C                   MOVE      XXPYET        EOPYET
     C                   MOVE      XXPYUT        EOPYUT
     C***  End   Add  ***  01/28/05  **************************************
     C*** BEG ADD ***** 05/12/09 ******************************************
     C                   MOVE      SVCAR#        EOCAR#
     C*** END ADD ***** 05/12/09 ******************************************
     C     EOEMP#        IFNE      XXEMPH
     C     EODIV         ORNE      XXDIVH
     C     EOFYR         ORNE      XXFYRH
      *
     C     RSKY          CHAIN     PRS01                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       RSJRST
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C     CPKY          CHAIN     WMCPL                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CPDESC
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT
     C*** BEGIN ADD ***  08/21/01  ************************************
     C     ELANDT        ADD       10000         XXANDT            7 0
     C*
     C     ELANDT        IFLT      ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C                   Z-ADD     ELANDT        XXEFDT
     C                   ENDIF
     C*** END ADD *****  08/21/01  ************************************
      *
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     EL1APR        MULT      RR1STP        XXAPR
     C     XXAPR         DIV(H)    100           EUAS1
     C     EL2APR        MULT      RR2NDP        XXAPR
     C     XXAPR         DIV(H)    100           EUAS2
     C     EL3APR        MULT      RR3RDP        XXAPR
     C     XXAPR         DIV(H)    100           EUAS3
      *** BEG ADD ***** 12/09/03 ************************************
     C     EL4APR        MULT      RR4THP        XXAPR
     C     XXAPR         DIV(H)    100           EUAS4
     C     EL5APR        MULT      RR5THP        XXAPR
     C     XXAPR         DIV(H)    100           EUAS5
      *** END ADD ***** 12/09/03 ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C     EL6APR        MULT      RR6THP        XXAPR
     C     XXAPR         DIV(H)    100           EUAS6
     C     EL7APR        MULT      RR7THP        XXAPR
     C     XXAPR         DIV(H)    100           EUAS7
     C***  End   Add  ***  12/11/08  **************************************
      *
     C                   ELSE
      *
     C                   Z-ADD     0             EUAS1
     C                   Z-ADD     0             EUAS2
     C                   Z-ADD     0             EUAS3
      *** BEG ADD ***** 12/09/03 ************************************
     C                   Z-ADD     0             EUAS4
     C                   Z-ADD     0             EUAS5
      *** END ADD ***** 12/09/03 ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C                   Z-ADD     0             EUAS6
     C                   Z-ADD     0             EUAS7
     C***  End   Add  ***  12/11/08  **************************************
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   Z-ADD     ELTXDU        EUTAX
     C*** BEGIN ADD ***  04/05/02  ************************************
     C                   Z-ADD     EL1AAM        EUAS1
     C                   Z-ADD     EL2AAM        EUAS2
     C                   Z-ADD     EL3AAM        EUAS3
     C*** END ADD *****  04/05/02  ************************************
     C*** BEGIN ADD ***  12/09/03  ************************************
     C                   Z-ADD     EL4AAM        EUAS4
     C                   Z-ADD     EL5AAM        EUAS5
     C*** END   ADD ***  12/09/03  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C                   Z-ADD     EL6AAM        EUAS6
     C                   Z-ADD     EL7AAM        EUAS7
     C***  End   Add  ***  12/11/08  **************************************
     C                   MOVE      EOEMP#        XXEMPH
     C                   MOVE      EODIV         XXDIVH
     C                   MOVE      EOFYR         XXFYRH
      *
     C                   ELSE
      *
     C                   Z-ADD     0             EUAS1
     C                   Z-ADD     0             EUAS2
     C                   Z-ADD     0             EUAS3
     C*** BEGIN ADD ***  12/09/03  ************************************
     C                   Z-ADD     0             EUAS4
     C                   Z-ADD     0             EUAS5
     C*** END   ADD ***  12/09/03  ************************************
     C***  Begin Add  ***  12/11/08  **************************************
     C                   Z-ADD     0             EUAS6
     C                   Z-ADD     0             EUAS7
     C***  End   Add  ***  12/11/08  **************************************
     C                   Z-ADD     0             EUTAX
      *
     C                   ENDIF                                                  EOEMP# IFNE XXEMPH
      *
     C                   MOVE      RSJRST        EUJRST
     C                   MOVE      EMCHAP        EUCHAP
     C                   MOVEL     CPDESC        EUCPDS
     C                   MOVEL     FNNAME        EUGNAM
     C                   MOVEL     CONAME        EUCNAM
     C                   MOVE      EUFAF2        EUTYPE
     C                   MOVE      XXSORT        EUCODE
     C                   MOVE      ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EUPTDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ZZEFDT
     C                   Z-ADD     ELEMP#        ZZEMP#
     C                   Z-ADD     ELDIV         ZZDIV
     C                   Z-ADD     ELFYR         ZZFYR
     C*
     C*    Clear fields before loading
     C                   Z-ADD     0             EUAGT#
     C                   Z-ADD     0             EUSAGT
     C                   Z-ADD     0             EUPROD
     C                   Z-ADD     0             EUSPRD
     C                   Z-ADD     0             EUAGFE
     C                   Z-ADD     0             EUSFEI
     C                   MOVE      *BLANKS       EUAGNM
     C                   MOVE      *BLANKS       EUSANM
     C                   MOVE      *BLANKS       EUPRNM
     C                   MOVE      *BLANKS       EUSPNM
     C                   Z-ADD     0             EURATE
     C                   Z-ADD     0             EUSRAT
     C*
     C     AHKY1         SETGT     PAH01
     C     AHKY          READPE    PAH01                                  60
     C     *IN60         DOWEQ     *OFF
     C     AHSTAT        ANDEQ     'I'
     C     AHKY          READPE    PAH01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*    Fill fields for primary agent
     C                   Z-ADD     AHAGT#        EUAGT#
     C                   Z-ADD     AHPROD        EUPROD
     C                   Z-ADD     AHAGT#        ZZAGT#
     C                   Z-ADD     AHPROD        ZZPROD
     C                   Z-ADD     AHRT#         ZZRT
     C                   EXSR      GETAGT
     C                   Z-ADD     ZZFEIN        EUAGFE
     C                   MOVEL     ZZNAME        EUAGNM
     C*** BEG ADD *****  02/12/09  ************************************
     C                   MOVEL     ZZMKRG        EUMKRG
     C*** END ADD *****  02/12/09  ************************************
     C                   MOVEL     ZZPNAM        EUPRNM
     C                   Z-ADD     ZZRATE        EURATE
     C*
     C*    Fill fields for secondary agent
     C                   Z-ADD     AHSAGT        EUSAGT
     C                   Z-ADD     AHSPRD        EUSPRD
     C                   Z-ADD     AHSAGT        ZZAGT#
     C                   Z-ADD     AHSPRD        ZZPROD
     C                   Z-ADD     AHSRT#        ZZRT
     C                   EXSR      GETAGT
     C                   Z-ADD     ZZFEIN        EUSFEI
     C                   MOVEL     ZZNAME        EUSANM
     C                   MOVEL     ZZPNAM        EUSPNM
     C                   Z-ADD     ZZRATE        EUSRAT
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C                   MOVE      ELRTCD        EURTCD
     C*
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVE      EMRGN#        EURGN#
     C                   MOVE      *BLANKS       EURGDS
     C     REKY          CHAIN     PRE01                              60
     C  N60              MOVEL     REDESC        EURGDS
     C*
     C*** BEG ADD *****  11/23/04  ************************************
     C                   ADD       1             XXSEQ#
     C                   Z-ADD     XXSEQ#        EUSEQ#
     C*** END ADD *****  11/23/04  ************************************

     C*** beg add *****  06/21/16  ************************************
      /free
       WrtRptCriteria();
      /end-free
     C*** end add *****  06/21/16  ************************************

     C*** BEGIN CHG ***  10/17/01  ************************************
     C*********            WRITEPEUOUT
     C   92              WRITE     PEUOUT
     C*** END CHG *****  10/17/01  ************************************
     C*
     C                   CLEAR                   XXINPR
     C                   CLEAR                   XXYDWR
     C                   CLEAR                   XXYDEA
     C                   CLEAR                   XXUNEA
     C                   CLEAR                   XXMDWR
     C                   CLEAR                   XXMDEA
     C                   CLEAR                   XXPMUN
     C                   CLEAR                   XXPYUN
     C                   CLEAR                   XXPYWR
     C                   CLEAR                   XXPYEA
      *** BEG ADD ***** 07/26/04 ******************************************
     C                   CLEAR                   XXIMPR
     C                   CLEAR                   XXUNMN
     C                   CLEAR                   XXYDWM
     C                   CLEAR                   XXYDEM
     C                   CLEAR                   XXMDWM
     C                   CLEAR                   XXMDEM
     C                   CLEAR                   XXPMUM
     C                   CLEAR                   XXPYWM
     C                   CLEAR                   XXPYEM
     C                   CLEAR                   XXPYUM
      *** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   CLEAR                   XXITER
     C                   CLEAR                   XXUNTR
     C                   CLEAR                   XXMDTR
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   CLEAR                   XXIVLC
     C                   CLEAR                   XXUNVL
     C                   CLEAR                   XXMDVL
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   CLEAR                   XXINEX
     C                   CLEAR                   XXUNEX
     C                   CLEAR                   XXMWEX
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  01/28/05  **************************************
     C                   CLEAR                   XXMDET
     C                   CLEAR                   XXMDWT
     C                   CLEAR                   XXYDWT
     C                   CLEAR                   XXPMUT
     C                   CLEAR                   XXPYUT
     C                   CLEAR                   XXPYTR
     C                   CLEAR                   XXYDET
     C                   CLEAR                   XXPYET
     C***  End   Add  ***  01/28/05  **************************************
      *** BEG ADD *******  01/26/06  **************************************
     C                   CLEAR                   XXYDWD
     C                   CLEAR                   XXYDED
     C                   CLEAR                   XXMDWD
     C                   CLEAR                   XXMDED
     C                   CLEAR                   XXPMUD
     C                   CLEAR                   XXPYWD
     C                   CLEAR                   XXPYED
     C                   CLEAR                   XXPYUD
     C                   CLEAR                   XXIDTR
     C                   CLEAR                   XXUDTR
      *** END ADD *******  01/26/06  **************************************
     C*
     C                   ENDSR
      /EJECT
     C**************************************************************************
     C*  CREATE REPORT ONLY REINSTATEMENT TO ALLOW FOR REVISION PROCESSING
     C**************************************************************************
     C     CRTRXC        BEGSR
     C*
     C*  SAVE THE REVISION RECORD DATA
     C*
     C                   MOVE      EUSORT        @@SORT
     C                   MOVE      EUARCD        @@ARCD
     C                   MOVE      EUAMT         @@AMT
     C                   MOVE      EUTRDT        @@TRDT
     C                   MOVE      EUPODT        @@PODT
     C                   MOVE      EUFAF         @@FAF
     C                   MOVE      EUIAPR        @@IAPR
     C                   MOVE      EUWAPR        @@WAPR
     C                   MOVE      EUWNMP        @@WNMP
     C                   MOVE      EUJAPR        @@JAPR
     C                   MOVE      EUXAPR        @@XAPR
     C                   MOVE      EUXNMP        @@XNMP
     C                   MOVE      EURWDT        @@RWDT
     C                   MOVE      EURWFL        @@RWFL
     C                   MOVE      EUFAF2        @@FAF2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      EUIMPR        @@IMPR
     C                   MOVE      EUWMPR        @@WMPR
     C                   MOVE      EUJMPR        @@JMPR
     C                   MOVE      EUXMPR        @@XMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     EUITER        @@ITER
     C                   Z-ADD     EUWTER        @@WTER
     C                   Z-ADD     EUJTER        @@JTER
     C                   Z-ADD     EUXTER        @@XTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     EUIDTR        @@IDTR
     C                   Z-ADD     EUWDTR        @@WDTR
     C                   Z-ADD     EUJDTR        @@JDTR
     C                   Z-ADD     EUXDTR        @@XDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     EUIVLC        @@IVLC
     C                   Z-ADD     EUWVLC        @@WVLC
     C                   Z-ADD     EUJVLC        @@JVLC
     C                   Z-ADD     EUXVLC        @@XVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   MOVE      XXFEXC        @@FEXC
     C                   MOVE      XXINXC        @@INXC
     C                   MOVE      EUPRTD        @@PRTD
     C***  End   Add  ***  01/08/04  **************************************
     C*
     C*  FABRICATE DATA TO ALLOW FOR REPORT ONLY REINSTATEMENT
     C*
     C                   MOVE      '7'           EUSORT
     C                   MOVE      *BLANK        EUARCD
     C                   MOVE      *ZEROS        EUAMT
     C                   MOVE      @@TRDT        EUTRDT
     C     *IN43         IFEQ      *ON
     C     @@PODT        ANDGT     @@TRDT
     C                   Z-ADD     @@PODT        EUTRDT
     C                   ENDIF                                                  *IN43 IFEQ *ON
     C*
     C*  RETRIEVE LAST CANCELLATION DATE
     C*
     C     Q             SUB       1             Z
     C*
     C     Z             DOWGE     1
     C     XACT(Z)       ANDNE     'CX'
     C     XACT(Z)       ANDNE     'XC'
     C*
     C*
     C                   SUB       1             Z
     C                   ENDDO
      *
     C     Z             IFGT      0
      *
     C     XACT(Z)       IFEQ      'CX'
     C     XACT(Z)       OREQ      'XC'
     C                   MOVE      FRDT(Z)       EUPODT
     C                   ENDIF
      *
     C                   ENDIF                                                  Z IFGT 0
     C*
     C                   MOVE      *BLANK        EUFAF
     C                   MOVE      *ZERO         EUIAPR
     C                   MOVE      *ZERO         EUWAPR
     C                   MOVE      *ZERO         EUWNMP
     C                   MOVE      *ZERO         EUJAPR
     C                   MOVE      *ZERO         EUXAPR
     C                   MOVE      *ZERO         EUXNMP
     C                   MOVE      *ZERO         EURWDT
     C                   MOVE      *BLANK        EURWFL
     C                   MOVE      *BLANK        EUFAF2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      *ZERO         EUIMPR
     C                   MOVE      *ZERO         EUWMPR
     C                   MOVE      *ZERO         EUJMPR
     C                   MOVE      *ZERO         EUXMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   Z-ADD     *ZERO         EUITER
     C                   Z-ADD     *ZERO         EUWTER
     C                   Z-ADD     *ZERO         EUJTER
     C                   Z-ADD     *ZERO         EUXTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   Z-ADD     *ZERO         EUIDTR
     C                   Z-ADD     *ZERO         EUWDTR
     C                   Z-ADD     *ZERO         EUJDTR
     C                   Z-ADD     *ZERO         EUXDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   Z-ADD     *ZERO         EUIVLC
     C                   Z-ADD     *ZERO         EUWVLC
     C                   Z-ADD     *ZERO         EUJVLC
     C                   Z-ADD     *ZERO         EUXVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXFEXC
     C                   Z-ADD     *ZERO         XXINXC
     C                   Z-ADD     *ZERO         EUPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C*
     C*  PROCESS THE FABRICATED REINSTATEMENT
     C*
     C                   EXSR      SRREIN
     C*
     C*  RESTORE THE REVISION DATA TO ALLOW FOR NORMAL TRANS PROCESSING
     C*
     C                   MOVE      @@SORT        EUSORT
     C                   MOVE      @@ARCD        EUARCD
     C                   MOVE      @@AMT         EUAMT
     C                   MOVE      @@TRDT        EUTRDT
     C                   MOVE      @@PODT        EUPODT
     C                   MOVE      @@FAF         EUFAF
     C                   MOVE      @@IAPR        EUIAPR
     C                   MOVE      @@WAPR        EUWAPR
     C                   MOVE      @@WNMP        EUWNMP
     C                   MOVE      @@JAPR        EUJAPR
     C                   MOVE      @@XAPR        EUXAPR
     C                   MOVE      @@XNMP        EUXNMP
     C                   MOVE      @@RWDT        EURWDT
     C                   MOVE      @@RWFL        EURWFL
     C                   MOVE      @@FAF2        EUFAF2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      @@IMPR        EUIMPR
     C                   MOVE      @@WMPR        EUWMPR
     C                   MOVE      @@JMPR        EUJMPR
     C                   MOVE      @@XMPR        EUXMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      @@ITER        EUITER
     C                   MOVE      @@WTER        EUWTER
     C                   MOVE      @@JTER        EUJTER
     C                   MOVE      @@XTER        EUXTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      @@IDTR        EUIDTR
     C                   MOVE      @@WDTR        EUWDTR
     C                   MOVE      @@JDTR        EUJDTR
     C                   MOVE      @@XDTR        EUXDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD **** 09/01/04 *******************************************
     C                   MOVE      @@IVLC        EUIVLC
     C                   MOVE      @@WVLC        EUWVLC
     C                   MOVE      @@JVLC        EUJVLC
     C                   MOVE      @@XVLC        EUXVLC
     C*** END ADD **** 09/01/04 *******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     @@FEXC        XXFEXC
     C                   Z-ADD     @@INXC        XXINXC
     C                   Z-ADD     @@PRTD        EUPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  03/10/04  **************************************
     C***  BEG DEL ***** 07/21/04 *****************************************
     C*****************    Z-ADDEUPRTD    SVPRTD
     C***  END DEL ***** 07/21/04 *****************************************
     C***  End   Add  ***  03/10/04  **************************************
     C*
     C                   ENDSR
      /EJECT
     C**************************************************************************
     C*  CREATE REPORT ONLY CANCELLATION TO ALLOW FOR REVISION PROCESSING
     C**************************************************************************
     C     CRTCXC        BEGSR
     C*
     C*  SAVE THE REVISION RECORD DATA
     C*
     C                   MOVE      EUSORT        @@SORT
     C                   MOVE      EUARCD        @@ARCD
     C                   MOVE      EUAMT         @@AMT
     C                   MOVE      EUTRDT        @@TRDT
     C                   MOVE      EUPODT        @@PODT
     C                   MOVE      EUFAF         @@FAF
     C                   MOVE      EUIAPR        @@IAPR
     C                   MOVE      EUWAPR        @@WAPR
     C                   MOVE      EUWNMP        @@WNMP
     C                   MOVE      EUJAPR        @@JAPR
     C                   MOVE      EUXAPR        @@XAPR
     C                   MOVE      EUXNMP        @@XNMP
     C                   MOVE      EURWDT        @@RWDT
     C                   MOVE      EURWFL        @@RWFL
     C                   MOVE      EUFAF2        @@FAF2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      EUIMPR        @@IMPR
     C                   MOVE      EUWMPR        @@WMPR
     C                   MOVE      EUJMPR        @@JMPR
     C                   MOVE      EUXMPR        @@XMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      EUITER        @@ITER
     C                   MOVE      EUWTER        @@WTER
     C                   MOVE      EUJTER        @@JTER
     C                   MOVE      EUXTER        @@XTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      EUIDTR        @@IDTR
     C                   MOVE      EUWDTR        @@WDTR
     C                   MOVE      EUJDTR        @@JDTR
     C                   MOVE      EUXDTR        @@XDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MOVE      EUIVLC        @@IVLC
     C                   MOVE      EUWVLC        @@WVLC
     C                   MOVE      EUJVLC        @@JVLC
     C                   MOVE      EUXVLC        @@XVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     XXFEXC        @@FEXC
     C                   Z-ADD     XXINXC        @@INXC
     C                   Z-ADD     EUPRTD        @@PRTD
     C***  End   Add  ***  01/08/04  **************************************
     C*
     C*  FABRICATE DATA TO ALLOW FOR REPORT ONLY RE-CANCELLATION
     C*
     C                   MOVE      '6'           EUSORT
     C                   MOVE      *BLANK        EUARCD
     C                   MOVE      *ZEROS        EUAMT
     C                   MOVE      @@TRDT        EUTRDT
     C*
     C*  RETRIEVE LAST CANCELLATION DATE
     C*
     C     Q             SUB       1             Z
     C*
     C     Z             DOWGE     1
     C     XACT(Z)       ANDNE     'RE'
     C     XACT(Z)       ANDNE     'XR'
     C*
     C*
     C                   SUB       1             Z
     C                   ENDDO
      *
     C     Z             IFGT      0
      *
     C     XACT(Z)       IFEQ      'RE'
     C     XACT(Z)       OREQ      'XR'
     C                   MOVE      FRDT(Z)       EUPODT
     C                   ENDIF
      *
     C                   ENDIF                                                  Z IFGT 0
     C*
     C                   MOVE      *BLANK        EUFAF
     C                   MOVE      *ZERO         EUIAPR
     C                   MOVE      *ZERO         EUWAPR
     C                   MOVE      *ZERO         EUWNMP
     C                   MOVE      *ZERO         EUJAPR
     C                   MOVE      *ZERO         EUXAPR
     C                   MOVE      *ZERO         EUXNMP
     C                   MOVE      *ZERO         EURWDT
     C                   MOVE      *BLANK        EURWFL
     C                   MOVE      *BLANK        EUFAF2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      *ZERO         EUIMPR
     C                   MOVE      *ZERO         EUWMPR
     C                   MOVE      *ZERO         EUJMPR
     C                   MOVE      *ZERO         EUXMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      *ZERO         EUITER
     C                   MOVE      *ZERO         EUWTER
     C                   MOVE      *ZERO         EUJTER
     C                   MOVE      *ZERO         EUXTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      *ZERO         EUIDTR
     C                   MOVE      *ZERO         EUWDTR
     C                   MOVE      *ZERO         EUJDTR
     C                   MOVE      *ZERO         EUXDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD ***** 09/01/04 ******************************************
     C                   MOVE      *ZERO         EUIVLC
     C                   MOVE      *ZERO         EUWVLC
     C                   MOVE      *ZERO         EUJVLC
     C                   MOVE      *ZERO         EUXVLC
     C*** END ADD ***** 09/01/04 ******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     *ZERO         XXFEXC
     C                   Z-ADD     *ZERO         XXINXC
     C                   Z-ADD     *ZERO         EUPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C*
     C*  PROCESS THE FABRICATED CANCELLATION
     C*
     C                   EXSR      SRCANX
     C*
     C*  RESTORE THE REVISION DATA TO ALLOW FOR NORMAL TRANS PROCESSING
     C*
     C                   MOVE      @@SORT        EUSORT
     C                   MOVE      @@ARCD        EUARCD
     C                   MOVE      @@AMT         EUAMT
     C                   MOVE      @@TRDT        EUTRDT
     C                   MOVE      @@PODT        EUPODT
     C                   MOVE      @@FAF         EUFAF
     C                   MOVE      @@IAPR        EUIAPR
     C                   MOVE      @@WAPR        EUWAPR
     C                   MOVE      @@WNMP        EUWNMP
     C                   MOVE      @@JAPR        EUJAPR
     C                   MOVE      @@XAPR        EUXAPR
     C                   MOVE      @@XNMP        EUXNMP
     C                   MOVE      @@RWDT        EURWDT
     C                   MOVE      @@RWFL        EURWFL
     C                   MOVE      @@FAF2        EUFAF2
     C*** BEG ADD ***** 07/26/04 ******************************************
     C                   MOVE      @@IMPR        EUIMPR
     C                   MOVE      @@WMPR        EUWMPR
     C                   MOVE      @@JMPR        EUJMPR
     C                   MOVE      @@XMPR        EUXMPR
     C*** END ADD ***** 07/26/04 ******************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      @@ITER        EUITER
     C                   MOVE      @@WTER        EUWTER
     C                   MOVE      @@JTER        EUJTER
     C                   MOVE      @@XTER        EUXTER
     C***  End   Add  ***  12/01/03  **************************************
     C***  BEG ADD ******  01/26/06  **************************************
     C                   MOVE      @@IDTR        EUIDTR
     C                   MOVE      @@WDTR        EUWDTR
     C                   MOVE      @@JDTR        EUJDTR
     C                   MOVE      @@XDTR        EUXDTR
     C***  END ADD ******  01/26/06  **************************************
     C*** BEG ADD **** 09/01/04 *******************************************
     C                   MOVE      @@IVLC        EUIVLC
     C                   MOVE      @@WVLC        EUWVLC
     C                   MOVE      @@JVLC        EUJVLC
     C                   MOVE      @@XVLC        EUXVLC
     C*** END ADD **** 09/01/04 *******************************************
     C***  Begin Add  ***  01/08/04  **************************************
     C                   Z-ADD     @@FEXC        XXFEXC
     C                   Z-ADD     @@INXC        XXINXC
     C                   Z-ADD     @@PRTD        EUPRTD
     C***  End   Add  ***  01/08/04  **************************************
     C***  Begin Add  ***  03/10/04  **************************************
     C***  BEG DEL ***** 07/21/04 *****************************************
     C*****************    Z-ADDEUPRTD    SVPRTD
     C***  END DEL ***** 07/21/04 *****************************************
     C***  End   Add  ***  03/10/04  **************************************
     C*
     C*
     C                   ENDSR
      /EJECT
     C*================================================================
     C*  GETAGT - Subroutine to get agent info
     C*================================================================
     C     GETAGT        BEGSR
     C*
     C                   MOVE      *ZEROS        ZZFEIN
     C                   MOVE      *ZEROS        ZZMOD
     C                   MOVE      *BLANKS       ZZNAME
     C                   MOVE      *BLANKS       ZZPNAM
     C                   Z-ADD     *ZERO         ZZRATE
     C*** BEG ADD ***** 02/12/09 ***********************************************
     C                   MOVE      *BLANKS       ZZMKRG
     C*** END ADD ***** 02/12/09 ***********************************************
     C*
     C*  Chain to agent assignment file
     C     AAKY          CHAIN     PAA01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     AAFEIN        ZZFEIN
     C                   Z-ADD     AAMOD         ZZMOD
     C*
     C*    Get agent name
     C     AGKY          CHAIN     PAG01                              62
     C     *IN62         IFEQ      *OFF
     C                   MOVEL     AGNAME        ZZNAME
     C*** BEG ADD ***** 02/12/09 ***********************************************
     C     ZZAGT#        IFNE      0
     C     ZZAGT#        ANDEQ     AHAGT#
     C                   MOVEL     AGMKRG        ZZMKRG
     C                   ENDIF                                                   PRIMARY AGENT
     C*** END ADD ***** 02/12/09 ***********************************************
     C                   ENDIF                                                  *IN62=*OFF
     C*
     C*    Get producer name
     C     APKY          CHAIN     PAP01                              62
     C     *IN62         IFEQ      *OFF
     C     APLSN         CAT       APFSN:1       ZZPNAM
     C                   ENDIF                                                  *IN62=*OFF
     C*
     C                   ENDIF                                                  *IN61=*OFF
     C*
     C*** BEG DEL ***** 09/20/06 ***********************************************
     C***Check*if employer has prior year policy (then reset to curr year)
     C**********           Z-ADDEUEMP#    ZZEMP#
     C**********           Z-ADDEUDIV     ZZDIV
     C********** SVFYR     SUB  1         ZZFYRP
     C********** ELKYP     CHAINPEL01                65
     C********** ELKY      CHAINPEL01                60
      *****BEGIN ADD **** 05/29/01 *********************************************
     C********** *IN65     IFEQ *OFF
     C**********           MOVE 'R'       EUNWRW
     C**********           ELSE
     C****BEGIN*CHG ***  02/27/06  ************************************
     C*************        MOVE 'N'       EUNWRW
     C********** EXKY      CHAINPEX01                65
     C********** *IN65     IFEQ *OFF
     C**********           MOVE 'R'       EUNWRW
     C**********           ELSE
     C**********           MOVE 'N'       EUNWRW
     C**********           ENDIF                           *IN65 IFEQ *OFF
     C****END*CHG *****  02/27/06  ************************************
     C**********           ENDIF                           *IN65 IFEQ *OFF
      **** END   ADD **** 05/29/01 *********************************************
     C*** END DEL ***** 09/20/06 ***********************************************
     C*
     C*    Get agent rate (new or renew)
     C     ARKY1         SETGT     PAR01
     C     ARKY          READPE    PAR01                                  61
     C*
     C     *IN61         DOWEQ     *OFF
     C     ARSTAT        ANDEQ     'I'
     C     ARKY          READPE    PAR01                                  61
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     *IN61         IFEQ      *OFF
      *** BEG ADD ***** 09/03/10 **********************************
     C     EUNWRW        IFEQ      'N'
     C                   Z-ADD     ARNWRT        ZZRATE
     C                   ELSE
     C     EUNWRW        IFEQ      'R'
     C                   Z-ADD     ARRNRT        ZZRATE
     C                   ENDIF                                                  EUNWRW IFEQ 'R'
     C                   ENDIF                                                  EUNWRW IFEQ 'N'
     C***65****            Z-ADDARNWRT    ZZRATE
     C**N65****            Z-ADDARRNRT    ZZRATE
      *** END ADD ***** 09/03/10 **********************************
     C                   ENDIF                                                  *IN61
     C*
     C                   ENDSR                                                  GETAGT
      *** BEG ADD ***** 07/26/04 **********************************
     C**************************************************************************
     C*  CALCULATE MANUAL PREMIUM FOR SELF REPORTING A/R TRANSACTIONS
     C**************************************************************************
     C     CALMAN        BEGSR
      *
     C                   CLEAR                   XXMANP
      *
      *** BEG DEL ***** 02/15/10 ************************
     C********** ELANDT    IFNE *ZEROS
     C********** ELANDT    ANDNEELPFDT
     C********** XXANDT    ANDGTELPFDT
      **********
     C********** XXRPDT    IFGE XXANDT
     C********** XXANDT    ANDNEELPTDT
     C**********           Z-ADDXXANDT    KKEFDT
     C**********           ELSE
     C**********           Z-ADDELANDT    KKEFDT
     C**********           END
      **********
     C**********           ELSE
     C**********           Z-ADDELPFDT    KKEFDT
     C**********           END
     C**********
     C**USE*4/1/3 FL RATES IF APPLICABLE.
     C**********
     C********** XXST      IFEQ 'FL'
     C********** KKEFDT    ANDGE1030101
     C********** KKEFDT    ANDLE1030331
     C********** ELPFDT    ANDLT1030401
     C********** KKRPMY    ANDGE10304
     C**********           Z-ADD1030401   KKEFDT
     C**********           ENDIF                           XXST IFEQ
     C**********
     C**USE*10/1/3 FL RATES IF APPLICABLE.
     C**********
     C********** XXST      IFEQ 'FL'
     C********** XXEFDT    ANDLT1031001
     C********** ELPFDT    ANDLT1031001
     C********** KKRPMY    ANDGE10310
     C**********           Z-ADD1031001   KKEFDT
     C**********           ENDIF                           XXST IFEQ
      *** END DEL ***** 02/15/10 ************************
     C*** BEG ADD *****  04/01/08  *******************************************
     C                   Z-ADD     0             KKEFD1
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   SVEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   SVCLAS        ##CLAS            5
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
      *** beg add ***** 12/15/14 *********************************************
     C                   MOVE      ##DATE        ##EFDT
      *** end add ***** 12/15/14 *********************************************
     C*** END ADD *****  04/01/08  *******************************************
      *
     C     PRKY          SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      *OFF
      *
     C     ELSTAT        IFEQ      'Y'
     C     PRARFL        ANDEQ     'Y'
      *
     C     EPKY1         SETGT     PEP09
     C     EPKY          READPE    PEP09                                  60
     C     *IN60         IFEQ      *OFF
      **** Beg Chg ***** 06/17/24 ******************************************
     C****************** Z-ADD     EPARAT        XXARAT            5 2
       xxArAt = EParat;
      **** End Chg ***** 06/17/24 ******************************************
     C                   ELSE
      **** Beg Chg ***** 06/17/24 ******************************************
     C****************** Z-ADD     *ZERO         XXARAT            5 2
       Clear xxArAt;
      **** End Chg ***** 06/17/24 ******************************************
     C                   ENDIF
      *
     C                   ELSE
     C                   Z-ADD     PRRT          XXARAT
      *** BEG ADD ***** 02/15/10 ************************
     C                   MOVEL     ELRTCD        ##RTCD
     C                   CALL      'SRLCM'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##RTCD            3
     C                   PARM                    ##CLAS            5
     C                   PARM                    ##EFDT            7
     C                   PARM      *BLANKS       ##LCM             5
      *
     C     ##LCM         IFNE      *BLANKS
     C                   MOVE      ##LCM         XXLCM             5 4
     C                   ENDIF
      *
     C     XXARAT        MULT(H)   XXLCM         XXARAT
      *** END ADD ***** 02/15/10 ************************
     C                   ENDIF
      *
     C                   SELECT
      *
     C     PRPCF         WHENEQ    'P'
     C     XXCPTT        MULT(H)   XXARAT        XXMANP           13 2
     C     XXMANP        DIV(H)    100           XXMANP
     C     XXMPTT        ADD       XXMANP        XXMPTT           15 2
     C*
     C***  Begin Add  ***  01/28/05  **************************************
     C                   MOVE      RSJRST        XXST
     C     ELANDT        ADD       10000         XXANDT            7 0
     C*
     C                   SELECT
     C     XXST          WHENEQ    'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C     ELANDT        WHENLT    ELPFDT
     C     XXANDT        ANDGT     ELPFDT
     C                   Z-ADD     ELANDT        XXEFDT
     C                   OTHER
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   ENDSL
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     RRTERR        DIV(H)    100           XTHOLD            5 5
     C     SRAMTP        MULT(H)   XTHOLD        XXMANT           13 2
     C                   ADD       XXMANT        XXWATR
     C                   ENDIF
     C***  End   Add  ***  01/28/05  **************************************
      *
     C     PRPCF         WHENEQ    'C'
     C*** BEG ADD ****** 09/07/09 *************************************
      *** BEG DEL ***** 06/21/10 ***********************************************
     C***********WCFL42    IFEQ 'Y'
     C***********XXCPTT    DIV  10        XXCPTT
     C***********          ENDIF
      *** BEG DEL ***** 06/21/10 ***********************************************
     C*** END ADD ****** 09/07/09 *************************************
     C     XXCPTT        MULT(H)   XXARAT        XXMANP           13 2
     C     XXMPTT        ADD       XXMANP        XXMPTT
      *
     C     PRPCF         WHENEQ    'O'
      *
     C     EPKY1         SETGT     PEP09
     C     EPKY          READPE    PEP09                                  60
     C                   CLEAR                   TTEMPR
     C     EPKY2         CHAIN     PEP0Q                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       EPEMPR        TTEMPR           15 2
     C     EPKY2         READE     PEP0Q                                  60
     C                   ENDDO
      *
     C     TTEMPR        DIV(H)    12            XXMANP
     C     XXMPTT        ADD       XXMANP        XXMPTT
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
     C                   ENDSR
      *** END ADD ***** 07/26/04 **********************************
      *** BEG ADD ***** 12/02/09 **********************************
      *============================================================
      * GETFYR - SET UP FOR FISCAL YEAR DATES
      *          IF FISCAL YEAR START MONTH IS PROVIDED, FIND
      *          FIRST DAY OF CURRENT FISCAL YEAR AND PRIOR
      *          FISCAL YEAR END DATE
      *============================================================
     C     GETFYR        BEGSR
     C                   Z-ADD     COFYSM        XXFYSM            2 0
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C     F2FYSM        ANDNE     *ZERO
     C                   Z-ADD     F2FYSM        XXFYSM
     C                   ENDIF
     C     XXFYSM        IFNE      *ZERO
     C                   MOVEL     XXFYSM        XAFYSM            4
     C                   MOVE      '01'          XAFYSM
     C                   MOVE      XAFYSM        XXJAN1
     C     XXFYSM        IFGT      XXVMTH
     C                   SUB       10000         XXJAN1
     C                   ENDIF
     C                   Z-ADD     XXJAN1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFRM8
     C                   MOVEL     SRDTO         XXFRDT
     C                   CALL      'SRPAST8'
     C                   PARM                    XQFRM8            8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM      *BLANK        QQERR             1
     C                   Z-ADD     XQFRM8        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYEND            7 0
     C                   ENDIF
     C                   ENDSR
      *** END ADD ***** 12/02/09 **********************************
     C**************************************************************
      /EJECT
     O*
     OQSYSPRT   E            ROUTE          1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE          1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE          1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE          1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE          1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE          1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE          3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   X1USNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTTL               80
     O          E            ROUTE          2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE          2
     O                                           19 'OUTPUT QUEUE:'
     O               08      OROUTQ              30
     O                  09   QQQUE               30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            ROUTE          1
     O                                           20 'DATE RANGE   :'
     O                       XXFRDT              32 '  /  /    '
     O                                           34 '-'
     O                       XXTODT              45 '  /  /    '
     O          E            ROUTE          1
     O                                           20 'REPORT TYPE  :'
     O                       QQTYPE              23
      *** BEG CHG ***** 12/02/09 *************************************
     O******* E  1     77      ROUTE
     O          E            ROUTE          1
      *** END CHG ***** 12/02/09 *************************************
     O                                           20 'VALUED AS OF :'
     O                       XXVLDT              32 '  /  /    '
     O          E            ROUTE          1
     O                                           20 'DET/SUMM/BOTH:'
     O                       QQPRNT              23
      **** BEGIN ADD **** 02/28/01 ************************************
     O          E            ROUTE          1
     O                                           28 'EXCLUDE EXPENSE CONST:'
     O                       QQPRTD              31
      **** BEGIN ADD **** 10/22/02 ************************************
     O          E            ROUTE          1
     O                                           28 'SORT BY STATE . . . .:'
     O                       QQSRST              31
     O          E            ROUTE          1
     O                                           28 'TYPE OF OUTPUT  . . .:'
     O                       QQPFB               31
      **** END   ADD **** 10/22/02 ************************************
      **** END   ADD **** 02/28/01 ************************************
      *** BEG CHG ***** 12/02/09 **************************************
     O********E** 60           ROUTE
     O***********                        23 '(C) COPYRIGHT PROGRESSI'
     O***********                      +000 'VE DATA SOLUTIONS, INC.,'
     O***********              *YEAR   +001
     O          E            ROUTE            59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE       1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
      *** END CHG ***** 12/02/09 **************************************
     O          E            HEAD           2 04
     O                       DSTIME              08 '  :  :  '
     O                       SCCONM             118
      **** BEGIN CHG **** 02/28/01 ************************************
     O************************ DSDATEY  170
     O                       DSDATE        Y    197
      **** END   CHG **** 02/28/01 ************************************
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTTL              115
      **** BEGIN CHG **** 02/28/01 ************************************
     O********                 PAGE  Z  170
     O********                          166 'PAGE:'
     O                       PAGE          Z    197
     O                                          193 'PAGE:'
      **** END   CHG **** 02/28/01 ************************************
     O          E            HEAD           1
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
      **** BEGIN CHG **** 02/28/01 ************************************
     O*************** N77               112 'CALENDAR DATE RANGE:'
     O***************  77               112 'POLICY DATE RANGE:'
     O***************          XXFRDT   123 '  /  /    '
     O***************                   125 '-'
     O***************          XXTODT   136 '  /  /    '
     O***************  77               152 'VALUED AS OF:'
     O***************  77      XXVLDT   163 '  /  /    '
     O***************
      *** BEG CHG ***** 12/02/09 **************************************
     O*************** N77                95 'CALENDAR DATE RANGE:'
     O***************  77                95 'POLICY DATE RANGE:'
     O                       XXDRNV              95
      *** END CHG ***** 12/02/09 **************************************
     O                       XXFRDT             106 '  /  /    '
     O                                          108 '-'
     O                       XXTODT             119 '  /  /    '
      *** BEG CHG ***** 12/02/09 **************************************
     O***********      77               135 'VALUED AS OF:'
     O***********      77      XXVLDT   146 '  /  /    '
     O                                          135 'VALUED AS OF:'
     O                       XXVLDT             146 '  /  /    '
      *** END CHG ***** 12/02/09 **************************************
     O                                          182 'EXCLUDE EXPENSE CONST:'
     O                       QQPRTD             185
      **** END   CHG **** 02/28/01 ************************************
      **** BEGIN ADD **** 10/22/02 ************************************
     O          E            SHEAD          1
     O                                           06 'STATE:'
     O                       STATNM              47
      **** END   ADD **** 10/22/02 ************************************
     O          E            POLHD       1  0
     O                                           19 '_____________'
     O                                           31 '__________'
     O                                           57 '________________________'
     O                                           73 '________________'
     O          E            POLHD          1
     O                                           19 'POLICY NUMBER'
     O                                           31 'EFF DATE'
     O                                           37 'NAME'
     O          E            POLHD          1
     O                       ELEMP#               9
     O                                           10 '-'
     O                       ELDIV               15
     O                       ELFYR               19
     O                                           16 '-'
     O                       XXPFDT              31 '  /  /    '
     O                       EMNAME              72
     O          E            POLHD          1
     O                                          149 'PRIOR'
     O                                          165 'PRIOR'
     O                                          181 'PRIOR'
     O                                          197 'PRIOR'
     O          E            POLHD          0
     O                                           25 '__________'
     O                                           37 '__________'
     O                                           53 '_______________'
     O                                           69 '___________'
     O                                           85 '__________'
     O                                          101 '________'
     O                                          117 '___________'
     O                                          133 '__________'
     O                                          149 '____________'
     O                                          165 '__________'
     O                                          181 '_________'
     O                                          197 '___________'
     O          E            POLHD          0
     O                                           25 'POST DT'
     O                                           37 'TRANS DT'
     O                                           53 'INFORCE PREMIUM'
     O                                           69 'YTD WRITTEN'
     O                                           85 'YTD EARNED'
     O                                          101 'UNEARNED'
     O                                          117 'MTD WRITTEN'
     O                                          133 'MTD EARNED'
     O                                          149 'MTH UNEARNED'
     O                                          165 'YR WRITTEN'
     O                                          181 'YR EARNED'
     O                                          197 'YR UNEARNED'
     O          E            DET         1  0
     O               89                           8 'REWRITE'
     O                       EUFAF2              10
     O                       XXSORT              13
     O                       XXPODT              25 '  /  /    '
     O                       XXTRDT              37 '  /  /    '
     O                       XXINPR        J     54
     O                       XXYDWR        J     70
     O                       XXYDEA        J     86
     O                       XXUNEA        J    102
     O                       XXMDWR        J    118
     O                       XXMDEA        J    134
     O                       XXPMUN        J    150
     O                       XXPYWR        J    166
     O                       XXPYEA        J    182
     O                       XXPYUN        J    198
     O          E            POLTOT         0
     O                                           53 '____________'
     O                                           69 '____________'
     O                                           85 '____________'
     O                                          101 '____________'
     O                                          117 '____________'
     O                                          133 '____________'
     O                                          149 '____________'
     O                                          165 '____________'
     O                                          181 '____________'
     O                                          197 '____________'
     O          E            POLTOT      1  2
     O                                           33 'TOTAL:'
     O                       XPINPR        J     54
     O                       XPYDWR        J     70
     O                       XPYDEA        J     86
     O                       XPUNEA        J    102
     O                       XPMDWR        J    118
     O                       XPMDEA        J    134
     O                       XPPMUN        J    150
     O                       XPPYWR        J    166
     O                       XPPYEA        J    182
     O                       XPPYUN        J    198
      **** BEGIN ADD **** 10/22/02 ************************************
     O          E            STTOT          1
     O                                           53 '____________'
     O                                           69 '____________'
     O                                           85 '____________'
     O                                          101 '____________'
     O                                          117 '____________'
     O                                          133 '____________'
     O                                          149 '____________'
     O                                          165 '____________'
     O                                          181 '____________'
     O                                          197 '____________'
     O          E            STTOT          2
     O                       STAT15              30
     O                                           37 'TOTAL:'
     O                       XSINPR        J     54
     O                       XSYDWR        J     70
     O                       XSYDEA        J     86
     O                       XSUNEA        J    102
     O                       XSMDWR        J    118
     O                       XSMDEA        J    134
     O                       XSPMUN        J    150
     O                       XSPYWR        J    166
     O                       XSPYEA        J    182
     O                       XSPYUN        J    198
      **** END   ADD **** 10/22/02 ************************************
     O*
     O          E            TOT            1
     O                                           53 '____________'
     O                                           69 '____________'
     O                                           85 '____________'
     O                                          101 '____________'
     O                                          117 '____________'
     O                                          133 '____________'
     O                                          149 '____________'
     O                                          165 '____________'
     O                                          181 '____________'
     O                                          197 '____________'
     O          E            TOT            2
     O                                           37 'GROUP TOTAL:'
     O                       XTINPR        J     54
     O                       XTYDWR        J     70
     O                       XTYDEA        J     86
     O                       XTUNEA        J    102
     O                       XTMDWR        J    118
     O                       XTMDEA        J    134
     O                       XTPMUN        J    150
     O                       XTPYWR        J    166
     O                       XTPYEA        J    182
     O                       XTPYUN        J    198
     O*
     OQSYSPRT1  E            ROUTE1         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE1         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE1         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE1         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE1         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE1         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE1         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE1         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE1         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE1         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE1         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   X1USNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       XXTTL1              80
     O          E            ROUTE1         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE1         2
     O                                           19 'OUTPUT QUEUE:'
     O               08      OROUTQ              30
     O                  09   QQQUE               30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            ROUTE1         1
     O                                           20 'DATE RANGE   :'
     O                       XXFRDT              32 '  /  /    '
     O                                           34 '-'
     O                       XXTODT              45 '  /  /    '
     O          E            ROUTE1         1
     O                                           20 'REPORT TYPE  :'
     O                       QQTYPE              23
      *** BEG CHG ***** 12/02/09 *************************************
     O******* E  1     77      ROUTE1
     O          E            ROUTE1         1
      *** END CHG ***** 12/02/09 *************************************
     O                                           20 'VALUED AS OF :'
     O                       XXVLDT              32 '  /  /    '
     O          E            ROUTE1         1
     O                                           20 'DET/SUMM/BOTH:'
     O                       QQPRNT              23
      **** BEGIN ADD **** 02/28/01 ************************************
     O          E            ROUTE1         1
     O                                           28 'EXCLUDE EXPENSE CONST:'
     O                       QQPRTD              31
      **** BEGIN ADD **** 10/22/02 ************************************
     O          E            ROUTE1         1
     O                                           28 'SORT BY STATE . . . .:'
     O                       QQSRST              31
     O          E            ROUTE1         1
     O                                           28 'TYPE OF OUTPUT  . . .:'
     O                       QQPFB               31
      **** END   ADD **** 10/22/02 ************************************
      **** END   ADD **** 02/28/01 ************************************
      *** BEG CHG ***** 12/02/09 **************************************
     O********E** 60           ROUTE1
     O***********                        23 '(C) COPYRIGHT PROGRESSI'
     O***********                      +000 'VE DATA SOLUTIONS, INC.,'
     O***********              *YEAR   +001
     O          E            ROUTE1           59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE1      1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
      *** END CHG ***** 12/02/09 **************************************
     O          E            HEAD1          2 04
     O                       DSTIME              08 '  :  :  '
     O                       SCCONM             118
      **** BEGIN CHG **** 02/28/01 ************************************
     O***********************  DSDATEY  170
     O                       DSDATE        Y    197
      **** END   CHG **** 02/28/01 ************************************
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       XXTTL1             115
      **** BEGIN CHG **** 02/28/01 ************************************
     O***********************  PAGE1 Z  170
     O***********************           166 'PAGE:'
     O                       PAGE1         Z    197
     O                                          193 'PAGE:'
      **** END   CHG **** 02/28/01 ************************************
     O          E            HEAD1          1
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
      **** BEGIN CHG **** 02/28/01 ************************************
     O*************** N77               112 'CALENDAR DATE RANGE:'
     O***************  77               112 'POLICY DATE RANGE:'
     O***************          XXFRDT   123 '  /  /    '
     O***************                   125 '-'
     O***************          XXTODT   136 '  /  /    '
     O***************  77               152 'VALUED AS OF:'
     O***************  77      XXVLDT   163 '  /  /    '
     O***************
      *** BEG CHG ***** 12/02/09 *************************************
     O*************** N77                95 'CALENDAR DATE RANGE:'
     O***************  77                95 'POLICY DATE RANGE:'
     O                       XXDRNV              95
      *** END CHG ***** 12/02/09 *************************************
     O                       XXFRDT             106 '  /  /    '
     O                                          108 '-'
     O                       XXTODT             119 '  /  /    '
      *** BEG CHG ***** 12/02/09 *************************************
     O***********      77               135 'VALUED AS OF:'
     O***********      77      XXVLDT   146 '  /  /    '
     O                                          135 'VALUED AS OF:'
     O                       XXVLDT             146 '  /  /    '
      *** END CHG ***** 12/02/09 *************************************
     O                                          182 'EXCLUDE EXPENSE CONST:'
     O                       QQPRTD             185
      **** END   CHG **** 02/28/01 ************************************
      **** BEGIN ADD **** 10/22/02 ************************************
     O          E            SHEAD1         1
     O                                           06 'STATE:'
     O                       STATNM              47
      **** END   ADD **** 10/22/02 ************************************
     O          E            POLHD1         1
     O                                           19 '___________________'
     O                                           31 '__________'
     O                                           57 '________________________'
     O                                           73 '________________'
     O          E            POLHD1         1
     O                                           19 'POLICY NUMBER'
     O                                           31 'EFF DATE'
     O                                           37 'NAME'
     O          E            POLHD1         1
     O                       ELEMP#               9
     O                                           10 '-'
     O                       ELDIV               15
     O                       ELFYR               19
     O                                           16 '-'
     O                       XXPFDT              31 '  /  /    '
     O                       EMNAME              72
     O          E            POLHD1         1
     O                                          145 'PRIOR'
     O                                          161 'PRIOR'
     O                                          177 'PRIOR'
     O                                          193 'PRIOR'
     O          E            POLHD1         0
     O                                           49 '_______________'
     O                                           65 '___________'
     O                                           81 '__________'
     O                                           97 '________'
     O                                          113 '___________'
     O                                          129 '__________'
     O                                          145 '____________'
     O                                          161 '__________'
     O                                          177 '_________'
     O                                          193 '___________'
     O          E            POLHD1         0
     O                                           49 'INFORCE PREMIUM'
     O                                           65 'YTD WRITTEN'
     O                                           81 'YTD EARNED'
     O                                           97 'UNEARNED'
     O                                          113 'MTD WRITTEN'
     O                                          129 'MTD EARNED'
     O                                          145 'MTH UNEARNED'
     O                                          161 'YR WRITTEN'
     O                                          177 'YR EARNED'
     O                                          193 'YR UNEARNED'
     O          E            POLTO1      1  2
     O                       XPINPR        J     50
     O                       XPYDWR        J     66
     O                       XPYDEA        J     82
     O                       XPUNEA        J     98
     O                       XPMDWR        J    114
     O                       XPMDEA        J    130
     O                       XPPMUN        J    146
     O                       XPPYWR        J    162
     O                       XPPYEA        J    178
     O                       XPPYUN        J    194
      **** BEGIN ADD **** 10/22/02 ************************************
     O          E            STTOT1         1
     O                                           49 '____________'
     O                                           65 '____________'
     O                                           81 '____________'
     O                                           97 '____________'
     O                                          113 '____________'
     O                                          129 '____________'
     O                                          145 '____________'
     O                                          161 '____________'
     O                                          177 '____________'
     O                                          193 '____________'
     O          E            STTOT1         2
     O                       STAT15              26
     O                                           33 'TOTAL:'
     O                       XSINPR        J     50
     O                       XSYDWR        J     66
     O                       XSYDEA        J     82
     O                       XSUNEA        J     98
     O                       XSMDWR        J    114
     O                       XSMDEA        J    130
     O                       XSPMUN        J    146
     O                       XSPYWR        J    162
     O                       XSPYEA        J    178
     O                       XSPYUN        J    194
      **** END   ADD **** 10/22/02 ************************************
     O*
     O          E            TOT1           1
     O                                           49 '____________'
     O                                           65 '____________'
     O                                           81 '____________'
     O                                           97 '____________'
     O                                          113 '____________'
     O                                          129 '____________'
     O                                          145 '____________'
     O                                          161 '____________'
     O                                          177 '____________'
     O                                          193 '____________'
     O          E            TOT1           2
     O                                           33 'GROUP TOTAL:'
     O                       XTINPR        J     50
     O                       XTYDWR        J     66
     O                       XTYDEA        J     82
     O                       XTUNEA        J     98
     O                       XTMDWR        J    114
     O                       XTMDEA        J    130
     O                       XTPMUN        J    146
     O                       XTPYWR        J    162
     O                       XTPYEA        J    178
     O                       XTPYUN        J    194
     OQSYSPRT2  E            ROUTE2         1 06
     O                                            8 'XXXXXXXX'
     O                                           20 'XXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXX     XX'
     O                                           77 'XXXXXX'
     O          E            ROUTE2         1
     O                                            9 'XXXXXXXXX'
     O                                           21 'XXXXXXXX'
     O                                           34 'XX      XX'
     O                                           45 'XXXXXXXXXX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XXXX    XX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XXXX    XX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                           10 'XX      XX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            9 'XXXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX XX   XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            8 'XXXXXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           71 'XX'
     O          E            ROUTE2         1
     O                                            4 'XXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX  XX  XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE2         1
     O                                            5 'XXXXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX    XXXX'
     O          E            ROUTE2         1
     O                                            6 'XX XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX   XX XX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                            7 'XX  XXX'
     O                                           22 'XX      XX'
     O                                           34 'XX      XX'
     O                                           41 'XX'
     O                                           52 'XX'
     O                                           67 'XX    XXXX'
     O                                           79 'XX      XX'
     O          E            ROUTE2         1
     O                                            8 'XX   XXX'
     O                                           21 'XXXXXXXX'
     O                                           33 'XXXXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX    XXXX'
     O                                           78 'XXXXXXXX'
     O          E            ROUTE2         3
     O                                            9 'XX    XXX'
     O                                           20 'XXXXXX'
     O                                           32 'XXXXXX'
     O                                           41 'XX'
     O                                           56 'XXXXXXXXXX'
     O                                           67 'XX     XXX'
     O                                           77 'XXXXXX'
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   X1USNM              67
     O          E            ROUTE2         1
     O               08      ORDIS2              60
     O          E            ROUTE2         1
     O               08      ORDIS3              60
     O          E            ROUTE2         2
     O               08      ORDIS4              60
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       XXTTL2              80
     O          E            ROUTE2         2
     O                                           19 'PROGRAM NAME:'
     O                       ORPNAM              30
     O                                           36 'FILE:'
     O                       ORFIL#              38
     O                                           46 'COPIES:'
     O                       ORCOPY        Z     50
     O                                           56 'HOLD:'
     O                       ORHOLD              61
     O          E            ROUTE2         2
     O                                           19 'OUTPUT QUEUE:'
     O               08      OROUTQ              30
     O                  09   QQQUE               30
     O                                           37 'FORMS:'
     O                       ORFORM              48
     O                                           56 'SAVE:'
     O                       ORSAVE              61
     O          E            ROUTE2         1
     O                                           20 'DATE RANGE   :'
     O                       XXFRDT              32 '  /  /    '
     O                                           34 '-'
     O                       XXTODT              45 '  /  /    '
     O          E            ROUTE2         1
     O                                           20 'REPORT TYPE  :'
     O                       QQTYPE              23
      *** BEG CHG ***** 12/02/09 **************************************
     O******* E  1     77      ROUTE2
     O          E            ROUTE2         1
      *** END CHG ***** 12/02/09 **************************************
     O                                           20 'VALUED AS OF :'
     O                       XXVLDT              32 '  /  /    '
     O          E            ROUTE2         1
     O                                           20 'DET/SUMM/BOTH:'
     O                       QQPRNT              23
      **** BEGIN ADD **** 02/28/01 ************************************
     O          E            ROUTE2         1
     O                                           28 'EXCLUDE EXPENSE CONST:'
     O                       QQPRTD              31
      **** BEGIN ADD **** 10/22/02 ************************************
     O          E            ROUTE2         1
     O                                           28 'SORT BY STATE . . . .:'
     O                       QQSRST              31
     O          E            ROUTE2         1
     O                                           28 'TYPE OF OUTPUT  . . .:'
     O                       QQPFB               31
      **** END   ADD **** 10/22/02 ************************************
      **** END   ADD **** 02/28/01 ************************************
      *** BEG CHG ***** 12/02/09 **************************************
     O********E** 60           ROUTE2
     O***********                        23 '(C) COPYRIGHT PROGRESSI'
     O***********                      +000 'VE DATA SOLUTIONS, INC.,'
     O***********              *YEAR   +001
     O          E            ROUTE2           59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE2      1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
      *** END CHG ***** 12/02/09 **************************************
     O          E            ERRHD          2 04
     O                       DSTIME              08 '  :  :  '
     O                       SCCONM              85
     O                       DSDATE        Y    132
     O          E            ERRHD          1
     O                       Q1PGM               10
     O                       XXTTL1              82
     O                       PAGE2         Z    132
     O                                          128 'PAGE:'
     O          E            ERRHD          2
      *** BEG CHG ***** 12/02/09 **************************************
     O***********     N77                62 'CALENDAR DATE RANGE:'
     O***********      77                62 'POLICY DATE RANGE:'
     O                       XXDRNV              62
      *** END CHG ***** 12/02/09 **************************************
     O                       XXFRDT              73 '  /  /    '
     O                                           75 '-'
     O                       XXTODT              86 '  /  /    '
      *** BEG CHG ***** 12/02/09 **************************************
     O***********      77               102 'VALUED AS OF:'
     O***********      77      XXVLDT   113 '  /  /    '
     O                                          102 'VALUED AS OF:'
     O                       XXVLDT             113 '  /  /    '
      *** END CHG ***** 12/02/09 **************************************
     O          E            ERRHD          2
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
      **** BEGIN ADD **** 02/28/01 ************************************
     O                                           99 'EXCLUDE EXPENSE CONST:'
     O                       QQPRTD             102
      **** END   ADD **** 02/28/01 ************************************
     O          E            ERRHD          2
     O                                           56 'CANCELLED EMPLOYERS WITH'
     O                                           81 'NO MATCHING CANCELLATION'
     O                                           96 'HISTORY RECORD'
     O          E            ERRHD          0
     O                                           19 '___________________'
     O                                           31 '__________'
     O                                           43 '__________'
     O                                           71 '________________________'
     O                                           87 '________________'
     O          E            ERRHD          1
     O                                           19 'POLICY NUMBER'
     O                                           31 'EFF DATE'
     O                                           43 'CX DATE'
     O                                           51 'NAME'
     O          E            ERR            1
     O                       ELEMP#               9
     O                                           10 '-'
     O                       ELDIV               15
     O                                           16 '-'
     O                       ELFYR               19
     O                       XXPFDT              31 '  /  /    '
     O                       XXCXDT              43 '  /  /    '
     O                       EMNAME              87
      *** BEG ADD ***** 06/03/10 ****************************
     O          E            NOERR          1
     O                       NE(1)               66
      *** BEG ADD ***** 06/03/10 ****************************
      *** BEG ADD 07/21/04 **********************************
     OQSYSPRT7  E            ARRHD          2 06
     O                                              'GRP'
     O                                         +  2 ' EMPLOYER'
     O                                         +  2 '  DIV'
     O                                         +  2 'FYR'
     O                                         +  2 'ACT'
     O                                         +  3 'FROMDT '
     O                                         +  2 'TODATE '
     O                                         +  2 'INFORC PREM'
     O                                         +  4 'INF MAN PRM'
     O                                         +  4 'WRT AN PREM'
     O                                         +  4 'WRT MAN PRM'
     O                                         +  4 'WRT NRM PRM'
     O                                         +  4 ' INF TERROR'
     O                                         +  4 ' WRT TERROR'
     O                                         +  4 '  EXP CONST'
     O                                         +  4 '  INF CONST'
     O                                         +  2 'DAYS'
     O          E            ARRDT          1
     O                       SVFND         4
     O                       SVEMP#        4   +  2
     O                       SVDIV         4   +  2
     O                       SVFYR         4   +  2
     O                       XACT(I)           +  2
     O                       FRDT(I)           +  3
     O                       TODT(I)           +  2
     O                       INPR(I)       L   +  2
     O                       IMPR(I)       L   +  2
     O                       WAPR(I)       L   +  2
     O                       WMPR(I)       L   +  2
     O                       WNMP(I)       L   +  2
     O                       ITER(I)       L   +  2
     O                       WTER(I)       L   +  2
     O                       PRTD(I)       L   +  2
     O                       INEX(I)       L   +  2
     O                       DAYS(I)           +  2
     O          E            SPACE          1
      *** END ADD 07/21/04 **********************************
      *** beg add ***** 06/21/16 **************************************
      *==============================================================
     p* Procedure name: WrtRptCriteria
     p* Purpose:        Writes report parameters to workfile
     p* Returns:
     p WrtRptCriteria  b
     d WrtRptCriteria  pi
      /free
       EOxmlfile = xmlFile;
       EOrfrdt   = %int(qqFrDt);
       EOrtodt   = %int(qqToDt);
       EOrvldt   = %int(qqVlDt);
       EOexc     = QQprtd;
       EOstatnm  = statNm;
       //*** begin add ***  08/05/16  *********************************
       EOEmpNam  = EMNAME + EMNAM2;
       EOEmpFed  = EMFDNM + EMFDN2;
       //*** end add *****  08/05/16  *********************************
      /end-free
     p WrtRptCriteria  e
      *** end add ***** 06/21/16 **************************************
**  MD - DAYS IN EACH MONTH
312831303130313130313031
**  NE
NO RECORDS MET SELECTION CRITERIA
